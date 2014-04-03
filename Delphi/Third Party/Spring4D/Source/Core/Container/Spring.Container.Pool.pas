{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Container.Pool;

{$I Spring.inc}

interface

uses
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  IObjectPool = interface
    ['{E5842280-3750-46C0-8C91-0888EFFB0ED5}']
    procedure Initialize(const resolver: IDependencyResolver);
    function GetInstance(const resolver: IDependencyResolver): TObject;
    procedure ReleaseInstance(instance: TObject);
  end;

  IObjectPool<T> = interface(IObjectPool)
    function GetInstance(const resolver: IDependencyResolver): T;
    procedure ReleaseInstance(instance: T);
  end;

  IPoolableObjectFactory = interface(IComponentActivator)
    ['{56F9E805-A115-4E3A-8583-8D0B5462D98A}']
    function Validate(instance: TObject): Boolean;
    procedure Activate(instance: TObject);
    procedure Passivate(instance: TObject);
  end;

  IPoolableObjectFactory<T> = interface(IPoolableObjectFactory)
    function Validate(instance: T): Boolean;
    procedure Activate(instance: T);
    procedure Passivate(instance: T);
  end;

  TSimpleObjectPool = class(TInterfacedObject, IObjectPool)
  private
    fLock: TCriticalSection;
    fActivator: IComponentActivator;
    fMinPoolsize: Nullable<Integer>;
    fMaxPoolsize: Nullable<Integer>;
    fAvailableList: IQueue<TObject>;
    fActiveList: IList<TObject>;
    fInstances: IList<TObject>;
    fInitialized: Boolean;
  protected
    function AddNewInstance(const resolver: IDependencyResolver): TObject;
    procedure CollectInactiveInstances;
    function GetAvailableObject: TObject;
    procedure InstancesChanged(Sender: TObject; const item: TObject;
      action: TCollectionChangedAction);
    property MinPoolsize: Nullable<Integer> read fMinPoolsize;
    property MaxPoolsize: Nullable<Integer> read fMaxPoolsize;
  public
    constructor Create(const activator: IComponentActivator; minPoolSize, maxPoolSize: Integer);
    destructor Destroy; override;
    procedure Initialize(const resolver: IDependencyResolver); virtual;
    function GetInstance(const resolver: IDependencyResolver): TObject; virtual;
    procedure ReleaseInstance(instance: TObject); virtual;
  end;

implementation

uses
  SysUtils,
  Spring.Services;

type
  TInterfacedObjectAccess = class(TInterfacedObject);


{$REGION 'TSimpleObjectPool'}

constructor TSimpleObjectPool.Create(const activator: IComponentActivator;
  minPoolSize, maxPoolSize: Integer);
begin
  inherited Create;
  fActivator := activator;
  if minPoolSize > 0 then
  begin
    fMinPoolsize := minPoolSize;
  end;
  if maxPoolSize > 0 then
  begin
    fMaxPoolsize := maxPoolSize;
  end;
  fInstances := TCollections.CreateList<TObject>;
  fInstances.OnChanged.Add(InstancesChanged);
  fAvailableList := TCollections.CreateQueue<TObject>;
  fActiveList := TCollections.CreateList<TObject>;
  fLock := TCriticalSection.Create;
end;

destructor TSimpleObjectPool.Destroy;
begin
  fLock.Free;
  inherited;
end;

function TSimpleObjectPool.AddNewInstance(const resolver: IDependencyResolver): TObject;
var
  refCounted: IRefCounted;
begin
  Result := fActivator.CreateInstance(resolver).AsObject;
  if Result.InheritsFrom(TInterfacedObject) then
    TInterfacedObjectAccess(Result)._AddRef
  else if Supports(Result, IRefCounted, refCounted) then
    refCounted._AddRef;
  fInstances.Add(Result);
end;

procedure TSimpleObjectPool.CollectInactiveInstances;
var
  instance: TObject;
  refCounted: IRefCounted;
begin
  for instance in fInstances.ToArray do
  begin
    if instance.InheritsFrom(TInterfacedObject)
      and (TInterfacedObject(instance).RefCount = 1)
      and fActiveList.Contains(instance) then
    begin
      ReleaseInstance(instance);
    end else
    if Supports(instance, IRefCounted, refCounted)
      and (refCounted.RefCount = 2)
      and fActiveList.Contains(instance) then
    begin
      refCounted := nil;
      ReleaseInstance(instance);
    end;
  end;
end;

procedure TSimpleObjectPool.Initialize(const resolver: IDependencyResolver);
var
  i: Integer;
  instance: TObject;
begin
  if not fMinPoolsize.HasValue then
    Exit;
  for i := 0 to fMinPoolsize.Value - 1 do
  begin
    instance := AddNewInstance(resolver);
    fAvailableList.Enqueue(instance);
  end;
  fInitialized := True;
end;

procedure TSimpleObjectPool.InstancesChanged(Sender: TObject;
  const item: TObject; action: TCollectionChangedAction);
var
  refCounted: IRefCounted;
begin
  if action = caRemoved then
  begin
    if item.InheritsFrom(TInterfacedObject) then
      TInterfacedObjectAccess(item)._Release
    else if Supports(item, IRefCounted, refCounted) then
      refCounted._Release
    else
{$IFNDEF AUTOREFCOUNT}
      item.Free;
{$ELSE}
      item.DisposeOf;
{$ENDIF}
  end;
end;

function TSimpleObjectPool.GetAvailableObject: TObject;
begin
  Result := fAvailableList.Dequeue;
end;

function TSimpleObjectPool.GetInstance(const resolver: IDependencyResolver): TObject;
begin
  fLock.Acquire;
  try
    if not fInitialized then
      Initialize(resolver);

    if fAvailableList.IsEmpty then
      CollectInactiveInstances;

    if not fAvailableList.IsEmpty then
      Result := GetAvailableObject
    else
      Result := AddNewInstance(resolver);

    fActiveList.Add(Result);
  finally
    fLock.Release;
  end;
end;

procedure TSimpleObjectPool.ReleaseInstance(instance: TObject);
var
  recyclable: IRecyclable;
begin
  Guard.CheckNotNull(instance, 'instance');

  fLock.Acquire;
  try
    fActiveList.Remove(instance);

    if Supports(instance, IRecyclable, recyclable) then
      recyclable.Recycle;

    if not fMaxPoolsize.HasValue or (fAvailableList.Count < fMaxPoolsize.Value) then
      fAvailableList.Enqueue(instance)
    else
      fInstances.Remove(instance);
  finally
    fLock.Release;
  end;
end;

{$ENDREGION}


end.
