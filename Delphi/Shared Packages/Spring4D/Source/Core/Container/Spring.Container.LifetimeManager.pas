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

{$I Spring.inc}

unit Spring.Container.LifetimeManager;

interface

uses
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Container.Core,
  Spring.Container.Pool;

type
  TLifetimeManagerBase = class abstract(TInterfacedObject, ILifetimeManager)
  private
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fModel: TComponentModel;
    function GetActivator: IComponentActivator;
  protected
    procedure DoAfterConstruction(const instance: TValue); virtual;
    procedure DoBeforeDestruction(const instance: TValue); virtual;
  protected
    function TryGetInterfaceWithoutCopy(const instance: TValue; const IID: TGUID; out intf): Boolean;
    property ComponentActivator: IComponentActivator read GetActivator;
    property Model: TComponentModel read fModel;
  public
    constructor Create(const model: TComponentModel); virtual;
    function Resolve(const context: ICreationContext): TValue; overload; virtual; abstract;
    procedure Release(const instance: TValue); virtual; abstract;
  end;

  TLifetimeManagerClass = class of TLifetimeManagerBase;

  TSingletonLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstance: TFunc<TValue>;
  public
    destructor Destroy; override;
    function Resolve(const context: ICreationContext): TValue; override;
    procedure Release(const instance: TValue); override;
  end;

  TTransientLifetimeManager = class(TLifetimeManagerBase)
  public
    function Resolve(const context: ICreationContext): TValue; override;
    procedure Release(const instance: TValue); override;
  end;

  TSingletonPerThreadLifetimeManager = class(TLifetimeManagerBase)
  private
    fInstances: IDictionary<TThreadID, TFunc<TValue>>;
  protected
    procedure HandleValueChanged(sender: TObject; const item: TFunc<TValue>;
      action: TCollectionChangedAction);
    function CreateHolder(const instance: TValue): TFunc<TValue>; virtual;
  public
    constructor Create(const model: TComponentModel); override;
    function Resolve(const context: ICreationContext): TValue; override;
    procedure Release(const instance: TValue); override;
  end;

  TPooledLifetimeManager = class(TLifetimeManagerBase)
  private
    fPool: IObjectPool;
  public
    constructor Create(const model: TComponentModel); override;
    function Resolve(const context: ICreationContext): TValue; override;
    procedure Release(const instance: TValue); override;
  end;

implementation

uses
  Classes,
  Rtti,
  TypInfo,
  Spring.Container.Common,
  Spring.Container.ResourceStrings,
  Spring.Reflection;


{$REGION 'TLifetimeManagerBase'}

constructor TLifetimeManagerBase.Create(const model: TComponentModel);
begin
  Guard.CheckNotNull(model, 'model');
  inherited Create;
  fModel := model;
end;

function TLifetimeManagerBase.TryGetInterfaceWithoutCopy(const instance: TValue;
  const IID: TGUID; out intf): Boolean;

  function GetInterface(Self: TObject; const IID: TGUID; out Obj): Boolean;
  var
    interfaceEntry: PInterfaceEntry;
  begin
    interfaceEntry := Self.GetInterfaceEntry(IID);
    if Assigned(interfaceEntry) and (interfaceEntry.IOffset <> 0) then
    begin
      Pointer(Obj) := Pointer(NativeInt(Self) + InterfaceEntry.IOffset);
      Result := Pointer(Obj) <> nil;
    end
    else
      Result := Self.GetInterface(IID, Obj);
  end;

var
  localIntf: Pointer; // weak-reference
begin
  Guard.CheckFalse(instance.IsEmpty, 'instance should not be empty.');

  case instance.Kind of
    tkClass:
    begin
      Result := GetInterface(instance.AsObject, IInitializable, intf);
      Result := Result or (GetInterface(instance.AsObject, IInterface, localIntf)
        and (IInterface(localIntf).QueryInterface(IID, intf) = S_OK));
    end;
    tkInterface:
      Result := instance.AsInterface.QueryInterface(IID, intf) = S_OK;
  else
    Result := False;
  end
end;

procedure TLifetimeManagerBase.DoAfterConstruction(const instance: TValue);
var
  intf: Pointer;
begin
  if TryGetInterfaceWithoutCopy(instance, IInitializable, intf) then
    IInitializable(intf).Initialize;
end;

procedure TLifetimeManagerBase.DoBeforeDestruction(const instance: TValue);
var
  intf: Pointer;
begin
  if TryGetInterfaceWithoutCopy(instance, IDisposable, intf) then
    IDisposable(intf).Dispose;
end;

function TLifetimeManagerBase.GetActivator: IComponentActivator;
begin
  Result := fModel.ComponentActivator;
end;

{$ENDREGION}


{$REGION 'TSingletonLifetimeManager'}

destructor TSingletonLifetimeManager.Destroy;
begin
  Release(nil);
  inherited Destroy;
end;

function TSingletonLifetimeManager.Resolve(
  const context: ICreationContext): TValue;
var
  newInstance: TValue;
begin
  if not Assigned(fInstance) then
  begin
    newInstance := ComponentActivator.CreateInstance(context);
    fInstance := TValueHolder.Create(newInstance, Model.RefCounting);
    DoAfterConstruction(fInstance);
  end;
  Result := fInstance;
end;

procedure TSingletonLifetimeManager.Release(const instance: TValue);
var
  value: TValue;
begin
  if Assigned(fInstance) then
  begin
    value := fInstance;
    if not value.IsEmpty then
      DoBeforeDestruction(value);
    fInstance := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TTransientLifetimeManager'}

function TTransientLifetimeManager.Resolve(
  const context: ICreationContext): TValue;
begin
  Result := ComponentActivator.CreateInstance(context);
  DoAfterConstruction(Result);
end;

procedure TTransientLifetimeManager.Release(const instance: TValue);
begin
  Guard.CheckNotNull(instance, 'instance');
  DoBeforeDestruction(instance);
{$IFNDEF AUTOREFCOUNT}
  if instance.IsObject then
    instance.AsObject.Free;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TSingletonPerThreadLifetimeManager'}

constructor TSingletonPerThreadLifetimeManager.Create(const model: TComponentModel);
begin
  inherited Create(model);
  fInstances := TCollections.CreateDictionary<TThreadID, TFunc<TValue>>;
  fInstances.OnValueChanged.Add(HandleValueChanged);
end;

function TSingletonPerThreadLifetimeManager.CreateHolder(
  const instance: TValue): TFunc<TValue>;
begin
  Result := TValueHolder.Create(instance, Model.RefCounting);
end;

procedure TSingletonPerThreadLifetimeManager.HandleValueChanged(sender: TObject;
  const item: TFunc<TValue>; action: TCollectionChangedAction);
begin
  if action = caRemoved then
  begin
    DoBeforeDestruction(item);
  end;
end;

function TSingletonPerThreadLifetimeManager.Resolve(
  const context: ICreationContext): TValue;
var
  threadID: THandle;
  instance: TValue;
  holder: TFunc<TValue>;
begin
  threadID := TThread.CurrentThread.ThreadID;
  MonitorEnter(Self);
  try
    if not fInstances.TryGetValue(threadID, holder) then
    begin
      instance := ComponentActivator.CreateInstance(context);
      holder := CreateHolder(instance);
      fInstances.AddOrSetValue(threadID, holder);
      DoAfterConstruction(holder);
    end;
  finally
    MonitorExit(Self);
  end;
  Result := holder;
end;

procedure TSingletonPerThreadLifetimeManager.Release(const instance: TValue);
begin
end;

{$ENDREGION}


{$REGION 'TPooledLifetimeManager'}

constructor TPooledLifetimeManager.Create(const model: TComponentModel);

  procedure CheckPoolingSupported(const componentType: TRttiType);
  begin
    if not (componentType.IsInstance
      and (componentType.AsInstance.MetaclassType.InheritsFrom(TInterfacedObject)
      or Supports(componentType.AsInstance.MetaclassType, IRefCounted))) then
      raise ERegistrationException.CreateResFmt(@SPoolingNotSupported, [
        componentType.DefaultName]);
  end;

begin
  CheckPoolingSupported(model.ComponentType);
  inherited Create(model);
  fPool := TSimpleObjectPool.Create(
    model.ComponentActivator, model.MinPoolsize, model.MaxPoolsize);
end;

function TPooledLifetimeManager.Resolve(
  const context: ICreationContext): TValue;
begin
  Result := fPool.GetInstance(context);
  DoAfterConstruction(Result);
end;

procedure TPooledLifetimeManager.Release(const instance: TValue);
begin
  Guard.CheckNotNull(instance, 'instance');
  DoBeforeDestruction(instance);
  fPool.ReleaseInstance(instance.AsObject);
end;

{$ENDREGION}


end.
