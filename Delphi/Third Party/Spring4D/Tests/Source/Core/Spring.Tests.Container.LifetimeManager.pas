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

unit Spring.Tests.Container.LifetimeManager;

interface

uses
  Classes,
  TestFramework,
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Container.Core,
  Spring.Container.LifetimeManager;

type
  TMockContext = class(TInterfacedObject, IContainerContext)
  public
    function GetComponentBuilder: IComponentBuilder;
    function GetComponentRegistry: IComponentRegistry;
    function GetDependencyResolver: IDependencyResolver;
    function GetInjectionFactory: IInjectionFactory;
    function GetServiceResolver: IServiceResolver;
  public
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const name: string): Boolean; overload;
    function CreateLifetimeManager(const model: TComponentModel): ILifetimeManager;
    procedure AddExtension(const extension: IContainerExtension);
    property ComponentBuilder: IComponentBuilder read GetComponentBuilder;
    property ComponentRegistry: IComponentRegistry read GetComponentRegistry;
    property DependencyResolver: IDependencyResolver read GetDependencyResolver;
    property InjectionFactory: IInjectionFactory read GetInjectionFactory;
    property ServiceResolver: IServiceResolver read GetServiceResolver;
  end;

  TMockActivator = class(TInterfaceBase, IComponentActivator, IInterface)
  private
    fModel: TComponentModel;
  public
    constructor Create(model: TComponentModel);
    function CreateInstance: TValue; overload;
    function CreateInstance(const resolver: IDependencyResolver): TValue; overload;
    property Model: TComponentModel read fModel;
  end;

  TMockObject = class
  end;

  TMockComponent = class(TComponent, IInterface)
{$IFNDEF AUTOREFCOUNT}
  private class var
    fFreed : Boolean;
{$ENDIF}
  private
    fRefCount: Integer;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

//  [Ignore]
  TLifetimeManagerTestCase = class abstract(TTestCase)
  protected
    fContainerContext: IContainerContext;
    fContext: TRttiContext;
    fLifetimeManager: ILifetimeManager;
    fModel: TComponentModel;
    fActivator: TMockActivator;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestSingletonLifetimeManager = class(TLifetimeManagerTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReferences;
  end;

  TTestRefCounting = class(TTestCase)
  protected
    fContainerContext: IContainerContext;
    fContext: TRttiContext;
    fLifetimeManager: ILifetimeManager;
    fModel: TComponentModel;
    fActivator: TMockActivator;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReferences;
  end;

  TTestTransientLifetimeManager = class(TLifetimeManagerTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReferences;
  end;

  TTestPerThreadLifetimeManager = class(TLifetimeManagerTestCase)

  end;

implementation

uses
  Spring.Services;

{ TLifetimeManagerTestCase }

procedure TLifetimeManagerTestCase.SetUp;
begin
  inherited;
  fContainerContext := TMockContext.Create;
  fContext := TRttiContext.Create;
  fModel := TComponentModel.Create(fContainerContext, fContext.GetType(TMockObject).AsInstance);
  fActivator := TMockActivator.Create(fModel);
  fModel.ComponentActivator := fActivator;
end;

procedure TLifetimeManagerTestCase.TearDown;
begin
  fModel.Free;
  fActivator.Free;
  fContext.Free;
  fContainerContext := nil;
  inherited;
end;

{ TTestSingletonLifetimeManager }

procedure TTestSingletonLifetimeManager.SetUp;
begin
  inherited;
  fLifetimeManager := TSingletonLifetimeManager.Create(fModel);
end;

procedure TTestSingletonLifetimeManager.TearDown;
begin
  fLifetimeManager := nil;
  inherited;
end;

procedure TTestSingletonLifetimeManager.TestReferences;
var
  obj1, obj2: TObject;
begin
  obj1 := fLifetimeManager.GetInstance(nil).AsObject;
  obj2 := fLifetimeManager.GetInstance(nil).AsObject;
  try
    CheckIs(obj1, TMockObject, 'obj1');
    CheckIs(obj2, TMockObject, 'obj2');
    CheckSame(obj1, obj2);
    CheckSame(fActivator.Model, fModel);
  finally
    fLifetimeManager.ReleaseInstance(obj1);
    fLifetimeManager.ReleaseInstance(obj2);
  end;
end;

{ TTestTransientLifetimeManager }

procedure TTestTransientLifetimeManager.SetUp;
begin
  inherited;
  fLifetimeManager := TTransientLifetimeManager.Create(fModel);
end;

procedure TTestTransientLifetimeManager.TearDown;
begin
  fLifetimeManager := nil;
  inherited;
end;

procedure TTestTransientLifetimeManager.TestReferences;
var
  obj1, obj2: TObject;
begin
  obj1 := fLifetimeManager.GetInstance(nil).AsObject;
  obj2 := fLifetimeManager.GetInstance(nil).AsObject;
  try
    CheckIs(obj1, TMockObject, 'obj1');
    CheckIs(obj2, TMockObject, 'obj2');
    CheckTrue(obj1 <> obj2);
    CheckSame(fActivator.Model, fModel);
  finally
    fLifetimeManager.ReleaseInstance(obj1);
    fLifetimeManager.ReleaseInstance(obj2);
  end;
end;

{ TMockComponentActivator }

constructor TMockActivator.Create(
  model: TComponentModel);
begin
  inherited Create;
  fModel := model;
end;

function TMockActivator.CreateInstance: TValue;
begin
  Result := fModel.ComponentType.AsInstance.MetaclassType.Create;
end;

function TMockActivator.CreateInstance(
  const resolver: IDependencyResolver): TValue;
begin
  Result := CreateInstance;
end;

{ TMockContext }

procedure TMockContext.AddExtension(const extension: IContainerExtension);
begin
  raise Exception.Create('AddExtension');
end;

function TMockContext.CreateLifetimeManager(const model: TComponentModel): ILifetimeManager;
begin
  raise Exception.Create('CreateLifetimeManager');
end;

function TMockContext.GetComponentBuilder: IComponentBuilder;
begin
  raise Exception.Create('GetComponentBuilder');
end;

function TMockContext.GetComponentRegistry: IComponentRegistry;
begin
  raise Exception.Create('GetComponentRegistry');
end;

function TMockContext.GetDependencyResolver: IDependencyResolver;
begin
  raise Exception.Create('GetDependencyResolver');
end;

function TMockContext.GetInjectionFactory: IInjectionFactory;
begin
  raise Exception.Create('GetInjectionFactory');
end;

function TMockContext.GetServiceResolver: IServiceResolver;
begin
  raise Exception.Create('GetServiceResolver');
end;

function TMockContext.HasService(const name: string): Boolean;
begin
  raise Exception.Create('HasService');
end;

function TMockContext.HasService(serviceType: PTypeInfo): Boolean;
begin
  raise Exception.Create('HasService');
end;

{ TMockComponent }

function TMockComponent._AddRef: Integer;
begin
  Inc(fRefCount);
{$IFNDEF AUTOREFCOUNT}
  Result := fRefCount;
{$ELSE}
  Result := inherited __ObjAddRef;
{$ENDIF}
end;

function TMockComponent._Release: Integer;
begin
  Dec(fRefCount);
{$IFNDEF AUTOREFCOUNT}
  Result := fRefCount;
  if Result = 0 then begin
    fFreed := true;
    Destroy;
  end;
{$ELSE}
  Result := inherited __ObjRelease;
{$ENDIF}
end;

{ TTestRefCounting }

procedure TTestRefCounting.SetUp;
begin
  inherited;
  fContainerContext := TMockContext.Create;
  fContext := TRttiContext.Create;
  fModel := TComponentModel.Create(fContainerContext, fContext.GetType(TMockComponent).AsInstance);
  fModel.RefCounting := TRefCounting.True;
  fActivator := TMockActivator.Create(fModel);
  fModel.ComponentActivator := fActivator;
  fLifetimeManager := TSingletonLifetimeManager.Create(fModel);
end;

procedure TTestRefCounting.TearDown;
begin
  fLifetimeManager := nil;
  fModel.Free;
  fActivator.Free;
  fContext.Free;
  fContainerContext := nil;
  inherited;
end;

procedure TTestRefCounting.TestReferences;
var
  obj: TObject;
  intf: IInterface;
  val: TValue;
begin
  fLifetimeManager := TSingletonLifetimeManager.Create(fModel);
{$IFNDEF AUTOREFCOUNT}
  TMockComponent.fFreed := false;
{$ENDIF}
  val := fLifetimeManager.GetInstance(nil);
  obj := val.AsObject;
{$IFDEF AUTOREFCOUNT}
  val := val.Empty; //Clear the TValue so that it doesn't keep holding reference count to obj
{$ENDIF}
  CheckNotNull(obj, 'returned object must not be nil');
  CheckTrue(Supports(obj, IInterface, intf), 'interface not supported');
  CheckIs(obj, TMockComponent, 'invalid object returned: ' + obj.ClassName);
  CheckEquals(2, TMockComponent(obj).fRefCount, 'invalid reference count');
  intf := nil;
  CheckEquals(1, TMockComponent(obj).fRefCount, 'invalid reference count');
  fLifetimeManager := nil;
{$IFNDEF AUTOREFCOUNT}
  //Check that reference count reached zero
  CheckTrue(TMockComponent.fFreed, 'invalid reference count');
{$ELSE}
  //Since automatic reference countin is in place, the object isn't destroyed
  //and we can safely test our own reference count
  CheckEquals(0, TMockComponent(obj).fRefCount, 'invalid reference count');
  CheckEquals(1, obj.RefCount, 'invalid reference count');
  obj := nil;
{$ENDIF}
end;

end.
