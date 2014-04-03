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

unit Spring.Container.Resolvers;

{$I Spring.inc}

interface

uses
  Rtti,
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TResolver = class(TInterfacedObject, IResolver)
  private
    fContext: IContainerContext;
    fRegistry: IComponentRegistry;
    fOnResolve: IEvent<TResolveEvent>;
    procedure DoResolve(var instance: TValue);
    function GetOnResolve: IEvent<TResolveEvent>;
  protected
    procedure ConstructValue(typeInfo: PTypeInfo; const instance: TValue; out value: TValue);

    property Context: IContainerContext read fContext;
    property Registry: IComponentRegistry read fRegistry;
  public
    constructor Create(const context: IContainerContext; const registry: IComponentRegistry);
    destructor Destroy; override;

    property OnResolve: IEvent<TResolveEvent> read GetOnResolve;
  end;

  TDependencyResolver = class(TResolver, IDependencyResolver, IInterface)
  private
    fLock: TCriticalSection;
    fDependencyTypes: IList<TRttiType>;
  protected
    procedure CheckCircularDependency(dependency: TRttiType);
    function GetEligibleModel(dependency: TRttiType; const argument: TValue): TComponentModel;
  public
    constructor Create(const context: IContainerContext; const registry: IComponentRegistry);
    destructor Destroy; override;

    function CanResolveDependency(dependency: TRttiType): Boolean; overload; virtual;
    function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean; overload; virtual;
    function ResolveDependency(dependency: TRttiType): TValue; overload; virtual;
    function ResolveDependency(dependency: TRttiType; const argument: TValue): TValue; overload; virtual;

    function ResolveLazyDependency(dependency: TRttiType; argument: TValue): TValue;
    function ResolveManyDependency(dependency: TRttiType; argument: TValue): TValue;

    function CanResolveDependencies(const dependencies: TArray<TRttiType>): Boolean; overload; virtual;
    function CanResolveDependencies(const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean; overload; virtual;
    function ResolveDependencies(const dependencies: TArray<TRttiType>): TArray<TValue>; overload; virtual;
    function ResolveDependencies(const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): TArray<TValue>; overload; virtual;

    function CanResolveDependencies(const Inject: IInjection): Boolean; overload; virtual;
    function CanResolveDependencies(const Inject: IInjection; const arguments: TArray<TValue>): Boolean; overload; virtual;
    function ResolveDependencies(const Inject: IInjection): TArray<TValue>; overload; virtual;
    function ResolveDependencies(const Inject: IInjection; const arguments: TArray<TValue>): TArray<TValue>; overload; virtual;
  end;

  TServiceResolver = class(TResolver, IServiceResolver, IInterface)
  protected
    function InternalResolve(const model: TComponentModel; serviceType: PTypeInfo;
      const resolver: IDependencyResolver): TValue;
    function InternalResolveLazy(model: TComponentModel; serviceType: PTypeInfo;
      resolver: IDependencyResolver): TValue;
  public
    function CanResolve(serviceType: PTypeInfo): Boolean; overload;
    function CanResolve(const name: string): Boolean; overload;
    function Resolve(serviceType: PTypeInfo): TValue; overload;
    function Resolve(serviceType: PTypeInfo; const resolverOverride: IResolverOverride): TValue; overload;
    function Resolve(const name: string): TValue; overload;
    function Resolve(const name: string; const resolverOverride: IResolverOverride): TValue; overload;
    function ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
  end;

  TResolverOverride = class(TInterfacedObject, IResolverOverride, IInterface)
  public
    function GetResolver(context: IContainerContext): IDependencyResolver; virtual; abstract;
  end;

  TParameterOverride = class(TResolverOverride)
  private
    fName: string;
    fValue: TValue;

    type
      TResolver = class(TDependencyResolver)
      private
        fName: string;
        fValue: TValue;
        fInject: IInjection;
      public
        constructor Create(const context: IContainerContext; const registry: IComponentRegistry;
          const name: string; const value: TValue);

        function CanResolveDependencies(const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean; override;
        function ResolveDependencies(const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): TArray<TValue>; override;

        function CanResolveDependencies(const Inject: IInjection): Boolean; override;
        function ResolveDependencies(const Inject: IInjection): TArray<TValue>; override;
      end;
  public
    constructor Create(const name: string; const value: TValue);

    function GetResolver(context: IContainerContext): IDependencyResolver; override;
  end;

  TOrderedParametersOverride = class(TResolverOverride)
  private
    fArguments: TArray<TValue>;

    type
      TResolver = class(TDependencyResolver)
      private
        fArguments: TArray<TValue>;
      public
        constructor Create(const context: IContainerContext; const registry: IComponentRegistry;
          arguments: TArray<TValue>);

        function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean; override;

        function CanResolveDependencies(const Inject: IInjection): Boolean; override;
        function ResolveDependencies(const Inject: IInjection): TArray<TValue>; override;
      end;
  public
    constructor Create(const arguments: array of TValue);

    function GetResolver(context: IContainerContext): IDependencyResolver; override;
  end;

  TDependencyOverride = class(TResolverOverride)
  private
    fTypeInfo: PTypeInfo;
    fValue: TValue;

    type
      TResolver = class(TDependencyResolver)
      private
        fTypeInfo: PTypeInfo;
        fValue: TValue;
      public
        constructor Create(const context: IContainerContext; const registry: IComponentRegistry;
          typeInfo: PTypeInfo; const value: TValue);

        function CanResolveDependency(dependency: TRttiType; const argument: TValue): Boolean; override;
        function ResolveDependency(dependency: TRttiType; const argument: TValue): TValue; override;
      end;
  public
    constructor Create(typeInfo: PTypeInfo; const value: TValue);

    function GetResolver(context: IContainerContext): IDependencyResolver; override;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Spring.Container.ResourceStrings,
  Spring.Events.Base,
  Spring.Helpers,
  Spring.Reflection;


{$REGION 'TResolveEventImpl'}

type
  TResolveEventImpl = class(TEventBase, IEvent<TResolveEvent>)
  private
    function GetInvoke: TResolveEvent;
    procedure Add(handler: TResolveEvent);
    procedure Remove(handler: TResolveEvent);
    procedure ForEach(const action: TAction<TResolveEvent>);

    procedure InternalInvoke(Sender: TObject; var instance: TValue);
  public
    constructor Create;
  end;

constructor TResolveEventImpl.Create;
begin
  inherited;
  fInvoke.Code := @TResolveEventImpl.InternalInvoke;
  fInvoke.Data := Self;
end;

procedure TResolveEventImpl.Add(handler: TResolveEvent);
begin
  inherited Add(TMethod(handler));
end;

procedure TResolveEventImpl.ForEach(const action: TAction<TResolveEvent>);
var
  i: Integer;
begin
  for i := 0 to Handlers.Count - 1 do
    action(TResolveEvent(Handlers.Items[i]));
end;

function TResolveEventImpl.GetInvoke: TResolveEvent;
begin
  Result := TResolveEvent(inherited Invoke);
end;

procedure TResolveEventImpl.InternalInvoke(Sender: TObject;
  var instance: TValue);
var
  i: Integer;
begin
  if Enabled then
    for i := 0 to Handlers.Count - 1 do
      TResolveEvent(Handlers.Items[i])(Sender, instance);
end;

procedure TResolveEventImpl.Remove(handler: TResolveEvent);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


{$REGION 'TResolver'}

constructor TResolver.Create(const context: IContainerContext;
  const registry: IComponentRegistry);
begin
  Guard.CheckNotNull(context, 'context');
  Guard.CheckNotNull(registry, 'registry');
  inherited Create;
  fContext := context;
  fRegistry := registry;
  fOnResolve := TResolveEventImpl.Create;
end;

destructor TResolver.Destroy;
begin
  fOnResolve := nil;
  inherited;
end;

procedure TResolver.DoResolve(var instance: TValue);
begin
  fOnResolve.Invoke(Self, instance);
end;

function TResolver.GetOnResolve: IEvent<TResolveEvent>;
begin
  Result := fOnResolve;
end;

procedure TResolver.ConstructValue(typeInfo: PTypeInfo; const instance: TValue;
  out value: TValue);
var
  localInterface: Pointer;
begin
  Guard.CheckFalse(instance.IsEmpty, 'instance should not be empty.');

  case typeInfo.Kind of
    tkClass, tkRecord:
    begin
      value := instance;
    end;
    tkInterface:
    begin
      if instance.IsObject then
      begin
        instance.AsObject.GetInterface(GetTypeData(typeInfo).Guid, localInterface);
      end
      else
      begin
        if TType.IsDelegate(typeInfo) then
        begin
          localInterface := nil;
          IInterface(localInterface) := instance.AsInterface;
        end
        else
        begin
          instance.AsInterface.QueryInterface(GetTypeData(typeInfo).Guid, localInterface);
        end;
      end;
      TValue.MakeWithoutCopy(@localInterface, typeInfo, value);
    end;
  else
    value := TValue.Empty;
  end;

  DoResolve(value);
end;

{$ENDREGION}


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const context: IContainerContext;
  const registry: IComponentRegistry);
begin
  inherited Create(context, registry);
  fDependencyTypes := TCollections.CreateList<TRttiType>(False);
  fLock := TCriticalSection.Create;
end;

destructor TDependencyResolver.Destroy;
begin
  fLock.Free;
  inherited;
end;

function TDependencyResolver.GetEligibleModel(dependency: TRttiType;
  const argument: TValue): TComponentModel;
var
  name: string;
begin
  if argument.IsEmpty then
  begin
    if not Registry.HasService(dependency.Handle) then
    begin
      if dependency.IsClassOrInterface and not TType.IsLazy(dependency.Handle) then
      begin
        raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
      end;
      Result := nil;
    end
    else
    begin
      Result := Registry.FindDefault(dependency.Handle);
      if not Assigned(Result) then
      begin
        raise EUnsatisfiedDependencyException.CreateResFmt(
          @SUnsatisfiedDependency, [dependency.Name]);
      end;
    end;
  end
  else
  begin
    name := argument.AsString;
    Result := Registry.FindOne(name);
    if not Assigned(Result) then
    begin
      if TType.IsLazy(dependency.Handle) then
        Exit;
      raise EResolveException.CreateResFmt(@SInvalidServiceName, [name]);
    end;
    if not Result.HasService(dependency.Handle) then
    begin
      if not TType.IsLazy(dependency.Handle) then
        raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
      Result := nil;
    end;
  end;
end;

procedure TDependencyResolver.CheckCircularDependency(dependency: TRttiType);
begin
  Guard.CheckNotNull(dependency, 'dependency');

  if fDependencyTypes.Contains(dependency) then
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected, [dependency.Name]);
end;

function TDependencyResolver.CanResolveDependency(
  dependency: TRttiType): Boolean;
begin
  Result := CanResolveDependency(dependency, TValue.Empty);
end;

function TDependencyResolver.CanResolveDependency(dependency: TRttiType;
  const argument: TValue): Boolean;
var
  serviceName: string;
  serviceType: PTypeInfo;
  lazyType: TRttiType;
  model: TComponentModel;
begin
  if dependency.IsClassOrInterface or dependency.IsRecord then
  begin
    if argument.IsEmpty then
    begin
      Result := Registry.HasDefault(dependency.Handle) or TType.IsLazy(dependency.Handle);
    end
    else
    begin
      Result := argument.IsType<string>;
      if Result then
      begin
        serviceName := argument.AsString;
        model := Registry.FindOne(serviceName);
        Result := Assigned(model);
        if Result then
        begin
          serviceType := model.Services[serviceName];
          Result := serviceType = dependency.Handle;
          if not Result and TType.IsLazy(dependency.Handle) then
          begin
            lazyType := TType.FindType(TType.GetLazyTypeName(dependency.Handle));
            Result := Assigned(lazyType) and (serviceType = lazyType.Handle);
          end;
        end;
      end;
    end;
  end
  else
    Result := argument.IsEmpty or argument.IsType(dependency.Handle);
end;

function TDependencyResolver.ResolveDependency(dependency: TRttiType): TValue;
begin
  Result := ResolveDependency(dependency, TValue.Empty);
end;

function TDependencyResolver.ResolveDependency(dependency: TRttiType;
  const argument: TValue): TValue;
var
  model: TComponentModel;
  instance: TValue;
begin
  Guard.CheckNotNull(dependency, 'dependency');

  if dependency.IsDynamicArray then
  begin
    Result := ResolveManyDependency(dependency, argument);
    Exit;
  end;

  if not (dependency.IsClassOrInterface or dependency.IsRecord)
    or (argument.Kind in [tkClass, tkInterface, tkRecord]) then
  begin
    Result := argument;
{$IFDEF DELPHI2010}
    if Result.IsEmpty then
      TValue.Make(nil, dependency.Handle, Result);
{$ENDIF}
    Exit;
  end;

  fLock.Enter;
  try
    CheckCircularDependency(dependency);
    model := GetEligibleModel(dependency, argument);
    if not Assigned(model) then
    begin
      Result := ResolveLazyDependency(dependency, argument);
      Exit;
    end;
    fDependencyTypes.Add(dependency);
    try
      instance := model.LifetimeManager.GetInstance(Self);
    finally
      fDependencyTypes.Remove(dependency);
    end;
  finally
    fLock.Leave;
  end;

  ConstructValue(dependency.Handle, instance, Result);
end;

function TDependencyResolver.ResolveLazyDependency(dependency: TRttiType;
  argument: TValue): TValue;
var
  lazyKind: TLazyKind;
  name: string;
  modelType: TRttiType;
  valueFactoryObj: TFunc<TObject>;
  valueFactoryIntf: TFunc<IInterface>;
begin
  lazyKind := TType.GetLazyKind(dependency.Handle);
  if lazyKind = lkNone then
    Exit(argument);

  name := TType.GetLazyTypeName(dependency.Handle);
  modelType := TType.FindType(name);
  if not Assigned(modelType) or not CanResolveDependency(modelType, argument) then
  begin
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
  end;
  CheckCircularDependency(modelType);

  case modelType.TypeKind of
    tkClass:
    begin
      valueFactoryObj :=
        function: TObject
        begin
          Result := ResolveDependency(modelType, argument).AsObject;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<TObject>>(valueFactoryObj);
        lkRecord: Result := TValue.From<Lazy<TObject>>(Lazy<TObject>.Create(valueFactoryObj));
        lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(valueFactoryObj));
      end;
    end;
    tkInterface:
    begin
      valueFactoryIntf :=
        function: IInterface
        begin
          Result := ResolveDependency(modelType, argument).AsInterface;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<IInterface>>(valueFactoryIntf);
        lkRecord: Result := TValue.From<Lazy<IInterface>>(Lazy<IInterface>.Create(valueFactoryIntf));
        lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(valueFactoryIntf));
      end;
    end;
  else
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);
  end;

  TValueData(Result).FTypeInfo := dependency.Handle;
end;

function TDependencyResolver.ResolveManyDependency(dependency: TRttiType;
  argument: TValue): TValue;
var
  dependencyType: TRttiType;
  serviceType: PTypeInfo;
  lazyTypeName: string;
  models: IEnumerable<TComponentModel>;
  values: array of TValue;
  i: Integer;
  model: TComponentModel;
begin
  if not dependency.IsDynamicArray then
    raise EResolveException.CreateResFmt(@SCannotResolveDependency, [dependency.Name]);

  dependencyType := dependency.AsDynamicArray.ElementType;
  if TType.IsLazy(dependencyType.Handle) then
  begin
    lazyTypeName := TType.GetLazyTypeName(dependencyType.Handle);
    serviceType := TType.FindType(lazyTypeName).Handle;
  end
  else
    serviceType := dependencyType.Handle;
  models := fRegistry.FindAll(serviceType);
  SetLength(values, models.Count);
  i := 0;
  for model in models do
  begin
    values[i] := ResolveDependency(dependencyType, model.GetServiceName(serviceType));
    Inc(i);
  end;
  Result := TValue.FromArray(dependency.Handle, values);
end;

function TDependencyResolver.CanResolveDependencies(
  const dependencies: TArray<TRttiType>): Boolean;
begin
  Result := CanResolveDependencies(dependencies, nil);
end;

function TDependencyResolver.CanResolveDependencies(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean;
var
  dependency: TRttiType;
  i: Integer;
begin
  Result := True;
  if Length(dependencies) = Length(arguments) then
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      if not CanResolveDependency(dependency, arguments[i]) then
      begin
        Exit(False);
      end;
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for dependency in dependencies do
    begin
      if not CanResolveDependency(dependency, TValue.Empty) then
      begin
        Exit(False);
      end;
    end;
  end
  else
  begin
    Exit(False);
  end;
end;

function TDependencyResolver.ResolveDependencies(
  const dependencies: TArray<TRttiType>): TArray<TValue>;
begin
  Result := ResolveDependencies(dependencies, nil);
end;

function TDependencyResolver.ResolveDependencies(
  const dependencies: TArray<TRttiType>;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  dependency: TRttiType;
  hasArgument: Boolean;
  i: Integer;
begin
  hasArgument := Length(arguments) > 0;
  if hasArgument and (Length(arguments) <> Length(dependencies)) then
  begin
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  end;
  SetLength(Result, Length(dependencies));
  if hasArgument then
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := ResolveDependency(dependency, arguments[i]);
    end;
  end
  else
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := ResolveDependency(dependency, TValue.Empty);
    end;
  end;
end;

function TDependencyResolver.CanResolveDependencies(
  const Inject: IInjection): Boolean;
var
  arguments: TArray<TValue>;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  arguments := Inject.Model.GetInjectionArguments(Inject);
  Result := CanResolveDependencies(Inject, arguments);
end;

function TDependencyResolver.CanResolveDependencies(const Inject: IInjection;
  const arguments: TArray<TValue>): Boolean;
var
  dependencyTypes: TArray<TRttiType>;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  dependencyTypes := Inject.GetDependencies;
  Result := CanResolveDependencies(dependencyTypes, arguments);
end;

function TDependencyResolver.ResolveDependencies(
  const Inject: IInjection): TArray<TValue>;
var
  dependencyArguments: TArray<TValue>;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  dependencyArguments := Inject.Model.GetInjectionArguments(Inject);
  Result := ResolveDependencies(Inject, dependencyArguments);
end;

function TDependencyResolver.ResolveDependencies(const Inject: IInjection;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  dependencyTypes: TArray<TRttiType>;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  dependencyTypes := Inject.GetDependencies;
  Result := ResolveDependencies(dependencyTypes, arguments);
end;

{$ENDREGION}


{$REGION 'TServiceResolver'}

function TServiceResolver.CanResolve(serviceType: PTypeInfo): Boolean;
begin
  Result := Registry.HasService(serviceType);
end;

function TServiceResolver.CanResolve(const name: string): Boolean;
begin
  Result := Registry.HasService(name);
end;

function TServiceResolver.InternalResolve(const model: TComponentModel;
  serviceType: PTypeInfo; const resolver: IDependencyResolver): TValue;
var
  instance: TValue;
begin
  Guard.CheckNotNull(model, 'model');
  Guard.CheckNotNull(serviceType, 'serviceType');
  Guard.CheckNotNull(model.LifetimeManager, 'model.LifetimeManager');

  instance := model.LifetimeManager.GetInstance(resolver);
  ConstructValue(serviceType, instance, Result);
end;

function TServiceResolver.InternalResolveLazy(model: TComponentModel;
  serviceType: PTypeInfo; resolver: IDependencyResolver): TValue;
var
  lazyKind: TLazyKind;
  name: string;
  lazyType: PTypeInfo;
  valueFactoryObj: TFunc<TObject>;
  valueFactoryIntf: TFunc<IInterface>;
begin
  Guard.CheckNotNull(model, 'model');
  Guard.CheckNotNull(serviceType, 'serviceType');
  Guard.CheckNotNull(model.LifetimeManager, 'model.LifetimeManager');

  lazyKind := TType.GetLazyKind(serviceType);
  if lazyKind <> lkNone then
  begin
    name := TType.GetLazyTypeName(serviceType);
    lazyType := TType.FindType(TType.GetLazyTypeName(serviceType)).Handle;
  end;

  case lazyType.Kind of
    tkClass:
    begin
      valueFactoryObj :=
        function: TObject
        begin
          Result := InternalResolve(model, lazyType, resolver).AsObject;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<TObject>>(valueFactoryObj);
        lkRecord: Result := TValue.From<Lazy<TObject>>(Lazy<TObject>.Create(valueFactoryObj));
        lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(valueFactoryObj));
      end;
    end;
    tkInterface:
    begin
      valueFactoryIntf :=
        function: IInterface
        begin
          Result := InternalResolve(model, lazyType, resolver).AsInterface;
        end;

      case lazyKind of
        lkFunc: Result := TValue.From<TFunc<IInterface>>(valueFactoryIntf);
        lkRecord: Result := TValue.From<Lazy<IInterface>>(Lazy<IInterface>.Create(valueFactoryIntf));
        lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(valueFactoryIntf));
      end;
    end;
  end;

  TValueData(Result).FTypeInfo := serviceType;
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo): TValue;
begin
  Result := Resolve(serviceType, nil);
end;

function TServiceResolver.Resolve(serviceType: PTypeInfo;
  const resolverOverride: IResolverOverride): TValue;
var
  serviceName: string;
  isLazy: Boolean;
  lazyType: PTypeInfo;
  model: TComponentModel;
  resolver: IDependencyResolver;
begin
  isLazy := TType.IsLazy(serviceType);
  lazyType := serviceType;
  if isLazy then
    serviceType := TType.FindType(TType.GetLazyTypeName(serviceType)).Handle;

  serviceName := GetTypeName(serviceType);

  if not Registry.HasService(serviceType) then
  begin
    raise EResolveException.CreateResFmt(@SNoComponentRegistered, [serviceName]);
  end
  else
  begin
    model := Registry.FindDefault(serviceType);
    if not Assigned(model) then
    begin
      raise EUnsatisfiedDependencyException.CreateResFmt(
        @SUnsatisfiedDependency, [serviceName]);
    end;
  end;
  if Assigned(resolverOverride) then
    resolver := resolverOverride.GetResolver(Context)
  else
    resolver := Context.DependencyResolver;

  if isLazy then
    Result := InternalResolveLazy(model, lazyType, resolver)
  else
    Result := InternalResolve(model, serviceType, resolver);
end;

function TServiceResolver.Resolve(const name: string): TValue;
begin
  Result := Resolve(name, nil);
end;

function TServiceResolver.Resolve(const name: string;
  const resolverOverride: IResolverOverride): TValue;
var
  model: TComponentModel;
  serviceType: PTypeInfo;
  resolver: IDependencyResolver;
begin
  model := Registry.FindOne(name);
  if not Assigned(model) then
  begin
    raise EResolveException.CreateResFmt(@SInvalidServiceName, [name]);
  end;
  serviceType := model.GetServiceType(name);
  if Assigned(resolverOverride) then
    resolver := resolverOverride.GetResolver(Context)
  else
    resolver := Context.DependencyResolver;
  Result := InternalResolve(model, serviceType, resolver);
end;

function TServiceResolver.ResolveAll(serviceType: PTypeInfo): TArray<TValue>;
var
  isLazy: Boolean;
  models: IEnumerable<TComponentModel>;
  model: TComponentModel;
  i: Integer;
  modelType: PTypeInfo;
begin
  isLazy := TType.IsLazy(serviceType);
  if isLazy then
    modelType := TType.FindType(TType.GetLazyTypeName(serviceType)).Handle
  else
    modelType := serviceType;
  models := Registry.FindAll(modelType);
  SetLength(Result, models.Count);
  i := 0;
  for model in models do
  begin
    if isLazy then
      Result[i] := InternalResolveLazy(model, serviceType, Context.DependencyResolver)
    else
      Result[i] := InternalResolve(model, serviceType, Context.DependencyResolver);
    Inc(i);
  end;
end;

{$ENDREGION}


{$REGION 'TOrderedParametersOverride'}

constructor TOrderedParametersOverride.Create(const arguments: array of TValue);
var
  i: Integer;
begin
  SetLength(fArguments, Length(arguments));
  for i := Low(arguments) to High(arguments) do
    fArguments[i] := arguments[i];
end;

function TOrderedParametersOverride.GetResolver(context: IContainerContext): IDependencyResolver;
begin
  Result := TResolver.Create(context, context.ComponentRegistry, fArguments);
end;

{$ENDREGION}


{$REGION 'TOrderedParametersOverride.TResolver'}

constructor TOrderedParametersOverride.TResolver.Create(const context: IContainerContext;
  const registry: IComponentRegistry; arguments: TArray<TValue>);
begin
  inherited Create(context, registry);
  fArguments := arguments;
end;

function TOrderedParametersOverride.TResolver.CanResolveDependency(
  dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := argument.IsType(dependency.Handle);
end;

function TOrderedParametersOverride.TResolver.CanResolveDependencies(
  const Inject: IInjection): Boolean;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  Result := CanResolveDependencies(Inject, fArguments);
end;

function TOrderedParametersOverride.TResolver.ResolveDependencies(
  const Inject: IInjection): TArray<TValue>;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  if Inject.Target.IsConstructor then
    Result := fArguments
  else
    Result := fContext.DependencyResolver.ResolveDependencies(Inject);
end;

{$ENDREGION}


{$REGION 'TParameterOverride'}

constructor TParameterOverride.Create(const name: string; const value: TValue);
begin
  fName := name;
  fValue := value;
end;

function TParameterOverride.GetResolver(
  context: IContainerContext): IDependencyResolver;
begin
  Result := TResolver.Create(context, context.ComponentRegistry, fName, fValue);
end;

{$ENDREGION}


{$REGION 'TParameterOverride.TResolver'}

constructor TParameterOverride.TResolver.Create(
  const context: IContainerContext; const registry: IComponentRegistry;
  const name: string; const value: TValue);
begin
  inherited Create(context, registry);
  fName := name;
  fValue := value;
end;

function TParameterOverride.TResolver.CanResolveDependencies(
  const dependencies: TArray<TRttiType>; const arguments: TArray<TValue>): Boolean;
var
  dependency: TRttiType;
  i: Integer;
  parameters: TArray<TRttiParameter>;
begin
  Result := True;
  if Length(dependencies) = Length(arguments) then
  begin
    parameters := fInject.Target.AsMethod.GetParameters;
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      if SameText(parameters[i].Name, fName) then
        Continue;
      if not CanResolveDependency(dependency, arguments[i]) then
      begin
        Exit(False);
      end;
    end;
  end
  else if Length(arguments) = 0 then
  begin
    for dependency in dependencies do
    begin
      if not CanResolveDependency(dependency, TValue.Empty) then
      begin
        Exit(False);
      end;
    end;
  end
  else
  begin
    Exit(False);
  end;
end;

function TParameterOverride.TResolver.CanResolveDependencies(
  const Inject: IInjection): Boolean;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  fInject := Inject;
  Result := inherited;
end;

function TParameterOverride.TResolver.ResolveDependencies(
  const dependencies: TArray<TRttiType>;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  dependency: TRttiType;
  hasArgument: Boolean;
  i: Integer;
  parameters: TArray<TRttiParameter>;
begin
  hasArgument := Length(arguments) > 0;
  if hasArgument and (Length(arguments) <> Length(dependencies)) then
  begin
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  end;
  SetLength(Result, Length(dependencies));
  if hasArgument then
  begin
    parameters := fInject.Target.AsMethod.GetParameters;
    for i := 0 to High(dependencies) do
    begin
      if SameText(parameters[i].Name, fName) then
        Result[i] := fValue
      else
      begin
        dependency := dependencies[i];
        Result[i] := ResolveDependency(dependency, arguments[i]);
      end;
    end;
  end
  else
  begin
    for i := 0 to High(dependencies) do
    begin
      dependency := dependencies[i];
      Result[i] := ResolveDependency(dependency, TValue.Empty);
    end;
  end;
end;

function TParameterOverride.TResolver.ResolveDependencies(
  const Inject: IInjection): TArray<TValue>;
begin
  Guard.CheckNotNull(Inject, 'Inject');
  fInject := Inject;
  Result := inherited;
end;

{$ENDREGION}


{$REGION 'TDependencyOverride'}

constructor TDependencyOverride.Create(typeInfo: PTypeInfo;
  const value: TValue);
begin
  fTypeInfo := typeInfo;
  fValue := value;
end;

function TDependencyOverride.GetResolver(
  context: IContainerContext): IDependencyResolver;
begin
  Result := TResolver.Create(context, context.ComponentRegistry, fTypeInfo, fValue)
end;

{$ENDREGION}


{$REGION 'TDependencyOverride.TResolver'}

constructor TDependencyOverride.TResolver.Create(
  const context: IContainerContext; const registry: IComponentRegistry;
  typeInfo: PTypeInfo; const value: TValue);
begin
  inherited Create(context, registry);
  fTypeInfo := typeInfo;
  fValue := value;
end;

function TDependencyOverride.TResolver.CanResolveDependency(
  dependency: TRttiType; const argument: TValue): Boolean;
begin
  Result := dependency.Handle = fTypeInfo;
  if not Result then
    Result := fContext.DependencyResolver.CanResolveDependency(dependency, argument);
end;

function TDependencyOverride.TResolver.ResolveDependency(dependency: TRttiType;
  const argument: TValue): TValue;
begin
  if dependency.Handle = fTypeInfo then
    Result := fValue
  else
    Result := fContext.DependencyResolver.ResolveDependency(dependency, argument);
end;

{$ENDREGION}


end.
