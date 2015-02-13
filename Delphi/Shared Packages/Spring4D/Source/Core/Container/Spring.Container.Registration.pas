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

unit Spring.Container.Registration;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
{$IFDEF DELPHIXE}
  Spring.Reflection.Compatibility,
{$ENDIF}
  Spring.Container.Common,
  Spring.Container.Core;

type
  ///	<summary>
  ///	  TComponentRegistry
  ///	</summary>
  TComponentRegistry = class(TInterfacedObject, IComponentRegistry)
  private
    fKernel: IKernel;
    fModels: IList<TComponentModel>;
    fDefaultRegistrations: IDictionary<PTypeInfo, TComponentModel>;
    fUnnamedRegistrations: IMultiMap<PTypeInfo, TComponentModel>;
    fServiceTypeMappings: IMultiMap<PTypeInfo, TComponentModel>;
    fServiceNameMappings: IDictionary<string, TComponentModel>;
    fOnChanged: ICollectionChangedEvent<TComponentModel>;
    function GetOnChanged: ICollectionChangedEvent<TComponentModel>;
  protected
    procedure CheckIsNonGuidInterface(const serviceType: TRttiType);
{$IFDEF DELPHIXE_UP}
    procedure InternalRegisterFactory(const model: TComponentModel;
      const invokeEvent: TVirtualInterfaceInvokeEvent);
{$ENDIF}
    procedure RegisterUnnamed(const model: TComponentModel; serviceType: PTypeInfo);
    procedure Validate(const componentType, serviceType: TRttiType; var serviceName: string);
  public
    constructor Create(const kernel: IKernel);

    function RegisterComponent(componentTypeInfo: PTypeInfo): TComponentModel;
    procedure RegisterService(const model: TComponentModel; serviceType: PTypeInfo); overload;
    procedure RegisterService(const model: TComponentModel; serviceType: PTypeInfo;
      const serviceName: string); overload;
    procedure RegisterDefault(const model: TComponentModel; serviceType: PTypeInfo);
{$IFDEF DELPHIXE_UP}
    procedure RegisterFactory(const model: TComponentModel); overload;
    procedure RegisterFactory(const model: TComponentModel;
      const resolvedServiceName: string); overload;
{$ENDIF}
    procedure UnregisterAll;
    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(const serviceName: string): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const serviceName: string): Boolean; overload;
    function HasDefault(serviceType: PTypeInfo): Boolean;
    function FindOne(componentType: PTypeInfo): TComponentModel; overload;
    function FindOne(const serviceName: string): TComponentModel; overload;
    function FindOne(serviceType: PTypeInfo; const argument: TValue): TComponentModel; overload;
    function FindDefault(serviceType: PTypeInfo): TComponentModel;
    function FindAll: IEnumerable<TComponentModel>; overload;
    function FindAll(serviceType: PTypeInfo): IEnumerable<TComponentModel>; overload;
  end;

  /// <summary>
  ///   Internal helper for non-generic fluent style registration of a type.
  /// </summary>
  TRegistration = class sealed(TInterfacedObject, IRegistration)
  private
    fKernel: IKernel;
    fModel: TComponentModel;
    constructor Create(const kernel: IKernel; componentType: PTypeInfo);
  public
    function Implements(serviceType: PTypeInfo): IRegistration; overload;
    function Implements(serviceType: PTypeInfo; const serviceName: string): IRegistration; overload;

    function DelegateTo(const delegate: TActivatorDelegate): IRegistration; overload;

    {$REGION 'Typed Injections'}

    function InjectConstructor: IRegistration; overload;
    function InjectConstructor(const parameterTypes: array of PTypeInfo): IRegistration; overload;
    function InjectProperty(const propertyName: string): IRegistration; overload;
    function InjectMethod(const methodName: string): IRegistration; overload;
    function InjectMethod(const methodName: string; const parameterTypes: array of PTypeInfo): IRegistration; overload;
    function InjectField(const fieldName: string): IRegistration; overload;

    {$ENDREGION}

    {$REGION 'Named/Valued Injections'}

    function InjectConstructor(const arguments: array of TValue): IRegistration; overload;
    function InjectProperty(const propertyName: string; const value: TValue): IRegistration; overload;
    function InjectMethod(const methodName: string; const arguments: array of TValue): IRegistration; overload;
    function InjectField(const fieldName: string; const value: TValue): IRegistration; overload;

    {$ENDREGION}

    function AsSingleton(refCounting: TRefCounting = TRefCounting.Unknown): IRegistration;
    function AsSingletonPerThread(refCounting: TRefCounting = TRefCounting.Unknown): IRegistration;
    function AsTransient: IRegistration;
    function AsPooled(minPoolSize, maxPoolSize: Integer): IRegistration; {$IFDEF CPUARM}experimental;{$ENDIF}

    function PerResolve: IRegistration;

    function AsDefault: IRegistration; overload;
    function AsDefault(serviceType: PTypeInfo): IRegistration; overload;

{$IFDEF DELPHIXE_UP}
    function AsFactory: IRegistration; overload;
    function AsFactory(const resolvedServiceName: string): IRegistration; overload;
{$ENDIF}
  end;

  /// <summary>
  ///   Internal helper for generic fluent style registration of a type.
  /// </summary>
  TRegistration<T> = record
  private
    fRegistration: IRegistration;
    // prevent some bug with wrong reference counting in
    // records containing only one field of an interface type
    {$IFDEF DELPHI2010}{$HINTS OFF}fDummy: Pointer;{$ENDIF}
    constructor Create(const kernel: IKernel);
  public
    function Implements(serviceType: PTypeInfo): TRegistration<T>; overload;
    function Implements(serviceType: PTypeInfo; const serviceName: string): TRegistration<T>; overload;
    function Implements<TServiceType>: TRegistration<T>; overload;
    function Implements<TServiceType>(const serviceName: string): TRegistration<T>; overload;

    function DelegateTo(const delegate: TActivatorDelegate<T>): TRegistration<T>; overload;

  {$REGION 'Typed Injections'}

    function InjectConstructor: TRegistration<T>; overload;
    function InjectConstructor(const parameterTypes: array of PTypeInfo): TRegistration<T>; overload;
    function InjectProperty(const propertyName: string): TRegistration<T>; overload;
    function InjectMethod(const methodName: string): TRegistration<T>; overload;
    function InjectMethod(const methodName: string; const parameterTypes: array of PTypeInfo): TRegistration<T>; overload;
    function InjectField(const fieldName: string): TRegistration<T>; overload;

  {$ENDREGION}

  {$REGION 'Named/Valued Injections'}

    function InjectConstructor(const arguments: array of TValue): TRegistration<T>; overload;
    function InjectProperty(const propertyName: string; const value: TValue): TRegistration<T>; overload;
    function InjectMethod(const methodName: string; const arguments: array of TValue): TRegistration<T>; overload;
    function InjectField(const fieldName: string; const value: TValue): TRegistration<T>; overload;

  {$ENDREGION}

    function AsSingleton(refCounting: TRefCounting = TRefCounting.Unknown): TRegistration<T>;
    function AsSingletonPerThread(refCounting: TRefCounting = TRefCounting.Unknown): TRegistration<T>;
    function AsTransient: TRegistration<T>;
    function AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration<T>; {$IFDEF CPUARM}experimental;{$ENDIF}

    function PerResolve: TRegistration<T>;

    function AsDefault: TRegistration<T>; overload;
    function AsDefault(serviceType: PTypeInfo): TRegistration<T>; overload;
    function AsDefault<TServiceType>: TRegistration<T>; overload;

{$IFDEF DELPHIXE_UP}
    function AsFactory: TRegistration<T>; overload;
    function AsFactory(const resolvedServiceName: string): TRegistration<T>; overload;
{$ENDIF}
  end;

  /// <summary>
  ///   Provides both generic and non-generic fluent-style registration
  ///   methods.
  /// </summary>
  /// <remarks>
  ///   TRegistration(T) is defined as record and the constructors of
  ///   TRegistration and TRegistration(T) are private because they serve as
  ///   helpers to provide generic and non-generic fluent-style registration
  ///   with only necessary methods.
  /// </remarks>
  TRegistrationManager = class
  private
    fKernel: IKernel;
  public
    constructor Create(const kernel: IKernel);
    function RegisterType<TComponentType>: TRegistration<TComponentType>; overload;
    function RegisterType(componentType: PTypeInfo): IRegistration; overload;
  end;


implementation

uses
  SysUtils,
  TypInfo,
  Spring.Collections.Events,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.Container.Resolvers,
  Spring.Container.ResourceStrings,
  Spring.Reflection;


{$REGION 'TComponentRegistry'}

constructor TComponentRegistry.Create(const kernel: IKernel);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
{$ENDIF}

  inherited Create;
  fKernel := kernel;
  fOnChanged := TCollectionChangedEventImpl<TComponentModel>.Create;
  fModels := TCollections.CreateObjectList<TComponentModel>(True);
  fModels.OnChanged.Add(fOnChanged.Invoke);
  fDefaultRegistrations := TCollections.CreateDictionary<PTypeInfo, TComponentModel>;
  fDefaultRegistrations.OnValueChanged.Add(fOnChanged.Invoke);
  fUnnamedRegistrations := TCollections.CreateMultiMap<PTypeInfo, TComponentModel>;
  fUnnamedRegistrations.OnValueChanged.Add(fOnChanged.Invoke);
  fServiceTypeMappings := TCollections.CreateMultiMap<PTypeInfo, TComponentModel>;
  fServiceTypeMappings.OnValueChanged.Add(fOnChanged.Invoke);
  fServiceNameMappings := TCollections.CreateDictionary<string, TComponentModel>;
  fServiceNameMappings.OnValueChanged.Add(fOnChanged.Invoke);
end;

procedure TComponentRegistry.CheckIsNonGuidInterface(const serviceType: TRttiType);
begin
  if serviceType.IsInterface and not serviceType.AsInterface.HasGuid
    and not TType.IsDelegate(serviceType.Handle) then
    raise ERegistrationException.CreateResFmt(@SMissingGuid, [serviceType.DefaultName]);
end;

procedure TComponentRegistry.Validate(const componentType, serviceType: TRttiType;
  var serviceName: string);
begin
  CheckIsNonGuidInterface(serviceType);
  if not serviceType.IsAssignableFrom(componentType)
    and not componentType.IsInterface then
    raise ERegistrationException.CreateResFmt(@SIncompatibleTypes, [
      componentType.DefaultName, serviceType.DefaultName]);
  if serviceName = '' then
    serviceName := serviceType.DefaultName + '@' + componentType.DefaultName;
  if HasService(serviceName) then
    raise ERegistrationException.CreateResFmt(@SDuplicateServiceName, [serviceName]);
end;

procedure TComponentRegistry.UnregisterAll;
begin
  fOnChanged.Enabled := False;
  try
    fServiceNameMappings.Clear;
    fServiceTypeMappings.Clear;
    fDefaultRegistrations.Clear;
    fUnnamedRegistrations.Clear;
    fModels.Clear;
  finally
    fOnChanged.Enabled := True;
  end;
end;

procedure TComponentRegistry.RegisterService(const model: TComponentModel;
  serviceType: PTypeInfo);
begin
  RegisterService(model, serviceType, '');
end;

procedure TComponentRegistry.RegisterService(const model: TComponentModel;
  serviceType: PTypeInfo; const serviceName: string);
var
  internalServiceName: string;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(model, 'model');
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  internalServiceName := serviceName;
  Validate(model.ComponentType, TType.GetType(serviceType), internalServiceName);
  model.Services[internalServiceName] := serviceType;
  fServiceTypeMappings.Add(serviceType, model);
  fServiceNameMappings.Add(internalServiceName, model);
  if serviceName = '' then
  begin
    RegisterDefault(model, serviceType);
    RegisterUnnamed(model, serviceType);
  end;
end;

procedure TComponentRegistry.RegisterUnnamed(const model: TComponentModel;
  serviceType: PTypeInfo);
begin
  fUnnamedRegistrations.Add(serviceType, model);
end;

procedure TComponentRegistry.RegisterDefault(const model: TComponentModel;
  serviceType: PTypeInfo);
begin
  if not model.HasService(serviceType) then
    raise ERegistrationException.CreateResFmt(@SServiceNotFound, [
      serviceType.TypeName]);
  fDefaultRegistrations.AddOrSetValue(serviceType, model);
end;

{$IFDEF DELPHIXE_UP}
type
  TVirtualInterfaceHack = class(TInterfacedObject)
  private
    type
    {$POINTERMATH ON}
      PVTable = ^Pointer;
    {$POINTERMATH OFF}
    var VTable: PVTable;
  end;

procedure TComponentRegistry.InternalRegisterFactory(
  const model: TComponentModel; const invokeEvent: TVirtualInterfaceInvokeEvent);
var
  methods: TArray<TRttiMethod>;
  maxVirtualIndex: SmallInt;
  method: TRttiMethod;
begin
  methods := model.ComponentType.GetMethods;

  if Length(methods) = 0 then
    raise ERegistrationException.CreateResFmt(@SUnsupportedFactoryType, [
      model.ComponentTypeName]);

  for method in methods do
    if not Assigned(method.ReturnType)
      or method.Parameters.Any(TParameterFilters.HasFlags([pfOut])) then
      raise ERegistrationException.CreateResFmt(@SUnsupportedFactoryMethod, [
        model.ComponentTypeName, method.ToString]);

  maxVirtualIndex := 2;
  for method in methods do
    if maxVirtualIndex < method.VirtualIndex then
      maxVirtualIndex := method.VirtualIndex;

  model.ActivatorDelegate :=
    function: TValue
    var
      factory: TVirtualInterface;
      intf: IInterface;
    begin
      factory := TVirtualInterface.Create(model.ComponentTypeInfo, invokeEvent);
      if TType.IsDelegate(model.ComponentTypeInfo) then
        if maxVirtualIndex > 3 then
          TVirtualInterfaceHack(factory).VTable[3] :=
            TVirtualInterfaceHack(factory).VTable[maxVirtualIndex];
      factory.QueryInterface(GetTypeData(model.ComponentTypeInfo).Guid, intf);
      TValue.Make(@intf, model.ComponentTypeInfo, Result);
    end;
end;

procedure TComponentRegistry.RegisterFactory(const model: TComponentModel);
var
  invokeEvent: TVirtualInterfaceInvokeEvent;
begin
  invokeEvent :=
    procedure(method: TRttiMethod; const args: TArray<TValue>; out result: TValue)
    begin
      result := (fKernel as IKernelInternal).Resolve(
        method.ReturnType.Handle, Copy(args, 1, High(args)));
    end;

  InternalRegisterFactory(model, invokeEvent);
end;

procedure TComponentRegistry.RegisterFactory(const model: TComponentModel;
  const resolvedServiceName: string);
var
  invokeEvent: TVirtualInterfaceInvokeEvent;
begin
  invokeEvent :=
    procedure(method: TRttiMethod; const args: TArray<TValue>; out result: TValue)
    begin
      result := (fKernel as IKernelInternal).Resolve(
        resolvedServiceName, Copy(args, 1, High(args)));
    end;

  InternalRegisterFactory(model, invokeEvent);
end;
{$ENDIF}

function TComponentRegistry.RegisterComponent(
  componentTypeInfo: PTypeInfo): TComponentModel;
var
  componentType: TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(componentTypeInfo, 'componentTypeInfo');
{$ENDIF}

  componentType := TType.GetType(componentTypeInfo);
  Result := TComponentModel.Create(componentType);
  fModels.Add(Result);
end;

function TComponentRegistry.FindOne(const serviceName: string): TComponentModel;
begin
  fServiceNameMappings.TryGetValue(serviceName, Result);
end;

function TComponentRegistry.FindOne(componentType: PTypeInfo): TComponentModel;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(componentType, 'componentType');
{$ENDIF}

  Result := fModels.FirstOrDefault(
    function(const model: TComponentModel): Boolean
    begin
      Result := model.ComponentTypeInfo = componentType;
    end);
end;

function TComponentRegistry.FindOne(serviceType: PTypeInfo;
  const argument: TValue): TComponentModel;
var
  serviceName: string;
begin
  if argument.IsEmpty then
  begin
    if not HasService(serviceType) then
      raise EResolveException.CreateResFmt(@SCannotResolveType, [
        serviceType.TypeName])
    else
    begin
      Result := FindDefault(serviceType);
      if not Assigned(Result) then
        raise EResolveException.CreateResFmt(@SNoDefaultFound, [
          serviceType.TypeName]);
    end;
  end
  else if argument.IsString then
  begin
    serviceName := argument.AsString;
    Result := FindOne(serviceName);
    if not Assigned(Result) then
      raise EResolveException.CreateResFmt(@SServiceNotFound, [serviceName]);
    if not Result.HasService(serviceType) then
      raise EResolveException.CreateResFmt(@SCannotResolveType, [
        serviceType.TypeName]);
  end
  else
    raise EResolveException.CreateResFmt(@SCannotResolveType, [
      serviceType.TypeName]);
end;

function TComponentRegistry.GetOnChanged: ICollectionChangedEvent<TComponentModel>;
begin
  Result := fOnChanged;
end;

function TComponentRegistry.FindDefault(
  serviceType: PTypeInfo): TComponentModel;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  if not fDefaultRegistrations.TryGetValue(serviceType, Result) then
    fServiceTypeMappings[serviceType].TryGetSingle(Result);
end;

function TComponentRegistry.FindAll: IEnumerable<TComponentModel>;
begin
  Result := fModels;
end;

function TComponentRegistry.FindAll(
  serviceType: PTypeInfo): IEnumerable<TComponentModel>;
var
  models: IReadOnlyCollection<TComponentModel>;
  unnamedModels: IReadOnlyCollection<TComponentModel>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  if fServiceTypeMappings.TryGetValues(serviceType, models) then
  begin
    if fUnnamedRegistrations.TryGetValues(serviceType, unnamedModels) then
      Result := TExceptIterator<TComponentModel>.Create(models, unnamedModels)
    else
      Result := models;
  end
  else
    Result := TEnumerable.Empty<TComponentModel>;
end;

function TComponentRegistry.HasService(serviceType: PTypeInfo): Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  Result := fServiceTypeMappings.ContainsKey(serviceType);
end;

function TComponentRegistry.HasService(const serviceName: string): Boolean;
begin
  Result := fServiceNameMappings.ContainsKey(serviceName);
end;

function TComponentRegistry.HasService(serviceType: PTypeInfo;
  const serviceName: string): Boolean;
var
  model: TComponentModel;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(serviceType, 'serviceType');
{$ENDIF}

  Result := fServiceNameMappings.TryGetValue(serviceName, model)
    and model.HasService(serviceType);
end;

function TComponentRegistry.HasDefault(serviceType: PTypeInfo): Boolean;
begin
  Result := fDefaultRegistrations.ContainsKey(serviceType)
    or (fServiceTypeMappings[serviceType].Count = 1);
end;

{$ENDREGION}


{$REGION 'TRegistration'}

constructor TRegistration.Create(const kernel: IKernel;
  componentType: PTypeInfo);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
  Guard.CheckNotNull(componentType, 'componentType');
{$ENDIF}

  fKernel := kernel;
  fModel := fKernel.Registry.RegisterComponent(componentType);
end;

function TRegistration.Implements(serviceType: PTypeInfo): IRegistration;
begin
  fKernel.Registry.RegisterService(fModel, serviceType);
  Result := Self;
end;

function TRegistration.Implements(serviceType: PTypeInfo;
  const serviceName: string): IRegistration;
begin
  fKernel.Registry.RegisterService(fModel, serviceType, serviceName);
  Result := Self;
end;

function TRegistration.DelegateTo(const delegate: TActivatorDelegate): IRegistration;
begin
  fModel.ActivatorDelegate := delegate;
  Result := Self;
end;

function TRegistration.InjectConstructor: IRegistration;
begin
  fKernel.Injector.InjectConstructor(fModel);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const parameterTypes: array of PTypeInfo): IRegistration;
begin
  fKernel.Injector.InjectConstructor(fModel, parameterTypes);
  Result := Self;
end;

function TRegistration.InjectProperty(
  const propertyName: string): IRegistration;
begin
  fKernel.Injector.InjectProperty(fModel, propertyName);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): IRegistration;
begin
  fKernel.Injector.InjectMethod(fModel, methodName, parameterTypes);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string): IRegistration;
begin
  fKernel.Injector.InjectMethod(fModel, methodName);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string): IRegistration;
begin
  fKernel.Injector.InjectField(fModel, fieldName);
  Result := Self;
end;

function TRegistration.InjectConstructor(
  const arguments: array of TValue): IRegistration;
begin
  fKernel.Injector.InjectConstructor(fModel, arguments);
  Result := Self;
end;

function TRegistration.InjectProperty(const propertyName: string;
  const value: TValue): IRegistration;
begin
  fKernel.Injector.InjectProperty(fModel, propertyName, value);
  Result := Self;
end;

function TRegistration.InjectMethod(const methodName: string;
  const arguments: array of TValue): IRegistration;
begin
  fKernel.Injector.InjectMethod(fModel, methodName, arguments);
  Result := Self;
end;

function TRegistration.InjectField(const fieldName: string;
  const value: TValue): IRegistration;
begin
  fKernel.Injector.InjectField(fModel, fieldName, value);
  Result := Self;
end;

function TRegistration.AsSingleton(refCounting: TRefCounting): IRegistration;
begin
  fModel.LifetimeType := TLifetimeType.Singleton;
  fModel.RefCounting := refCounting;
  Result := Self;
end;

function TRegistration.AsSingletonPerThread(refCounting: TRefCounting): IRegistration;
begin
  fModel.LifetimeType := TLifetimeType.SingletonPerThread;
  fModel.RefCounting := refCounting;
  Result := Self;
end;

function TRegistration.AsTransient: IRegistration;
begin
  fModel.LifetimeType := TLifetimeType.Transient;
  Result := Self;
end;

function TRegistration.AsPooled(minPoolSize, maxPoolSize: Integer): IRegistration;
begin
  fModel.LifetimeType := TLifetimeType.Pooled;
  fModel.MinPoolsize := minPoolSize;
  fModel.MaxPoolsize := maxPoolSize;
  Result := Self;
end;

function TRegistration.AsDefault: IRegistration;
var
  serviceType: PTypeInfo;
begin
  for serviceType in fModel.Services.Values do
    fKernel.Registry.RegisterDefault(fModel, serviceType);
  Result := Self;
end;

function TRegistration.AsDefault(serviceType: PTypeInfo): IRegistration;
begin
  fKernel.Registry.RegisterDefault(fModel, serviceType);
  Result := Self;
end;

{$IFDEF DELPHIXE_UP}
function TRegistration.AsFactory: IRegistration;
begin
  fKernel.Registry.RegisterFactory(fModel);
  Result := Self;
end;

function TRegistration.AsFactory(const resolvedServiceName: string): IRegistration;
begin
  fKernel.Registry.RegisterFactory(fModel, resolvedServiceName);
  Result := Self;
end;
{$ENDIF}

function TRegistration.PerResolve: IRegistration;
begin
  fModel.LifetimeType := TLifetimeType.PerResolve;
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TRegistration<T>'}

constructor TRegistration<T>.Create(const kernel: IKernel);
begin
  fRegistration := TRegistration.Create(kernel, TypeInfo(T));
end;

function TRegistration<T>.Implements(serviceType: PTypeInfo): TRegistration<T>;
begin
  fRegistration.Implements(serviceType);
  Result := Self;
end;

function TRegistration<T>.Implements(serviceType: PTypeInfo;
  const serviceName: string): TRegistration<T>;
begin
  fRegistration.Implements(serviceType, serviceName);
  Result := Self;
end;

function TRegistration<T>.Implements<TServiceType>: TRegistration<T>;
begin
  Result := Implements(TypeInfo(TServiceType));
end;

function TRegistration<T>.Implements<TServiceType>(
  const serviceName: string): TRegistration<T>;
begin
  Result := Implements(TypeInfo(TServiceType), serviceName);
end;

function TRegistration<T>.DelegateTo(
  const delegate: TActivatorDelegate<T>): TRegistration<T>;
begin
  fRegistration.DelegateTo(
    function: TValue
    begin
      Result := TValue.From<T>(delegate());
    end);
  Result := Self;
end;

function TRegistration<T>.InjectConstructor: TRegistration<T>;
begin
  fRegistration.InjectConstructor;
  Result := Self;
end;

function TRegistration<T>.InjectConstructor(
  const parameterTypes: array of PTypeInfo): TRegistration<T>;
begin
  fRegistration.InjectConstructor(parameterTypes);
  Result := Self;
end;

function TRegistration<T>.InjectProperty(
  const propertyName: string): TRegistration<T>;
begin
  fRegistration.InjectProperty(propertyName);
  Result := Self;
end;

function TRegistration<T>.InjectMethod(const methodName: string;
  const parameterTypes: array of PTypeInfo): TRegistration<T>;
begin
  fRegistration.InjectMethod(methodName, parameterTypes);
  Result := Self;
end;

function TRegistration<T>.InjectMethod(
  const methodName: string): TRegistration<T>;
begin
  fRegistration.InjectMethod(methodName);
  Result := Self;
end;

function TRegistration<T>.InjectField(
  const fieldName: string): TRegistration<T>;
begin
  fRegistration.InjectField(fieldName);
  Result := Self;
end;

function TRegistration<T>.InjectConstructor(
  const arguments: array of TValue): TRegistration<T>;
begin
  fRegistration.InjectConstructor(arguments);
  Result := Self;
end;

function TRegistration<T>.InjectProperty(const propertyName: string;
  const value: TValue): TRegistration<T>;
begin
  fRegistration.InjectProperty(propertyName, value);
  Result := Self;
end;

function TRegistration<T>.InjectMethod(const methodName: string;
  const arguments: array of TValue): TRegistration<T>;
begin
  fRegistration.InjectMethod(methodName, arguments);
  Result := Self;
end;

function TRegistration<T>.InjectField(const fieldName: string;
  const value: TValue): TRegistration<T>;
begin
  fRegistration.InjectField(fieldName, value);
  Result := Self;
end;

function TRegistration<T>.AsSingleton(refCounting: TRefCounting): TRegistration<T>;
begin
  fRegistration.AsSingleton(refCounting);
  Result := Self;
end;

function TRegistration<T>.AsSingletonPerThread(refCounting: TRefCounting): TRegistration<T>;
begin
  fRegistration.AsSingletonPerThread(refCounting);
  Result := Self;
end;

function TRegistration<T>.AsTransient: TRegistration<T>;
begin
  fRegistration.AsTransient;
  Result := Self;
end;

function TRegistration<T>.AsPooled(minPoolSize, maxPoolSize: Integer): TRegistration<T>;
begin
{$WARN SYMBOL_EXPERIMENTAL OFF}
  fRegistration.AsPooled(minPoolSize, maxPoolSize);
{$WARN SYMBOL_EXPERIMENTAL ON}
  Result := Self;
end;

function TRegistration<T>.AsDefault: TRegistration<T>;
begin
  fRegistration.AsDefault;
  Result := Self;
end;

function TRegistration<T>.AsDefault(serviceType: PTypeInfo): TRegistration<T>;
begin
  fRegistration.AsDefault(serviceType);
  Result := Self;
end;

function TRegistration<T>.AsDefault<TServiceType>: TRegistration<T>;
begin
  Result := AsDefault(TypeInfo(TServiceType));
end;

{$IFDEF DELPHIXE_UP}
function TRegistration<T>.AsFactory: TRegistration<T>;
begin
  fRegistration.AsFactory;
  Result := Self;
end;

function TRegistration<T>.AsFactory(const resolvedServiceName: string): TRegistration<T>;
begin
  fRegistration.AsFactory(resolvedServiceName);
  Result := Self;
end;
{$ENDIF}

function TRegistration<T>.PerResolve: TRegistration<T>;
begin
  fRegistration.PerResolve;
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TRegistrationManager'}

constructor TRegistrationManager.Create(const kernel: IKernel);
begin
  inherited Create;
  fKernel := kernel;
end;

function TRegistrationManager.RegisterType(
  componentType: PTypeInfo): IRegistration;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(componentType, 'componentType');
{$ENDIF}

  Result := TRegistration.Create(fKernel, componentType);
end;

function TRegistrationManager.RegisterType<TComponentType>: TRegistration<TComponentType>;
begin
  Result := TRegistration<TComponentType>.Create(fKernel);
end;

{$ENDREGION}


end.

