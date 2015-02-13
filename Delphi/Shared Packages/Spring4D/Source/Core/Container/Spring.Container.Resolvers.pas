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

unit Spring.Container.Resolvers;

interface

uses
  Rtti,
  SyncObjs,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TSubDependencyResolverBase = class abstract(TInterfacedObject, ISubDependencyResolver)
  private
    fKernel: IKernel;
  protected
    property Kernel: IKernel read fKernel;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; virtual;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; virtual; abstract;
  end;

  TDependencyResolver = class(TSubDependencyResolverBase, IDependencyResolver)
  private
    fSubResolvers: IList<ISubDependencyResolver>;
  protected
    function CanResolveFromArgument(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function CanResolveFromContext(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function CanResolveFromSubResolvers(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function InternalResolveValue(typeInfo: PTypeInfo;
      const instance: TValue): TValue;
  public
    constructor Create(const kernel: IKernel);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; overload; override;
    function CanResolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): Boolean; reintroduce; overload; virtual;

    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; overload; override;
    function Resolve(const context: ICreationContext;
      const dependencies: TArray<TDependencyModel>;
      const arguments: TArray<TValue>): TArray<TValue>; reintroduce; overload; virtual;

    procedure AddSubResolver(const subResolver: ISubDependencyResolver);
    procedure RemoveSubResolver(const subResolver: ISubDependencyResolver);
  end;

  TLazyResolver = class(TSubDependencyResolverBase)
  private
    function InternalResolveClass(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue; lazyKind: TLazyKind): TValue;
    function InternalResolveInterface(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue; lazyKind: TLazyKind): TValue;
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;

  TDynamicArrayResolver = class(TSubDependencyResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;

  TListResolver = class(TSubDependencyResolverBase)
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  TypInfo,
  Spring.Collections.Lists,
  Spring.Container.CreationContext,
  Spring.Container.ResourceStrings,
  Spring.Reflection;


{$REGION 'TSubDependencyResolverBase'}

constructor TSubDependencyResolverBase.Create(const kernel: IKernel);
begin
{$IFNDEF DISABLE_GUARD}
  Guard.CheckNotNull(kernel, 'kernel');
{$ENDIF}

  inherited Create;
  fKernel := kernel;
end;

function TSubDependencyResolverBase.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
begin
  Result := not Kernel.Registry.HasService(dependency.TypeInfo)
    and (dependency.TypeInfo <> argument.TypeInfo);
end;

{$ENDREGION}


{$REGION 'TDependencyResolver'}

constructor TDependencyResolver.Create(const kernel: IKernel);
begin
  inherited Create(kernel);
  fSubResolvers := TCollections.CreateInterfaceList<ISubDependencyResolver>;
end;

procedure TDependencyResolver.AddSubResolver(
  const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Add(subResolver);
end;

procedure TDependencyResolver.RemoveSubResolver(
  const subResolver: ISubDependencyResolver);
begin
  fSubResolvers.Remove(subResolver);
end;

function TDependencyResolver.InternalResolveValue(
  typeInfo: PTypeInfo; const instance: TValue): TValue;
var
  intf: Pointer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
  Guard.CheckNotNull(not instance.IsEmpty, 'instance');
{$ENDIF}

  if typeInfo.Kind = tkInterface then
  begin
    if instance.IsObject then
      instance.AsObject.GetInterface(GetTypeData(typeInfo).Guid, intf)
    else
    begin
      if TType.IsDelegate(typeInfo) then
      begin
        intf := nil;
        IInterface(intf) := instance.AsInterface;
      end
      else
        instance.AsInterface.QueryInterface(GetTypeData(typeInfo).Guid, intf);
    end;
    TValue.MakeWithoutCopy(@intf, typeInfo, Result);
  end
  else
    Result := instance;
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  kind: TTypeKind;
  serviceName: string;
  serviceType: PTypeInfo;
  componentModel: TComponentModel;
begin
  if CanResolveFromContext(context, dependency, argument) then
    Exit(True);

  if CanResolveFromSubResolvers(context, dependency, argument) then
    Exit(True);

  if argument.IsEmpty then
    Result := Kernel.Registry.HasDefault(dependency.TypeInfo)
  else if CanResolveFromArgument(context, dependency, argument) then
    Result := True
  else if argument.TryAsType<TTypeKind>(Kind) and (kind = tkDynArray) then
    Result := Kernel.Registry.HasService(dependency.TypeInfo)
  else
  begin
    Result := argument.IsString;
    if Result then
    begin
      serviceName := argument.AsString;
      componentModel := Kernel.Registry.FindOne(serviceName);
      Result := Assigned(componentModel);
      if Result then
      begin
        serviceType := componentModel.Services[serviceName];
        Result := TType.IsAssignable(dependency.TypeInfo, serviceType);
      end;
    end;
  end;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  i: Integer;
  componentModel: TComponentModel;
  modelRef: SmartPointer<TObject>;
  instance: TValue;
begin
  if CanResolveFromContext(context, dependency, argument) then
    Exit(context.Resolve(context, dependency, argument));

  for i := fSubResolvers.Count - 1 downto 0 do
    if fSubResolvers[i].CanResolve(context, dependency, argument) then
      Exit(fSubResolvers[i].Resolve(context, dependency, argument));

  if CanResolveFromArgument(context, dependency, argument) then
    Exit(argument);

  if not Kernel.Registry.HasService(dependency.TypeInfo)
    and dependency.TargetType.IsClass then
  begin
    componentModel := TComponentModel.Create(dependency.TargetType);
    componentModel.Services.AddOrSetValue('default', dependency.TypeInfo);
    Kernel.Builder.Build(componentModel);
    modelRef := componentModel;
  end
  else
    componentModel := Kernel.Registry.FindOne(dependency.TypeInfo, argument);

  if context.EnterResolution(componentModel, instance) then
  try
    instance := componentModel.LifetimeManager.Resolve(context);
  finally
    context.LeaveResolution(componentModel);
  end;
  Result := InternalResolveValue(dependency.TypeInfo, instance);
end;

function TDependencyResolver.CanResolve(const context: ICreationContext;
  const dependencies: TArray<TDependencyModel>;
  const arguments: TArray<TValue>): Boolean;
var
  i: Integer;
begin
  if Length(dependencies) = Length(arguments) then
  begin
    for i := Low(dependencies) to High(dependencies) do
      if not CanResolve(context, dependencies[i], arguments[i]) then
        Exit(False);
  end
  else if Length(arguments) = 0 then
  begin
    for i := Low(dependencies) to High(dependencies) do
      if not CanResolve(context, dependencies[i], nil) then
        Exit(False);
  end
  else
    Exit(False);
  Result := True;
end;

function TDependencyResolver.CanResolveFromArgument(
  const context: ICreationContext; const dependency: TDependencyModel;
  const argument: TValue): Boolean;
begin
  Result := Assigned(argument.TypeInfo) and argument.IsType(dependency.TypeInfo);
  if not Result and (argument.Kind in [tkInteger, tkFloat, tkInt64]) then
    Result := argument.Kind = dependency.TypeInfo.Kind;
  if Result and argument.IsString then
    Result := not Kernel.Registry.HasService(dependency.TypeInfo, argument.AsString);
end;

function TDependencyResolver.CanResolveFromContext(
  const context: ICreationContext;  const dependency: TDependencyModel;
  const argument: TValue): Boolean;
begin
  Result := Assigned(context)
    and context.CanResolve(context, dependency, argument);
end;

function TDependencyResolver.CanResolveFromSubResolvers(
  const context: ICreationContext; const dependency: TDependencyModel;
  const argument: TValue): Boolean;
var
  i: Integer;
begin
  for i := fSubResolvers.Count - 1 downto 0 do
    if fSubResolvers[i].CanResolve(context, dependency, argument) then
      Exit(True);
  Result := False;
end;

function TDependencyResolver.Resolve(const context: ICreationContext;
  const dependencies: TArray<TDependencyModel>;
  const arguments: TArray<TValue>): TArray<TValue>;
var
  hasArgument: Boolean;
  i: Integer;
begin
  hasArgument := Length(arguments) > 0;
  if hasArgument and (Length(arguments) <> Length(dependencies)) then
    raise EResolveException.CreateRes(@SUnsatisfiedResolutionArgumentCount);
  SetLength(Result, Length(dependencies));
  if hasArgument then
    for i := Low(dependencies) to High(dependencies) do
      Result[i] := Resolve(context, dependencies[i], arguments[i])
  else
    for i := Low(dependencies) to High(dependencies) do
      Result[i] := Resolve(context, dependencies[i], nil);
end;

{$ENDREGION}


{$REGION 'TLazyResolver'}

function TLazyResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
begin
  Result := inherited CanResolve(context, dependency, argument)
    and TType.IsLazy(dependency.TypeInfo);
  if Result then
  begin
    targetType := dependency.TargetType.GetGenericArguments[0];
    dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
    Result := Kernel.Resolver.CanResolve(context, dependencyModel, argument);
  end;
end;

function TLazyResolver.InternalResolveClass(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue;
  lazyKind: TLazyKind): TValue;
var
  dependencyModel: TDependencyModel;
  value: TValue;
  factory: TFunc<TObject>;
begin
  dependencyModel := dependency;
  value := argument;
  factory :=
    function: TObject
    begin
      Result := Kernel.Resolver.Resolve(context, dependencyModel, value).AsObject;
    end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<TObject>>(factory);
    lkRecord: Result := TValue.From<Lazy<TObject>>(Lazy<TObject>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<TObject>>(TLazy<TObject>.Create(factory));
  end;
end;

function TLazyResolver.InternalResolveInterface(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue;
  lazyKind: TLazyKind): TValue;
var
  dependencyModel: TDependencyModel;
  value: TValue;
  factory: TFunc<IInterface>;
begin
  dependencyModel := dependency;
  value := argument;
  factory :=
    function: IInterface
    begin
      Result := Kernel.Resolver.Resolve(context, dependencyModel, value).AsInterface;
    end;

  case lazyKind of
    lkFunc: Result := TValue.From<TFunc<IInterface>>(factory);
    lkRecord: Result := TValue.From<Lazy<IInterface>>(Lazy<IInterface>.Create(factory));
    lkInterface: Result := TValue.From<ILazy<IInterface>>(TLazy<IInterface>.Create(factory));
  end;
end;

function TLazyResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  lazyKind: TLazyKind;
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
  componentModel: TComponentModel;
begin
  if not TType.IsLazy(dependency.TypeInfo) then
    raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);

  lazyKind := TType.GetLazyKind(dependency.TypeInfo);
  targetType := dependency.TargetType.GetGenericArguments[0];
  dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
  componentModel := Kernel.Registry.FindOne(targetType.Handle, argument);

  if context.EnterResolution(componentModel, Result) then
  try
    case targetType.TypeKind of
      tkClass: Result := InternalResolveClass(
        context, dependencyModel, argument, lazyKind);
      tkInterface: Result := InternalResolveInterface(
        context, dependencyModel, argument, lazyKind);
    else
      raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
    end;
    TValueData(Result).FTypeInfo := dependency.TypeInfo;
  finally
    context.LeaveResolution(componentModel);
  end;
end;

{$ENDREGION}


{$REGION 'TDynamicArrayResolver'}

function TDynamicArrayResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
begin
  targetType := dependency.TargetType;
  Result := inherited CanResolve(context, dependency, argument)
    and targetType.IsDynamicArray;
  if Result then
  begin
    targetType := targetType.AsDynamicArray.ElementType;
    dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
    Result := Kernel.Resolver.CanResolve(context, dependencyModel, TValue.From(tkDynArray));
  end;
end;

function TDynamicArrayResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
  serviceType: PTypeInfo;
  models: TArray<TComponentModel>;
  values: TArray<TValue>;
  i: Integer;
  serviceName: string;
begin
  targetType := dependency.TargetType;
  if not targetType.IsDynamicArray then
    raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
  targetType := targetType.AsDynamicArray.ElementType;
  dependencyModel := TDependencyModel.Create(targetType, dependency.Target);

  // TODO: remove dependency on lazy type
  if TType.IsLazy(targetType.Handle) then
    serviceType := targetType.GetGenericArguments[0].Handle
  else
    serviceType := targetType.Handle;
  models := Kernel.Registry.FindAll(serviceType).ToArray;

  SetLength(values, Length(models));
  for i := Low(models) to High(models) do
  begin
    serviceName := models[i].GetServiceName(serviceType);
    values[i] := Kernel.Resolver.Resolve(context, dependencyModel, serviceName);
  end;
  Result := TValue.FromArray(dependency.TypeInfo, values);
end;

{$ENDREGION}


{$REGION 'TListResolver'}

function TListResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
const
  SupportedTypes: array[0..3] of string = (
    'IList<>', 'IReadOnlyList<>', 'ICollection<>', 'IEnumerable<>');
var
  targetType: TRttiType;
  dependencyModel: TDependencyModel;
begin
  targetType := dependency.TargetType;
  Result := inherited CanResolve(context, dependency, argument)
    and targetType.IsGenericType
    and MatchText(targetType.GetGenericTypeDefinition, SupportedTypes);
  if Result then
  begin
    targetType := targetType.GetGenericArguments[0];
    dependencyModel := TDependencyModel.Create(targetType, dependency.Target);
    Result := targetType.IsClassOrInterface
      and Kernel.Resolver.CanResolve(context, dependencyModel, TValue.From(tkDynArray));
  end;
end;

function TListResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  itemType: TRttiType;
  arrayType: TRttiType;
  values: TValue;
begin
  itemType := dependency.TargetType.GetGenericArguments[0];
  arrayType := dependency.TargetType.GetMethod('ToArray').ReturnType;
  values := (Kernel as IKernelInternal).Resolve(arrayType.Handle);
  case itemType.TypeKind of
    tkClass:
    begin
      TValueData(values).FTypeInfo := TypeInfo(TArray<TObject>);
      Result := TValue.From(TList<TObject>.Create(
        values.AsType<TArray<TObject>>()));
    end;
    tkInterface:
    begin
      TValueData(values).FTypeInfo := TypeInfo(TArray<IInterface>);
      Result := TValue.From(TList<IInterface>.Create(
        values.AsType<TArray<IInterface>>()));
    end;
  else
    raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
  end;
  Result := Result.Cast(dependency.TypeInfo);
end;

{$ENDREGION}


end.
