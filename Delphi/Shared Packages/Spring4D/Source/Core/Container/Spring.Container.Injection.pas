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

unit Spring.Container.Injection;

interface

uses
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TInjectionBase = class abstract(TInterfacedObject, IInjection)
  private
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fTarget: TRttiMember;
    fTargetName: string;
    fDependencies: TArray<TDependencyModel>;
    fArguments: TArray<TValue>;
    function GetDependencyCount: Integer;
    function GetTarget: TRttiMember;
    function GetHasTarget: Boolean;
    function GetTargetName: string;
    function GetArguments: TArray<TValue>;
    function GetDependencies: TArray<TDependencyModel>;
  protected
    procedure Validate(const target: TRttiMember); virtual;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); virtual; abstract;
    procedure InitializeArguments(const arguments: array of TValue);
    procedure InitializeDependencies(out dependencies: TArray<TDependencyModel>); overload; virtual; abstract;
    procedure InitializeDependencies(const parameterTypes: array of PTypeInfo); overload;
  public
    constructor Create(const targetName: string = '');
    procedure Initialize(const target: TRttiMember);
    procedure Inject(const instance: TValue; const arguments: array of TValue);

    property DependencyCount: Integer read GetDependencyCount;
    property Target: TRttiMember read GetTarget;
    property TargetName: string read GetTargetName;
    property HasTarget: Boolean read GetHasTarget;
    property Arguments: TArray<TValue> read GetArguments;
    property Dependencies: TArray<TDependencyModel> read GetDependencies;
  end;

  TConstructorInjection = class(TInjectionBase)
  protected
    procedure Validate(const target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TDependencyModel>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TPropertyInjection = class(TInjectionBase)
  protected
    procedure Validate(const target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TDependencyModel>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TMethodInjection = class(TInjectionBase)
  protected
    procedure Validate(const target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TDependencyModel>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TFieldInjection = class(TInjectionBase)
  protected
    procedure Validate(const target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TDependencyModel>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TDependencyInjector = class(TInterfacedObject, IDependencyInjector)
  public
    function InjectConstructor(const model: TComponentModel;
      const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectMethod(const model: TComponentModel;
      const methodName: string): IInjection; overload;
    function InjectMethod(const model: TComponentModel; const methodName: string;
      const parameterTypes: array of PTypeInfo): IInjection; overload;
    function InjectProperty(const model: TComponentModel;
      const propertyName: string): IInjection; overload;
    function InjectField(const model: TComponentModel;
      const fieldName: string): IInjection; overload;

    function InjectConstructor(const model: TComponentModel): IInjection; overload;
    function InjectConstructor(const model: TComponentModel;
      const arguments: array of TValue): IInjection; overload;
    function InjectMethod(const model: TComponentModel; const methodName: string;
      const arguments: array of TValue): IInjection; overload;
    function InjectProperty(const model: TComponentModel;
      const propertyName: string; const value: TValue): IInjection; overload;
    function InjectField(const model: TComponentModel;
      const fieldName: string; const value: TValue): IInjection; overload;
  end;

implementation

uses
  TypInfo,
  Spring.Container.ResourceStrings,
  Spring.Reflection,
  Spring.ResourceStrings;


{$REGION 'TInjectionBase'}

constructor TInjectionBase.Create(const targetName: string);
begin
  inherited Create;
  fTargetName := targetName;
end;

procedure TInjectionBase.Initialize(const target: TRttiMember);
begin
  Guard.CheckNotNull(target, 'target');
  Validate(target);
  fTarget := target;
  InitializeDependencies(fDependencies);
end;

procedure TInjectionBase.Validate(const target: TRttiMember);
begin
end;

procedure TInjectionBase.Inject(const instance: TValue;
  const arguments: array of TValue);
begin
  Guard.CheckNotNull(instance, 'instance');
  if not Assigned(fTarget) then
    raise EInjectionException.CreateRes(@SInjectionTargetNeeded);
  DoInject(instance, arguments);
end;

procedure TInjectionBase.InitializeArguments(const arguments: array of TValue);
begin
  fArguments := TArray.Copy<TValue>(arguments);
end;

procedure TInjectionBase.InitializeDependencies(
  const parameterTypes: array of PTypeInfo);
var
  i: Integer;
begin
  for i := Low(parameterTypes) to High(parameterTypes) do
    fDependencies[i] := TDependencyModel.Create(
      TType.GetType(parameterTypes[i]), fDependencies[i].Target);
end;

function TInjectionBase.GetArguments: TArray<TValue>;
begin
  Result := fArguments;
end;

function TInjectionBase.GetDependencies: TArray<TDependencyModel>;
begin
  Result := fDependencies;
end;

function TInjectionBase.GetTarget: TRttiMember;
begin
  Result := fTarget;
end;

function TInjectionBase.GetTargetName: string;
begin
  Result := fTargetName;
end;

function TInjectionBase.GetHasTarget: Boolean;
begin
  Result := Assigned(fTarget);
end;

function TInjectionBase.GetDependencyCount: Integer;
begin
  Result := Length(fDependencies);
end;

{$ENDREGION}


{$REGION 'TConstructorInjection'}

procedure TConstructorInjection.Validate(const target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsConstructor then
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
end;

procedure TConstructorInjection.InitializeDependencies(
  out dependencies: TArray<TDependencyModel>);
var
  params: TArray<TRttiParameter>;
  i: Integer;
begin
  params := Target.AsMethod.GetParameters;
  SetLength(dependencies, Length(params));
  for i := Low(params) to High(params) do
    dependencies[i] := TDependencyModel.Create(params[i].ParamType, params[i]);
end;

procedure TConstructorInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Target.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TPropertyInjection'}

procedure TPropertyInjection.Validate(const target: TRttiMember);
begin
  inherited Validate(target);
  if not target.IsProperty then
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
end;

procedure TPropertyInjection.InitializeDependencies(
  out dependencies: TArray<TDependencyModel>);
begin
  SetLength(dependencies, 1);
  dependencies[0] := TDependencyModel.Create(Target.AsProperty.PropertyType, Target);
end;

procedure TPropertyInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Guard.CheckRange(Length(arguments) = 1, 'arguments');
  Target.AsProperty.SetValue(instance, arguments[0]);
end;

{$ENDREGION}


{$REGION 'TMethodInjection'}

procedure TMethodInjection.Validate(const target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsMethod then
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
end;

procedure TMethodInjection.InitializeDependencies(
  out dependencies: TArray<TDependencyModel>);
var
  params: TArray<TRttiParameter>;
  i: Integer;
begin
  params := Target.AsMethod.GetParameters;
  SetLength(dependencies, Length(params));
  for i := Low(params) to High(params) do
    dependencies[i] := TDependencyModel.Create(params[i].ParamType, params[i]);
end;

procedure TMethodInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Target.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TFieldInjection'}

procedure TFieldInjection.Validate(const target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsField then
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
end;

procedure TFieldInjection.InitializeDependencies(
  out dependencies: TArray<TDependencyModel>);
begin
  SetLength(dependencies, 1);
  dependencies[0] := TDependencyModel.Create(Target.AsField.FieldType, Target);
end;

procedure TFieldInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Guard.CheckRange(Length(arguments) = 1, 'arguments');
  Target.AsField.SetValue(instance, arguments[0]);
end;

{$ENDREGION}


{$REGION 'TDependencyInjector'}

function TDependencyInjector.InjectConstructor(const model: TComponentModel;
  const parameterTypes: array of PTypeInfo): IInjection;
var
  predicate: TPredicate<TRttiMethod>;
  method: TRttiMethod;
begin
  predicate := TMethodFilters.IsConstructor
    and TMethodFilters.HasParameterTypes(parameterTypes);
  method := model.ComponentType.Methods.FirstOrDefault(predicate);
  if not Assigned(method) then
    raise ERegistrationException.CreateResFmt(
      @SUnsatisfiedConstructorParameters, [model.ComponentTypeName]);
  Result := TConstructorInjection.Create;
  Result.Initialize(method);
  Result.InitializeDependencies(parameterTypes);
  model.ConstructorInjections.Add(Result);
end;

function TDependencyInjector.InjectMethod(const model: TComponentModel;
  const methodName: string): IInjection;
var
  method: TRttiMethod;
  injectionExists: Boolean;
begin
  method := model.ComponentType.GetMethod(methodName);
  if not Assigned(method) then
    raise ERegistrationException.CreateResFmt(@SMethodNotFound,
      [model.ComponentTypeName, methodName]);
  injectionExists := model.MethodInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(method));
  if not injectionExists then
    Result := TMethodInjection.Create(methodName);
  Result.Initialize(method);
  if not injectionExists then
    model.MethodInjections.Add(Result);
end;

function TDependencyInjector.InjectMethod(const model: TComponentModel;
  const methodName: string; const parameterTypes: array of PTypeInfo): IInjection;
var
  predicate: TPredicate<TRttiMethod>;
  method: TRttiMethod;
  injectionExists: Boolean;
begin
  predicate := TMethodFilters.IsNamed(methodName)
    and TMethodFilters.IsInstanceMethod
    and TMethodFilters.HasParameterTypes(parameterTypes);
  method := model.ComponentType.Methods.FirstOrDefault(predicate);
  if not Assigned(method) then
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedMethodParameterTypes, [methodName]);
  injectionExists := model.MethodInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(method));
  if not injectionExists then
    Result := TMethodInjection.Create(methodName);
  Result.Initialize(method);
  Result.InitializeDependencies(parameterTypes);
  if not injectionExists then
    model.MethodInjections.Add(Result);
end;

function TDependencyInjector.InjectProperty(const model: TComponentModel;
  const propertyName: string): IInjection;
var
  propertyMember: TRttiProperty;
  injectionExists: Boolean;
begin
  propertyMember := model.ComponentType.GetProperty(propertyName);
  if not Assigned(propertyMember) then
    raise ERegistrationException.CreateResFmt(@SPropertyNotFound,
      [model.ComponentTypeName, propertyName]);
  injectionExists := model.PropertyInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(propertyMember));
  if not injectionExists then
    Result := TPropertyInjection.Create(propertyName);
  Result.Initialize(propertyMember);
  if not injectionExists then
    model.PropertyInjections.Add(Result);
end;

function TDependencyInjector.InjectField(const model: TComponentModel;
  const fieldName: string): IInjection;
var
  field: TRttiField;
  injectionExists: Boolean;
begin
  field := model.ComponentType.GetField(fieldName);
  if not Assigned(field) then
    raise ERegistrationException.CreateResFmt(@SFieldNotFound,
      [model.ComponentTypeName, fieldName]);
  injectionExists := model.FieldInjections.TryGetFirst(Result,
    TInjectionFilters.ContainsMember(field));
  if not injectionExists then
    Result := TFieldInjection.Create(fieldName);
  Result.Initialize(field);
  if not injectionExists then
    model.FieldInjections.Add(Result);
end;

function TDependencyInjector.InjectConstructor(
  const model: TComponentModel): IInjection;
var
  predicate: TPredicate<TRttiMethod>;
  method: TRttiMethod;
begin
  predicate := TMethodFilters.IsConstructor
    and TMethodFilters.HasParameterTypes([]);
  method := model.ComponentType.Methods.FirstOrDefault(predicate);
  Result := TConstructorInjection.Create;
  Result.Initialize(method);
  model.ConstructorInjections.Add(Result);
end;

function TDependencyInjector.InjectConstructor(const model: TComponentModel;
  const arguments: array of TValue): IInjection;
begin
  Result := TConstructorInjection.Create;
  model.ConstructorInjections.Add(Result);
  Result.InitializeArguments(arguments);
end;

function TDependencyInjector.InjectMethod(const model: TComponentModel;
  const methodName: string; const arguments: array of TValue): IInjection;
begin
  Result := TMethodInjection.Create(methodName);
  model.MethodInjections.Add(Result);
  Result.InitializeArguments(arguments);
end;

function TDependencyInjector.InjectProperty(const model: TComponentModel;
  const propertyName: string; const value: TValue): IInjection;
begin
  Result := InjectProperty(model, propertyName);
  Result.InitializeArguments(value);
end;

function TDependencyInjector.InjectField(const model: TComponentModel;
  const fieldName: string; const value: TValue): IInjection;
begin
  Result := InjectField(model, fieldName);
  Result.InitializeArguments(value);
end;

{$ENDREGION}


end.
