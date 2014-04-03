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

unit Spring.Container.Injection;

{$I Spring.inc}

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TInjectionBase = class abstract(TInterfacedObject, IInjection, IInterface)
  private
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fModel: TComponentModel;  // Consider remove the dependency of TComponentModel
    fTarget: TRttiMember;
    fTargetName: string;
    fDependencies: TArray<TRttiType>;
    function GetDependencyCount: Integer;
    function GetTarget: TRttiMember;
    function GetModel: TComponentModel;
    function GetHasTarget: Boolean;
    function GetTargetName: string;
  protected
    procedure Validate(target: TRttiMember); virtual;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); virtual; abstract;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); virtual; abstract;
  public
    constructor Create(const model: TComponentModel); overload;
    constructor Create(const model: TComponentModel; const targetName: string); overload;
    procedure Initialize(target: TRttiMember); virtual;
    procedure Inject(const instance: TValue; const arguments: array of TValue);
    function GetDependencies: TArray<TRttiType>;
    property Target: TRttiMember read GetTarget;
    property TargetName: string read GetTargetName;
    property HasTarget: Boolean read GetHasTarget;
    property DependencyCount: Integer read GetDependencyCount;
    property Model: TComponentModel read GetModel;
  end;

  TConstructorInjection = class(TInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TPropertyInjection = class(TInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TMethodInjection = class(TInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TFieldInjection = class(TInjectionBase)
  protected
    procedure Validate(target: TRttiMember); override;
    procedure InitializeDependencies(out dependencies: TArray<TRttiType>); override;
    procedure DoInject(const instance: TValue; const arguments: array of TValue); override;
  end;

  TInjectionFactory = class(TInterfacedObject, IInjectionFactory)
  public
    function CreateConstructorInjection(const model: TComponentModel): IInjection;
    function CreateMethodInjection(const model: TComponentModel; const methodName: string): IInjection;
    function CreatePropertyInjection(const model: TComponentModel; const propertyName: string): IInjection;
    function CreateFieldInjection(const model: TComponentModel; const fieldName: string): IInjection;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Spring.Container.ResourceStrings,
  Spring.Helpers,
  Spring.ResourceStrings;


{$REGION 'TInjectionBase'}

constructor TInjectionBase.Create(const model: TComponentModel);
begin
  Create(model, '');
end;

constructor TInjectionBase.Create(const model: TComponentModel;
  const targetName: string);
begin
  inherited Create;
  fModel := model;
  fTargetName := targetName;
end;

procedure TInjectionBase.Initialize(target: TRttiMember);
begin
  Guard.CheckNotNull(target, 'target');
  Validate(target);
  fTarget := target;
  InitializeDependencies(fDependencies);
end;

procedure TInjectionBase.Validate(target: TRttiMember);
begin
end;

procedure TInjectionBase.Inject(const instance: TValue;
  const arguments: array of TValue);
begin
  Guard.CheckNotNull(instance, 'instance');
  if fTarget = nil then
  begin
    raise EInjectionException.CreateRes(@SInjectionTargetNeeded);
  end;
  DoInject(instance, arguments);
end;

function TInjectionBase.GetDependencies: TArray<TRttiType>;
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
  Result := fTarget <> nil;
end;

function TInjectionBase.GetDependencyCount: Integer;
begin
  Result := Length(fDependencies);
end;

function TInjectionBase.GetModel: TComponentModel;
begin
  Result := fModel;
end;

{$ENDREGION}


{$REGION 'TConstructorInjection'}

procedure TConstructorInjection.Validate(target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsConstructor then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TConstructorInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := Target.AsMethod.GetParameters;
  SetLength(dependencies, Length(parameters));
  for i := 0 to High(parameters) do
  begin
    dependencies[i] := parameters[i].ParamType;
  end;
end;

procedure TConstructorInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Target.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TPropertyInjection'}

procedure TPropertyInjection.Validate(target: TRttiMember);
begin
  inherited Validate(target);
  if not target.IsProperty then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TPropertyInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
begin
  dependencies := TArray<TRttiType>.Create(Target.AsProperty.PropertyType);
end;

procedure TPropertyInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Guard.CheckTrue(Length(arguments) = 1, SUnexpectedArgumentLength);
  Target.AsProperty.SetValue(instance, arguments[0]);
end;

{$ENDREGION}


{$REGION 'TMethodInjection'}

procedure TMethodInjection.Validate(target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsMethod then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TMethodInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := Target.AsMethod.GetParameters;
  SetLength(dependencies, Length(parameters));
  for i := 0 to High(parameters) do
  begin
    dependencies[i] := parameters[i].ParamType;
  end;
end;

procedure TMethodInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Target.AsMethod.Invoke(instance, arguments);
end;

{$ENDREGION}


{$REGION 'TFieldInjection'}

procedure TFieldInjection.Validate(target: TRttiMember);
begin
  inherited Validate(Target);
  if not target.IsField then
  begin
    raise ERegistrationException.CreateResFmt(@SUnsatisfiedTarget, [target.Name]);
  end;
end;

procedure TFieldInjection.InitializeDependencies(
  out dependencies: TArray<TRttiType>);
begin
  dependencies := TArray<TRttiType>.Create(Target.AsField.FieldType);
end;

procedure TFieldInjection.DoInject(const instance: TValue;
  const arguments: array of TValue);
begin
  Guard.CheckTrue(Length(arguments) = 1, SUnexpectedArgumentLength);
  Target.AsField.SetValue(instance, arguments[0]);
end;

{$ENDREGION}


{$REGION 'TInjectionFactory'}

function TInjectionFactory.CreateConstructorInjection(
  const model: TComponentModel): IInjection;
begin
  Result := TConstructorInjection.Create(model);
end;

function TInjectionFactory.CreateMethodInjection(
  const model: TComponentModel; const methodName: string): IInjection;
begin
  Result := TMethodInjection.Create(model, methodName);
end;

function TInjectionFactory.CreatePropertyInjection(
  const model: TComponentModel; const propertyName: string): IInjection;
begin
  Result := TPropertyInjection.Create(model, propertyName);
end;

function TInjectionFactory.CreateFieldInjection(
  const model: TComponentModel; const fieldName: string): IInjection;
begin
  Result := TFieldInjection.Create(model, fieldName);
end;

{$ENDREGION}


end.
