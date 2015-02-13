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

unit Spring.Container.CreationContext;

interface

uses
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  TCreationContext = class(TInterfacedObject, ICreationContext)
  private
    fResolutionStack: IStack<TComponentModel>;
    fModel: TComponentModel;
    fArguments: IList<TValue>;
    fPerResolveInstances: IDictionary<TComponentModel, TValue>;
    fNamedArguments: IList<TNamedValue>;
    fTypedArguments: IList<TTypedValue>;
  public
    constructor Create(const model: TComponentModel;
      const arguments: array of TValue);

    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): Boolean;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel; const argument: TValue): TValue;

    function EnterResolution(const model: TComponentModel;
      out instance: TValue): Boolean;
    procedure LeaveResolution(const model: TComponentModel);

    procedure AddArgument(const argument: TValue);
    procedure AddPerResolve(const model: TComponentModel; const instance: TValue);
    function TryHandle(const injection: IInjection;
      out handled: IInjection): Boolean;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Spring.Container.Injection,
  Spring.Container.ResourceStrings,
  Spring.Reflection;


{$REGION 'TCreationContext'}

constructor TCreationContext.Create(const model: TComponentModel;
  const arguments: array of TValue);
var
  i: Integer;
begin
  inherited Create;
  fResolutionStack := TCollections.CreateStack<TComponentModel>;
  fModel := model;
  fArguments := TCollections.CreateList<TValue>;
  fNamedArguments := TCollections.CreateList<TNamedValue>;
  fTypedArguments := TCollections.CreateList<TTypedValue>;
  for i := Low(arguments) to High(arguments) do
    AddArgument(arguments[i]);
  fPerResolveInstances := TCollections.CreateDictionary<TComponentModel, TValue>;
end;

procedure TCreationContext.AddArgument(const argument: TValue);
begin
  if argument.IsType<TTypedValue> then
    fTypedArguments.Add(argument)
  else if argument.IsType<TNamedValue> then
    fNamedArguments.Add(argument)
  else
    fArguments.Add(argument);
end;

procedure TCreationContext.AddPerResolve(const model: TComponentModel;
  const instance: TValue);
begin
  fPerResolveInstances.Add(model, instance);
end;

function TCreationContext.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  i: Integer;
begin
  for i := fTypedArguments.Count - 1 downto 0 do // check most recently added first
    if fTypedArguments[i].TypeInfo = dependency.TypeInfo then
      Exit(True);
  Result := False;
end;

function TCreationContext.TryHandle(const injection: IInjection;
  out handled: IInjection): Boolean;
var
  arguments: TArray<TValue>;
  i: Integer;
  parameters: TArray<TRttiParameter>;
  value: TNamedValue;
begin
  arguments := Copy(injection.Arguments);
  if not fModel.ConstructorInjections.Contains(injection) then
  begin
    handled := injection;
    Exit(True);
  end;

  parameters := injection.Target.AsMethod.GetParameters;
  if Length(parameters) = fArguments.Count then
  begin
    // arguments for ctor are provided and count is correct
    for i := Low(parameters) to High(parameters) do // check all parameters
      if fArguments[i].IsType(parameters[i].ParamType.Handle) then
        arguments[i] := fArguments[i]
      else
        Exit(False); // argument and parameter types did not match
  end
  else if fArguments.Any then
    Exit(False);
  for value in fNamedArguments do // check all named arguments
  begin
    Result := False;
    for i := Low(parameters) to High(parameters) do
    begin // look for parameter that matches the name and type
      if SameText(parameters[i].Name, value.Name)
        and value.Value.IsType(parameters[i].ParamType.Handle) then
      begin
        arguments[i] := value.Value;
        Result := True;
        Break;
      end;
    end;
    if not Result then // named argument was not found
      Exit;
  end;

  Result := True; // all parameters are handled - create new injection
  handled := TConstructorInjection.Create;
  handled.Initialize(injection.Target);
  handled.InitializeArguments(arguments);
  for i := 0 to injection.DependencyCount - 1 do
    handled.Dependencies[i] := injection.Dependencies[i];
end;

function TCreationContext.EnterResolution(const model: TComponentModel;
  out instance: TValue): Boolean;
begin
  if not Assigned(fModel) then // set the model if we don't know it yet
    fModel := model;
  if fPerResolveInstances.TryGetValue(model, instance) then
    Exit(False);
  if fResolutionStack.Contains(model) then
    raise ECircularDependencyException.CreateResFmt(
      @SCircularDependencyDetected, [model.ComponentTypeName]);
  fResolutionStack.Push(model);
  Result := True;
end;

procedure TCreationContext.LeaveResolution(const model: TComponentModel);
begin
  if fResolutionStack.Pop <> model then
    raise EResolveException.CreateRes(@SResolutionStackUnbalanced);
end;

function TCreationContext.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  i: Integer;
begin
  for i := fTypedArguments.Count - 1 downto 0 do
    if fTypedArguments[i].TypeInfo = dependency.TypeInfo then
      Exit(fTypedArguments[i].Value);
  raise EResolveException.CreateResFmt(@SCannotResolveType, [dependency.Name]);
end;

{$ENDREGION}


end.
