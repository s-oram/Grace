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

unit Spring.Container.ComponentActivator;

{$I Spring.inc}

interface

uses
  Spring,
  Spring.Collections,
  Spring.Container.Core;

type
  ///	<summary>
  ///	  Abstract ComponentActivator
  ///	</summary>
  TComponentActivatorBase = class abstract(TInterfacedObject, IComponentActivator, IInterface)
  protected
    {$IFDEF WEAKREF}[Weak]{$ENDIF}
    fModel: TComponentModel;
    procedure ExecuteInjections(const instance: TValue;
      const injections: IList<IInjection>; const resolver: IDependencyResolver);
  public
    constructor Create(const model: TComponentModel);
    function CreateInstance(const resolver: IDependencyResolver): TValue; overload; virtual; abstract;
  end;

  ///	<summary>
  ///	  Activates an instance by reflection.
  ///	</summary>
  TReflectionComponentActivator = class(TComponentActivatorBase)
  private
    function GetEligibleConstructor(const model: TComponentModel;
      const resolver: IDependencyResolver): IInjection;
  public
    function CreateInstance(const resolver: IDependencyResolver): TValue; override;
  end;

  ///	<summary>
  ///	  Activates an instance by a TActivatorDelegate delegate.
  ///	</summary>
  TDelegateComponentActivator = class(TComponentActivatorBase)
  public
    function CreateInstance(const resolver: IDependencyResolver): TValue; override;
  end;

implementation

uses
  Spring.Container.ResourceStrings,
  Spring.Helpers,
  Spring.Reflection,
  Spring.Services;


{$REGION 'TComponentActivatorBase'}

constructor TComponentActivatorBase.Create(const model: TComponentModel);
begin
  inherited Create;
  fModel := model;
end;

procedure TComponentActivatorBase.ExecuteInjections(const instance: TValue;
  const injections: IList<IInjection>; const resolver: IDependencyResolver);
var
  injection: IInjection;
  arguments: TArray<TValue>;
begin
  for injection in injections do
  begin
    arguments := resolver.ResolveDependencies(injection);
    injection.Inject(instance, arguments);
  end;
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator'}

function TReflectionComponentActivator.CreateInstance(
  const resolver: IDependencyResolver): TValue;
var
  constructorInjection: IInjection;
  constructorArguments: TArray<TValue>;
begin
  constructorInjection := GetEligibleConstructor(fModel, resolver);
  if constructorInjection = nil then
  begin
    raise EActivatorException.CreateRes(@SUnsatisfiedConstructor);
  end;
  constructorArguments := resolver.ResolveDependencies(constructorInjection);
  Result := TActivator.CreateInstance(
    fModel.ComponentType.AsInstance,
    constructorInjection.Target.AsMethod,
    constructorArguments
  );
  try
    ExecuteInjections(Result, fModel.FieldInjections, resolver);
    ExecuteInjections(Result, fModel.PropertyInjections, resolver);
    ExecuteInjections(Result, fModel.MethodInjections, resolver);
  except
    if not Result.IsEmpty and Result.IsObject then
    begin
{$IFNDEF AUTOREFCOUNT}
      Result.AsObject.Free;
{$ENDIF}
      Result := TValue.Empty;
    end;
    raise;
  end;
end;

function TReflectionComponentActivator.GetEligibleConstructor(
  const model: TComponentModel; const resolver: IDependencyResolver): IInjection;
var
  candidate: IInjection;
  winner: IInjection;
  maxCount: Integer;
begin
  winner := nil;
  maxCount := -1;

  for candidate in model.ConstructorInjections do
  begin
    if candidate.Target.HasCustomAttribute<InjectAttribute> then
    begin
      winner := candidate;
      Break;
    end;
    if resolver.CanResolveDependencies(candidate) then
    begin
      if candidate.DependencyCount > maxCount then
      begin
        winner := candidate;
        maxCount := candidate.DependencyCount;
      end;
    end;
  end;
  Result := winner;
end;

{$ENDREGION}


{$REGION 'TDelegateComponentActivator'}

function TDelegateComponentActivator.CreateInstance(
  const resolver: IDependencyResolver): TValue;
begin
  if not Assigned(fModel.ActivatorDelegate) then
  begin
    raise EActivatorException.CreateRes(@SActivatorDelegateExpected);
  end;
  Result := fModel.ActivatorDelegate.Invoke;
  try
    ExecuteInjections(Result, fModel.FieldInjections, resolver);
    ExecuteInjections(Result, fModel.PropertyInjections, resolver);
    ExecuteInjections(Result, fModel.MethodInjections, resolver);
  except
    if not Result.IsEmpty and Result.IsObject then
    begin
{$IFNDEF AUTOREFCOUNT}
      Result.AsObject.Free;
{$ENDIF}
      Result := TValue.Empty;
    end;
    raise;
  end;
end;

{$ENDREGION}


end.
