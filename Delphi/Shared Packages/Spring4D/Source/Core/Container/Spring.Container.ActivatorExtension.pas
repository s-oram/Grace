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

unit Spring.Container.ActivatorExtension;

interface

uses
  Spring.Container,
  Spring.Container.Builder,
  Spring.Container.ComponentActivator,
  Spring.Container.Core,
  Spring.Container.Extensions;

type
  TActivatorContainerExtension = class(TContainerExtension)
  protected
    procedure Initialize; override;
  end;

  TActivatorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel;
      const model: TComponentModel); override;
  end;

  TReflectionComponentActivator2 = class(TReflectionComponentActivator)
  protected
    function SelectEligibleConstructor(
      const context: ICreationContext): IInjection; override;
  end;

implementation

uses
  Generics.Defaults,
  Rtti,
  Spring.Collections,
  Spring.Container.Common,
  Spring.Container.ResourceStrings,
  Spring.Reflection;


{$REGION 'TActivatorContainerExtension'}

procedure TActivatorContainerExtension.Initialize;
begin
  Kernel.Builder.AddInspector(TActivatorInspector.Create);
end;

{$ENDREGION}


{$REGION 'TActivatorInspector'}

procedure TActivatorInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
begin
  if not Assigned(model.ActivatorDelegate) then
    model.ComponentActivator := TReflectionComponentActivator2.Create(kernel, model);
end;

{$ENDREGION}


{$REGION 'TReflectionComponentActivator2'}

function TReflectionComponentActivator2.SelectEligibleConstructor(
  const context: ICreationContext): IInjection;
var
  maxCount: Integer;
  targetType: TRttiType;
  candidates: IEnumerable<IInjection>;
  candidate: IInjection;
begin
  Result := Model.ConstructorInjections.FirstOrDefault(
    function(const injection: IInjection): Boolean
    begin
      Result := injection.Target.AsMethod.HasCustomAttribute(InjectAttribute);
    end);
  if Assigned(Result) then
    Exit;

  maxCount := -1;
  targetType := nil;

  candidates := Model.ConstructorInjections.Ordered(
    function(const left, right: IInjection): Integer
    begin
      Result := right.Target.Parent.AncestorCount - left.Target.Parent.AncestorCount;
      if Result = 0 then
        Result := right.DependencyCount - left.DependencyCount;
    end).TakeWhile(
    function(const injection: IInjection): Boolean
    begin
      if maxCount = -1 then
        maxCount := injection.DependencyCount;
      if targetType = nil then
        targetType := injection.Target.Parent;
      Result := (injection.DependencyCount = maxCount)
        and (targetType = injection.Target.Parent);
    end).Where(
    function(const injection: IInjection): Boolean
    begin
      Result := TryHandle(context, injection, candidate);
    end);
  if candidates.Count > 1 then
    raise EResolveException.CreateResFmt(@SAmbiguousConstructor, [targetType.DefaultName]);
  Result := candidate;
end;

{$ENDREGION}


end.
