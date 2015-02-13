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

unit Spring.Container.DecoratorExtension;

interface

uses
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Container.Builder,
  Spring.Container.ComponentActivator,
  Spring.Container.Core,
  Spring.Container.Extensions;

type
  TDecoratorContainerExtension = class(TContainerExtension)
  protected
    procedure Initialize; override;
  end;

  TDecoratorInspector = class(TInspectorBase)
  protected
    procedure DoProcessModel(const kernel: IKernel; const model: TComponentModel); override;
  end;

  TDecoratorComponentActivator = class(TInterfacedObject, IComponentActivator)
  private
    fKernel: IKernel;
    fComponentActivator: IComponentActivator;
    fDecoratorModel: TComponentModel;
    fServiceType: PTypeInfo;
  public
    constructor Create(const kernel: IKernel;
      const componentActivator: IComponentActivator;
      const decoratorModel: TComponentModel; serviceType: PTypeInfo);

    function CreateInstance(const context: ICreationContext): TValue;
  end;

implementation

uses
  Rtti,
  TypInfo,
  Spring.Container.Resolvers,
  Spring.Reflection;


{$REGION 'TDecoratorExtension'}

procedure TDecoratorContainerExtension.Initialize;
begin
  Kernel.Builder.AddInspector(TDecoratorInspector.Create);
end;

{$ENDREGION}


{$REGION 'TDecoratorInspector'}

procedure TDecoratorInspector.DoProcessModel(const kernel: IKernel;
  const model: TComponentModel);
var
  serviceType: PTypeInfo;
  componentModel: TComponentModel;
  decoratorModel: TComponentModel;
  predicate: TPredicate<TRttiMethod>;
begin
  for serviceType in model.Services.Values do
  begin
    // only support decorators that have a constructor
    // with exact one parameter that is of the decorating type
    predicate := TMethodFilters.IsConstructor
      and TMethodFilters.ContainsParameterType(serviceType);
    decoratorModel := nil;
    for componentModel in kernel.Registry.FindAll.Where(
      function(const model: TComponentModel): Boolean
      begin
        Result := model.HasService(serviceType);
      end) do
    begin
      if Assigned(decoratorModel) and (model = componentModel) then
      begin
        model.ComponentActivator := TDecoratorComponentActivator.Create(kernel,
          model.ComponentActivator, decoratorModel, serviceType);
      end
      else
        if componentModel.ComponentType.Methods.Any(predicate) then
          decoratorModel := componentModel;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TComponentActivatorDecorator'}

constructor TDecoratorComponentActivator.Create(const kernel: IKernel;
  const componentActivator: IComponentActivator;
  const decoratorModel: TComponentModel; serviceType: PTypeInfo);
begin
  inherited Create;
  fKernel := kernel;
  fComponentActivator := componentActivator;
  fDecoratorModel := decoratorModel;
  fServiceType := serviceType;
end;

function TDecoratorComponentActivator.CreateInstance(
  const context: ICreationContext): TValue;

{$IFDEF DELPHI2010}
  function ConvClass2Inf(const AObject: TObject; ATarget: PTypeInfo): TValue;
  var
    intf: Pointer;
  begin
    if AObject.GetInterface(GetTypeData(ATarget).Guid, intf) then
      TValue.MakeWithoutCopy(@intf, ATarget, Result);
  end;
{$ENDIF}

begin
  Result := fComponentActivator.CreateInstance(context);
{$IFDEF DELPHI2010}
  if Result.IsObject and (fServiceType.Kind = tkInterface) then
    Result := ConvClass2Inf(Result.AsObject, fServiceType);
{$ENDIF}
  context.AddArgument(TTypedValue.Create(fServiceType, Result));
  Result := fDecoratorModel.ComponentActivator.CreateInstance(context);
end;

{$ENDREGION}


end.
