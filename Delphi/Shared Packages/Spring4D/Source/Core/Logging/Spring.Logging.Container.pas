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

unit Spring.Logging.Container;

interface

uses
  Rtti,
  TypInfo,
  Spring.Container,
  Spring.Container.Core,
  Spring.Container.Resolvers,
  Spring.Logging,
  Spring.Logging.Configuration;

type
  {$REGION 'TLoggingContainerHelper'}
  TLoggingContainerHelper = record
    class procedure RegisterAllAppenders(const container : TContainer); static;
    class procedure RegisterLogging(const container : TContainer); static;
    class procedure RegisterLoggingWithConfiguration(
      const container : TContainer); static;
  end;
  {$ENDREGION}


  {$REGION 'TLoggerResolver'}
  /// <summary>
  ///   Subresolver that will inject proper logger defined by the configuration.
  ///   In order to use this resolver make sure to register
  ///   <c>TLoggingConfiguration</c>.
  ///   <para>Rules:</para>
  ///   <para>* Base classes are checked as well so registering <c>TObject</c>
  ///   with some logger will make all objects log to that logger (unles their
  ///   logger injections are named).</para>
  ///   <para>* Named injections will be ignored by this resolver.</para>
  /// </summary>
  TLoggerResolver = class(TSubDependencyResolverBase)
  private
    {$IFDEF WEAKREF}[Unsafe]{$ENDIF}
    fConfiguration: TLoggingConfiguration;
    procedure EnsureConfiguration; inline;
  public
    function CanResolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): Boolean; override;
    function Resolve(const context: ICreationContext;
      const dependency: TDependencyModel;
      const argument: TValue): TValue; override;
  end;
  {$ENDREGION}


implementation

uses
  Spring.Logging.Controller;


{$REGION 'TLoggingContainerHelper'}

class procedure TLoggingContainerHelper.RegisterAllAppenders(
  const container: TContainer);
var
  controller: ILoggerController;
  appender: ILogAppender;
begin
  controller := container.Resolve<ILoggerController>;
  for appender in container.ResolveAll<ILogAppender> do
    controller.AddAppender(appender);
end;

class procedure TLoggingContainerHelper.RegisterLogging(
  const container: TContainer);
begin
  container.RegisterType<TLoggerController>.AsSingleton;
end;

class procedure TLoggingContainerHelper.RegisterLoggingWithConfiguration(
  const container: TContainer);
begin
  container.Kernel.Resolver.AddSubResolver(
    TLoggerResolver.Create(container.Kernel));
  container.RegisterType<TLoggingConfiguration>
    .Implements<TLoggingConfiguration>
    .AsSingleton;
  RegisterLogging(container);
end;

{$ENDREGION}


{$REGION 'TLoggerResolver'}

function TLoggerResolver.CanResolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): Boolean;
var
  componentType: TRttiType;
begin
  Result := (dependency.TypeInfo = System.TypeInfo(ILogger))
    and (dependency.TypeInfo <> argument.TypeInfo)
    and argument.IsEmpty // this is true for injections and even false for named injections
    and Assigned(dependency.Target) and Assigned(dependency.Target.Parent);
  if Result then
  begin
    EnsureConfiguration;
    componentType := dependency.ParentType;
    Result := fConfiguration.HasLogger(componentType.Handle);

    if not Result and componentType.IsInstance then
    begin
      while not Result and Assigned(TRttiInstanceType(componentType).BaseType) do
      begin
        componentType := TRttiInstanceType(componentType).BaseType;
        Result := fConfiguration.HasLogger(componentType.Handle);
      end;
    end;
  end;

  //Do not call inherited as registry most likely has some ILogger, just perform
  //the other check
  //If the argument is (string) not empty do nothing as the user already asked for very
  //specific implementation
end;

procedure TLoggerResolver.EnsureConfiguration;
begin
  if not Assigned(fConfiguration) then
    fConfiguration := (Kernel as IKernelInternal).Resolve(
      TypeInfo(TLoggingConfiguration)).AsType<TLoggingConfiguration>;
end;

function TLoggerResolver.Resolve(const context: ICreationContext;
  const dependency: TDependencyModel; const argument: TValue): TValue;
var
  handle: PTypeInfo;
  componentType: TRttiType;
begin
  Assert(Assigned(fConfiguration));

  componentType := dependency.ParentType;
  if fConfiguration.HasLogger(componentType.Handle) then
    handle := componentType.Handle
  else
  begin
    handle := nil;
    Assert(componentType.IsInstance);
    while Assigned(TRttiInstanceType(componentType).BaseType) do
    begin
      componentType := TRttiInstanceType(componentType).BaseType;
      if fConfiguration.HasLogger(componentType.Handle) then
      begin
        handle := componentType.Handle;
        Break;
      end;
    end;
  end;

  Assert(Assigned(handle));
  Result := (Kernel as IKernelInternal).Resolve(fConfiguration.GetLogger(handle));
end;

{$ENDREGION}


end.
