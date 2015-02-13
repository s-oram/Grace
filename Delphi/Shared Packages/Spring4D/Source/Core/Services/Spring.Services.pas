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

///	<summary>
///	  Defines the common interfaces for a service locator.
///	</summary>
unit Spring.Services;

interface

uses
  Spring,
  Spring.Container.Common;

type

  {$REGION 'Lifetime Type & Attributes'}

  TLifetimeType = Spring.Container.Common.TLifetimeType
    deprecated 'Use Spring.Container.Common.TLifetimeType';

  TRefCounting = Spring.Container.Common.TRefCounting
    deprecated 'Use Spring.Container.Common.TRefCounting';

  LifetimeAttributeBase = Spring.Container.Common.LifetimeAttributeBase
    deprecated 'Use Spring.Container.Common.LifetimeAttributeBase';

  SingletonAttribute = Spring.Container.Common.SingletonAttribute
    deprecated 'Use Spring.Container.Common.SingletonAttribute';

  TransientAttribute = Spring.Container.Common.TransientAttribute
    deprecated 'Use Spring.Container.Common.TransientAttribute';

  SingletonPerThreadAttribute = Spring.Container.Common.SingletonPerThreadAttribute
    deprecated 'Use Spring.Container.Common.SingletonPerThreadAttribute';

  PooledAttribute = Spring.Container.Common.PooledAttribute
    deprecated 'Use Spring.Container.Common.PooledAttribute';

  InjectAttribute = Spring.Container.Common.InjectAttribute
    deprecated 'Use Spring.Container.Common.InjectAttribute';

  ImplementsAttribute = Spring.Container.Common.ImplementsAttribute
    deprecated 'Use Spring.Container.Common.ImplementsAttribute';

  {$ENDREGION}


  {$REGION 'Lifecycle Interfaces'}

  IInitializable = Spring.Container.Common.IInitializable
    deprecated 'Use Spring.Container.Common.IInitializable';

  IStartable = Spring.Container.Common.IStartable
    deprecated 'Use Spring.Container.Common.IStartable';

  IRecyclable = Spring.Container.Common.IRecyclable
    deprecated 'Use Spring.Container.Common.IRecyclable';

  IDisposable = Spring.Container.Common.IDisposable
    deprecated 'Use Spring.Container.Common.IDisposable';

  IRefCounted = Spring.Container.Common.IRefCounted
    deprecated 'Use Spring.Container.Common.IRefCounted';

  {$ENDREGION}


  {$REGION 'Service locator'}

  ///	<summary>
  ///	  Defines an abstract interface to locate services.
  ///	</summary>
  IServiceLocator = interface
    ['{E8C39055-6634-4428-B343-2FB0E75527BC}']
    function GetService(serviceType: PTypeInfo): TValue; overload;
    function GetService(serviceType: PTypeInfo; const serviceName: string): TValue; overload;
    function GetService(serviceType: PTypeInfo; const args: array of TValue): TValue; overload;
    function GetService(serviceType: PTypeInfo; const serviceName: string; const args: array of TValue): TValue; overload;

    function GetAllServices(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const serviceName: string): Boolean; overload;
  end;

  TServiceLocatorDelegate = reference to function: IServiceLocator;

  ///	<summary>
  ///	  Provides a portal to get and query an instance of a service. Use the
  ///	  global <see cref="Spring.Services|ServiceLocator" /> method to get the
  ///	  shared instance.
  ///	</summary>
  ///	<remarks>
  ///	  You should use ServiceLocator to query a service insteading of directly
  ///	  using Spring.Container namespace in your library. The namespace is
  ///	  supposed to be used to register components in your bootstrap code.
  ///	</remarks>
  TServiceLocator = class sealed(TInterfaceBase, IServiceLocator)
  private
    fServiceLocatorProvider: TServiceLocatorDelegate;
    class var GlobalInstance: TServiceLocator;
    function GetServiceLocator: IServiceLocator;
    procedure RaiseNotInitialized;
    type
      TValueArray = array of TValue;
  protected
    class constructor Create;
    class destructor Destroy;
  public
    procedure Initialize(const provider: TServiceLocatorDelegate);

    function GetService<T>: T; overload;
    function GetService<T>(const serviceName: string): T; overload;
    function GetService<T>(const args: array of TValue): T; overload;
    function GetService<T>(const serviceName: string; const args: array of TValue): T; overload;
    function GetService(serviceType: PTypeInfo): TValue; overload;
    function GetService(serviceType: PTypeInfo; const serviceName: string): TValue; overload;
    function GetService(serviceType: PTypeInfo; const args: array of TValue): TValue; overload;
    function GetService(serviceType: PTypeInfo; const serviceName: string; const args: array of TValue): TValue; overload;

    function GetAllServices<TServiceType>: TArray<TServiceType>; overload;
    function GetAllServices(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const serviceName: string): Boolean; overload;

    function TryGetService<T>(out service: T): Boolean; overload;
    function TryGetService<T>(const serviceName: string; out service: T): Boolean; overload;
  end;

  {$ENDREGION}


///	<summary>
///	  Gets the shared instance of <see cref="TServiceLocator" /> class.
///	</summary>
///	<remarks>
///	  Since Delphi doesn't support generic methods for interfaces, the result
///	  type is TServiceLocator instead of IServiceLocator.
///	</remarks>
{$IFDEF AUTOREFCOUNT}[Result: Unsafe]{$ENDIF}
function ServiceLocator: TServiceLocator; {$IFNDEF AUTOREFCOUNT}inline;{$ENDIF}

implementation

uses
  SysUtils,
  Spring.ResourceStrings;

function ServiceLocator: TServiceLocator;
begin
  Result := TServiceLocator.GlobalInstance;
end;


{$REGION 'TServiceLocator'}

class constructor TServiceLocator.Create;
begin
  GlobalInstance := TServiceLocator.Create;
end;

class destructor TServiceLocator.Destroy;
begin
  FreeAndNil(GlobalInstance);
end;

procedure TServiceLocator.Initialize(const provider: TServiceLocatorDelegate);
begin
  fServiceLocatorProvider := provider;
end;

procedure TServiceLocator.RaiseNotInitialized;
begin
  raise EInvalidOperationException.Create(SServiceLocatorNotInitialized);
end;

function TServiceLocator.GetServiceLocator: IServiceLocator;
begin
  if Assigned(fServiceLocatorProvider) then
    Result := fServiceLocatorProvider();
  if not Assigned(Result) then
    RaiseNotInitialized;
end;

function TServiceLocator.GetService(serviceType: PTypeInfo): TValue;
begin
  Result := GetServiceLocator.GetService(serviceType);
end;

function TServiceLocator.GetService(serviceType: PTypeInfo; const serviceName: string): TValue;
begin
  Result := GetServiceLocator.GetService(serviceType, serviceName);
end;

function TServiceLocator.GetService(serviceType: PTypeInfo;
  const args: array of TValue): TValue;
begin
  Result := GetServiceLocator.GetService(serviceType, args);
end;

function TServiceLocator.GetService(serviceType: PTypeInfo; const serviceName: string;
  const args: array of TValue): TValue;
begin
  Result := GetServiceLocator.GetService(serviceType, serviceName, args);
end;

function TServiceLocator.GetService<T>: T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T));
  Result := value.AsType<T>;
end;

function TServiceLocator.GetService<T>(const serviceName: string): T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T), serviceName);
  Result := value.AsType<T>;
end;

function TServiceLocator.GetService<T>(const args: array of TValue): T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T), args);
  Result := value.AsType<T>;
end;

function TServiceLocator.GetService<T>(const serviceName: string;
  const args: array of TValue): T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T), serviceName, args);
  Result := value.AsType<T>;
end;

function TServiceLocator.GetAllServices(serviceType: PTypeInfo): TArray<TValue>;
begin
  Result := GetServiceLocator.GetAllServices(serviceType);
end;

function TServiceLocator.GetAllServices<TServiceType>: TArray<TServiceType>;
var
  services: TArray<TValue>;
  i: Integer;
begin
  services := GetServiceLocator.GetAllServices(TypeInfo(TServiceType));
  SetLength(Result, Length(services));
  for i := Low(Result) to High(Result) do
  begin
    // accessing TArray<TValue> directly causes an internal error URW1111 in Delphi 2010 (see QC #77575)
    // the hardcast prevents it but does not cause any differences in compiled code
    Result[i] := TValueArray(services)[i].AsType<TServiceType>;
  end;
end;

function TServiceLocator.HasService(serviceType: PTypeInfo): Boolean;
begin
  Result := GetServiceLocator.HasService(serviceType);
end;

function TServiceLocator.HasService(serviceType: PTypeInfo; const serviceName: string): Boolean;
begin
  Result := GetServiceLocator.HasService(serviceType, serviceName);
end;

function TServiceLocator.TryGetService<T>(out service: T): Boolean;
begin
  Result := GetServiceLocator.HasService(TypeInfo(T));
  if Result then
    service := GetService<T>
  else
    service := Default(T);
end;

function TServiceLocator.TryGetService<T>(const serviceName: string; out service: T): Boolean;
begin
  Result := GetServiceLocator.HasService(TypeInfo(T), serviceName);
  if Result then
    service := GetService<T>(serviceName)
  else
    service := Default(T);
end;

{$ENDREGION}


end.
