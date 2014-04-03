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

///	<summary>
///	  Defines the common interfaces for a service locator.
///	</summary>
unit Spring.Services;

{$I Spring.inc}

interface

uses
  Spring;

{$SCOPEDENUMS ON}

type

  {$REGION 'Lifetime Type & Attributes'}

  ///	<summary>
  ///	  Lifetime Type Enumeration.
  ///	</summary>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="PooledAttribute" />
  TLifetimeType = (
    ///	<summary>
    ///	  Unknown lifetime type.
    ///	</summary>
    Unknown,

    ///	<summary>
    ///	  Single instance.
    ///	</summary>
    Singleton,

    ///	<summary>
    ///	  Different instances.
    ///	</summary>
    Transient,

    ///	<summary>
    ///	  Every thread has a single instance.
    ///	</summary>
    SingletonPerThread,

    ///	<summary>
    ///	  Instances are transient except that they are recyclable.
    ///	</summary>
    Pooled,

    ///	<summary>
    ///	  Customized lifetime type.
    ///	</summary>
    Custom
  );

  ///	<summary>
  ///	  Defines if type is using reference counting
  ///	</summary>
  TRefCounting = (
    ///	<summary>
    ///	  Container decides (Yes for TInterfacedObject descendants, No for others)
    ///	</summary>
    Unknown,

    ///	<summary>
    ///	  Type is using reference counting
    ///	</summary>
    True,

    ///	<summary>
    ///	  Type is not using reference counting
    ///	</summary>
    False
  );

  ///	<summary>
  ///	  Represents an abstract lifetime attribute class base.
  ///	</summary>
  LifetimeAttributeBase = class abstract(TCustomAttribute)
  private
    fLifetimeType: TLifetimeType;
  public
    constructor Create(lifetimeType: TLifetimeType);
    property LifetimeType: TLifetimeType read fLifetimeType;
  end;

  ///	<summary>
  ///	  Applies this attribute when a component shares the single instance.
  ///	</summary>
  ///	<remarks>
  ///	  When this attribute is applied to a component, the shared instance will
  ///	  be returned whenever get the implementation of a service.
  ///	</remarks>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	[Singleton]
  ///	TEmailSender = class(TInterfacedObject, IEmailSender)
  ///	//...
  ///	end;
  ///	  </code>
  ///	</example>
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="PooledAttribute" />
  ///	<seealso cref="TLifetimeType" />
  SingletonAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  ///	<summary>
  ///	  Represents that a new instance of the component will be created when
  ///	  requested.
  ///	</summary>
  ///	<remarks>
  ///	  <note type="note">
  ///	    This attribute is the default option.
  ///	  </note>
  ///	</remarks>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="PooledAttribute" />
  ///	<seealso cref="TLifetimeType" />
  TransientAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  ///	<summary>
  ///	  Applies this attribute when a component shares the single instance per
  ///	  thread.
  ///	</summary>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="PooledAttribute" />
  ///	<seealso cref="TLifetimeType" />
  SingletonPerThreadAttribute = class(LifetimeAttributeBase)
  public
    constructor Create;
  end;

  ///	<summary>
  ///	  Represents that the target component can be pooled.
  ///	</summary>
  ///	<seealso cref="SingletonAttribute" />
  ///	<seealso cref="TransientAttribute" />
  ///	<seealso cref="SingletonPerThreadAttribute" />
  ///	<seealso cref="TLifetimeType" />
  PooledAttribute = class(LifetimeAttributeBase)
  private
    fMinPoolsize: Integer;
    fMaxPoolsize: Integer;
  public
    constructor Create(minPoolSize, maxPoolSize: Integer);
    property MinPoolsize: Integer read fMinPoolsize;
    property MaxPoolsize: Integer read fMaxPoolsize;
  end {$IFDEF CPUARM}experimental{$ENDIF};

  ///	<summary>
  ///	  Applies the <c>InjectAttribute</c> to injectable instance members of a
  ///	  class. e.g. constructors, methods, properties and even fields. Also
  ///	  works on parameters of a method.
  ///	</summary>
  ///	<seealso cref="ImplementsAttribute" />
  InjectAttribute = class(TCustomAttribute)
  private
    fValue: TValue;
    function GetHasValue: Boolean;
  public
    constructor Create; overload;
    constructor Create(const value: string); overload;
    constructor Create(value: Integer); overload;
    constructor Create(value: Extended); overload;
    constructor Create(value: Int64); overload;
    constructor Create(value: Boolean); overload;
    property Value: TValue read fValue;
    property HasValue: Boolean read GetHasValue;
  end;

  InjectionAttribute = InjectAttribute deprecated;

  ///	<summary>
  ///	  Applies this attribute to tell the IoC container which service is
  ///	  implemented by the target component. In addition, a service name can be
  ///	  specified.
  ///	</summary>
  ///	<remarks>
  ///	  <note type="note">
  ///	    This attribute can be specified more than once.
  ///	  </note>
  ///	</remarks>
  ///	<example>
  ///	  <code lang="Delphi">
  ///	[Implements(TypeInfo(IEmailSender))]
  ///	TRegularEmailSender = class(TInterfacedObject, IEmailSender)
  ///	end;
  ///	[Implements(TypeInfo(IEmailSender), 'mock-email-sender')]
  ///	TMockEmailSender = class(TInterfacedObject, IEmailSender)
  ///	end;
  ///	  </code>
  ///	</example>
  ///	<seealso cref="InjectAttribute" />
  ImplementsAttribute = class(TCustomAttribute)
  private
    fServiceType: PTypeInfo;
    fName: string;
  public
    constructor Create(serviceType: PTypeInfo); overload;
    constructor Create(serviceType: PTypeInfo; const name: string); overload;
    property ServiceType: PTypeInfo read fServiceType;
    property Name: string read fName;
  end;


  {$ENDREGION}


  {$REGION 'Lifecycle Interfaces'}

  ///	<summary>
  ///	  Lifecycle interface. If a component implements this interface, the
  ///	  dependency injection container will invoke the <c>Initialize</c> method
  ///	  when initiating an instance of the component.
  ///	</summary>
  ///	<seealso cref="IStartable" />
  ///	<seealso cref="IRecyclable" />
  ///	<seealso cref="IDisposable" />
  IInitializable = interface
    ['{A36BB399-E592-4DFB-A091-EDBA3BE0648B}']

    ///	<summary>
    ///	  Initializes the component.
    ///	</summary>
    procedure Initialize;
  end;

  ///	<summary>
  ///	  Lifecycle interface. Represents that the component can be started and
  ///	  stopped.
  ///	</summary>
  ///	<seealso cref="IInitializable" />
  ///	<seealso cref="IRecyclable" />
  ///	<seealso cref="IDisposable" />
  IStartable = interface
    ['{8D0252A1-7993-44AA-B0D9-326019B58E78}']
    procedure Start;
    procedure Stop;
  end;

  ///	<summary>
  ///	  Lifecycle interface. Only called for components that belongs to a pool
  ///	  when the component comes back to the pool.
  ///	</summary>
  ///	<seealso cref="IInitializable" />
  ///	<seealso cref="IStartable" />
  ///	<seealso cref="IDisposable" />
  IRecyclable = interface
    ['{85114F41-70E5-4AF4-A375-E445D4619E4D}']
    procedure Recycle;
  end;

  ///	<summary>
  ///	  Lifecycle interface. If the component implements this interface, all
  ///	  resources will be deallocate by calling the <c>Dispose</c> method.
  ///	</summary>
  ///	<seealso cref="IInitializable" />
  ///	<seealso cref="IStartable" />
  ///	<seealso cref="IRecyclable" />
  IDisposable = interface
    ['{6708F9BF-0237-462F-AFA2-DF8EF21939EB}']
    procedure Dispose;
  end;

  ///	<summary>
  ///	  Lifecycle interface. Implement this interface on a class that does not
  ///	  inherit from TInterfacedObject to make it compatible with pooling.
  ///	</summary>
  IRefCounted = interface
    ['{8779F9E7-2311-44AB-94A6-6BADE93551FF}']
    function GetRefCount: Integer;
    property RefCount: Integer read GetRefCount;
  end;

  {$ENDREGION}


  {$REGION 'Service locator'}

  ///	<summary>
  ///	  Defines an abstract interface to locate services.
  ///	</summary>
  IServiceLocator = interface
    ['{E8C39055-6634-4428-B343-2FB0E75527BC}']
    function GetService(serviceType: PTypeInfo): TValue; overload;
    function GetService(serviceType: PTypeInfo; const name: string): TValue; overload;
    function GetService(serviceType: PTypeInfo; const args: array of TValue): TValue; overload;
    function GetService(serviceType: PTypeInfo; const name: string; const args: array of TValue): TValue; overload;

    function GetAllServices(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const name: string): Boolean; overload;
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
    function GetService<T>(const name: string): T; overload;
    function GetService<T>(const args: array of TValue): T; overload;
    function GetService<T>(const name: string; const args: array of TValue): T; overload;
    function GetService(serviceType: PTypeInfo): TValue; overload;
    function GetService(serviceType: PTypeInfo; const name: string): TValue; overload;
    function GetService(serviceType: PTypeInfo; const args: array of TValue): TValue; overload;
    function GetService(serviceType: PTypeInfo; const name: string; const args: array of TValue): TValue; overload;

    function GetAllServices<TServiceType>: TArray<TServiceType>; overload;
    function GetAllServices(serviceType: PTypeInfo): TArray<TValue>; overload;

    function HasService(serviceType: PTypeInfo): Boolean; overload;
    function HasService(serviceType: PTypeInfo; const name: string): Boolean; overload;

    function TryGetService<T>(out service: T): Boolean; overload;
    function TryGetService<T>(const name: string; out service: T): Boolean; overload;
  end;

  {$ENDREGION}


  {$REGION 'Deprecated LifetimeType constants'}

const
  ltUnknown = TLifetimeType.Unknown deprecated;
  ltSingleton = TLifetimeType.Singleton deprecated;
  ltTransient = TLifetimeType.Transient deprecated;
  ltSingletonPerThread = TLifetimeType.SingletonPerThread deprecated;
  ltPooled = TLifetimeType.Pooled deprecated;
  ltCustom = TLifetimeType.Custom deprecated;

  {$ENDREGION}


///	<summary>
///	  Gets the shared instance of <see cref="TServiceLocator" /> class.
///	</summary>
///	<remarks>
///	  Since Delphi doesn't support generic methods for interfaces, the result
///	  type is TServiceLocator instead of IServiceLocator.
///	</remarks>
function ServiceLocator: TServiceLocator; inline;

implementation

uses
  SysUtils,
  Spring.ResourceStrings;

function ServiceLocator: TServiceLocator;
begin
  Result := TServiceLocator.GlobalInstance;
end;


{$REGION 'Attributes'}

{ LifetimeAttributeBase }

constructor LifetimeAttributeBase.Create(lifetimeType: TLifetimeType);
begin
  inherited Create;
  fLifetimeType := lifetimeType;
end;

{ SingletonAttribute }

constructor SingletonAttribute.Create;
begin
  inherited Create(TLifetimeType.Singleton);
end;

{ TransientAttribute }

constructor TransientAttribute.Create;
begin
  inherited Create(TLifetimeType.Transient);
end;

{ SingletonPerThreadAttribute }

constructor SingletonPerThreadAttribute.Create;
begin
  inherited Create(TLifetimeType.SingletonPerThread);
end;

{ PooledAttribute }

constructor PooledAttribute.Create(minPoolSize, maxPoolSize: Integer);
begin
  inherited Create(TLifetimeType.Pooled);
  fMinPoolsize := minPoolSize;
  fMaxPoolsize := maxPoolsize;
end;

{ InjectAttribute }

constructor InjectAttribute.Create;
begin
  inherited Create;
  fValue := TValue.Empty;
end;

constructor InjectAttribute.Create(const value: string);
begin
  inherited Create;
  fValue := value;
end;

constructor InjectAttribute.Create(value: Integer);
begin
  inherited Create;
  fValue := value;
end;

constructor InjectAttribute.Create(value: Extended);
begin
  inherited Create;
  fValue := value;
end;

constructor InjectAttribute.Create(value: Int64);
begin
  inherited Create;
  fValue := value;
end;

constructor InjectAttribute.Create(value: Boolean);
begin
  inherited Create;
  fValue := value;
end;

function InjectAttribute.GetHasValue: Boolean;
begin
  Result := not fValue.IsEmpty;
end;

{ ImplementsAttribute }

constructor ImplementsAttribute.Create(serviceType: PTypeInfo);
begin
  Create(serviceType, '');
end;

constructor ImplementsAttribute.Create(serviceType: PTypeInfo;
  const name: string);
begin
  inherited Create;
  fServiceType := serviceType;
  fName := name;
end;

{$ENDREGION}


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

function TServiceLocator.GetService(serviceType: PTypeInfo; const name: string): TValue;
begin
  Result := GetServiceLocator.GetService(serviceType, name);
end;

function TServiceLocator.GetService(serviceType: PTypeInfo;
  const args: array of TValue): TValue;
begin
  Result := GetServiceLocator.GetService(serviceType, args);
end;

function TServiceLocator.GetService(serviceType: PTypeInfo; const name: string;
  const args: array of TValue): TValue;
begin
  Result := GetServiceLocator.GetService(serviceType, name, args);
end;

function TServiceLocator.GetService<T>: T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T));
  Result := value.AsType<T>;
end;

function TServiceLocator.GetService<T>(const name: string): T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T), name);
  Result := value.AsType<T>;
end;

function TServiceLocator.GetService<T>(const args: array of TValue): T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T), args);
  Result := value.AsType<T>;
end;

function TServiceLocator.GetService<T>(const name: string;
  const args: array of TValue): T;
var
  value: TValue;
begin
  value := GetServiceLocator.GetService(TypeInfo(T), name, args);
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
  for i := 0 to High(Result) do
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

function TServiceLocator.HasService(serviceType: PTypeInfo; const name: string): Boolean;
begin
  Result := GetServiceLocator.HasService(serviceType, name);
end;

function TServiceLocator.TryGetService<T>(out service: T): Boolean;
begin
  Result := GetServiceLocator.HasService(TypeInfo(T));
  if Result then
    service := GetService<T>
  else
    service := Default(T);
end;

function TServiceLocator.TryGetService<T>(const name: string; out service: T): Boolean;
begin
  Result := GetServiceLocator.HasService(TypeInfo(T), name);
  if Result then
    service := GetService<T>(name)
  else
    service := Default(T);
end;

{$ENDREGION}


end.
