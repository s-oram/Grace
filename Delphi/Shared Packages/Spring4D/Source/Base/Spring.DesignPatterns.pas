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
///	  This namespace contains the following classical design patterns:
///	  <list type="bullet">
///	    <item>
///	      <b>Factory Pattern</b>
///	    </item>
///	    <item>
///	      <b><see cref="TSingleton">Singleton Pattern</see></b>
///	    </item>
///	    <item>
///	      <b>Observer Pattern</b>
///	    </item>
///	    <item>
///	      <b><see cref="ISpecification&lt;T&gt;">Specification Pattern</see></b>
///	    </item>
///	  </list>
///	</summary>
///	<preliminary />
unit Spring.DesignPatterns;

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  TypInfo,
  Spring,
  Spring.Collections;

type

  {$REGION 'Singleton Pattern'}

  ///	<summary>
  ///	  <para>
  ///	    Provides a simple implementation of the <b>Singleton Pattern</b>. Use
  ///	    this portal to get the shared instance of a certain class which must
  ///	    have a default constructor.
  ///	  </para>
  ///	  <para>
  ///	    It also keeps track of the lifetime of the instances and will free
  ///	    them in reversed order.
  ///	  </para>
  ///	</summary>
  ///	<remarks>
  ///	  This class just demonstrates how to apply the classical Singleton
  ///	  Pattern. It's recommended to use the Spring IoC container which is more
  ///	  flexible.
  ///	</remarks>
  ///	<threadsafety static="true" />
  TSingleton = record
  strict private
    class var
      fMappings: IDictionary<TClass, TObject>;

      ///	<summary>
      ///	  Tracks all instances of the singleton objects and free them in
      ///	  reversed order.
      ///	</summary>
      fInstances: IList<TObject>;

      fCriticalSection: TCriticalSection;

    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}

  public

    ///	<summary>
    ///	  Gets the shared instance of a class.
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type of a class which must have a default constructor.
    ///	</typeparam>
    class function GetInstance<T: class, constructor>: T; static;
  end;

  {$ENDREGION}


  {$REGION 'Observer Pattern'}

  ///	<summary>
  ///	  Represents an observable subject.
  ///	</summary>
  IObservable<T> = interface
    procedure AddListener(const listener: T);
    procedure RemoveListener(const listener: T);
    procedure NotifyListeners(callback: TProc<T>);
  end;

  TListenerNotification = (
    lnAdded,
    lnRemoved
  );

  TObservable<T> = class(TInterfacedObject, IObservable<T>, IInterface)
  private
    fListeners: IList<T>;
  protected
    procedure Validate(const listener: T); virtual;
    procedure DoListenerAdded(const listener: T); virtual;
    procedure DoListenerRemoved(const listener: T); virtual;
    procedure Notify(const listener: T; action: TListenerNotification); virtual;
    function Contains(const listener: T): Boolean; virtual;
    function GetListeners: IList<T>; virtual;
    property Listeners: IList<T> read GetListeners;
  public
    destructor Destroy; override;
    procedure AddListener(const listener: T); virtual;
    procedure RemoveListener(const listener: T); virtual;
    procedure NotifyListeners(callback: TProc<T>); virtual;
  end;

  TSynchronizedObservable<T> = class(TObservable<T>, IObservable<T>, IInterface)
  protected
    fListenersLock: IReadWriteSync;
    function Contains(const listener: T): Boolean; override;
  public
    constructor Create;
    procedure AddListener(const listener: T); override;
    procedure RemoveListener(const listener: T); override;
    procedure NotifyListeners(callback: TProc<T>); override;
  end;

//  TAsyncObservable<T> = class(TInterfacedObject, IObservable<T>, IInterface)
//
//  end;

  {$ENDREGION}


  {$REGION 'Specification Pattern'}

//  ISpecification = interface
//    ['{9029A971-3A6C-4241-A246-C0F613ABE51C}']
//    function IsSatisfiedBy(const obj: TValue): Boolean;
//  end;

  ///	<summary>
  ///	  Defines the core methods of a specification interface.
  ///	</summary>
  ISpecification<T> = interface
    function IsSatisfiedBy(const item: T): Boolean;
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  ///	<summary>
  ///	  Provides the easy-going specification holder with operator overloads.
  ///	</summary>
  TSpecification<T> = record
  private
    fSpecification: ISpecification<T>;
  public
    function IsSatisfiedBy(const item: T): Boolean;

    class operator Implicit(const specification: ISpecification<T>): TSpecification<T>;
    class operator Implicit(const specification: TPredicate<T>): TSpecification<T>;
    class operator Implicit(const specification: TSpecification<T>): ISpecification<T>;
    class operator Implicit(const specification: TSpecification<T>): TPredicate<T>;
    class operator Explicit(const specification: ISpecification<T>): TSpecification<T>;
    class operator Explicit(const specification: TSpecification<T>): ISpecification<T>;
    class operator LogicalAnd(const left, right: TSpecification<T>): TSpecification<T>;
    class operator LogicalOr(const left, right: TSpecification<T>): TSpecification<T>;
    class operator LogicalNot(const value: TSpecification<T>): TSpecification<T>;
  end;

  ///	<summary>
  ///	  Provides the abstract base class for Specification.
  ///	</summary>
  TSpecificationBase<T> = class abstract(TInterfacedObject, ISpecification<T>, TPredicate<T>)
  protected
    function TPredicate<T>.Invoke = IsSatisfiedBy;
    function IsSatisfiedBy(const item: T): Boolean; virtual; abstract;
  end;

  TUnarySpecification<T> = class abstract(TSpecificationBase<T>)
  protected
    fSpecification: ISpecification<T>;
  public
    constructor Create(const specification: ISpecification<T>);
  end;

  TLogicalNotSpecification<T> = class sealed(TUnarySpecification<T>)
  protected
    function IsSatisfiedBy(const item: T): Boolean; override;
  end;

  TBinarySpecification<T> = class abstract(TSpecificationBase<T>)
  protected
    fLeft: ISpecification<T>;
    fRight: ISpecification<T>;
  public
    constructor Create(const left, right: ISpecification<T>);
  end;

  TLogicalAndSpecification<T> = class sealed(TBinarySpecification<T>)
  protected
    function IsSatisfiedBy(const item: T): Boolean; override;
  end;

  TLogicalOrSpecification<T> = class sealed(TBinarySpecification<T>)
  protected
    function IsSatisfiedBy(const item: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'Factory Pattern (Experimental)'}

  EFactoryMethodKeyAlreadyRegisteredException = class(Exception);
  EFactoryMethodKeyNotRegisteredException = class(Exception);

  TFactoryMethodKeyAlreadyRegisteredException = EFactoryMethodKeyAlreadyRegisteredException;
  TFactoryMethodKeyNotRegisteredException = EFactoryMethodKeyNotRegisteredException;

  TFactoryMethod<TBaseType> = reference to function: TBaseType;

  TFactory<TKey, TBaseType> = class
  private
    fFactoryMethods: IDictionary<TKey, TFactoryMethod<TBaseType>>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    procedure RegisterFactoryMethod(key: TKey; factoryMethod: TFactoryMethod<TBaseType>);
    procedure UnregisterFactoryMethod(key: TKey);
    function IsRegistered(key: TKey): boolean;
    function GetInstance(key: TKey): TBaseType;
  end;

  {$ENDREGION}


  {$REGION 'Registry (Experimental)'}

  ITypeRegistry<TClassType, TValue> = interface
  {$REGION 'Property Accessors'}
    function GetTypes: IEnumerable<TClassType>;
    function GetValues: IEnumerable<TValue>;
  {$ENDREGION}

    procedure Register(classType: TClassType; const value: TValue);
    procedure Unregister(classType: TClassType);
    procedure UnregisterAll;

    function GetValue(classType: TClassType): TValue;
    function TryGetValue(classType: TClassType; out value: TValue): Boolean;

    property Types: IEnumerable<TClassType> read GetTypes;
    property Values: IEnumerable<TValue> read GetValues;
  end;

  IClassTypeRegistry<TValue> = interface(ITypeRegistry<TClass, TValue>)
  end;

  TClassTypeRegistry<TValue> = class(TInterfacedObject, IClassTypeRegistry<TValue>)
  protected
    fLookup: IDictionary<TClass, TValue>;
    function GetTypes: IEnumerable<TClass>;
    function GetValues: IEnumerable<TValue>;
  public
    constructor Create;

    procedure Register(classType: TClass; const value: TValue);
    procedure Unregister(classType: TClass);
    procedure UnregisterAll;

    function GetValue(classType: TClass): TValue;
    function TryGetValue(classType: TClass; out value: TValue): Boolean;

    property Types: IEnumerable<TClass> read GetTypes;
    property Values: IEnumerable<TValue> read GetValues;
  end;


  {$ENDREGION}


implementation

uses
  Spring.ResourceStrings;


{$REGION 'TSingleton'}

class constructor TSingleton.Create;
begin
  fMappings :=  TCollections.CreateDictionary<TClass, TObject>(4);
  fInstances := TCollections.CreateObjectList<TObject>(True);
  fCriticalSection := TCriticalSection.Create;
end;

class destructor TSingleton.Destroy;
begin
  fCriticalSection.Free;
end;

class function TSingleton.GetInstance<T>: T;
begin
  fCriticalSection.Enter;
  try
    if not fMappings.TryGetValue(T, TObject(Result)) then
    begin
      Result := T.Create;
      fMappings.AddOrSetValue(T, TObject(Result));
      fInstances.Add(Result);
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

{$ENDREGION}


{$REGION 'TObservable<T>'}

destructor TObservable<T>.Destroy;
begin
  inherited Destroy;
end;

function TObservable<T>.Contains(const listener: T): Boolean;
begin
  Result := Listeners.Contains(listener);
end;

procedure TObservable<T>.Validate(const listener: T);
begin
end;

procedure TObservable<T>.DoListenerAdded(const listener: T);
begin
end;

procedure TObservable<T>.DoListenerRemoved(const listener: T);
begin
end;

procedure TObservable<T>.Notify(const listener: T;
  action: TListenerNotification);
begin
  case action of
    lnAdded:
    begin
      Listeners.Add(listener);
      DoListenerAdded(listener);
    end;
    lnRemoved:
    begin
      Listeners.Remove(listener);
      DoListenerRemoved(listener);
    end;
  end;
end;

procedure TObservable<T>.AddListener(const listener: T);
begin
  Validate(listener);
  if not Contains(listener) then
  begin
    Notify(listener, lnAdded);
  end;
end;

procedure TObservable<T>.RemoveListener(const listener: T);
begin
  Validate(listener);
  if Contains(listener) then
  begin
    Notify(listener, lnRemoved);
  end;
end;

procedure TObservable<T>.NotifyListeners(callback: TProc<T>);
var
  listener: T;
begin
  Guard.CheckNotNull(Assigned(callback), 'callback');

  for listener in Listeners do
  begin
    callback(listener);
  end;
end;

function TObservable<T>.GetListeners: IList<T>;
begin
  if fListeners = nil then
  begin
    fListeners := TCollections.CreateList<T>;
  end;
  Result := fListeners;
end;

{$ENDREGION}


{$REGION 'TSynchronizedObservable<T>'}

constructor TSynchronizedObservable<T>.Create;
begin
  inherited Create;
  fListenersLock := TMREWSync.Create;
  fListeners := TCollections.CreateList<T>;
end;

procedure TSynchronizedObservable<T>.AddListener(const listener: T);
begin
  fListenersLock.BeginWrite;
  try
    inherited AddListener(listener);
  finally
    fListenersLock.EndWrite;
  end;
end;

procedure TSynchronizedObservable<T>.RemoveListener(const listener: T);
begin
  fListenersLock.BeginWrite;
  try
    inherited RemoveListener(listener);
  finally
    fListenersLock.EndWrite;
  end;
end;

procedure TSynchronizedObservable<T>.NotifyListeners(callback: TProc<T>);
begin
  fListenersLock.BeginRead;
  try
    inherited NotifyListeners(callback);
  finally
    fListenersLock.EndRead;
  end;
end;

function TSynchronizedObservable<T>.Contains(const listener: T): Boolean;
begin
  fListenersLock.BeginRead;
  try
    Result := inherited Contains(listener);
  finally
    fListenersLock.EndRead;
  end;
end;

{$ENDREGION}


{$REGION 'TSpecification<T>'}

function TSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := Assigned(fSpecification) and fSpecification.IsSatisfiedBy(item);
end;

class operator TSpecification<T>.Implicit(
  const specification: ISpecification<T>): TSpecification<T>;
begin
  Result.fSpecification := specification;
end;

class operator TSpecification<T>.Implicit(
  const specification: TSpecification<T>): ISpecification<T>;
begin
  Result := specification.fSpecification;
end;

class operator TSpecification<T>.Implicit(
  const specification: TSpecification<T>): TPredicate<T>;
begin
  ISpecification<T>(Result) := specification.fSpecification;
end;

class operator TSpecification<T>.Implicit(
  const specification: TPredicate<T>): TSpecification<T>;
begin
  TPredicate<T>(Result.fSpecification) := specification;
end;

class operator TSpecification<T>.Explicit(
  const specification: ISpecification<T>): TSpecification<T>;
begin
  Result.fSpecification := specification;
end;

class operator TSpecification<T>.Explicit(
  const specification: TSpecification<T>): ISpecification<T>;
begin
  Result := specification.fSpecification;
end;

class operator TSpecification<T>.LogicalAnd(const left,
  right: TSpecification<T>): TSpecification<T>;
begin
  Result.fSpecification := TLogicalAndSpecification<T>.Create(
    left.fSpecification, right.fSpecification)
end;

class operator TSpecification<T>.LogicalOr(const left,
  right: TSpecification<T>): TSpecification<T>;
begin
  Result.fSpecification := TLogicalOrSpecification<T>.Create(
    left.fSpecification, right.fSpecification);
end;

class operator TSpecification<T>.LogicalNot(
  const value: TSpecification<T>): TSpecification<T>;
begin
  Result.fSpecification := TLogicalNotSpecification<T>.Create(
    value.fSpecification);
end;

{$ENDREGION}


{$REGION 'Logical Specifications'}

{ TUnarySpecification<T> }

constructor TUnarySpecification<T>.Create(const specification: ISpecification<T>);
begin
  inherited Create;
  fSpecification := specification;
end;

{ TBinarySpecification<T> }

constructor TBinarySpecification<T>.Create(const left, right: ISpecification<T>);
begin
  inherited Create;
  fLeft := left;
  fRight := right;
end;

{ TLogicalAndSpecification<T> }

function TLogicalAndSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(item) and fRight.IsSatisfiedBy(item);
end;

{ TLogicalOrSpecification<T> }

function TLogicalOrSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := fLeft.IsSatisfiedBy(item) or fRight.IsSatisfiedBy(item);
end;

{ TLogicalNotSpecification<T> }

function TLogicalNotSpecification<T>.IsSatisfiedBy(const item: T): Boolean;
begin
  Result := not fSpecification.IsSatisfiedBy(item);
end;

{$ENDREGION}


{$REGION 'TFactory<TKey, TBaseType>'}

constructor TFactory<TKey, TBaseType>.Create;
begin
  inherited Create;
  fFactoryMethods := TCollections.CreateDictionary<TKey, TFactoryMethod<TBaseType>>;
end;

destructor TFactory<TKey, TBaseType>.Destroy;
begin
  inherited;
end;

function TFactory<TKey, TBaseType>.GetCount: Integer;
begin
  Result := fFactoryMethods.Count;
end;

function TFactory<TKey, TBaseType>.GetInstance(key: TKey): TBaseType;
var
  factoryMethod : TFactoryMethod<TBaseType>;
begin
  if not IsRegistered(key) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');
  factoryMethod := fFactoryMethods.Items[key];
  if Assigned(factoryMethod) then
    Result := factoryMethod;
end;

function TFactory<TKey, TBaseType>.IsRegistered(key: TKey): boolean;
begin
  Result := fFactoryMethods.ContainsKey(key);
end;

procedure TFactory<TKey, TBaseType>.RegisterFactoryMethod(key: TKey;
  factoryMethod: TFactoryMethod<TBaseType>);
begin
  if IsRegistered(key) then
    raise TFactoryMethodKeyAlreadyRegisteredException.Create('Factory already registered');

  fFactoryMethods.Add(key, factoryMethod);
end;

procedure TFactory<TKey, TBaseType>.UnRegisterFactoryMethod(key: TKey);
begin
  if not IsRegistered(key) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');

  fFactoryMethods.Remove(key);
end;


{$ENDREGION}


{$REGION 'TClassTypeRegistry<TValue>'}

constructor TClassTypeRegistry<TValue>.Create;
begin
  inherited Create;
  fLookup := TCollections.CreateDictionary<TClass, TValue>;
end;

procedure TClassTypeRegistry<TValue>.Register(classType: TClass; const value: TValue);
begin
  fLookup.AddOrSetValue(classType, value);
end;

procedure TClassTypeRegistry<TValue>.Unregister(classType: TClass);
begin
  fLookup.Remove(classType);
end;

procedure TClassTypeRegistry<TValue>.UnregisterAll;
begin
  fLookup.Clear;
end;

function TClassTypeRegistry<TValue>.GetTypes: IEnumerable<TClass>;
begin
  Result := fLookup.Keys;
end;

function TClassTypeRegistry<TValue>.GetValue(classType: TClass): TValue;
begin
  if not TryGetValue(classType, Result) then
  begin
    raise Exception.Create('Failed to get value');
  end;
end;

function TClassTypeRegistry<TValue>.GetValues: IEnumerable<TValue>;
begin
  Result := fLookup.Values;
end;

function TClassTypeRegistry<TValue>.TryGetValue(classType: TClass; out value: TValue): Boolean;
begin
  Guard.CheckNotNull(classType, 'classType');

  Result := fLookup.TryGetValue(classType, value);
  while not Result and (classType.ClassParent <> nil) do
  begin
    classType := classType.ClassParent;
    Result := fLookup.TryGetValue(classType, value);
  end;
end;

{$ENDREGION}


end.
