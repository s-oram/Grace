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
///	  Declares the fundamental interfaces for the
///	  <see href="http://spring4d.org">Spring4D</see> Framework.
///	</summary>
unit Spring;

{$I Spring.inc}

interface

uses
  Classes,
  Diagnostics,
  Generics.Collections,
  Generics.Defaults,
  Rtti,
  SyncObjs,
  SysUtils,
  TimeSpan,
  Types,
  TypInfo,
  Variants;

type

  {$REGION 'Type redefinitions'}

  ///	<summary>
  ///	  Represents a dynamic array of Byte.
  ///	</summary>
  TBytes = SysUtils.TBytes;

  ///	<summary>
  ///	  Represents a dynamic array of string.
  ///	</summary>
  TStringDynArray = Types.TStringDynArray;

  ///	<summary>
  ///	  Represents a time interval.
  ///	</summary>
  TTimeSpan = TimeSpan.TTimeSpan;

  ///	<summary>
  ///	  Provides a set of methods and properties to accurately measure elapsed
  ///	  time.
  ///	</summary>
  TStopwatch = Diagnostics.TStopwatch;

  PTypeInfo = TypInfo.PTypeInfo;

  TValue = Rtti.TValue;

  ///	<summary>
  ///	  Represents the class type of <see cref="System|TCustomAttribute" />.
  ///	</summary>
  TAttributeClass = class of TCustomAttribute;

{$IFNDEF DELPHIXE_UP}
  TThreadID = LongWord;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'Procedure types'}

  ///	<summary>
  ///	  Represents a logical predicate.
  ///	</summary>
  ///	<param name="value">
  ///	  the value needs to be determined.
  ///	</param>
  ///	<returns>
  ///	  Returns <c>True</c> if the value was accepted, otherwise, returns
  ///	  <c>False</c>.
  ///	</returns>
  ///	<remarks>
  ///	  <note type="tip">
  ///	    This type redefined the
  ///	    <see cref="SysUtils|TPredicate`1">SysUtils.TPredicate&lt;T&gt;</see> 
  ///	    type with a const parameter.
  ///	  </note>
  ///	</remarks>
  ///	<seealso cref="Spring.DesignPatterns|ISpecification&lt;T&gt;" />
  TPredicate<T> = reference to function(const value: T): Boolean;

  ///	<summary>
  ///	  Represents an anonymous method that has a single parameter and does not
  ///	  return a value.
  ///	</summary>
  ///	<seealso cref="TActionProc&lt;T&gt;" />
  ///	<seealso cref="TActionMethod&lt;T&gt;" />
  TAction<T> = reference to procedure(const obj: T);

  ///	<summary>
  ///	  Represents a procedure that has a single parameter and does not return
  ///	  a value.
  ///	</summary>
  ///	<seealso cref="TAction&lt;T&gt;" />
  ///	<seealso cref="TActionMethod&lt;T&gt;" />
  TActionProc<T> = procedure(const obj: T);

  ///	<summary>
  ///	  Represents a instance method that has a single parameter and does not
  ///	  return a value.
  ///	</summary>
  ///	<seealso cref="TAction&lt;T&gt;" />
  ///	<seealso cref="TActionProc&lt;T&gt;" />
  TActionMethod<T> = procedure(const obj: T) of object;

  {$ENDREGION}


  {$REGION 'TInterfaceBase'}

  ///	<summary>
  ///	  Provides a non-reference-counted <see cref="System|IInterface" /> 
  ///	  implementation.
  ///	</summary>
  TInterfaceBase = class abstract(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  {$ENDREGION}


  {$REGION 'Guard'}

  ///	<summary>
  ///	  Provides static methods to check arguments and raise argument
  ///	  exceptions.
  ///	</summary>
  ///	<remarks>
  ///	  It's recommended that all arguments of public types and members should
  ///	  be checked.
  ///	</remarks>
  Guard = record
  strict private
    class procedure DoCheckIndex(length, index, indexBase: Integer); overload; static; inline;
  private
    class procedure DoCheckArrayIndex(length, index: Integer); static; inline;
    class procedure DoCheckArrayRange(length, startIndex, count: Integer); static; inline;
    class procedure DoCheckStringIndex(length, index: Integer); static; inline;
    class procedure DoCheckStringRange(length, startIndex, count: Integer); static; inline;
  public
    class procedure CheckTrue(condition: Boolean; const msg: string = ''); static; inline;
    class procedure CheckFalse(condition: Boolean; const msg: string = ''); static; inline;

    class procedure CheckInheritsFrom(obj: TObject; parentClass: TClass; const argumentName: string); overload; static; inline;
    class procedure CheckInheritsFrom(cls, parentClass: TClass; const argumentName: string); overload; static; inline;

    class procedure CheckNotNull(argumentValue: TObject; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(argumentValue: Pointer; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(const argumentValue: IInterface; const argumentName: string); overload; static; inline;
    class procedure CheckNotNull(condition: Boolean; const parameterName: string); overload; static; inline;
    class procedure CheckNotNull<T>(const argumentValue: T; const argumentName: string); overload; static; inline;

    class procedure CheckEnum<T{:enum}>(const argumentValue: T; const argumentName: string); overload; static; inline;
    class procedure CheckEnum<T{:enum}>(argumentValue: Integer; const argumentName: string); overload; static; inline;

    ///	<exception cref="Spring|EArgumentOutOfRangeException">
    ///	  Raised if the <paramref name="index" /> is out of range.
    ///	</exception>
    class procedure CheckRange(const buffer: array of Byte; index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Byte; startIndex, count: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; startIndex, count: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; index: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; startIndex, count: Integer); overload; static;
    class procedure CheckRange(const s: string; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: string; startIndex, count: Integer); overload; static; inline;
{$IFNDEF NEXTGEN}
    class procedure CheckRange(const s: WideString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: WideString; startIndex, count: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; startIndex, count: Integer); overload; static; inline;
{$ENDIF}
    class procedure CheckRange(condition: Boolean; const argumentName: string); overload; static; inline;
    class procedure CheckRange(length, startIndex, count: Integer; indexBase: Integer = 0); overload; static; inline;

    class procedure CheckTypeKind(typeInfo: PTypeInfo; expectedTypeKind: TTypeKind; const argumentName: string); overload; static;
    class procedure CheckTypeKind(typeInfo: PTypeInfo; expectedTypeKinds: TTypeKinds; const argumentName: string); overload; static;

    class function IsNullReference(const value; typeInfo: PTypeInfo): Boolean; static;

    ///	<summary>
    ///	  Raises an <see cref="EArgumentException" /> exception.
    ///	</summary>
    ///	<param name="msg">
    ///	  The general error message.
    ///	</param>
    class procedure RaiseArgumentException(const msg: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EFormatException" /> exception.
    ///	</summary>
    class procedure RaiseArgumentFormatException(const argumentName: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EArgumentNullException" /> exception.
    ///	</summary>
    class procedure RaiseArgumentNullException(const argumentName: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EArgumentOutOfRangeException" /> exception.
    ///	</summary>
    class procedure RaiseArgumentOutOfRangeException(const argumentName: string); overload; static; inline;

    ///	<summary>
    ///	  Raises an <see cref="EInvalidEnumArgumentException" /> exception.
    ///	</summary>
    class procedure RaiseInvalidEnumArgumentException(const argumentName: string); overload; static; inline;
  end;

  TArgument = Guard deprecated 'Use Guard instead';

  {$ENDREGION}


  {$REGION 'Nullable Types'}

  ///	<summary>
  ///	  A nullable type can represent the normal range of values for its
  ///	  underlying value type, plus an additional <c>Null</c> value.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The underlying value type of the <see cref="Nullable&lt;T&gt;" /> 
  ///	  generic type.
  ///	</typeparam>
  Nullable<T> = record
  private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;

    ///	<summary>
    ///	  Internal use. Marks the current instance as null.
    ///	</summary>
    ///	<remarks>
    ///	  The <see cref="Nullable&lt;T&gt;" /> type is immutable so that this
    ///	  method must be private.
    ///	</remarks>
    procedure Clear;

    ///	<summary>
    ///	  Determines whether a variant value is null or empty.
    ///	</summary>
    class function VarIsNullOrEmpty(const value: Variant): Boolean; static;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="Nullable&lt;T&gt;" /> 
    ///	  structure to the specified value.
    ///	</summary>
    constructor Create(const value: T); overload;

    ///	<summary>
    ///	  Initializes a new instance of the <see cref="Nullable&lt;T&gt;" /> 
    ///	  structure to the specified value.
    ///	</summary>
    constructor Create(const value: Variant); overload;

    ///	<summary>
    ///	  Retrieves the value of the current <see cref="Nullable&lt;T&gt;" /> 
    ///	  object, or the object's default value.
    ///	</summary>
    function GetValueOrDefault: T; overload;

    ///	<summary>
    ///	  Retrieves the value of the current <see cref="Nullable&lt;T&gt;" /> 
    ///	  object, or the specified default value.
    ///	</summary>
    ///	<param name="defaultValue">
    ///	  A value to return if the <see cref="HasValue" /> property is
    ///	  <c>False</c>.
    ///	</param>
    ///	<returns>
    ///	  The value of the <see cref="Value" /> property if the
    ///	  <see cref="HasValue" /> property is true; otherwise, the
    ///	  <paramref name="defaultValue" /> parameter.
    ///	</returns>
    ///	<remarks>
    ///	  The <see cref="GetValueOrDefault" /> method returns a value even if
    ///	  the <see cref="HasValue" /> property is false (unlike the
    ///	  <see cref="Value" /> property, which throws an exception).
    ///	</remarks>
    function GetValueOrDefault(const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Determines whether two nullable value are equal.
    ///	</summary>
    ///	<remarks>
    ///	  <p> If both two nullable values are null, return true; </p>
    ///	  <p> If either one is null, return false; </p>
    ///	  <p> else compares their values as usual. </p>
    ///	</remarks>
    function Equals(const other: Nullable<T>): Boolean;

    ///	<summary>
    ///	  Gets a value indicating whether the current
    ///	  <see cref="Nullable&lt;T&gt;" /> structure has a value.
    ///	</summary>
    property HasValue: Boolean read GetHasValue;

    ///	<summary>
    ///	  Gets the value of the current <see cref="Nullable&lt;T&gt;" /> value.
    ///	</summary>
    ///	<exception cref="Spring|EInvalidOperationException">
    ///	  Raised if the value is null.
    ///	</exception>
    property Value: T read GetValue;

    { Operator Overloads }
    class operator Implicit(const value: Nullable<T>): T;
    class operator Implicit(const value: T): Nullable<T>;
    class operator Implicit(const value: Nullable<T>): Variant;
    class operator Implicit(const value: Variant): Nullable<T>;
    class operator Implicit(value: Pointer): Nullable<T>;
    class operator Explicit(const value: Nullable<T>): T;
    class operator Equal(const a, b: Nullable<T>) : Boolean;
    class operator NotEqual(const a, b: Nullable<T>) : Boolean;
  end;


  ///	<summary>
  ///	  Represents a nullable unicode string.
  ///	</summary>
  TNullableString = Nullable<string>;
{$IFNDEF NEXTGEN}
  ///	<summary>
  ///	  Represents a nullable ansi string.
  ///	</summary>
  TNullableAnsiString = Nullable<AnsiString>;

  ///	<summary>
  ///	  Represents a nullable wide string.
  ///	</summary>
  TNullableWideString = Nullable<WideString>;
{$ENDIF}
  ///	<summary>
  ///	  Represents a nullable integer.
  ///	</summary>
  TNullableInteger = Nullable<Integer>;

  ///	<summary>
  ///	  Represents a nullable <c>Int64</c>.
  ///	</summary>
  TNullableInt64 = Nullable<Int64>;

  ///	<summary>
  ///	  Represents a nullable native integer.
  ///	</summary>
  TNullableNativeInt = Nullable<NativeInt>;

  ///	<summary>
  ///	  Represents a nullable <c>TDateTime</c>.
  ///	</summary>
  TNullableDateTime = Nullable<TDateTime>;

  ///	<summary>
  ///	  Represents a nullable <c>Currency</c>.
  ///	</summary>
  TNullableCurrency = Nullable<Currency>;

  ///	<summary>
  ///	  Represents a nullable <c>Double</c>.
  ///	</summary>
  TNullableDouble = Nullable<Double>;

  ///	<summary>
  ///	  Represents a nullable <c>Boolean</c>.
  ///	</summary>
  TNullableBoolean = Nullable<Boolean>;

  ///	<summary>
  ///	  Represents a nullable <c>TGuid</c>.
  ///	</summary>
  TNullableGuid = Nullable<TGUID>;

  {$ENDREGION}


  {$REGION 'Lazy Initialization'}

  ///	<summary>
  ///	  Provides support for lazy initialization.
  ///	</summary>
  ILazy = interface
    ['{40223BA9-0C66-49E7-AA33-BDAEF9F506D6}']
  {$REGION 'Property Accessors'}
    function GetIsValueCreated: Boolean;
    function GetValue: TValue;
  {$ENDREGION}

    ///	<summary>
    ///	  Gets a value that indicates whether a value has been created for this
    ///	  <see cref="ILazy" /> instance.
    ///	</summary>
    ///	<value>
    ///	  <b>True</b> if a value has been created for this
    ///	  <see cref="ILazy" /> instance; otherwise, <b>False</b>.
    ///	</value>
    property IsValueCreated: Boolean read GetIsValueCreated;

    ///	<summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="ILazy" /> instance.
    ///	</summary>
    ///	<value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="ILazy" /> instance.
    ///	</value>
    property Value: TValue read GetValue;
  end;

  ///	<summary>
  ///	  Provides support for lazy initialization.
  ///	</summary>
  ILazy<T> = interface(ILazy)
  {$REGION 'Property Accessors'}
    function GetValue: T;
  {$ENDREGION}

    ///	<summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="ILazy&lt;T&gt;" /> instance.
    ///	</summary>
    ///	<value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="ILazy&lt;T&gt;" /> instance.
    ///	</value>
    property Value: T read GetValue;
  end;

  TLazy = class(TInterfacedObject, ILazy)
  private
    fLock: TCriticalSection;
    fIsValueCreated: Boolean;
  {$REGION 'Property Accessors'}
    function GetIsValueCreated: Boolean;
    function GetValueNonGeneric: TValue; virtual; abstract;
    function ILazy.GetValue = GetValueNonGeneric;
  {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;

    ///	<summary>
    ///	  Gets a value that indicates whether a value has been created for this
    ///	  <see cref="TLazy&lt;T&gt;" /> instance.
    ///	</summary>
    ///	<value>
    ///	  <b>True</b> if a value has been created for this
    ///	  <see cref="TLazy&lt;T&gt;" /> instance; otherwise, <b>False</b>.
    ///	</value>
    property IsValueCreated: Boolean read GetIsValueCreated;
  end;

  ///	<summary>
  ///	  Provides support for lazy initialization.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of object that is being lazily initialized.
  ///	</typeparam>
  TLazy<T> = class(TLazy, ILazy<T>, TFunc<T>)
  private
    fValueFactory: TFunc<T>;
    fValue: T;
    procedure EnsureInitialized; inline;
  {$REGION 'Property Accessors'}
    function GetValue: T;
    function GetValueNonGeneric: TValue; override; final;
    function TFunc<T>.Invoke = GetValue;
  {$ENDREGION}
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="TLazy&lt;T&gt;" />
    ///	  class. When lazy initialization occurs, the specified initialization
    ///	  function is used.
    ///	</summary>
    ///	<param name="valueFactory">
    ///	  The delegate that is invoked to produce the lazily initialized value
    ///	  when it is needed.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>valueFactory</i> is <b>nil</b>.
    ///	</exception>
    constructor Create(const valueFactory: TFunc<T>);

    ///	<summary>
    ///	  Initializes a new instance of <see cref="TLazy&lt;T&gt;" /> with the
    ///	  specified value.
    ///	</summary>
    ///	<param name="value">
    ///	  The initialized value.
    ///	</param>
    constructor CreateFrom(const value: T);

    ///	<summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="TLazy&lt;T&gt;" /> instance.
    ///	</summary>
    ///	<value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="TLazy&lt;T&gt;" /> instance.
    ///	</value>
    property Value: T read GetValue;
  end;

  ///	<summary>
  ///	  Provides support for lazy initialization.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of object that is being lazily initialized.
  ///	</typeparam>
  Lazy<T> = record
  private
    fLazy: ILazy<T>;
    function GetIsAssigned: Boolean;
    function GetIsValueCreated: Boolean;
    function GetValue: T;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="Lazy&lt;T&gt;" />
    ///	  record. When lazy initialization occurs, the specified initialization
    ///	  function is used.
    ///	</summary>
    ///	<param name="valueFactory">
    ///	  The delegate that is invoked to produce the lazily initialized value
    ///	  when it is needed.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>valueFactory</i> is <b>nil</b>.
    ///	</exception>
    constructor Create(const valueFactory: TFunc<T>);

    ///	<summary>
    ///	  Initializes a new instance of <see cref="Lazy&lt;T&gt;" /> with the
    ///	  specified value.
    ///	</summary>
    ///	<param name="value">
    ///	  The initialized value.
    ///	</param>
    constructor CreateFrom(const value: T);

    class operator Implicit(const value: Lazy<T>): ILazy<T>;
    class operator Implicit(const value: Lazy<T>): T;
    class operator Implicit(const value: T): Lazy<T>;
    class operator Implicit(const value: TLazy<T>): Lazy<T>;

    property IsAssigned: Boolean read GetIsAssigned;

    ///	<summary>
    ///	  Gets a value that indicates whether a value has been created for this
    ///	  <see cref="Lazy&lt;T&gt;" /> instance.
    ///	</summary>
    ///	<value>
    ///	  <b>True</b> if a value has been created for this
    ///	  <see cref="Lazy&lt;T&gt;" /> instance; otherwise, <b>False</b>.
    ///	</value>
    property IsValueCreated: Boolean read GetIsValueCreated;

    ///	<summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="Lazy&lt;T&gt;" /> instance.
    ///	</summary>
    ///	<value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="Lazy&lt;T&gt;" /> instance.
    ///	</value>
    ///	<exception cref="Spring|EInvalidOperationException">
    ///	</exception>
    property Value: T read GetValue;
  end;

  TLazyInitializer = record
  public
    class function InterlockedCompareExchange(var target: Pointer; value, comparand: Pointer): Pointer; static;

    class function EnsureInitialized<T>(var target: T; const factoryMethod: TFunc<T>): T; overload; static;
    class function EnsureInitialized<T: class, constructor>(var target: T): T; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Multicast Event'}

  IEvent = interface
    ['{CFC14C4D-F559-4A46-A5B1-3145E9B182D8}']
  {$REGION 'Property Accessors'}
    function GetInvoke: TMethod;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetIsEmpty: Boolean;
    procedure SetEnabled(const value: Boolean);
  {$ENDREGION}

    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
    procedure ForEach(const action: TAction<TMethod>);
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property IsEmpty: Boolean read GetIsEmpty;
    property Invoke: TMethod read GetInvoke;
  end;

  ///	<summary>
  ///	  Represents a multicast event.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The event handler type must be an instance procedural type such as
  ///	  TNotifyEvent.
  ///	</typeparam>
  IEvent<T> = interface(IEvent)
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetIsEmpty: Boolean;
    procedure SetEnabled(const value: Boolean);
  {$ENDREGION}

    ///	<summary>
    ///	  Adds an event handler to the list.
    ///	</summary>
    procedure Add(handler: T);

    ///	<summary>
    ///	  Removes an event handler if it was added to the event.
    ///	</summary>
    procedure Remove(handler: T);

    ///	<summary>
    ///	  Removes all event handlers which were registered by an instance.
    ///	</summary>
    procedure RemoveAll(instance: Pointer);

    ///	<summary>
    ///	  Clears all event handlers.
    ///	</summary>
    procedure Clear;

    ///	<summary>
    ///	  Iterates all event handlers and perform the specified action on each
    ///	  one.
    ///	</summary>
    procedure ForEach(const action: TAction<T>);

    ///	<summary>
    ///	  Invokes all event handlers.
    ///	</summary>
    property Invoke: T read GetInvoke;

    ///	<summary>
    ///	  Gets the number of all event handlers.
    ///	</summary>
    property Count: Integer read GetCount;

    ///	<summary>
    ///	  Gets the value indicates whether the multicast event is enabled, or
    ///	  sets the value to enable or disable the event.
    ///	</summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;

    ///	<summary>
    ///	  Gets a value indicates whether there is not any event handler.
    ///	</summary>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

{$IFDEF SUPPORTS_GENERIC_EVENTS}
  Event<T> = record
  private
    fInstance: IEvent<T>;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetInvoke: T;
    function GetIsEmpty: Boolean;
    procedure SetEnabled(const value: Boolean);
    procedure EnsureInitialized;
  public
    class function Create: Event<T>; static;

    procedure Add(const handler: T);
    procedure Remove(const handler: T);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: T read GetInvoke;
    property IsEmpty: Boolean read GetIsEmpty;

    class operator Implicit(const value: IEvent<T>): Event<T>;
    class operator Implicit(var value: Event<T>): IEvent<T>;
    class operator Implicit(var value: Event<T>): T;
    class operator Implicit(const value: T): Event<T>;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'Property change notification'}

  TPropertyChangedEvent = procedure(Sender: TObject;
    const PropertyName: string) of object;

  IPropertyChangedEvent = IEvent<TPropertyChangedEvent>;

  INotifyPropertyChanged = interface
    ['{A517EC98-C651-466B-8290-F7EE96877E03}']
    function GetOnPropertyChanged: IPropertyChangedEvent;
    property OnPropertyChanged: IPropertyChangedEvent read GetOnPropertyChanged;
  end;

  {$ENDREGION}


  {$REGION 'Exceptions'}

  ENotSupportedException = SysUtils.ENotSupportedException;

{$IFDEF DELPHIXE_UP}
  ENotImplementedException = SysUtils.ENotImplemented;
  EInvalidOperationException = SysUtils.EInvalidOpException;
  EArgumentNilException = SysUtils.EArgumentNilException;
{$ELSE}
  ENotImplementedException = class(Exception);
  EInvalidOperationException = class(Exception);
  EArgumentNilException = class(EArgumentException);
{$ENDIF}

  EInvalidCastException = SysUtils.EInvalidCast;

  EInsufficientMemoryException = EOutOfMemory;

  EFormatException = class(Exception);
  EIndexOutOfRangeException = class(Exception);

  EArgumentException = SysUtils.EArgumentException;
  EArgumentOutOfRangeException = SysUtils.EArgumentOutOfRangeException;
  EArgumentNullException = EArgumentNilException;
  EInvalidEnumArgumentException = class(EArgumentException);

  ERttiException = class(Exception);

  {$ENDREGION}


  {$REGION 'TTypeInfoHelper}

  TTypeInfoHelper = record helper for TTypeInfo
  private
    function GetTypeName: string; inline;
  public
    property TypeName: string read GetTypeName;
  end;

  {$ENDREGION}


  {$REGION 'Routines'}

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer;
{$ENDIF}

procedure PlatformNotImplemented;

///	<summary>
///	  Raises an <see cref="Spring|EArgumentNullException" /> if the
///	  <paramref name="value" /> is nil.
///	</summary>
procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

///	<summary>
///	  Raises an <see cref="Spring|EArgumentNullException" /> if the
///	  <paramref name="value" /> is nil.
///	</summary>
procedure CheckArgumentNotNull(value: Pointer; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

function GetQualifiedClassName(AInstance: TObject): string; overload; inline;
function GetQualifiedClassName(AClass: TClass): string; overload; {$IFDEF DELPHIXE2_UP}inline;{$ENDIF}

///	<summary>
///	  Determines whether an instance of <c>leftType</c> can be assigned from an
///	  instance of <c>rightType</c>.
///	</summary>
function IsAssignableFrom(leftType, rightType: PTypeInfo): Boolean;

/// <summary>
///   Returns the size that is needed in order to pass an argument of the given
///   type.
/// </summary>
/// <remarks>
///   While in most cases the result is equal to the actual type size for short
///   strings it always returns SizeOf(Pointer) as short strings are always
///   passed as pointer.
/// </remarks>
function GetTypeSize(typeInfo: PTypeInfo): Integer;
{$ENDREGION}


implementation

uses
  Spring.Events,
  Spring.ResourceStrings;


{$REGION 'Routines'}

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer;
asm
  mov eax,[ebp+4]
end;
{$ENDIF}

procedure PlatformNotImplemented;
begin
  raise ENotImplementedException.Create('Not implemented in present platform.') at ReturnAddress;
end;

procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string);
begin
  CheckArgumentNotNull(Pointer(value), argumentName);
end;

procedure CheckArgumentNotNull(value: Pointer; const argumentName: string);
begin
  if not Assigned(value) then
    Guard.RaiseArgumentNullException(argumentName);
end;

function GetQualifiedClassName(AInstance: TObject): string;
begin
  Result := GetQualifiedClassName(AInstance.ClassType);
end;

function GetQualifiedClassName(AClass: TClass): string;
{$IFNDEF DELPHIXE2_UP}
var
  LUnitName: string;
{$ENDIF}
begin
{$IFDEF DELPHIXE2_UP}
  Result := AClass.QualifiedClassName;
{$ELSE}
  LUnitName := AClass.UnitName;
  if LUnitName = '' then
    Result := AClass.ClassName
  else
    Result := LUnitName + '.' + AClass.ClassName;
{$ENDIF}
end;

function IsAssignableFrom(leftType, rightType: PTypeInfo): Boolean;
var
  leftData, rightData: PTypeData;
begin
  Guard.CheckNotNull(leftType, 'leftType');
  Guard.CheckNotNull(rightType, 'rightType');

  if leftType = rightType then
    Exit(True);

  leftData := GetTypeData(leftType);
  rightData := GetTypeData(rightType);
  if (rightType.Kind = tkClass) and (leftType.Kind = tkClass) then
  begin
    Result := rightData.ClassType.InheritsFrom(leftData.ClassType);
  end
  else if (rightType.Kind = tkClass) and (leftType.Kind = tkInterface) then
  begin
    Result := (ifHasGuid in leftData.IntfFlags) and
      Supports(rightData.ClassType, leftData.Guid);
  end
  else if (rightType.Kind = tkInterface) and (leftType.Kind = tkInterface) then
  begin
    Result := Assigned(rightData.IntfParent) and (rightData.IntfParent^ = leftType);
    while not Result and Assigned(rightData.IntfParent) do
    begin
      Result := rightData.IntfParent^ = leftType;
      rightData := GetTypeData(rightData.IntfParent^);
    end;
  end
  else
    Result := False;
end;

function GetTypeSize(typeInfo: PTypeInfo): Integer;
var
  typeData: PTypeData;
const
  COrdinalSizes: array[TOrdType] of Integer = (
    SizeOf(ShortInt){1},
    SizeOf(Byte){1},
    SizeOf(SmallInt){2},
    SizeOf(Word){2},
    SizeOf(Integer){4},
    SizeOf(Cardinal){4});
  CFloatSizes: array[TFloatType] of Integer = (
    SizeOf(Single){4},
    SizeOf(Double){8},
{$IFDEF ALIGN_STACK}
    16,
{$ELSE}
    SizeOf(Extended){10},
{$ENDIF}
    SizeOf(Comp){8},
    SizeOf(Currency){8});
  CSetSizes: array[TOrdType] of Integer = (
    SizeOf(ShortInt){1},
    SizeOf(Byte){1},
    SizeOf(SmallInt){2},
    SizeOf(Word){2},
    SizeOf(Integer){4},
    SizeOf(Cardinal){4});
begin
  case typeInfo.Kind of
{$IFNDEF NEXTGEN}
    tkChar:
      Result := SizeOf(AnsiChar){1};
{$ENDIF}
    tkWChar:
      Result := SizeOf(WideChar){2};
    tkInteger, tkEnumeration:
      begin
        typeData := GetTypeData(typeInfo);
        Result := COrdinalSizes[typeData.OrdType];
      end;
    tkFloat:
      begin
        typeData := GetTypeData(typeInfo);
        Result := CFloatSizes[typeData.FloatType];
      end;
    tkString, tkLString, tkUString, tkWString, tkInterface, tkClass, tkClassRef, tkDynArray, tkPointer, tkProcedure:
      Result := SizeOf(Pointer);
    tkMethod:
      Result := SizeOf(TMethod);
    tkInt64:
      Result := SizeOf(Int64){8};
    tkVariant:
      Result := SizeOf(Variant);
    tkSet:
      begin
        // big sets have no typeInfo for now
        typeData := GetTypeData(typeInfo);
        Result := CSetSizes[typeData.OrdType];
      end;
    tkRecord:
      begin
        typeData := GetTypeData(typeInfo);
        Result := typeData.RecSize;
      end;
    tkArray:
      begin
        typeData := GetTypeData(typeInfo);
        Result := typeData.ArrayData.Size;
      end;
    else
      begin
        Assert(False, 'Unsupported type'); { TODO -o##jwp -cEnhance : add more context to the assert }
        Result := -1;
      end;
  end;
end;
{$ENDREGION}


{$REGION 'TInterfaceBase'}

function TInterfaceBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TInterfaceBase._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfaceBase._Release: Integer;
begin
  Result := -1;
end;

{$ENDREGION}


{$REGION 'Guard'}

class procedure Guard.DoCheckArrayIndex(length, index: Integer);
begin
  Guard.DoCheckIndex(length, index, 0);
end;

class procedure Guard.DoCheckArrayRange(length, startIndex, count: Integer);
begin
  Guard.CheckRange(length, startIndex, count, 0);
end;

class procedure Guard.DoCheckStringIndex(length, index: Integer);
begin
  Guard.DoCheckIndex(length, index, 1);
end;

class procedure Guard.DoCheckStringRange(length, startIndex, count: Integer);
begin
  Guard.CheckRange(length, startIndex, count, 1);
end;

class procedure Guard.DoCheckIndex(length, index, indexBase: Integer);
const
  IndexArgName = 'index';
begin
  if (index < indexBase) or (index > length + indexBase - 1) then
    Guard.RaiseArgumentOutOfRangeException(IndexArgName);
end;

class procedure Guard.CheckRange(length, startIndex, count, indexBase: Integer);
const
  StartIndexArgName = 'startIndex';
  CountArgName = 'count';
begin
  Guard.CheckRange(
    (startIndex >= indexBase) and (startIndex < indexBase + length),
    StartIndexArgName);
  Guard.CheckRange(count >= 0, CountArgName);
  if count > 0 then
    Guard.CheckRange(count <= indexBase + length - startIndex, CountArgName);
end;

class procedure Guard.CheckRange<T>(const buffer: array of T; index: Integer);
const
  IndexArgName = 'index';
begin
  if (index < 0) or (index >= Length(buffer)) then
    Guard.RaiseArgumentOutOfRangeException(IndexArgName);
end;

class procedure Guard.CheckRange<T>(const buffer: array of T;
  startIndex, count: Integer);
begin
  Guard.DoCheckArrayRange(Length(buffer), startIndex, count);
end;

class procedure Guard.CheckTrue(condition: Boolean; const msg: string);
begin
  if not condition then
    Guard.RaiseArgumentException(msg);
end;

class procedure Guard.CheckFalse(condition: Boolean; const msg: string);
begin
  if condition then
    Guard.RaiseArgumentException(msg);
end;

class procedure Guard.CheckInheritsFrom(cls, parentClass: TClass;
  const argumentName: string);
begin
  Guard.CheckNotNull(cls, 'cls');
  Guard.CheckNotNull(parentClass, 'parentClass');

  if not cls.InheritsFrom(parentClass) then
    raise EArgumentException.CreateResFmt(@SBadObjectInheritance, [argumentName,
      cls.ClassName, parentClass.ClassName]);
end;

class procedure Guard.CheckInheritsFrom(obj: TObject; parentClass: TClass;
  const argumentName: string);
begin
  if Assigned(obj) then
    Guard.CheckInheritsFrom(obj.ClassType, parentClass, argumentName);
end;

class procedure Guard.CheckNotNull(condition: Boolean;
  const parameterName: string);
begin
  if not condition then
    Guard.RaiseArgumentNullException(parameterName);
end;

class procedure Guard.CheckNotNull(argumentValue: Pointer;
  const argumentName: string);
begin
  Guard.CheckNotNull(Assigned(argumentValue), argumentName);
end;

class procedure Guard.CheckNotNull(const argumentValue: IInterface;
  const argumentName: string);
begin
  Guard.CheckNotNull(Assigned(argumentValue), argumentName);
end;

class procedure Guard.CheckNotNull(argumentValue: TObject;
  const argumentName: string);
begin
  Guard.CheckNotNull(Assigned(argumentValue), argumentName);
end;

class procedure Guard.CheckNotNull<T>(const argumentValue: T;
  const argumentName: string);
begin
  if Guard.IsNullReference(argumentValue, TypeInfo(T)) then
    Guard.RaiseArgumentNullException(argumentName);
end;

class procedure Guard.CheckEnum<T>(const argumentValue: T;
  const argumentName: string);
var
  intValue: Integer;
begin
  intValue := 0;
  Move(argumentValue, intValue, SizeOf(T));
  Guard.CheckEnum<T>(intValue, argumentName);
end;

class procedure Guard.CheckEnum<T>(argumentValue: Integer;
  const argumentName: string);
var
  typeInfo: PTypeInfo;
  data: PTypeData;
begin
  typeInfo := System.TypeInfo(T);
  Guard.CheckTypeKind(typeInfo, [tkEnumeration], 'T');

  data := GetTypeData(typeInfo);
  Guard.CheckNotNull(data, 'data');

  if (argumentValue < data.MinValue) or (argumentValue > data.MaxValue) then
    raise EInvalidEnumArgumentException.CreateResFmt(@SInvalidEnumArgument, [
      argumentName, GetTypeName(typeInfo), argumentValue]);
end;

class procedure Guard.CheckRange(condition: Boolean;
  const argumentName: string);
begin
  if not condition then
    Guard.RaiseArgumentOutOfRangeException(argumentName);
end;

class procedure Guard.CheckRange(const buffer: array of Byte;
  startIndex, count: Integer);
begin
  Guard.DoCheckArrayRange(Length(buffer), startIndex, count);
end;

class procedure Guard.CheckRange(const buffer: array of Char;
  startIndex, count: Integer);
begin
  Guard.DoCheckArrayRange(Length(buffer), startIndex, count);
end;

class procedure Guard.CheckRange(const buffer: array of Byte; index: Integer);
begin
  Guard.DoCheckArrayIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange(const buffer: array of Char; index: Integer);
begin
  Guard.DoCheckArrayIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange(const s: string; index: Integer);
begin
  Guard.DoCheckStringIndex(Length(s), index);
end;

class procedure Guard.CheckRange(const s: string; startIndex, count: Integer);
begin
  Guard.DoCheckStringRange(Length(s), startIndex, count);
end;

{$IFNDEF NEXTGEN}
class procedure Guard.CheckRange(const s: WideString; index: Integer);
begin
  Guard.DoCheckStringIndex(Length(s), index);
end;

class procedure Guard.CheckRange(const s: WideString; startIndex, count: Integer);
begin
  Guard.DoCheckStringRange(Length(s), startIndex, count);
end;

class procedure Guard.CheckRange(const s: RawByteString; index: Integer);
begin
  Guard.DoCheckStringIndex(Length(s), index);
end;

class procedure Guard.CheckRange(const s: RawByteString; startIndex, count: Integer);
begin
  Guard.DoCheckStringRange(Length(s), startIndex, count);
end;
{$ENDIF}

class procedure Guard.CheckTypeKind(typeInfo: PTypeInfo;
  expectedTypeKind: TTypeKind; const argumentName: string);
begin
  Guard.CheckNotNull(typeInfo, argumentName);
  if typeInfo.Kind <> expectedTypeKind then
    raise EArgumentException.CreateResFmt(@SUnexpectedTypeKindArgument,
      [typeInfo.TypeName, argumentName]);
end;

class procedure Guard.CheckTypeKind(typeInfo: PTypeInfo;
  expectedTypeKinds: TTypeKinds; const argumentName: string);
begin
  Guard.CheckNotNull(typeInfo, argumentName);
  if not (typeInfo.Kind in expectedTypeKinds) then
    raise EArgumentException.CreateResFmt(@SUnexpectedTypeKindArgument,
      [typeInfo.TypeName, argumentName]);
end;

class function Guard.IsNullReference(const value; typeInfo: PTypeInfo): Boolean;
const
  ReferenceKinds = [
    tkClass, tkMethod, tkInterface, tkClassRef, tkPointer, tkProcedure];
begin
  Result := False;
  if Assigned(typeInfo) and (typeInfo.Kind in ReferenceKinds) then
    if typeInfo.Kind = tkMethod then
      Result := not Assigned(TMethod(value).Code) and not Assigned(TMethod(value).Data)
    else
      Result := not Assigned(PPointer(@value)^);
end;

{$IFOPT O+}
  {$DEFINE OPTIMIZATIONS_ON}
  {$O-}
{$ENDIF}
class procedure Guard.RaiseArgumentException(const msg: string);
begin
  raise EArgumentException.Create(msg) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentNullException(const argumentName: string);
begin
  raise EArgumentNullException.CreateResFmt(
    @SArgumentNullException, [argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentOutOfRangeException(const argumentName: string);
begin
  raise EArgumentOutOfRangeException.CreateResFmt(
    @SArgumentOutOfRangeException, [argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseArgumentFormatException(const argumentName: string);
begin
  raise EFormatException.CreateResFmt(
    @SInvalidArgumentFormat, [argumentName]) at ReturnAddress;
end;

class procedure Guard.RaiseInvalidEnumArgumentException(const argumentName: string);
begin
  raise EInvalidEnumArgumentException.CreateResFmt(
    @SInvalidEnumArgument, [argumentName]) at ReturnAddress;
end;
{$IFDEF OPTIMIZATIONS_ON}
  {$UNDEF OPTIMIZATIONS_ON}
  {$O+}
{$ENDIF}

{$ENDREGION}


{$REGION 'Nullable<T>'}

const
  CHasValueFlag = '@';

constructor Nullable<T>.Create(const value: T);
begin
  fValue := value;
  fHasValue := CHasValueFlag;
end;

constructor Nullable<T>.Create(const value: Variant);
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    fValue := v.AsType<T>;
    fHasValue := CHasValueFlag;
  end
  else
    Clear;
end;

procedure Nullable<T>.Clear;
begin
  fHasValue := '';
  fValue := Default(T);
end;

class function Nullable<T>.VarIsNullOrEmpty(const value: Variant): Boolean;
begin
  Result := VarIsNull(value) or VarIsEmpty(value);
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := Length(fHasValue) > 0;
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
    raise EInvalidOperationException.CreateRes(@SNullableTypeHasNoValue);
  Result := fValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := Value
  else
    Result := Default(T);
end;

function Nullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := Value
  else
    Result := defaultValue;
end;

function Nullable<T>.Equals(const other: Nullable<T>): Boolean;
begin
  if HasValue and other.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(Value, other.Value)
  else
    Result := HasValue = other.HasValue;
end;

class operator Nullable<T>.Implicit(const value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(value);
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): Variant;
var
  v: TValue;
begin
  if value.HasValue then
  begin
    v := TValue.From<T>(value.Value);
    if v.IsType<Boolean> then
      Result := v.AsBoolean
    else
      Result := v.AsVariant;
  end
  else
    Result := Null;
end;

class operator Nullable<T>.Implicit(const value: Variant): Nullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    Result := Nullable<T>.Create(v.AsType<T>);
  end
  else
    Result.Clear;
end;

class operator Nullable<T>.Implicit(value: Pointer): Nullable<T>;
begin
  if not Assigned(value) then
    Result.Clear
  else
    raise EInvalidOperationException.CreateRes(@SCannotAssignPointerToNullable);
end;

class operator Nullable<T>.Explicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;

class operator Nullable<T>.Equal(const a, b: Nullable<T>): Boolean;
begin
  Result := a.Equals(b);
end;

class operator Nullable<T>.NotEqual(const a, b: Nullable<T>): Boolean;
begin
  Result := not a.Equals(b);
end;

{$ENDREGION}


{$REGION 'TLazy'}

constructor TLazy.Create;
begin
  inherited;
  fLock := TCriticalSection.Create;
end;

destructor TLazy.Destroy;
begin
  fLock.Free;
  inherited;
end;

function TLazy.GetIsValueCreated: Boolean;
begin
  Result := fIsValueCreated;
end;

{$ENDREGION}


{$REGION 'TLazy<T>'}

constructor TLazy<T>.Create(const valueFactory: TFunc<T>);
begin
  Guard.CheckNotNull(Assigned(valueFactory), 'valueFactory');

  inherited Create;
  fValueFactory := valueFactory;
end;

constructor TLazy<T>.CreateFrom(const value: T);
begin
  inherited Create;
  fValue := value;
  fIsValueCreated := True;
end;

procedure TLazy<T>.EnsureInitialized;
begin
  if fIsValueCreated then
    Exit;

  fLock.Enter;
  try
    if fIsValueCreated then
      Exit;

    if not Assigned(fValueFactory) then
      raise EInvalidOperationException.CreateRes(@SNoDelegateAssigned);

    fValue := fValueFactory();
    fIsValueCreated := True;
  finally
    fLock.Leave;
  end;
end;

function TLazy<T>.GetValue: T;
begin
  EnsureInitialized;
  Result := fValue;
end;

function TLazy<T>.GetValueNonGeneric: TValue;
begin
  Result := TValue.From<T>(Value);
end;

{$ENDREGION}


{$REGION 'Lazy<T>'}

constructor Lazy<T>.Create(const valueFactory: TFunc<T>);
begin
  fLazy := TLazy<T>.Create(valueFactory);
end;

constructor Lazy<T>.CreateFrom(const value: T);
begin
  fLazy := TLazy<T>.CreateFrom(value);
end;

function Lazy<T>.GetValue: T;
begin
  if not Assigned(fLazy) then
    raise EInvalidOperationException.CreateRes(@SNoDelegateAssigned);

  Result := fLazy.Value;
end;

function Lazy<T>.GetIsValueCreated: Boolean;
begin
  Result := Assigned(fLazy) and fLazy.IsValueCreated;
end;

function Lazy<T>.GetIsAssigned: Boolean;
begin
  Result := Assigned(fLazy);
end;

class operator Lazy<T>.Implicit(const value: Lazy<T>): ILazy<T>;
begin
  Result := value.fLazy;
end;

class operator Lazy<T>.Implicit(const value: Lazy<T>): T;
begin
  Result := value.Value;
end;

class operator Lazy<T>.Implicit(const value: T): Lazy<T>;
begin
  Result.fLazy := TLazy<T>.CreateFrom(value);
end;

class operator Lazy<T>.Implicit(const value: TLazy<T>): Lazy<T>;
begin
  Result.fLazy := value;
end;

{$ENDREGION}


{$REGION 'TLazyInitializer'}

class function TLazyInitializer.InterlockedCompareExchange(var target: Pointer;
  value, comparand: Pointer): Pointer;
{$IFNDEF DELPHIXE_UP}
asm
  XCHG EAX, EDX
  XCHG EAX, ECX
  LOCK CMPXCHG [EDX], ECX
end;
{$ELSE}
begin
  Result := TInterlocked.CompareExchange(target, value, comparand);
end;
{$ENDIF}

class function TLazyInitializer.EnsureInitialized<T>(var target: T;
  const factoryMethod: TFunc<T>): T;
var
  localValue: T;
begin
  if PPointer(@target)^ = nil then
  begin
    localValue := factoryMethod();
    if TLazyInitializer.InterlockedCompareExchange(PPointer(@target)^,
      PPointer(@localValue)^, nil) <> nil then
    begin
      if PTypeInfo(TypeInfo(T)).Kind = tkClass then
      begin
        PObject(@localValue)^.Free;
      end;
    end
    else if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
    begin
      PPointer(@localValue)^ := nil;
    end;
  end;
  Result := target;
end;

class function TLazyInitializer.EnsureInitialized<T>(var target: T): T;
var
  localValue: T;
begin
  if PPointer(@target)^ = nil then
  begin
    localValue := T.Create;
{$IFNDEF AUTOREFCOUNT}
    if TLazyInitializer.InterlockedCompareExchange(PPointer(@target)^,
      PPointer(@localValue)^, nil) <> nil then
    begin
      localValue.Free;
    end;
{$ELSE}
    if AtomicCmpExchange(PPointer(@target)^, Pointer(localvalue), nil) = nil then
    begin
      target.__ObjAddRef;
    end;
{$ENDIF AUTOREFCOUNT}
  end;
  Result := target;
end;

{$ENDREGION}


{$REGION 'Event<T>'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
class function Event<T>.Create: Event<T>;
begin
  Result := TEvent<T>.Create;
end;

procedure Event<T>.Add(const handler: T);
begin
  EnsureInitialized;
  fInstance.Add(handler);
end;

procedure Event<T>.Clear;
begin
  if Assigned(fInstance) then
    fInstance.Clear;
end;

procedure Event<T>.EnsureInitialized;
begin
  if not Assigned(fInstance) then
    fInstance := TEvent<T>.Create;
end;

function Event<T>.GetCount: Integer;
begin
  if Assigned(fInstance) then
    Result := fInstance.Count
  else
    Result := 0;
end;

function Event<T>.GetEnabled: Boolean;
begin
  Result := not Assigned(fInstance) or fInstance.Enabled;
end;

function Event<T>.GetInvoke: T;
begin
  EnsureInitialized;
  Result := fInstance.Invoke;
end;

function Event<T>.GetIsEmpty: Boolean;
begin
  Result := not Assigned(fInstance) or fInstance.IsEmpty;
end;

procedure Event<T>.Remove(const handler: T);
begin
  if Assigned(fInstance) then
    fInstance.Remove(handler);
end;

procedure Event<T>.RemoveAll(instance: Pointer);
begin
  if Assigned(fInstance) then
    fInstance.RemoveAll(instance);
end;

procedure Event<T>.SetEnabled(const value: Boolean);
begin
  EnsureInitialized;
  fInstance.Enabled := value;
end;

class operator Event<T>.Implicit(const value: IEvent<T>): Event<T>;
begin
  Result.fInstance := value;
end;

class operator Event<T>.Implicit(var value: Event<T>): IEvent<T>;
begin
  value.EnsureInitialized;
  Result := value.fInstance;
end;

class operator Event<T>.Implicit(var value: Event<T>): T;
begin
  Result := value.Invoke;
end;

class operator Event<T>.Implicit(const value: T): Event<T>;
begin
  Result.Clear;
  Result.Add(value);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTypeInfoHelper'}

function TTypeInfoHelper.GetTypeName: string;
begin
{$IFNDEF NEXTGEN}
  Result := UTF8ToString(Name);
{$ELSE}
  Result := NameFld.ToString;
{$ENDIF}
end;

{$ENDREGION}


end.
