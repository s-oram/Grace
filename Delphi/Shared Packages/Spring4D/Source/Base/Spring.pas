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

/// <summary>
///	  Declares the fundamental interfaces for the
///	  <see href="http://spring4d.org">Spring4D</see> Framework.
/// </summary>
unit Spring;

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

  /// <summary>
  ///   Represents a dynamic array of Byte.
  /// </summary>
  TBytes = SysUtils.TBytes;

  /// <summary>
  ///   Represents a dynamic array of string.
  /// </summary>
  TStringDynArray = Types.TStringDynArray;

  /// <summary>
  ///   Represents a time interval.
  /// </summary>
  TTimeSpan = TimeSpan.TTimeSpan;

  /// <summary>
  ///   Provides a set of methods and properties to accurately measure elapsed
  ///   time.
  /// </summary>
  TStopwatch = Diagnostics.TStopwatch;

  PTypeInfo = TypInfo.PTypeInfo;

  TValue = Rtti.TValue;

  /// <summary>
  ///   Represents the class type of <see cref="System|TCustomAttribute" />.
  /// </summary>
  TAttributeClass = class of TCustomAttribute;

{$IFNDEF DELPHIXE_UP}
  TThreadID = LongWord;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TCollectionChangedAction'}

  ///	<summary>
  ///	  Describes the action that caused a CollectionChanged event.
  ///	</summary>
  TCollectionChangedAction = (
    ///	<summary>
    ///	  An item was added to the collection.
    ///	</summary>
    caAdded,

    ///	<summary>
    ///	  An item was removed from the collection.
    ///	</summary>
    caRemoved,

    ///	<summary>
    ///	  An item was removed from the collection without considering ownership.
    ///	</summary>
    caExtracted,

    ///	<summary>
    ///	  An item was replaced in the collection.
    ///	</summary>
    caReplaced,

    ///	<summary>
    ///	  An item was moved within the collection.
    ///	</summary>
    caMoved,

    ///	<summary>
    ///	  The content of the collection changed dramatically.
    ///	</summary>
    caReseted,

    ///	<summary>
    ///	  An item in the collection was changed.
    ///	</summary>
    caChanged
  );

  {$ENDREGION}


  {$REGION 'TValueHelper'}

  TValueHelper = record helper for TValue
  private
    function TryAsInterface(typeInfo: PTypeInfo; out Intf): Boolean;
  public
    class function FromVarRec(const value: TVarRec): TValue; static;
{$IFDEF DELPHI2010}
    function AsString: string;
{$ENDIF}
    function AsType<T>: T;
    function Cast(typeInfo: PTypeInfo): TValue;
    function IsString: Boolean;
{$IFDEF DELPHI2010}
    function IsType<T>: Boolean; overload;
    function IsType(ATypeInfo: PTypeInfo): Boolean; overload;
{$ENDIF}
  end;

  {$ENDREGION}


  {$REGION 'TRttiMethodHelper'}

  TRttiMethodHelper = class helper for TRttiMethod
  private
    procedure DispatchValue(const value: TValue; typeInfo: PTypeInfo);
  public
    function Invoke(Instance: TObject; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TClass; const Args: array of TValue): TValue; overload;
    function Invoke(Instance: TValue; const Args: array of TValue): TValue; overload;
  end;

  {$ENDREGION}


  {$REGION 'Interfaces'}

  /// <summary>
  ///   Supports cloning, which creates a new instance of a class with the same
  ///   value as an existing instance.
  /// </summary>
  IClonable = interface(IInvokable)
    ['{B6BC3795-624B-434F-BB19-6E8F55149D0A}']
    /// <summary>
    ///   Creates a new object that is a copy of the current instance.
    /// </summary>
    /// <returns>
    ///   A new object that is a copy of this instance.
    /// </returns>
    function Clone: TObject;
  end;

  /// <summary>
  ///   Defines a generalized type-specific comparison method that a class
  ///   implements to order or sort its instances.
  /// </summary>
  IComparable = interface(IInvokable)
    ['{7F0E25C8-50D7-4CF0-AB74-1913EBD3EE42}']
    /// <summary>
    ///   Compares the current instance with another object of the same type
    ///   and returns an integer that indicates whether the current instance
    ///   precedes, follows, or occurs in the same position in the sort order
    ///   as the other object.
    /// </summary>
    /// <param name="obj">
    ///   An object to compare with this instance.
    /// </param>
    /// <returns>
    ///   <para>
    ///     A value that indicates the relative order of the objects being
    ///     compared. The return value has these meanings:
    ///   </para>
    ///   <list type="table">
    ///     <listheader>
    ///       <term>Value</term>
    ///       <description>Meaning</description>
    ///     </listheader>
    ///     <item>
    ///       <term>Less than zero</term>
    ///       <description>This instance precedes <i>obj</i> in the sort
    ///         order.</description>
    ///     </item>
    ///     <item>
    ///       <term>Zero</term>
    ///       <description>This instance occurs in the same position in
    ///         the sort order as <i>obj</i>.</description>
    ///     </item>
    ///     <item>
    ///       <term>Greater than zero</term>
    ///       <description>This instance follows <i>obj</i> in the sort
    ///         order.</description>
    ///     </item>
    ///   </list>
    /// </returns>
    /// <exception cref="Spring|EArgumentException">
    ///   <i>obj</i> is not the same type as this instance.
    /// </exception>
    function CompareTo(const obj: TObject): Integer;
  end;

  /// <summary>
  ///   Base interface for anything that has a countable quantity.
  /// </summary>
  ICountable = interface
    ['{CA225A9C-B6FD-4D6E-B3BD-22119CCE6C87}']
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
  {$ENDREGION}

    function Any: Boolean;
    property Count: Integer read GetCount;
  end;

  {$ENDREGION}


  {$REGION 'Procedure types'}

  /// <summary>
  ///   Represents a logical predicate.
  /// </summary>
  /// <param name="value">
  ///   the value needs to be determined.
  /// </param>
  /// <returns>
  ///	  Returns <c>True</c> if the value was accepted, otherwise, returns
  ///	  <c>False</c>.
  /// </returns>
  /// <remarks>
  ///   <note type="tip">
  ///	    This type redefined the
  ///	    <see cref="SysUtils|TPredicate`1">SysUtils.TPredicate&lt;T&gt;</see> 
  ///	    type with a const parameter.
  ///   </note>
  /// </remarks>
  /// <seealso cref="Spring.DesignPatterns|ISpecification&lt;T&gt;" />
  TPredicate<T> = reference to function(const value: T): Boolean;

  /// <summary>
  ///   Represents an anonymous method that has a single parameter and does not
  ///   return a value.
  /// </summary>
  /// <seealso cref="TActionProc&lt;T&gt;" />
  /// <seealso cref="TActionMethod&lt;T&gt;" />
  TAction<T> = reference to procedure(const obj: T);

  /// <summary>
  ///   Represents a procedure that has a single parameter and does not return
  ///   a value.
  /// </summary>
  /// <seealso cref="TAction&lt;T&gt;" />
  /// <seealso cref="TActionMethod&lt;T&gt;" />
  TActionProc<T> = procedure(const obj: T);

  /// <summary>
  ///   Represents a instance method that has a single parameter and does not
  ///   return a value.
  /// </summary>
  /// <seealso cref="TAction&lt;T&gt;" />
  /// <seealso cref="TActionProc&lt;T&gt;" />
  TActionMethod<T> = procedure(const obj: T) of object;

  /// <summary>
  ///   Represents a anonymous method that has the same signature as
  ///   TNotifyEvent.
  /// </summary>
  {$M+}
  TNotifyProc = reference to procedure(Sender: TObject);
  {$M-}

  TNotifyEvent<T> = procedure(Sender: TObject; const item: T) of object;

  {$ENDREGION}


  {$REGION 'TNamedValue'}

  TNamedValue = record
  private
    fValue: TValue;
    fName: string;
  public
    constructor Create(const name: string; const value: TValue);
    class function From<T>(const name: string; const value: T): TNamedValue; overload; static;

    class operator Implicit(const value: TNamedValue): TValue;
    class operator Implicit(const value: TValue): TNamedValue;

    property Name: string read fName;
    property Value: TValue read fValue;
  end;

  {$ENDREGION}


  {$REGION 'TTypedValue'}

  TTypedValue = record
  private
    fValue: TValue;
    fTypeInfo: PTypeInfo;
  public
    constructor Create(const typeInfo: PTypeInfo; const value: TValue);
    class function From<T>(const typeInfo: PTypeInfo; const value: T): TTypedValue; overload; static;

    class operator Implicit(const value: TTypedValue): TValue;
    class operator Implicit(const value: TValue): TTypedValue;

    property TypeInfo: PTypeInfo read fTypeInfo;
    property Value: TValue read fValue;
  end;

  {$ENDREGION}


  {$REGION 'TInterfaceBase'}

  /// <summary>
  ///   Provides a non-reference-counted <see cref="System|IInterface" />
  ///   implementation.
  /// </summary>
  TInterfaceBase = class abstract(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  {$ENDREGION}


  {$REGION 'Guard'}

  /// <summary>
  ///   Provides static methods to check arguments and raise argument
  ///   exceptions.
  /// </summary>
  /// <remarks>
  ///   It's recommended that all arguments of public types and members should
  ///   be checked.
  /// </remarks>
  Guard = record
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

    class procedure CheckSet<T{:set}>(const argumentValue: T; const argumentName: string); overload; static; inline;
    class procedure CheckSet<T{:set}>(argumentValue: Cardinal; const argumentName: string); overload; static; inline;

    class procedure CheckIndex(length, index: Integer; indexBase: Integer = 0); static; inline;

    /// <exception cref="Spring|EArgumentOutOfRangeException">
    ///   Raised if the <paramref name="index" /> is out of range.
    /// </exception>
    class procedure CheckRange(const buffer: array of Byte; index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Byte; index, count: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; index: Integer); overload; static;
    class procedure CheckRange(const buffer: array of Char; index, count: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; index: Integer); overload; static;
    class procedure CheckRange<T>(const buffer: array of T; index, count: Integer); overload; static;
    class procedure CheckRange(const s: string; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: string; index, count: Integer); overload; static; inline;
{$IFNDEF NEXTGEN}
    class procedure CheckRange(const s: WideString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: WideString; index, count: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; index: Integer); overload; static; inline;
    class procedure CheckRange(const s: RawByteString; index, count: Integer); overload; static; inline;
{$ENDIF}
    class procedure CheckRange(condition: Boolean; const argumentName: string); overload; static; inline;
    class procedure CheckRange(length, index, count: Integer; indexBase: Integer = 0); overload; static; inline;

    /// <summary>
    ///   Checks an argument to ensure it is in the specified range including
    ///   the bounds.
    /// </summary>
    /// <param name="value">
    ///   The argument value to check.
    /// </param>
    /// <param name="min">
    ///   The minimum allowed value for the argument.
    /// </param>
    /// <param name="max">
    ///   The maximum allowed value for the argument.
    /// </param>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   The value is not within the specified range.
    /// </exception>
    class procedure CheckRangeInclusive(value, min, max: Integer); overload; static; inline;

    /// <summary>
    ///   Checks an argument to ensure it is in the specified range excluding
    ///   the bounds.
    /// </summary>
    /// <param name="value">
    ///   The argument value to check.
    /// </param>
    /// <param name="min">
    ///   The minimum allowed value for the argument.
    /// </param>
    /// <param name="max">
    ///   The maximum allowed value for the argument. <br />
    /// </param>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   The value is not within the specified range.
    /// </exception>
    class procedure CheckRangeExclusive(value, min, max: Integer); overload; static; inline;

    class procedure CheckTypeKind(typeInfo: PTypeInfo; expectedTypeKind: TTypeKind; const argumentName: string); overload; static;
    class procedure CheckTypeKind(typeInfo: PTypeInfo; expectedTypeKinds: TTypeKinds; const argumentName: string); overload; static;

    class function IsNullReference(const value; typeInfo: PTypeInfo): Boolean; static;

    /// <summary>
    ///   Raises an <see cref="EArgumentException" /> exception.
    /// </summary>
    /// <param name="msg">
    ///   The general error message.
    /// </param>
    class procedure RaiseArgumentException(const msg: string); overload; static; inline;

    /// <summary>
    ///   Raises an <see cref="EFormatException" /> exception.
    /// </summary>
    class procedure RaiseArgumentFormatException(const argumentName: string); overload; static; inline;

    /// <summary>
    ///   Raises an <see cref="EArgumentNullException" /> exception.
    /// </summary>
    class procedure RaiseArgumentNullException(const argumentName: string); overload; static; inline;

    /// <summary>
    ///   Raises an <see cref="EArgumentOutOfRangeException" /> exception.
    /// </summary>
    class procedure RaiseArgumentOutOfRangeException(const argumentName: string); overload; static; inline;

    /// <summary>
    ///   Raises an <see cref="EInvalidEnumArgumentException" /> exception.
    /// </summary>
    class procedure RaiseInvalidEnumArgumentException(const argumentName: string); overload; static; inline;
  end;

  TArgument = Guard deprecated 'Use Guard instead';

  {$ENDREGION}


  {$REGION 'Nullable Types'}

  /// <summary>
  ///   A nullable type can represent the normal range of values for its
  ///   underlying value type, plus an additional <c>Null</c> value.
  /// </summary>
  /// <typeparam name="T">
  ///   The underlying value type of the <see cref="Nullable&lt;T&gt;" />
  ///   generic type.
  /// </typeparam>
  Nullable<T> = record
  private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean; inline;

    /// <summary>
    ///   Internal use. Marks the current instance as null.
    /// </summary>
    /// <remarks>
    ///   The <see cref="Nullable&lt;T&gt;" /> type is immutable so that this
    ///   method must be private.
    /// </remarks>
    procedure Clear;

    /// <summary>
    ///   Determines whether a variant value is null or empty.
    /// </summary>
    class function VarIsNullOrEmpty(const value: Variant): Boolean; static; inline;
  public
    /// <summary>
    ///   Initializes a new instance of the <see cref="Nullable&lt;T&gt;" />
    ///   structure to the specified value.
    /// </summary>
    constructor Create(const value: T); overload;

    /// <summary>
    ///   Initializes a new instance of the <see cref="Nullable&lt;T&gt;" />
    ///   structure to the specified value.
    /// </summary>
    constructor Create(const value: Variant); overload;

    /// <summary>
    ///   Retrieves the value of the current <see cref="Nullable&lt;T&gt;" />
    ///   object, or the object's default value.
    /// </summary>
    function GetValueOrDefault: T; overload;

    /// <summary>
    ///   Retrieves the value of the current <see cref="Nullable&lt;T&gt;" />
    ///   object, or the specified default value.
    /// </summary>
    /// <param name="defaultValue">
    ///   A value to return if the <see cref="HasValue" /> property is <c>False</c>
    ///    .
    /// </param>
    /// <returns>
    ///   The value of the <see cref="Value" /> property if the <see cref="HasValue" />
    ///    property is true; otherwise, the <paramref name="defaultValue" />
    ///   parameter.
    /// </returns>
    /// <remarks>
    ///   The <see cref="GetValueOrDefault" /> method returns a value even if
    ///   the <see cref="HasValue" /> property is false (unlike the <see cref="Value" />
    ///    property, which throws an exception).
    /// </remarks>
    function GetValueOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Determines whether two nullable value are equal.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     If both two nullable values are null, return true;
    ///   </para>
    ///   <para>
    ///     If either one is null, return false;
    ///   </para>
    ///   <para>
    ///     else compares their values as usual.
    ///   </para>
    /// </remarks>
    function Equals(const other: Nullable<T>): Boolean;

    /// <summary>
    ///   Gets a value indicating whether the current <see cref="Nullable&lt;T&gt;" />
    ///    structure has a value.
    /// </summary>
    property HasValue: Boolean read GetHasValue;

    /// <summary>
    ///   Gets the value of the current <see cref="Nullable&lt;T&gt;" /> value.
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   Raised if the value is null.
    /// </exception>
    property Value: T read GetValue;

    class operator Implicit(const value: Nullable<T>): T;
    class operator Implicit(const value: T): Nullable<T>;
    class operator Implicit(const value: Nullable<T>): Variant;
    class operator Implicit(const value: Variant): Nullable<T>;
    class operator Implicit(value: Pointer): Nullable<T>;
    class operator Explicit(const value: Nullable<T>): T;
    class operator Equal(const left, right: Nullable<T>): Boolean;
    class operator NotEqual(const left, right: Nullable<T>): Boolean;
  end;


  /// <summary>
  ///   Represents a nullable unicode string.
  /// </summary>
  TNullableString = Nullable<string>;
{$IFNDEF NEXTGEN}
  /// <summary>
  ///   Represents a nullable ansi string.
  /// </summary>
  TNullableAnsiString = Nullable<AnsiString>;

  /// <summary>
  ///   Represents a nullable wide string.
  /// </summary>
  TNullableWideString = Nullable<WideString>;
{$ENDIF}
  /// <summary>
  ///   Represents a nullable integer.
  /// </summary>
  TNullableInteger = Nullable<Integer>;

  /// <summary>
  ///   Represents a nullable <c>Int64</c>.
  /// </summary>
  TNullableInt64 = Nullable<Int64>;

  /// <summary>
  ///   Represents a nullable native integer.
  /// </summary>
  TNullableNativeInt = Nullable<NativeInt>;

  /// <summary>
  ///   Represents a nullable <c>TDateTime</c>.
  /// </summary>
  TNullableDateTime = Nullable<TDateTime>;

  /// <summary>
  ///   Represents a nullable <c>Currency</c>.
  /// </summary>
  TNullableCurrency = Nullable<Currency>;

  /// <summary>
  ///   Represents a nullable <c>Double</c>.
  /// </summary>
  TNullableDouble = Nullable<Double>;

  /// <summary>
  ///   Represents a nullable <c>Boolean</c>.
  /// </summary>
  TNullableBoolean = Nullable<Boolean>;

  /// <summary>
  ///   Represents a nullable <c>TGuid</c>.
  /// </summary>
  TNullableGuid = Nullable<TGUID>;

  {$ENDREGION}


  {$REGION 'Lazy Initialization'}

  /// <summary>
  ///   Specifies the kind of a lazy type.
  /// </summary>
  TLazyKind = (
    /// <summary>
    ///   Not a lazy type.
    /// </summary>
    lkNone,

    /// <summary>
    ///   Type is <see cref="SysUtils|TFunc&lt;T&gt;" />.
    /// </summary>
    lkFunc,

    /// <summary>
    ///   Type is <see cref="Spring|Lazy&lt;T&gt;" />.
    /// </summary>
    lkRecord,

    /// <summary>
    ///   Type is <see cref="Spring|ILazy&lt;T&gt;" />.
    /// </summary>
    lkInterface
  );

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  ILazy = interface
    ['{40223BA9-0C66-49E7-AA33-BDAEF9F506D6}']
  {$REGION 'Property Accessors'}
    function GetIsValueCreated: Boolean;
    function GetValue: TValue;
  {$ENDREGION}

    /// <summary>
    ///   Gets a value that indicates whether a value has been created for this
    ///   <see cref="ILazy" /> instance.
    /// </summary>
    /// <value>
    ///	  <b>True</b> if a value has been created for this
    ///	  <see cref="ILazy" /> instance; otherwise, <b>False</b>.
    /// </value>
    property IsValueCreated: Boolean read GetIsValueCreated;

    /// <summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="ILazy" /> instance.
    /// </summary>
    /// <value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="ILazy" /> instance.
    /// </value>
    property Value: TValue read GetValue;
  end;

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  ILazy<T> = interface(ILazy)
  {$REGION 'Property Accessors'}
    function GetValue: T;
  {$ENDREGION}

    /// <summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="ILazy&lt;T&gt;" /> instance.
    /// </summary>
    /// <value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="ILazy&lt;T&gt;" /> instance.
    /// </value>
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

    /// <summary>
    ///   Gets a value that indicates whether a value has been created for this
    ///   <see cref="TLazy&lt;T&gt;" /> instance.
    /// </summary>
    /// <value>
    ///	  <b>True</b> if a value has been created for this
    ///	  <see cref="TLazy&lt;T&gt;" /> instance; otherwise, <b>False</b>.
    /// </value>
    property IsValueCreated: Boolean read GetIsValueCreated;
  end;

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of object that is being lazily initialized.
  /// </typeparam>
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
    /// <summary>
    ///   Initializes a new instance of the <see cref="TLazy&lt;T&gt;" />
    ///   class. When lazy initialization occurs, the specified initialization
    ///   function is used.
    /// </summary>
    /// <param name="valueFactory">
    ///   The delegate that is invoked to produce the lazily initialized value
    ///   when it is needed.
    /// </param>
    /// <exception cref="EArgumentNullException">
    ///   <i>valueFactory</i> is <b>nil</b>.
    /// </exception>
    constructor Create(const valueFactory: TFunc<T>);

    /// <summary>
    ///   Initializes a new instance of <see cref="TLazy&lt;T&gt;" /> with the
    ///   specified value.
    /// </summary>
    /// <param name="value">
    ///   The initialized value.
    /// </param>
    constructor CreateFrom(const value: T);

    /// <summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="TLazy&lt;T&gt;" /> instance.
    /// </summary>
    /// <value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="TLazy&lt;T&gt;" /> instance.
    /// </value>
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   Provides support for lazy initialization.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of object that is being lazily initialized.
  /// </typeparam>
  Lazy<T> = record
  private
    fLazy: ILazy<T>;
    function GetIsAssigned: Boolean;
    function GetIsValueCreated: Boolean;
    function GetValue: T;
  public
    /// <summary>
    ///   Initializes a new instance of the <see cref="Lazy&lt;T&gt;" />
    ///   record. When lazy initialization occurs, the specified initialization
    ///   function is used.
    /// </summary>
    /// <param name="valueFactory">
    ///   The delegate that is invoked to produce the lazily initialized value
    ///   when it is needed.
    /// </param>
    /// <exception cref="EArgumentNullException">
    ///   <i>valueFactory</i> is <b>nil</b>.
    /// </exception>
    constructor Create(const valueFactory: TFunc<T>);

    /// <summary>
    ///   Initializes a new instance of <see cref="Lazy&lt;T&gt;" /> with the
    ///   specified value.
    /// </summary>
    /// <param name="value">
    ///   The initialized value.
    /// </param>
    constructor CreateFrom(const value: T);

    class operator Implicit(const value: Lazy<T>): ILazy<T>;
    class operator Implicit(const value: Lazy<T>): T;
    class operator Implicit(const value: T): Lazy<T>;
    class operator Implicit(const value: TLazy<T>): Lazy<T>;

    property IsAssigned: Boolean read GetIsAssigned;

    /// <summary>
    ///   Gets a value that indicates whether a value has been created for this
    ///   <see cref="Lazy&lt;T&gt;" /> instance.
    /// </summary>
    /// <value>
    ///	  <b>True</b> if a value has been created for this
    ///	  <see cref="Lazy&lt;T&gt;" /> instance; otherwise, <b>False</b>.
    /// </value>
    property IsValueCreated: Boolean read GetIsValueCreated;

    /// <summary>
    ///	  Gets the lazily initialized value of the current
    ///	  <see cref="Lazy&lt;T&gt;" /> instance.
    /// </summary>
    /// <value>
    ///	  The lazily initialized value of the current
    ///	  <see cref="Lazy&lt;T&gt;" /> instance.
    /// </value>
    ///	<exception cref="Spring|EInvalidOperationException">
    ///	</exception>
    property Value: T read GetValue;
  end;

  TLazyInitializer = record
  public
    class function EnsureInitialized<T: class, constructor>(var target: T): T; overload; static;
    class function EnsureInitialized<T>(var target: T; const factoryMethod: TFunc<T>): T; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Multicast Event'}

  TEventsChangedAction = caAdded..caRemoved;
  TMethodPointer = procedure of object;
  TEventsChangedEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TEventsChangedAction) of object;
  TEventsChangedEvent = procedure(Sender: TObject; const Item: TMethodPointer;
    Action: TEventsChangedAction) of object;


  IEvent = interface(ICountable)
    ['{CFC14C4D-F559-4A46-A5B1-3145E9B182D8}']
  {$REGION 'Property Accessors'}
    function GetInvoke: TMethodPointer;
    function GetEnabled: Boolean;
    function GetIsInvokable: Boolean;
    function GetOnChanged: TEventsChangedEvent;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TEventsChangedEvent);
  {$ENDREGION}

    procedure Add(const handler: TMethodPointer);
    procedure Remove(const handler: TMethodPointer);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
    procedure ForEach(const action: TAction<TMethodPointer>);

    /// <summary>
    ///   Gets the value indicates whether the multicast event is enabled, or
    ///   sets the value to enable or disable the event.
    /// </summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;

    /// <summary>
    ///   Returns <b>True</b> when the event will do anything because it is <see cref="Spring|IEvent.Enabled">
    ///   Enabled</see> and contains any event handler. Otherwise returns <b>
    ///   False</b>.
    /// </summary>
    property IsInvokable: Boolean read GetIsInvokable;
    property Invoke: TMethodPointer read GetInvoke;
    property OnChanged: TEventsChangedEvent read GetOnChanged write SetOnChanged;
  end;

  /// <summary>
  ///   Represents a multicast event.
  /// </summary>
  /// <typeparam name="T">
  ///   The event handler type must be an instance procedural type such as
  ///   TNotifyEvent.
  /// </typeparam>
  IEvent<T> = interface(IEvent)
  {$REGION 'Property Accessors'}
    function GetInvoke: T;
//    function GetOnChanged: TEventsChangedEvent<T>;
//    procedure SetOnChanged(const value: TEventsChangedEvent<T>);
  {$ENDREGION}

    /// <summary>
    ///   Adds an event handler to the list.
    /// </summary>
    procedure Add(handler: T);

    /// <summary>
    ///   Removes an event handler if it was added to the event.
    /// </summary>
    procedure Remove(handler: T);

    /// <summary>
    ///   Removes all event handlers which were registered by an instance.
    /// </summary>
    procedure RemoveAll(instance: Pointer);

    /// <summary>
    ///   Clears all event handlers.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Iterates all event handlers and perform the specified action on each
    ///   one.
    /// </summary>
    procedure ForEach(const action: TAction<T>);

    /// <summary>
    ///   Invokes all event handlers.
    /// </summary>
    property Invoke: T read GetInvoke;
  end;

{$IFDEF SUPPORTS_GENERIC_EVENTS}
  Event<T> = record
  private
    fInstance: IEvent<T>;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetInvoke: T;
    function GetOnChanged: TEventsChangedEvent<T>;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(value: TEventsChangedEvent<T>);
    procedure EnsureInitialized;
  public
    class function Create: Event<T>; static;

    function Any: Boolean;

    procedure Add(const handler: T);
    procedure Remove(const handler: T);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: T read GetInvoke;
    property OnChanged: TEventsChangedEvent<T> read GetOnChanged write SetOnChanged;

    class operator Implicit(const value: IEvent<T>): Event<T>;
    class operator Implicit(var value: Event<T>): IEvent<T>;
    class operator Implicit(var value: Event<T>): T;
    class operator Implicit(const value: T): Event<T>;
  end;
{$ENDIF}

  INotifyEvent<T> = interface(IEvent<TNotifyEvent<T>>)
  end;

  {$ENDREGION}


  {$REGION 'Property change notification'}

  IEventArgs = interface
    ['{162CDCDF-F8FC-4E5A-9CE8-55EABAE42EC3}']
  end;

  IPropertyChangedEventArgs = interface(IEventArgs)
    ['{DC7B4497-FA42-46D1-BE50-C764C4808197}']
    function GetPropertyName: string;
    property PropertyName: string read GetPropertyName;
  end;

  TEventArgs = class(TInterfacedObject, IEventArgs)
  strict protected
    constructor Create;
  end;

  TPropertyChangedEventArgs = class(TEventArgs, IPropertyChangedEventArgs)
  private
    fPropertyName: string;
    function GetPropertyName: string;
  public
    constructor Create(const propertyName: string);
    property PropertyName: string read GetPropertyName;
  end;

  {$M+}
  TEventHandler<T: IEventArgs> = reference to procedure(Sender: TObject;
    const EventArgs: T);
  {$M-}

  TPropertyChangedEvent = procedure(Sender: TObject;
    const EventArgs: IPropertyChangedEventArgs) of object;

  IPropertyChangedEvent = IEvent<TPropertyChangedEvent>;

  INotifyPropertyChanged = interface
    ['{A517EC98-C651-466B-8290-F7EE96877E03}']
    function GetOnPropertyChanged: IPropertyChangedEvent;
    property OnPropertyChanged: IPropertyChangedEvent read GetOnPropertyChanged;
  end;

  {$ENDREGION}


  {$REGION 'Notification handler'}

  TNotificationEvent = procedure(Component: TComponent;
    Operation: TOperation) of object;

  TNotificationHandler = class(TComponent)
  private
    fOnNotification: TNotificationEvent;
  protected
    procedure Notification(Component: TComponent;
      Operation: TOperation); override;
  public
    property OnNotification: TNotificationEvent
      read fOnNotification write fOnNotification;
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
{$IFNDEF DELPHIXE3_UP}
    function TypeData: PTypeData; inline;
{$ENDIF}
    property TypeName: string read GetTypeName;
  end;

  {$ENDREGION}


  {$REGION 'TInterlocked'}

{$IFDEF DELPHI2010}
  TInterlocked = class sealed
    class function Increment(var Target: Integer): Integer; overload; static; inline;
    class function Increment(var Target: Int64): Int64; overload; static; inline;
    class function Decrement(var Target: Integer): Integer; overload; static; inline;
    class function Decrement(var Target: Int64): Int64; overload; static; inline;
    class function Add(var Target: Integer; Increment: Integer): Integer; overload; static;
    class function Add(var Target: Int64; Increment: Int64): Int64; overload; static;
    class function CompareExchange(var Target: Pointer; Value: Pointer; Comparand: Pointer): Pointer; overload; static;
    class function CompareExchange(var Target: TObject; Value: TObject; Comparand: TObject): TObject; overload; static; inline;
    class function CompareExchange<T: class>(var Target: T; Value: T; Comparand: T): T; overload; static; inline;
  end;
{$ELSE}
  TInterlocked = SyncObjs.TInterlocked;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TInterfacedCriticalSection'}

  ICriticalSection = interface(IInvokable)
    ['{16C21E9C-6450-4EA4-A3D3-1D59277C9BA6}']
    procedure Enter;
    procedure Leave;
    function ScopedLock: IInterface;
  end;

  TInterfacedCriticalSection = class(TCriticalSection, IInterface, ICriticalSection)
  private type
    TScopedLock = class(TInterfacedObject)
    private
      fCriticalSection: ICriticalSection;
    public
      constructor Create(const criticalSection: ICriticalSection);
      destructor Destroy; override;
    end;
  protected
    fRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ScopedLock: IInterface;
  end;

  {$ENDREGION}


  {$REGION 'Lock'}

  /// <summary>
  ///   Provides an easy to use wrapper around TCriticalSection. It
  ///   automatically initializes the TCriticalSection instance when required
  ///   and destroys it when the Lock goes out of scope.
  /// </summary>
  Lock = record
  private
    fCriticalSection: ICriticalSection;
    procedure EnsureInitialized;
  public
    /// <summary>
    ///   Calls Enter on the underlying TCriticalSection. The first call also
    ///   initializes the TCriticalSection instance.
    /// </summary>
    procedure Enter;

    /// <summary>
    ///   Calls Leave on the underlying TCriticalSection. If no call to Enter
    ///   has been made before it will raise an exception.
    /// </summary>
    /// <exception cref="EInvalidOperationException">
    ///   When Enter was not called before
    /// </exception>
    procedure Leave;

    /// <summary>
    ///   Calls Enter on the underlying TCriticalSection and returns an
    ///   interface reference that will call Leave once it goes out of scope.
    /// </summary>
    /// <remarks>
    ///   Use this to avoid the classic try/finally block but keep in mind that
    ///   the scope will be the entire method this is used in unless you keep
    ///   hold of the returned interface and explicitly set it to nil causing
    ///   its destruction.
    /// </remarks>
    function ScopedLock: IInterface;
  end;

  {$ENDREGION}


  {$REGION 'Smart pointer'}

  ISmartPointer<T> = reference to function: T;

  SmartPointer<T> = record
  private
    type
      TSmartPointer = class(TInterfacedObject)
      private
        fValue: Pointer;
      public
        constructor Create(const value);
        destructor Destroy; override;
      end;
  strict private
    fValue: T;
    fFinalizer: IInterface;
  public
    class operator Implicit(const value: T): SmartPointer<T>;
    class operator Implicit(const value: SmartPointer<T>): T;
    property Value: T read fValue;
  end;

  TSmartPointer<T> = class(TInterfacedObject, ISmartPointer<T>)
  private
    fValue: T;
    function Invoke: T; inline;
  public
    constructor Create; overload;
    constructor Create(const value: T); overload;
    destructor Destroy; override;
  end;

  {$ENDREGION}


  {$REGION 'TActivator'}

  IObjectActivator = interface
    ['{CE05FB89-3467-449E-81EA-A5AEECAB7BB8}']
    function CreateInstance: TValue;
  end;

  TActivator = record
  private
    type TConstructor = function(InstanceOrVMT: Pointer; Alloc: ShortInt): Pointer;
    class var Context: TRttiContext;
    class var ConstructorCache: TDictionary<TClass,TConstructor>;
    class function FindConstructor(const classType: TRttiInstanceType;
      const arguments: array of TValue): TRttiMethod; static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure ClearCache; static;

    class function CreateInstance(const classType: TRttiInstanceType): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const arguments: array of TValue): TValue; overload; static;
    class function CreateInstance(const classType: TRttiInstanceType;
      const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue; overload; static;

    class function CreateInstance(typeInfo: PTypeInfo): TObject; overload; static;
    class function CreateInstance(const typeName: string): TObject; overload; static;

    class function CreateInstance(classType: TClass): TObject; overload; static; inline;
    class function CreateInstance(classType: TClass;
      const arguments: array of TValue): TObject; overload; static;

    class function CreateInstance<T: class>: T; overload; static; inline;
    class function CreateInstance<T: class>(
      const arguments: array of TValue): T; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'TFinalizer'}

  TFinalizer = record
  public
    class procedure FinalizeInstance(var instance: TValue); overload; static;
    class procedure FinalizeInstance<T>(const instance: T); overload; static; inline;
  end;

  {$ENDREGION}


  {$REGION 'Tuples'}

  Tuple<T1, T2> = record
  private
    fValue1: T1;
    fValue2: T2;
  public
    constructor Create(const value1: T1; const value2: T2);
    function Equals(const value: Tuple<T1, T2>): Boolean;
    procedure Unpack(out value1: T1; out value2: T2); overload;
    class operator Equal(const left, right: Tuple<T1, T2>): Boolean;
    class operator NotEqual(const left, right: Tuple<T1, T2>): Boolean;
    class operator Implicit(const value: Tuple<T1, T2>): TArray<TValue>;
    class operator Implicit(const value: TArray<TValue>): Tuple<T1, T2>;
    class operator Implicit(const value: array of const): Tuple<T1, T2>;
    property Value1: T1 read fValue1;
    property Value2: T2 read fValue2;
  end;

  Tuple<T1, T2, T3> = record
  private
    fValue1: T1;
    fValue2: T2;
    fValue3: T3;
  public
    constructor Create(const value1: T1; const value2: T2; const value3: T3);
    function Equals(const value: Tuple<T1, T2, T3>): Boolean;
    procedure Unpack(out value1: T1; out value2: T2); overload;
    procedure Unpack(out value1: T1; out value2: T2; out value3: T3); overload;
    class operator Equal(const left, right: Tuple<T1, T2, T3>): Boolean;
    class operator NotEqual(const left, right: Tuple<T1, T2, T3>): Boolean;
    class operator Implicit(const value: Tuple<T1, T2, T3>): TArray<TValue>;
    class operator Implicit(const value: Tuple<T1, T2, T3>): Tuple<T1, T2>;
    class operator Implicit(const value: TArray<TValue>): Tuple<T1, T2, T3>;
    class operator Implicit(const value: array of const): Tuple<T1, T2, T3>;
    property Value1: T1 read fValue1;
    property Value2: T2 read fValue2;
    property Value3: T3 read fValue3;
  end;

  Tuple<T1, T2, T3, T4> = record
  private
    fValue1: T1;
    fValue2: T2;
    fValue3: T3;
    fValue4: T4;
  public
    constructor Create(const value1: T1; const value2: T2; const value3: T3; const value4: T4);
    function Equals(const value: Tuple<T1, T2, T3, T4>): Boolean;
    procedure Unpack(out value1: T1; out value2: T2); overload;
    procedure Unpack(out value1: T1; out value2: T2; out value3: T3); overload;
    procedure Unpack(out value1: T1; out value2: T2; out value3: T3; out value4: T4); overload;
    class operator Equal(const left, right: Tuple<T1, T2, T3, T4>): Boolean;
    class operator NotEqual(const left, right: Tuple<T1, T2, T3, T4>): Boolean;
    class operator Implicit(const value: Tuple<T1, T2, T3, T4>): TArray<TValue>;
    class operator Implicit(const value: Tuple<T1, T2, T3, T4>): Tuple<T1, T2>;
    class operator Implicit(const value: Tuple<T1, T2, T3, T4>): Tuple<T1, T2, T3>;
    class operator Implicit(const value: TArray<TValue>): Tuple<T1, T2, T3, T4>;
    class operator Implicit(const value: array of const): Tuple<T1, T2, T3, T4>;
    property Value1: T1 read fValue1;
    property Value2: T2 read fValue2;
    property Value3: T3 read fValue3;
    property Value4: T4 read fValue4;
  end;

  Tuple = class
  public
    class function Pack<T1, T2>(const value1: T1;
      const value2: T2): Tuple<T1, T2>; overload; static;
    class function Pack<T1, T2, T3>(const value1: T1; const value2: T2;
      const value3: T3): Tuple<T1, T2, T3>; overload; static;
    class function Pack<T1, T2, T3, T4>(const value1: T1; const value2: T2;
      const value3: T3; const value4: T4): Tuple<T1, T2, T3, T4>; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'TArray'}

  TArray = class(Generics.Collections.TArray)
  public
    /// <summary>
    ///   Determines whether the specified item exists as an element in an
    ///   array.
    /// </summary>
    class function Contains<T>(const values: array of T; const item: T): Boolean; static;

    /// <summary>
    ///   Copies an open array to a dynamic array.
    /// </summary>
    class function Copy<T>(const values: array of T): TArray<T>; static;

    /// <summary>
    ///   Searches for the specified object and returns the index of the first
    ///   occurrence within the entire array.
    /// </summary>
    class function IndexOf<T>(const values: array of T; const item: T): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified object and returns the index of the first
    ///   occurrence within the range of elements in the array that extends
    ///   from the specified index to the last element.
    /// </summary>
    class function IndexOf<T>(const values: array of T; const item: T;
      index: Integer): Integer; overload; static;

    /// <summary>
    ///   Searches for the specified object and returns the index of the first
    ///   occurrence within the range of elements in the array that starts at
    ///   the specified index and contains the specified number of elements.
    /// </summary>
    class function IndexOf<T>(const values: array of T; const item: T;
      index, count: Integer): Integer; overload; static;

    class function IndexOf<T>(const values: array of T; const item: T;
      index, count: Integer;
      const comparer: IEqualityComparer<T>): Integer; overload; static;
  end;

  {$ENDREGION}


  {$REGION 'Dynamic array'}

{$IFDEF DELPHI2010}
  TArrayEnumerator<T> = class
{$ELSE}
  TArrayEnumerator<T> = record
{$ENDIF}
  private
    fItems: TArray<T>;
    fIndex: Integer;
    function GetCurrent: T; inline;
  public
    constructor Create(const items: TArray<T>);
    function MoveNext: Boolean; inline;
    property Current: T read GetCurrent;
  end;

  DynamicArray<T> = record
  private
    fItems: TArray<T>; // DO NOT ADD ANY OTHER MEMBERS !!!
    function GetCount: Integer; inline;
    function GetItem(index: Integer): T; inline;
    procedure SetCount(value: Integer); inline;
    procedure SetItem(index: Integer; const value: T); inline;
    procedure InternalInsert(index: Integer; const items: array of T); overload;
    function InternalEquals(const items: array of T): Boolean; overload;
    function InternalIndexOf(const item: T): Integer;
    function InternalIndexOfInt(const item: Integer): Integer;
    function InternalIndexOfStr(const item: string): Integer;
  public
    class operator Implicit(const value: TArray<T>): DynamicArray<T>; inline;
    class operator Implicit(const value: DynamicArray<T>): TArray<T>; inline;
    class operator Add(const left, right: DynamicArray<T>): DynamicArray<T>; inline;
    class operator Add(const left: DynamicArray<T>; const right: TArray<T>): DynamicArray<T>; inline;
    class operator Add(const left: TArray<T>; const right: DynamicArray<T>): DynamicArray<T>; inline;
    class operator Add(const left: DynamicArray<T>; const right: T): DynamicArray<T>; inline;
    class operator Add(const left: T; const right: DynamicArray<T>): DynamicArray<T>; inline;
    class operator Subtract(const left, right: DynamicArray<T>): DynamicArray<T>; inline;
    class operator Subtract(const left: DynamicArray<T>; const right: T): DynamicArray<T>; inline;
    class operator In(const left: T; const right: DynamicArray<T>): Boolean; inline;
    class operator In(const left, right: DynamicArray<T>): Boolean; inline;
    class operator In(const left: TArray<T>; const right: DynamicArray<T>): Boolean; inline;
    class operator Equal(const left, right: DynamicArray<T>): Boolean; inline;
    class operator NotEqual(const left, right: DynamicArray<T>): Boolean; inline;

    procedure Assign(const items: array of T);
    procedure Clear; inline;

    function Add(const item: T): Integer; overload; inline;
    procedure Add(const items: array of T); overload;
    procedure Add(const items: TArray<T>); overload; inline;
    procedure Add(const items: DynamicArray<T>); overload; inline;
    procedure Insert(index: Integer; const item: T); overload; inline;
    procedure Insert(index: Integer; const items: array of T); overload;
    procedure Insert(index: Integer; const items: TArray<T>); overload; inline;
    procedure Delete(index: Integer); overload; inline;
    procedure Delete(index: Integer; count: Integer); overload; inline;
    procedure Remove(const item: T); overload; inline;
    procedure Remove(const items: array of T); overload;
    procedure Remove(const items: TArray<T>); overload; inline;

    function Contains(const item: T): Boolean; overload; inline;
    function Contains(const items: array of T): Boolean; overload;
    function Contains(const items: TArray<T>): Boolean; overload;
    function IndexOf(const item: T): Integer; inline;
    function Equals(const items: array of T): Boolean; overload;
    function Equals(const items: TArray<T>): Boolean; overload; inline;

    function Slice(index: Integer): DynamicArray<T>; overload; inline;
    function Slice(index: Integer; count: Integer): DynamicArray<T>; overload; inline;
    function Splice(index: Integer; count: Integer): DynamicArray<T>; overload; inline;
    function Splice(index: Integer; count: Integer; const items: array of T): DynamicArray<T>; overload;

    procedure Sort; overload; inline;
    procedure Sort(const comparer: IComparer<T>); overload; inline;
    procedure Sort(const comparer: TComparison<T>); overload; inline;
    procedure Reverse;

    function GetEnumerator: TArrayEnumerator<T>; inline;
    property Count: Integer read GetCount;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property Length: Integer read GetCount write SetCount;
  end;

  {$ENDREGION}


  {$REGION 'Routines'}

{$IFNDEF DELPHIXE_UP}
function SplitString(const s: string; delimiter: Char): TStringDynArray;
{$ENDIF}

{$IFNDEF DELPHIXE2_UP}
function ReturnAddress: Pointer;
{$ENDIF}

procedure PlatformNotImplemented;

/// <summary>
///	  Raises an <see cref="Spring|EArgumentNullException" /> if the
///	  <paramref name="value" /> is nil.
/// </summary>
procedure CheckArgumentNotNull(const value: IInterface; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

/// <summary>
///	  Raises an <see cref="Spring|EArgumentNullException" /> if the
///	  <paramref name="value" /> is nil.
/// </summary>
procedure CheckArgumentNotNull(value: Pointer; const argumentName: string); overload; deprecated 'Use Guard.CheckNotNull instead';

function GetQualifiedClassName(AInstance: TObject): string; overload; inline;
function GetQualifiedClassName(AClass: TClass): string; overload; {$IFDEF DELPHIXE2_UP}inline;{$ENDIF}

/// <summary>
///   Determines whether an instance of <c>leftType</c> can be assigned from an
///   instance of <c>rightType</c>.
/// </summary>
function IsAssignableFrom(leftType, rightType: PTypeInfo): Boolean; overload;

function IsAssignableFrom(const leftTypes, rightTypes: array of PTypeInfo): Boolean; overload;

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

function GetTypeKind(typeInfo: PTypeInfo): TTypeKind; inline;

procedure FinalizeValue(const value; typeInfo: PTypeInfo); inline;

function MethodReferenceToMethodPointer(const methodRef): TMethodPointer;
function MethodPointerToMethodReference(const method: TMethodPointer): IInterface;
{$ENDREGION}


implementation

uses
  RTLConsts,
  SysConst,
  Spring.Events,
  Spring.ResourceStrings;


{$REGION 'Routines'}

{$IFNDEF DELPHIXE_UP}
function SplitString(const s: string; delimiter: Char): TStringDynArray;
var
  list: TStrings;
  i: Integer;
begin
  list := TStringList.Create;
  try
    list.StrictDelimiter := True;
    list.Delimiter := delimiter;
    list.DelimitedText := s;
    SetLength(Result, list.Count);
    for i := 0 to list.Count - 1 do
      Result[i] := list[i];
  finally
    list.Free;
  end;
end;
{$ENDIF}

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
type
  PPPTypeInfo = ^PPTypeInfo;
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
    Result := rightData.ClassType.InheritsFrom(leftData.ClassType)
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

function IsAssignableFrom(const leftTypes, rightTypes: array of PTypeInfo): Boolean;
var
  i: Integer;
begin
  Result := Length(leftTypes) = Length(rightTypes);
  if Result then
    for i := Low(leftTypes) to High(leftTypes) do
      if not IsAssignableFrom(leftTypes[i], rightTypes[i]) then
        Exit(False);
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

function GetTypeKind(typeInfo: PTypeInfo): TTypeKind;
begin
  Result := typeInfo.Kind;
end;

procedure FinalizeValue(const value; typeInfo: PTypeInfo);
var
  recTypeInfo: PTypeInfo;
begin
  case typeInfo.Kind of
    tkClass: {$IFNDEF AUTOREFCOUNT}TObject(value).Free;{$ELSE}TObject(value).DisposeOf;{$ENDIF}
    tkPointer:
    begin
      recTypeInfo := GetTypeData(typeInfo).RefType^;
      FinalizeArray(Pointer(value), recTypeInfo, 1);
      FillChar(Pointer(value)^, GetTypeData(recTypeInfo).RecSize, 0);
      FreeMem(Pointer(value));
    end;
  end;
end;

function MethodReferenceToMethodPointer(const methodRef): TMethodPointer;
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  TMethod(Result).Code := PPVtable(methodRef)^^[3];
  TMethod(Result).Data := Pointer(methodRef);
end;

function MethodPointerToMethodReference(const method: TMethodPointer): IInterface;
begin
  Result := IInterface(TMethod(method).Data);
end;

{$ENDREGION}


{$REGION 'TValueHelper'}

{$IFDEF DELPHI2010}
function TValueHelper.AsString: string;
begin
  Result := AsType<string>;
end;
{$ENDIF}

function TValueHelper.AsType<T>: T;
begin
{$IFDEF DELPHI2010}
  if IsEmpty then
    Exit(Default(T));
{$ENDIF}
  if not TryAsInterface(System.TypeInfo(T), Result) then
  if not TryAsType<T>(Result) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

function TValueHelper.Cast(typeInfo: PTypeInfo): TValue;
var
  intf: IInterface;
begin
  if TryAsInterface(typeInfo, intf) then
    TValue.Make(@intf, typeInfo, Result)
  else if not TryCast(typeInfo, Result) then
    raise EInvalidCast.CreateRes(@SInvalidCast);
end;

class function TValueHelper.FromVarRec(const value: TVarRec): TValue;
begin
  case value.VType of
    vtInteger: Result := value.VInteger;
    vtBoolean: Result := value.VBoolean;
{$IF Declared(AnsiChar)}
    vtChar: Result := string(value.VChar);
{$IFEND}
    vtExtended: Result := value.VExtended^;
{$IF Declared(ShortString)}
    vtString: Result := string(value.VString^);
{$IFEND}
    vtPointer: Result := value.VPointer;
{$IF Declared(PAnsiChar)}
    vtPChar: Result := string(value.VPChar);
{$IFEND}
    vtObject: Result := TObject(value.VObject);
    vtClass: Result := value.VClass;
    vtWideChar: Result := value.VWideChar;
    vtPWideChar: Result := string(value.VPWideChar);
{$IF Declared(AnsiString)}
    vtAnsiString: Result := string(value.VAnsiString);
{$IFEND}
    vtCurrency: Result := value.VCurrency^;
    vtVariant: Result := TValue.FromVariant(value.VVariant^);
    vtInterface: Result := TValue.From<IInterface>(IInterface(value.VInterface));
{$IF Declared(WideString)}
    vtWideString: Result := string(value.VWideString);
{$IFEND}
    vtInt64: Result := value.VInt64^;
    vtUnicodeString: Result := string(value.VUnicodeString);
  end;
end;

function TValueHelper.IsString: Boolean;
const
  StringKinds = [tkString, tkLString, tkWString, tkUString, tkChar, tkWChar];
begin
  Result := IsEmpty or (Kind in StringKinds);
end;

{$IFDEF DELPHI2010}
function TValueHelper.IsType(ATypeInfo: PTypeInfo): Boolean;
var
  unused: TValue;
begin
  Result := IsEmpty or TryCast(ATypeInfo, unused);
end;

function TValueHelper.IsType<T>: Boolean;
begin
  Result := IsType(System.TypeInfo(T));
end;
{$ENDIF}

function TValueHelper.TryAsInterface(typeInfo: PTypeInfo; out Intf): Boolean;
var
  typeData: PTypeData;
  obj: TObject;
begin
  if not (Kind in [tkClass, tkInterface]) then
    Exit(False);
  if typeInfo.Kind <> tkInterface then
    Exit(False);
  if Self.TypeInfo = typeInfo then
    Result := True
  else
  begin
    typeData := GetTypeData(typeInfo);
    if Kind = tkClass then
    begin
{$IFDEF AUTOREFCOUNT}
      Self.FData.FValueData.ExtractRawData(@obj);
{$ELSE}
      obj := TObject(Self.FData.FAsObject);
{$ENDIF}
      Exit(obj.GetInterface(typeData.Guid, Intf));
    end;
    Result := False;
    typeData := Self.TypeData;
    while Assigned(typeData) and Assigned(typeData.IntfParent) do
    begin
      if typeData.IntfParent^ = typeInfo then
      begin
        Result := True;
        Break;
      end;
      typeData := GetTypeData(typeData.IntfParent^);
    end;
  end;
  if Result then
    IInterface(Intf) := AsInterface;
end;

{$ENDREGION}


{$REGION 'TRttiMethodHelper'}

procedure TRttiMethodHelper.DispatchValue(const value: TValue;
  typeInfo: PTypeInfo);
type
  PValueData = ^TValueData;
begin
  if (value.TypeInfo <> typeInfo) and (value.Kind = tkInterface)
    and (typeInfo.Kind = tkInterface)
    and IsAssignableFrom(typeInfo, value.TypeInfo) then
    PValueData(@value).FTypeInfo := typeInfo;
end;

function TRttiMethodHelper.Invoke(Instance: TObject;
  const Args: array of TValue): TValue;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := GetParameters;
  if Length(Args) <> Length(parameters) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);
  for i := Low(Args) to High(Args) do
    DispatchValue(Args[i], parameters[i].ParamType.Handle);
  Result := Self.DispatchInvoke(Instance, Args);
end;

function TRttiMethodHelper.Invoke(Instance: TClass;
  const Args: array of TValue): TValue;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := GetParameters;
  if Length(Args) <> Length(parameters) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);
  for i := Low(Args) to High(Args) do
    DispatchValue(Args[i], parameters[i].ParamType.Handle);
  Result := Self.DispatchInvoke(Instance, Args);
end;

function TRttiMethodHelper.Invoke(Instance: TValue;
  const Args: array of TValue): TValue;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := GetParameters;
  if Length(Args) <> Length(parameters) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);
  for i := Low(Args) to High(Args) do
    DispatchValue(Args[i], parameters[i].ParamType.Handle);
  Result := Self.DispatchInvoke(Instance, Args);
end;

{$ENDREGION}


{$REGION 'TNamedValue'}

constructor TNamedValue.Create(const name: string; const value: TValue);
begin
  fName := name;
  fValue := value;
end;

class function TNamedValue.From<T>(const name: string;
  const value: T): TNamedValue;
begin
  Result.fName := name;
  Result.fValue := TValue.From<T>(value);
end;

class operator TNamedValue.Implicit(const value: TNamedValue): TValue;
begin
  Result := TValue.From(value);
end;

class operator TNamedValue.Implicit(const value: TValue): TNamedValue;
begin
  Result := value.AsType<TNamedValue>;
end;

{$ENDREGION}


{$REGION 'TTypedValue'}

constructor TTypedValue.Create(const typeInfo: PTypeInfo; const value: TValue);
begin
  fTypeInfo := typeInfo;
  fValue := value;
end;

class function TTypedValue.From<T>(const typeInfo: PTypeInfo;
  const value: T): TTypedValue;
begin
  Result.fTypeInfo := typeInfo;
  Result.fValue := TValue.From<T>(value);
end;

class operator TTypedValue.Implicit(const value: TTypedValue): TValue;
begin
  Result := TValue.From(value);
end;

class operator TTypedValue.Implicit(const value: TValue): TTypedValue;
begin
  Result := value.AsType<TTypedValue>;
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

class procedure Guard.CheckIndex(length, index, indexBase: Integer);
const
  IndexArgName = 'index';
begin
  if (index < indexBase) or (index >= indexBase + length) then
    Guard.RaiseArgumentOutOfRangeException(IndexArgName);
end;

class procedure Guard.CheckRange(length, index, count, indexBase: Integer);
const
  CountArgName = 'count';
begin
  Guard.CheckIndex(length, index, indexBase);
  if (count < 0) or (index + count > indexBase + length) then
    Guard.RaiseArgumentOutOfRangeException(CountArgName);
end;

class procedure Guard.CheckRange<T>(const buffer: array of T; index: Integer);
begin
  Guard.CheckIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange<T>(const buffer: array of T;
  index, count: Integer);
begin
  Guard.CheckRange(Length(buffer), index, count);
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
      argumentName, typeInfo.TypeName, argumentValue]);
end;

class procedure Guard.CheckRange(condition: Boolean;
  const argumentName: string);
begin
  if not condition then
    Guard.RaiseArgumentOutOfRangeException(argumentName);
end;

class procedure Guard.CheckRange(const buffer: array of Byte;
  index, count: Integer);
begin
  Guard.CheckRange(Length(buffer), index, count);
end;

class procedure Guard.CheckRange(const buffer: array of Char;
  index, count: Integer);
begin
  Guard.CheckRange(Length(buffer), index, count);
end;

class procedure Guard.CheckRange(const buffer: array of Byte; index: Integer);
begin
  Guard.CheckIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange(const buffer: array of Char; index: Integer);
begin
  Guard.CheckIndex(Length(buffer), index);
end;

class procedure Guard.CheckRange(const s: string; index: Integer);
begin
  Guard.CheckIndex(Length(s), index, 1);
end;

class procedure Guard.CheckRange(const s: string; index, count: Integer);
begin
  Guard.CheckRange(Length(s), index, count, 1);
end;

class procedure Guard.CheckRangeInclusive(value, min, max: Integer);
const
  ValueArgName = 'value';
begin
  if (value < min) or (value > max) then
    Guard.RaiseArgumentOutOfRangeException(ValueArgName);
end;

class procedure Guard.CheckSet<T>(const argumentValue: T;
  const argumentName: string);
var
  value: Integer;
begin
  value := 0;
  Move(argumentValue, value, SizeOf(T));
  Guard.CheckSet<T>(value, argumentName);
end;

class procedure Guard.CheckSet<T>(argumentValue: Cardinal;
  const argumentName: string);
var
  typeInfo: PTypeInfo;
  data: PTypeData;
  minValue, maxValue: Cardinal;
begin
  typeInfo := System.TypeInfo(T);
  Guard.CheckTypeKind(typeInfo, [tkSet], 'T');

  data := GetTypeData(typeInfo);
  Guard.CheckNotNull(data, 'data');

  if Assigned(data.CompType) then
  begin
    data := GetTypeData(data.CompType^);
    maxValue := (1 shl (data.MaxValue - data.MinValue + 1)) - 1;
  end
  else
    case data^.OrdType of
      otSByte, otUByte: maxValue := High(Byte);
      otSWord, otUWord: maxValue := High(Word);
      otSLong, otULong: Exit;
    else
      maxValue := 0;
    end;

  if argumentValue > maxValue then
    raise EInvalidEnumArgumentException.CreateResFmt(@SInvalidSetArgument, [
      argumentName, typeInfo.TypeName, argumentValue]);
end;

class procedure Guard.CheckRangeExclusive(value, min, max: Integer);
const
  ValueArgName = 'value';
begin
  if (value <= min) or (value >= max) then
    Guard.RaiseArgumentOutOfRangeException(ValueArgName);
end;


{$IFNDEF NEXTGEN}
class procedure Guard.CheckRange(const s: WideString; index: Integer);
begin
  Guard.CheckIndex(Length(s), index, 1);
end;

class procedure Guard.CheckRange(const s: WideString; index, count: Integer);
begin
  Guard.CheckRange(Length(s), index, count, 1);
end;

class procedure Guard.CheckRange(const s: RawByteString; index: Integer);
begin
  Guard.CheckIndex(Length(s), index, 1);
end;

class procedure Guard.CheckRange(const s: RawByteString; index, count: Integer);
begin
  Guard.CheckRange(Length(s), index, count, 1);
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
  Result := fHasValue <> '';
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
    Result := fValue
  else
    Result := Default(T);
end;

function Nullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := fValue
  else
    Result := defaultValue;
end;

function Nullable<T>.Equals(const other: Nullable<T>): Boolean;
begin
  if HasValue and other.HasValue then
  begin
    case {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} of
      tkInteger: Result := PInteger(@fValue)^ = PInteger(@other.fValue)^;
{$IFNDEF NEXTGEN}
      tkChar: Result := PAnsiChar(@fValue)^ = PAnsiChar(@other.fValue)^;
      tkString: Result := PShortString(@fValue)^ = PShortString(@other.fValue)^;
      tkLString: Result := PAnsiString(@fValue)^ = PAnsiString(@other.fValue)^;
      tkWString: Result := PWideString(@fValue)^ = PWideString(@other.fValue)^;
{$ENDIF}
      tkFloat:
      begin
        case GetTypeData(TypeInfo(T)).FloatType of
          ftSingle: Result := PSingle(@fValue)^ = PSingle(@other.fValue)^;
          ftDouble: Result := PDouble(@fValue)^ = PDouble(@other.fValue)^;
          ftExtended: Result := PExtended(@fValue)^ = PExtended(@other.fValue)^;
          ftComp: Result := PComp(@fValue)^ = PComp(@other.fValue)^;
          ftCurr: Result := PCurrency(@fValue)^ = PCurrency(@other.fValue)^;
        end;
      end;
      tkWChar: Result := PWideChar(@fValue)^ = PWideChar(@other.fValue)^;
      tkInt64: Result := PInt64(@fValue)^ = PInt64(@other.fValue)^;
      tkUString: Result := PUnicodeString(@fValue)^ = PUnicodeString(@other.fValue)^;
    else
    Result := TEqualityComparer<T>.Default.Equals(fValue, other.fValue)
    end;
  end
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
    v := TValue.From<T>(value.fValue);
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

class operator Nullable<T>.Equal(const left, right: Nullable<T>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Nullable<T>.NotEqual(const left, right: Nullable<T>): Boolean;
begin
  Result := not left.Equals(right);
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

class function TLazyInitializer.EnsureInitialized<T>(var target: T): T;
var
  value: T;
begin
  if target = nil then
  begin
    value := T.Create;
    if TInterlocked.CompareExchange<T>(target, value, nil) <> nil then
      value.Free;
  end;
  Result := target;
end;

class function TLazyInitializer.EnsureInitialized<T>(var target: T;
  const factoryMethod: TFunc<T>): T;
var
  value: T;
begin
  if PPointer(@target)^ = nil then
  begin
    value := factoryMethod;
    case PTypeInfo(TypeInfo(T)).Kind of
      tkClass:
        if TInterlocked.CompareExchange(PObject(@target)^, PObject(@value)^, TObject(nil)) <> nil then
          PObject(@value)^.Free;
      tkInterface:
        if TInterlocked.CompareExchange(PPointer(@target)^, PPointer(@value)^, nil) = nil then
          PPointer(@value)^ := nil;
    end;
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

function Event<T>.Any: Boolean;
begin
  Result := Assigned(fInstance) and fInstance.Any;
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

function Event<T>.GetOnChanged: TEventsChangedEvent<T>;
begin
  EnsureInitialized;
  Result := TEventsChangedEvent<T>(fInstance.OnChanged);
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

procedure Event<T>.SetOnChanged(value: TEventsChangedEvent<T>);
begin
  EnsureInitialized;
  fInstance.OnChanged := TEventsChangedEvent(value);
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

{$IFNDEF DELPHIXE3_UP}
function TTypeInfoHelper.TypeData: PTypeData;
begin
  Result := GetTypeData(@Self);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TInterlocked'}

{$IFDEF DELPHI2010}
class function TInterlocked.Add(var Target: Integer; Increment: Integer): Integer;
asm
  MOV  ECX,EDX
  XCHG EAX,EDX
  LOCK XADD [EDX],EAX
  ADD  EAX,ECX
end;

class function TInterlocked.Add(var Target: Int64; Increment: Int64): Int64;
asm
  PUSH  EBX
  PUSH  ESI
  MOV   ESI,Target
  MOV   EAX,DWORD PTR [ESI]
  MOV   EDX,DWORD PTR [ESI+4]
@@1:
  MOV   EBX,EAX
  MOV   ECX,EDX
  ADD   EBX,LOW Increment
  ADC   ECX,HIGH Increment
  LOCK  CMPXCHG8B [ESI]
  JNZ   @@1
  ADD   EAX,LOW Increment
  ADC   EDX,HIGH Increment
  POP   ESI
  POP   EBX
end;

class function TInterlocked.Decrement(var Target: Int64): Int64;
begin
  Result := Add(Target, -1);
end;

class function TInterlocked.Decrement(var Target: Integer): Integer;
begin
  Result := Add(Target, -1);
end;

class function TInterlocked.CompareExchange(var Target: Pointer; Value: Pointer; Comparand: Pointer): Pointer;
asm
  XCHG EAX,EDX
  XCHG EAX,ECX
  LOCK CMPXCHG [EDX],ECX
end;

class function TInterlocked.CompareExchange(var Target: TObject; Value, Comparand: TObject): TObject;
begin
  Result := TObject(CompareExchange(Pointer(Target), Pointer(Value), Pointer(Comparand)));
end;

class function TInterlocked.CompareExchange<T>(var Target: T; Value, Comparand: T): T;
begin
  TObject(Pointer(@Result)^) := CompareExchange(TObject(Pointer(@Target)^), TObject(Pointer(@Value)^), TObject(Pointer(@Comparand)^));
end;

class function TInterlocked.Increment(var Target: Integer): Integer;
begin
  Result := Add(Target, 1);
end;

class function TInterlocked.Increment(var Target: Int64): Int64;
begin
  Result := Add(Target, 1);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TEventArgs'}

constructor TEventArgs.Create;
begin
end;

{$ENDREGION}


{$REGION 'TPropertyChangedEventArgs'}

constructor TPropertyChangedEventArgs.Create(const propertyName: string);
begin
  inherited Create;
  fPropertyName := propertyName;
end;

function TPropertyChangedEventArgs.GetPropertyName: string;
begin
  Result := fPropertyName;
end;

{$ENDREGION}


{$REGION 'TNotificationHandler'}

procedure TNotificationHandler.Notification(Component: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Assigned(fOnNotification) then
    fOnNotification(Component, Operation);
end;

{$ENDREGION}


{$REGION 'TInterfacedCriticalSection'}

function TInterfacedCriticalSection.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedCriticalSection._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := TInterlocked.Increment(fRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TInterfacedCriticalSection._Release: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := TInterlocked.Decrement(fRefCount);
  if Result = 0 then
    Destroy;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

function TInterfacedCriticalSection.ScopedLock: IInterface;
begin
  Result := TScopedLock.Create(Self);
end;

{$ENDREGION}


{$REGION 'TInterfacedCriticalSection.TScopedLock'}

constructor TInterfacedCriticalSection.TScopedLock.Create(
  const criticalSection: ICriticalSection);
begin
  inherited Create;
  fCriticalSection := criticalSection;
  fCriticalSection.Enter;
end;

destructor TInterfacedCriticalSection.TScopedLock.Destroy;
begin
  fCriticalSection.Leave;
  inherited;
end;

{$ENDREGION}


{$REGION 'Lock'}

procedure Lock.EnsureInitialized;
var
  criticalSection: ICriticalSection;
begin
  if not Assigned(fCriticalSection) then
  begin
    criticalSection := TInterfacedCriticalSection.Create;
    if TInterlocked.CompareExchange(Pointer(fCriticalSection),
      Pointer(criticalSection), nil) = nil then
      Pointer(criticalSection) := nil;
  end;
end;

procedure Lock.Enter;
begin
  EnsureInitialized;
  fCriticalSection.Enter;
end;

procedure Lock.Leave;
begin
  if not Assigned(fCriticalSection) then
    raise EInvalidOperationException.CreateRes(@SCriticalSectionNotInitialized);
  fCriticalSection.Leave;
end;

function Lock.ScopedLock: IInterface;
begin
  EnsureInitialized;
  Result := fCriticalSection.ScopedLock;
end;

{$ENDREGION}


{$REGION 'SmartPointer<T>'}

class operator SmartPointer<T>.Implicit(const value: T): SmartPointer<T>;
begin
  Result.fValue := value;
  case {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} of
    {$IFNDEF AUTOREFCOUNT}tkClass,{$ENDIF}
    tkPointer: Result.fFinalizer := TSmartPointer.Create(Result.fValue);
  end;
end;

class operator SmartPointer<T>.Implicit(const value: SmartPointer<T>): T;
begin
  Result := value.fValue;
end;

{$ENDREGION}


{$REGION 'SmartPointer<T>.TSmartPointer'}

constructor SmartPointer<T>.TSmartPointer.Create(const value);
begin
  fValue := Pointer(value);
end;

destructor SmartPointer<T>.TSmartPointer.Destroy;
begin
  FinalizeValue(fValue, TypeInfo(T));
  inherited;
end;

{$ENDREGION}


{$REGION 'TSmartPointer<T>'}

constructor TSmartPointer<T>.Create;
begin
  case {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} of
    tkClass: PObject(@fValue)^ := TActivator.CreateInstance(TypeInfo(T));
    tkPointer: PPointer(@fValue)^ := AllocMem(GetTypeSize(GetTypeData(TypeInfo(T)).RefType^));
  end;
end;

constructor TSmartPointer<T>.Create(const value: T);
begin
  fValue := value;
end;

destructor TSmartPointer<T>.Destroy;
begin
  FinalizeValue(fValue, TypeInfo(T));
  inherited;
end;

function TSmartPointer<T>.Invoke: T;
begin
  Result := fValue;
end;

{$ENDREGION}


{$REGION 'TActivator'}

class constructor TActivator.Create;
begin
  Context := TRttiContext.Create;
  ConstructorCache := TDictionary<TClass,TConstructor>.Create;
end;

class destructor TActivator.Destroy;
begin
  ConstructorCache.Free;
  Context.Free;
end;

class procedure TActivator.ClearCache;
begin
  ConstructorCache.Clear;
  Context.Free;
  Context := TRttiContext.Create;
end;

class function TActivator.CreateInstance(
  const classType: TRttiInstanceType): TValue;
begin
  Result := CreateInstance(classType, []);
end;

class function TActivator.CreateInstance(const classType: TRttiInstanceType;
  const arguments: array of TValue): TValue;
var
  method: TRttiMethod;
begin
  method := FindConstructor(classType, arguments);
  if Assigned(method) then
    Result := CreateInstance(classType, method, arguments)
  else
    raise ENotSupportedException.CreateResFmt(
      @SMissingConstructor, [classType.ClassName]);
end;

class function TActivator.CreateInstance(const classType: TRttiInstanceType;
  const constructorMethod: TRttiMethod; const arguments: array of TValue): TValue;
begin
  Result := constructorMethod.Invoke(classType.MetaclassType, arguments);
end;

class function TActivator.CreateInstance(typeInfo: PTypeInfo): TObject;
var
  classType: TClass;
  ctor: TConstructor;
  rttiType: TRttiType;
begin
  classType := typeInfo.TypeData.ClassType;
  if ConstructorCache.TryGetValue(classType, ctor) then
    Result := ctor(classType, 1)
  else
  begin
    rttiType := Context.GetType(typeInfo);
    Result := CreateInstance(TRttiInstanceType(rttiType), []).AsObject;
  end;
end;

class function TActivator.CreateInstance(const typeName: string): TObject;
var
  rttiType: TRttiType;
begin
  rttiType := Context.FindType(typeName);
  Result := CreateInstance(TRttiInstanceType(rttiType), []).AsObject;
end;

class function TActivator.CreateInstance(classType: TClass): TObject;
begin
  Result := CreateInstance(classType.ClassInfo);
end;

class function TActivator.CreateInstance(classType: TClass;
  const arguments: array of TValue): TObject;
var
  rttiType: TRttiType;
begin
  rttiType := Context.GetType(classType);
  Result := CreateInstance(TRttiInstanceType(rttiType), arguments).AsObject;
end;

class function TActivator.CreateInstance<T>: T;
begin
  Result := T(CreateInstance(TypeInfo(T)));
end;

class function TActivator.CreateInstance<T>(
  const arguments: array of TValue): T;
begin
  Result := T(CreateInstance(TClass(T), arguments));
end;

class function TActivator.FindConstructor(const classType: TRttiInstanceType;
  const arguments: array of TValue): TRttiMethod;

  function Assignable(const params: TArray<TRttiParameter>;
    const args: array of TValue): Boolean;
  var
    i: Integer;
    v: TValue;
  begin
    Result := Length(params) = Length(args);
    if Result then
      for i := Low(args) to High(args) do
        if not args[i].TryCast(params[i].paramType.Handle, v) then
          Exit(False);
  end;

var
  method: TRttiMethod;
begin
  for method in classType.GetMethods do
  begin
    if method.MethodKind <> mkConstructor then
      Continue;

    if Assignable(method.GetParameters, arguments) then
    begin
      if Length(arguments) = 0 then
        ConstructorCache.AddOrSetValue(classType.MetaclassType, method.CodeAddress);
      Exit(method);
    end;
  end;
  Result := nil;
end;

{$ENDREGION}


{$REGION 'TFinalizer'}

class procedure TFinalizer.FinalizeInstance(var instance: TValue);
var
  p: Pointer;
begin
  p := instance.GetReferenceToRawData;
  if Assigned(p) then
    FinalizeValue(p^, instance.TypeInfo);
end;

class procedure TFinalizer.FinalizeInstance<T>(const instance: T);
begin
  FinalizeValue(instance, TypeInfo(T));
end;

{$ENDREGION}


{$REGION 'Tuple<T1, T2>'}

constructor Tuple<T1, T2>.Create(const value1: T1; const value2: T2);
begin
  fValue1 := value1;
  fValue2 := value2;
end;

function Tuple<T1, T2>.Equals(const value: Tuple<T1, T2>): Boolean;
var
  comparer1: IEqualityComparer<T1>;
  comparer2: IEqualityComparer<T2>;
begin
  comparer1 := TEqualityComparer<T1>.Default;
  comparer2 := TEqualityComparer<T2>.Default;
  Result := comparer1.Equals(fValue1, value.Value1)
    and comparer2.Equals(fValue2, value.Value2);
end;

class operator Tuple<T1, T2>.Equal(const left, right: Tuple<T1, T2>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Tuple<T1, T2>.Implicit(
  const value: Tuple<T1, T2>): TArray<TValue>;
begin
  SetLength(Result, 2);
  Result[0] := TValue.From<T1>(value.Value1);
  Result[1] := TValue.From<T2>(value.Value2);
end;

class operator Tuple<T1, T2>.Implicit(
  const value: TArray<TValue>): Tuple<T1, T2>;
begin
  Result.fValue1 := value[0].AsType<T1>;
  Result.fValue2 := value[1].AsType<T2>;
end;

class operator Tuple<T1, T2>.Implicit(
  const value: array of const): Tuple<T1, T2>;
begin
  Result.fValue1 := TValue.FromVarRec(value[0]).AsType<T1>;
  Result.fValue2 := TValue.FromVarRec(value[1]).AsType<T2>;
end;

class operator Tuple<T1, T2>.NotEqual(const left,
  right: Tuple<T1, T2>): Boolean;
begin
  Result := not left.Equals(right);
end;

procedure Tuple<T1, T2>.Unpack(out value1: T1; out value2: T2);
begin
  value1 := fValue1;
  value2 := fValue2;
end;

{$ENDREGION}


{$REGION 'Tuple<T1, T2, T3>'}

constructor Tuple<T1, T2, T3>.Create(const value1: T1; const value2: T2;
  const value3: T3);
begin
  fValue1 := value1;
  fValue2 := value2;
  fValue3 := value3;
end;

function Tuple<T1, T2, T3>.Equals(const value: Tuple<T1, T2, T3>): Boolean;
var
  comparer1: IEqualityComparer<T1>;
  comparer2: IEqualityComparer<T2>;
  comparer3: IEqualityComparer<T3>;
begin
  comparer1 := TEqualityComparer<T1>.Default;
  comparer2 := TEqualityComparer<T2>.Default;
  comparer3 := TEqualityComparer<T3>.Default;
  Result := comparer1.Equals(fValue1, value.Value1)
    and comparer2.Equals(fValue2, value.Value2)
    and comparer3.Equals(fValue3, value.Value3);
end;

class operator Tuple<T1, T2, T3>.Equal(const left,
  right: Tuple<T1, T2, T3>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const value: Tuple<T1, T2, T3>): TArray<TValue>;
begin
  SetLength(Result, 3);
  Result[0] := TValue.From<T1>(value.Value1);
  Result[1] := TValue.From<T2>(value.Value2);
  Result[2] := TValue.From<T3>(value.Value3);
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const value: Tuple<T1, T2, T3>): Tuple<T1, T2>;
begin
  Result.fValue1 := value.Value1;
  Result.fValue2 := value.Value2;
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const value: TArray<TValue>): Tuple<T1, T2, T3>;
begin
  Result.fValue1 := value[0].AsType<T1>;
  Result.fValue2 := value[1].AsType<T2>;
  Result.fValue3 := value[2].AsType<T3>;
end;

class operator Tuple<T1, T2, T3>.Implicit(
  const value: array of const): Tuple<T1, T2, T3>;
begin
  Result.fValue1 := TValue.FromVarRec(value[0]).AsType<T1>;
  Result.fValue2 := TValue.FromVarRec(value[1]).AsType<T2>;
  Result.fValue3 := TValue.FromVarRec(value[2]).AsType<T3>;
end;

class operator Tuple<T1, T2, T3>.NotEqual(const left,
  right: Tuple<T1, T2, T3>): Boolean;
begin
  Result := not left.Equals(right);
end;

procedure Tuple<T1, T2, T3>.Unpack(out value1: T1; out value2: T2);
begin
  value1 := fValue1;
  value2 := fValue2;
end;

procedure Tuple<T1, T2, T3>.Unpack(out value1: T1; out value2: T2;
  out value3: T3);
begin
  value1 := fValue1;
  value2 := fValue2;
  value3 := fValue3;
end;

{$ENDREGION}


{$REGION 'Tuple<T1, T2, T3, T4>'}

constructor Tuple<T1, T2, T3, T4>.Create(const value1: T1; const value2: T2;
  const value3: T3; const value4: T4);
begin
  fValue1 := value1;
  fValue2 := value2;
  fValue3 := value3;
  fValue4 := value4;
end;

function Tuple<T1, T2, T3, T4>.Equals(
  const value: Tuple<T1, T2, T3, T4>): Boolean;
var
  comparer1: IEqualityComparer<T1>;
  comparer2: IEqualityComparer<T2>;
  comparer3: IEqualityComparer<T3>;
  comparer4: IEqualityComparer<T4>;
begin
  comparer1 := TEqualityComparer<T1>.Default;
  comparer2 := TEqualityComparer<T2>.Default;
  comparer3 := TEqualityComparer<T3>.Default;
  comparer4 := TEqualityComparer<T4>.Default;
  Result := comparer1.Equals(fValue1, value.Value1)
    and comparer2.Equals(fValue2, value.Value2)
    and comparer3.Equals(fValue3, value.Value3)
    and comparer4.Equals(fValue4, value.Value4);
end;

class operator Tuple<T1, T2, T3, T4>.Equal(const left,
  right: Tuple<T1, T2, T3, T4>): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const value: Tuple<T1, T2, T3, T4>): TArray<TValue>;
begin
  SetLength(Result, 4);
  Result[0] := TValue.From<T1>(value.Value1);
  Result[1] := TValue.From<T2>(value.Value2);
  Result[2] := TValue.From<T3>(value.Value3);
  Result[3] := TValue.From<T4>(value.Value4);
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const value: Tuple<T1, T2, T3, T4>): Tuple<T1, T2>;
begin
  Result.fValue1 := value.Value1;
  Result.fValue2 := value.Value2;
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const value: Tuple<T1, T2, T3, T4>): Tuple<T1, T2, T3>;
begin
  Result.fValue1 := value.Value1;
  Result.fValue2 := value.Value2;
  Result.fValue3 := value.Value3;
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const value: TArray<TValue>): Tuple<T1, T2, T3, T4>;
begin
  Result.fValue1 := value[0].AsType<T1>;
  Result.fValue2 := value[1].AsType<T2>;
  Result.fValue3 := value[2].AsType<T3>;
  Result.fValue4 := value[3].AsType<T4>;
end;

class operator Tuple<T1, T2, T3, T4>.Implicit(
  const value: array of const): Tuple<T1, T2, T3, T4>;
begin
  Result.fValue1 := TValue.FromVarRec(value[0]).AsType<T1>;
  Result.fValue2 := TValue.FromVarRec(value[1]).AsType<T2>;
  Result.fValue3 := TValue.FromVarRec(value[2]).AsType<T3>;
  Result.fValue4 := TValue.FromVarRec(value[3]).AsType<T4>;
end;

class operator Tuple<T1, T2, T3, T4>.NotEqual(const left,
  right: Tuple<T1, T2, T3, T4>): Boolean;
begin
  Result := not left.Equals(right);
end;

procedure Tuple<T1, T2, T3, T4>.Unpack(out value1: T1; out value2: T2);
begin
  value1 := fValue1;
  value2 := fValue2;
end;

procedure Tuple<T1, T2, T3, T4>.Unpack(out value1: T1; out value2: T2;
  out value3: T3);
begin
  value1 := fValue1;
  value2 := fValue2;
  value3 := fValue3;
end;

procedure Tuple<T1, T2, T3, T4>.Unpack(out value1: T1; out value2: T2;
  out value3: T3; out value4: T4);
begin
  value1 := fValue1;
  value2 := fValue2;
  value3 := fValue3;
  value4 := fValue4;
end;

{$ENDREGION}


{$REGION 'Tuple'}

class function Tuple.Pack<T1, T2>(const value1: T1;
  const value2: T2): Tuple<T1, T2>;
begin
  Result := Tuple<T1, T2>.Create(value1, value2);
end;

class function Tuple.Pack<T1, T2, T3>(const value1: T1; const value2: T2;
  const value3: T3): Tuple<T1, T2, T3>;
begin
  Result := Tuple<T1, T2, T3>.Create(value1, value2, value3);
end;

class function Tuple.Pack<T1, T2, T3, T4>(const value1: T1; const value2: T2;
  const value3: T3; const value4: T4): Tuple<T1, T2, T3, T4>;
begin
  Result := Tuple<T1, T2, T3, T4>.Create(value1, value2, value3, value4);
end;

{$ENDREGION}


{$REGION 'TArray'}

class function TArray.Contains<T>(const values: array of T;
  const item: T): Boolean;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := TEqualityComparer<T>.Default;
  for i := Low(Values) to High(Values) do
    if comparer.Equals(values[i], item) then
      Exit(True);
  Result := False;
end;

class function TArray.Copy<T>(const values: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Length(values));
  for i := Low(values) to High(values) do
    Result[i] := values[i];
end;

class function TArray.IndexOf<T>(const values: array of T;
  const item: T): Integer;
begin
  Result := IndexOf<T>(values, item, 0, Length(values));
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T;
  index: Integer): Integer;
begin
  Result := IndexOf<T>(values, item, index, Length(values) - index);
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T; index,
  count: Integer): Integer;
begin
  Result := IndexOf<T>(values, item, index, count, TEqualityComparer<T>.Default);
end;

class function TArray.IndexOf<T>(const values: array of T; const item: T; index,
  count: Integer; const comparer: IEqualityComparer<T>): Integer;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Length(values)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(values) - index), 'count');
{$ENDIF}

  for i := index to index + count - 1 do
    if comparer.Equals(values[i], item) then
      Exit(i);
  Result := -1;
end;

{$ENDREGION}


{$REGION 'DynamicArray<T>'}

class operator DynamicArray<T>.Add(const left, right: DynamicArray<T>): DynamicArray<T>;
begin
  Result := left;
  Result.Add(right.fItems);
end;

class operator DynamicArray<T>.Add(const left: DynamicArray<T>;
  const right: TArray<T>): DynamicArray<T>;
begin
  Result := left;
  Result.Add(right);
end;

class operator DynamicArray<T>.Add(const left: TArray<T>;
  const right: DynamicArray<T>): DynamicArray<T>;
begin
  Result := left;
  Result.Add(right.fItems);
end;

class operator DynamicArray<T>.Add(const left: DynamicArray<T>;
  const right: T): DynamicArray<T>;
begin
  Result := left;
  Result.Add(right);
end;

class operator DynamicArray<T>.Add(const left: T;
  const right: DynamicArray<T>): DynamicArray<T>;
begin
  SetLength(Result.fItems, 1);
  Result.fItems[0] := left;
  Result.Add(right);
end;

function DynamicArray<T>.Add(const item: T): Integer;
begin
  Result := System.Length(fItems);
  SetLength(fItems, Result + 1);
  fItems[Result] := item;
end;

procedure DynamicArray<T>.Add(const items: array of T);
begin
  InternalInsert(System.Length(fItems), items);
end;

procedure DynamicArray<T>.Add(const items: TArray<T>);
begin
{$IFNDEF DELPHIXE7_UP}
  InternalInsert(System.Length(fItems), items);
{$ELSE}
  System.Insert(items, fItems, System.Length(fItems));
{$ENDIF}
end;

procedure DynamicArray<T>.Add(const items: DynamicArray<T>);
begin
{$IFNDEF DELPHIXE7_UP}
  InternalInsert(System.Length(items.fItems), items.fItems);
{$ELSE}
  System.Insert(items.fItems, fItems, System.Length(items.fItems));
{$ENDIF}
end;

procedure DynamicArray<T>.Assign(const items: array of T);
begin
  fItems := TArray.Copy<T>(items);
end;

procedure DynamicArray<T>.Clear;
begin
  fItems := nil;
end;

function DynamicArray<T>.Contains(const item: T): Boolean;
begin
  Result := IndexOf(item) > -1;
end;

function DynamicArray<T>.Contains(const items: array of T): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(items) do
    if IndexOf(items[i]) = -1 then
      Exit(False);
  Result := True;
end;

function DynamicArray<T>.Contains(const items: TArray<T>): Boolean;
var
  i: Integer;
begin
  for i := 0 to System.Length(items) - 1 do
    if IndexOf(items[i]) = -1 then
      Exit(False);
  Result := True;
end;

procedure DynamicArray<T>.Delete(index: Integer);
{$IFNDEF DELPHIXE7_UP}
var
  n, i: Integer;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  n := System.Length(fItems);
  if (index < 0) or (index >= n) then
    Exit;
  Dec(n);
  fItems[index] := Default(T);
  if index <> n then
{$IFDEF WEAKREF}
    if {$IFDEF DELPHIXE7_UP}System.HasWeakRef(T){$ELSE}HasWeakRef(TypeInfo(T)){$ENDIF} then
    begin
      for i := index to n - 1 do
        fItems[i] := fItems[i + 1];
    end
    else
{$ENDIF}
    begin
      System.Move(fItems[index + 1], fItems[index], (n - index) * SizeOf(T));
      System.FillChar(fItems[n], SizeOf(T), 0);
    end;
  SetLength(fItems, n);
{$ELSE}
  System.Delete(fItems, index, 1);
{$ENDIF}
end;

procedure DynamicArray<T>.Delete(index, count: Integer);
{$IFNDEF DELPHIXE7_UP}
var
  n, i: Integer;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  n := System.Length(fItems);
  if (index < 0) or (index >= n) then
    Exit;
  if count > n - index then
    count := n - index;
  Dec(n, count);
  for i := index to index + count - 1 do
    fItems[i] := Default(T);
  if index <> n then
{$IFDEF WEAKREF}
    if {$IFDEF DELPHIXE7_UP}System.HasWeakRef(T){$ELSE}HasWeakRef(TypeInfo(T)){$ENDIF} then
    begin
      for i := index to n - count do
        fItems[i] := fItems[i + count];
    end
    else
{$ENDIF}
    begin
      System.Move(fItems[index + count], fItems[index], (n - index) * SizeOf(T));
      System.FillChar(fItems[n], count * SizeOf(T), 0);
    end;
  SetLength(fItems, n);
{$ELSE}
  System.Delete(fItems, index, count);
{$ENDIF}
end;

class operator DynamicArray<T>.Equal(const left, right: DynamicArray<T>): Boolean;
begin
  Result := left.Equals(right.fItems);
end;

function DynamicArray<T>.Equals(const items: array of T): Boolean;
var
  n, i: Integer;
begin
  n := System.Length(fItems);
  if n <> System.Length(items) then
    Exit(False);
  Result := True;
  case {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} of
    tkInteger:
      for i := 0 to n - 1 do
        if PInteger(@fItems[i])^ <> PInteger(@items[i])^ then
          Exit(False);
    tkUString:
      for i := 0 to n - 1 do
        if PUnicodeString(@fItems[i])^ <> PUnicodeString(@items[i])^ then
          Exit(False);
  else
    Result := InternalEquals(items);
  end;
end;

function DynamicArray<T>.Equals(const items: TArray<T>): Boolean;
var
  n, i: Integer;
begin
  n := System.Length(fItems);
  if n <> System.Length(items) then
    Exit(False);
  Result := True;
  case {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} of
    tkInteger:
      for i := 0 to n - 1 do
        if PInteger(@fItems[i])^ <> PInteger(@items[i])^ then
          Exit(False);
    tkUString:
      for i := 0 to n - 1 do
        if PUnicodeString(@fItems[i])^ <> PUnicodeString(@items[i])^ then
          Exit(False);
  else
    Result := InternalEquals(items);
  end;
end;

function DynamicArray<T>.GetCount: Integer;
begin
  Result := System.Length(fItems);
end;

function DynamicArray<T>.GetEnumerator: TArrayEnumerator<T>;
begin
{$IFDEF DELPHI2010}
  Result := TArrayEnumerator<T>.Create(fItems);
{$ELSE}
  Result.fItems := fItems;
  Result.fIndex := -1;
{$ENDIF}
end;

function DynamicArray<T>.GetItem(index: Integer): T;
begin
  Result := fItems[index];
end;

class operator DynamicArray<T>.Implicit(const value: TArray<T>): DynamicArray<T>;
begin
  Result.fItems := value;
end;

class operator DynamicArray<T>.Implicit(const value: DynamicArray<T>): TArray<T>;
begin
  Result := value.fItems;
end;

class operator DynamicArray<T>.In(const left: T;
  const right: DynamicArray<T>): Boolean;
begin
  Result := right.Contains(left);
end;

class operator DynamicArray<T>.In(const left, right: DynamicArray<T>): Boolean;
begin
  Result := right.Contains(left.fItems);
end;

class operator DynamicArray<T>.In(const left: TArray<T>;
  const right: DynamicArray<T>): Boolean;
begin
  Result := right.Contains(left);
end;

function DynamicArray<T>.IndexOf(const item: T): Integer;
begin
  case {$IFDEF DELPHIXE7_UP}System.GetTypeKind(T){$ELSE}GetTypeKind(TypeInfo(T)){$ENDIF} of
    tkInteger: Result := InternalIndexOfInt(PInteger(@item)^);
    tkUString: Result := InternalIndexOfStr(PUnicodeString(@item)^);
  else
    Result := InternalIndexOf(item);
  end;
end;

procedure DynamicArray<T>.Insert(index: Integer; const item: T);
{$IFNDEF DELPHIXE7_UP}
var
  count: Integer;
  i: Integer;
{$ENDIF}
begin
{$IFNDEF DELPHIXE7_UP}
  count := System.Length(fItems);
  SetLength(fItems, count + 1);
  if index <> count then
{$IFDEF WEAKREF}
    if {$IFDEF DELPHIXE7_UP}System.HasWeakRef(T){$ELSE}HasWeakRef(TypeInfo(T)){$ENDIF} then
    begin
      for i := count - 1 downto index do
        fItems[i + 1] := fItems[i];
    end
    else
{$ENDIF}
    begin
      System.Move(fItems[index], fItems[index + 1], (count - index) * SizeOf(T));
      System.FillChar(fItems[index], SizeOf(T), 0);
    end;
  fItems[index] := item;
{$ELSE}
  System.Insert(item, fItems, index);
{$ENDIF}
end;

procedure DynamicArray<T>.Insert(index: Integer; const items: array of T);
begin
  InternalInsert(index, items);
end;

procedure DynamicArray<T>.Insert(index: Integer; const items: TArray<T>);
begin
{$IFNDEF DELPHIXE7_UP}
  InternalInsert(index, items);
{$ELSE}
  System.Insert(items, fItems, index);
{$ENDIF}
end;

function DynamicArray<T>.InternalEquals(const items: array of T): Boolean;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := TEqualityComparer<T>.Default;
  for i := 0 to System.Length(fItems) - 1 do
    if not comparer.Equals(fItems[i], items[i]) then
      Exit(False);
  Result := True;
end;

function DynamicArray<T>.InternalIndexOf(const item: T): Integer;
var
  comparer: IEqualityComparer<T>;
begin
  comparer := TEqualityComparer<T>.Default;
  for Result := 0 to High(fItems) do
    if comparer.Equals(fItems[Result], item) then
      Exit;
  Result := -1;
end;

function DynamicArray<T>.InternalIndexOfInt(const item: Integer): Integer;
begin
  for Result := 0 to High(fItems) do
    if PInteger(@fItems[Result])^ = item then
      Exit;
  Result := -1;
end;

function DynamicArray<T>.InternalIndexOfStr(const item: string): Integer;
begin
  for Result := 0 to High(fItems) do
    if PUnicodeString(@fItems[Result])^ = item then
      Exit;
  Result := -1;
end;

procedure DynamicArray<T>.InternalInsert(index: Integer; const items: array of T);
var
  count, len, i: Integer;
begin
  count := System.Length(fItems);
  len := System.Length(items);
  SetLength(fItems, count + len);
  if index <> count then
{$IFDEF WEAKREF}
    if {$IFDEF DELPHIXE7_UP}System.HasWeakRef(T){$ELSE}HasWeakRef(TypeInfo(T)){$ENDIF} then
    begin
      for i := count - 1 downto index do
        fItems[i + len] := fItems[i];
    end
    else
{$ENDIF}
    begin
      System.Move(fItems[index], fItems[index + len], (count - index) * SizeOf(T));
      if {$IFDEF DELPHIXE7_UP}System.IsManagedType(T){$ELSE}Rtti.IsManaged(TypeInfo(T)){$ENDIF} then
        System.FillChar(fItems[index], len * SizeOf(T), 0);
    end;
  if {$IFDEF DELPHIXE7_UP}System.IsManagedType(T){$ELSE}Rtti.IsManaged(TypeInfo(T)){$ENDIF} then
  begin
    for i := Low(items) to High(items) do
    begin
      fItems[index] := items[i];
      Inc(index);
    end;
  end
  else
    System.Move(items[0], fItems[index], len * SizeOf(T));
end;

class operator DynamicArray<T>.NotEqual(const left, right: DynamicArray<T>): Boolean;
begin
  Result := not left.Equals(right.fItems);
end;

procedure DynamicArray<T>.Remove(const item: T);
var
  index: Integer;
begin
  index := IndexOf(item);
  if index > -1 then
    Delete(index);
end;

procedure DynamicArray<T>.Remove(const items: array of T);
var
  item: T;
  index: Integer;
begin
  for item in items do
  begin
    index := IndexOf(item);
    if index > -1 then
      Delete(index);
  end;
end;

procedure DynamicArray<T>.Remove(const items: TArray<T>);
var
  item: T;
  index: Integer;
begin
  for item in items do
  begin
    index := IndexOf(item);
    if index > -1 then
      Delete(index);
  end;
end;

procedure DynamicArray<T>.Reverse;
var
  tmp: T;
  b, e: Integer;
begin
  b := 0;
  e := Count - 1;
  while b < e do
  begin
    tmp := fItems[b];
    fItems[b] := fItems[e];
    fItems[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure DynamicArray<T>.SetCount(value: Integer);
begin
  SetLength(fItems, value);
end;

procedure DynamicArray<T>.SetItem(index: Integer; const value: T);
begin
  fItems[index] := value;
end;

function DynamicArray<T>.Slice(index: Integer): DynamicArray<T>;
begin
  Result.fItems := Copy(fItems, index);
end;

function DynamicArray<T>.Slice(index, count: Integer): DynamicArray<T>;
begin
  Result.fItems := Copy(fItems, index, count);
end;

procedure DynamicArray<T>.Sort;
begin
  TArray.Sort<T>(fItems);
end;

procedure DynamicArray<T>.Sort(const comparer: IComparer<T>);
begin
  TArray.Sort<T>(fItems, comparer);
end;

procedure DynamicArray<T>.Sort(const comparer: TComparison<T>);
begin
  TArray.Sort<T>(fItems, IComparer<T>(PPointer(@comparer)^));
end;

function DynamicArray<T>.Splice(index, count: Integer): DynamicArray<T>;
begin
  Result := Splice(index, count, []);
end;

function DynamicArray<T>.Splice(index, count: Integer;
  const items: array of T): DynamicArray<T>;
var
  n, i: Integer;
begin
  n := System.Length(fItems);
  if (index < 0) or (index >= n) then
    Exit;
  if count > n - index then
    count := n - index;
  Result.fItems := Copy(fItems, index, count);
  Delete(index, count);
  Insert(index, items);
end;

class operator DynamicArray<T>.Subtract(const left,
  right: DynamicArray<T>): DynamicArray<T>;
begin
  Result := left;
  Result.Remove(right.fItems);
end;

class operator DynamicArray<T>.Subtract(const left: DynamicArray<T>;
  const right: T): DynamicArray<T>;
begin
  Result := left;
  Result.Remove(right);
end;

{$ENDREGION}


{$REGION 'TArrayEnumerator<T>' }

constructor TArrayEnumerator<T>.Create(const items: TArray<T>);
begin
  fItems := items;
  fIndex := -1;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := fItems[fIndex];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex < System.Length(fItems);
end;

{$ENDREGION}


end.
