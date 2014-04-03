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
///	  Provides many easy to use class helpers &amp; record helpers that extend
///	  some common classes in the VCL framework.
///	</summary>
///	<remarks>
///	  <para>
///	    Classes helpers and record helpers have been introduced since Delphi
///	    2007. The initial purpose is to allow developers to extend a class
///	    without change the original structure.
///	  </para>
///	  <note type="note">
///	    A class helper type may not declare instance data, but class fields are
///	    allowed.
///	  </note>
///	  <note type="warning">
///	    Class helpers and record helpers are not intended to be a design tool
///	    in Delphi. It is some kind of "patching" technology.
///	  </note>
///	  <para>
///	    If you want to use these helpers, just uses the <b>Spring.Helpers</b>nam
///	    espace in the target unit.
///	  </para>
///	</remarks>
///	<example>
///	  See examples in the <see cref="TGuidHelper" />.
///	</example>
unit Spring.Helpers;

{$I Spring.inc}

interface

uses
  Classes,
  Generics.Collections,
  Rtti,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Reflection;

type
  ///	<summary>
  ///	  Represents a record helper for the <c>System.TGuid</c> structure to
  ///	  make it easy to use.
  ///	</summary>
  ///	<remarks>
  ///	  <note type="tip">
  ///	    You can use the equal ("=") or not equal ("&lt;&gt;") operator
  ///	    loading in latest Delphi XE.
  ///	  </note>
  ///	</remarks>
  ///	<example>
  ///	  The following code demonstrates how to create a new guid and use it in
  ///	  OO-style:
  ///	  <code lang="Delphi">
  ///	procedure TestGuidHelper;
  ///	var
  ///	  guid: TGuid;
  ///	begin
  ///	  // generates a new guid.
  ///	  guid := TGuid.NewGuid;
  ///	  // this guid must not be empty.
  ///	  Assert(not guid.IsEmpty);
  ///	  // print the string representation
  ///	  Writeln(guid.ToString);
  ///	  // print the quoted string
  ///	  Writeln(guid.ToQuotedString);
  ///	  // This guid must equal to itself.
  ///	  Assert(guid.Equals(guid));
  ///	end;</code>
  ///	</example>
  TGuidHelper = record helper for TGuid
  private
    class function GetEmpty: TGuid; static;
    function GetIsEmpty: Boolean;
  public
    ///	<summary>
    ///	  Creates a guid structure from the specified guid string.
    ///	</summary>
    ///	<param name="guidString">
    ///	  the guid string.
    ///	</param>
    class function Create(const guidString: string): TGuid; overload; static;
    class function Create(const bytes: TBytes): TGuid; overload; static;
    class function Create(a: Integer; b: SmallInt; c: SmallInt; const d: TBytes): TGuid; overload; static;
    class function Create(a: Integer; b: SmallInt; c: SmallInt; d, e, f, g, h, i, j, k: Byte): TGuid; overload; static;
    class function Create(a: Cardinal; b: Word; c: Word; d, e, f, g, h, i, j, k: Byte): TGuid; overload; static;

    ///	<summary>
    ///	  Generates a new <c>TGuid</c> instance.
    ///	</summary>
    class function NewGuid: TGuid; static;

    function ToBytes: TBytes;

    function ToByteArray: TBytes;

    ///	<summary>
    ///	  Returns a string representation of the guid.
    ///	</summary>
    function ToString: string;

    ///	<summary>
    ///	  Determines whether the guid equals to another TGuid structure.
    ///	</summary>
    function Equals(const guid: TGuid): Boolean;

    ///	<summary>
    ///	  Returns the quoted string representation of the guid.
    ///	</summary>
    function ToQuotedString: string;

    ///	<summary>
    ///	  Gets a value which indicates whether the guid is empty (all zero).
    ///	</summary>
    property IsEmpty: Boolean read GetIsEmpty;

    ///	<summary>
    ///	  Gets the shared empty guid.
    ///	</summary>
    ///	<value>
    ///	  The value of the empty guid is
    ///	  <c>{00000000-0000-0000-0000-000000000000}</c>
    ///	</value>
    class property Empty: TGuid read GetEmpty;
  end;

  ///	<summary>
  ///	  Provides a static method to create a TMethod structure with an instance
  ///	  and a methodaddress.
  ///	</summary>
  TMethodHelper = record helper for TMethod
  public
    class function Create(const instance, methodAddress: Pointer): TMethod; static;
  end;

  TStreamHelper = class helper for TStream
  public
    ///	<summary>
    ///	  Reads a value of a value type, which could be an Integer, record,
    ///	  etc., from the stream.
    ///	</summary>
    ///	<remarks>
    ///	  <note type="tip">
    ///	    The generic argument could be omitted if the compiler can
    ///	    automatically inreference the type.
    ///	  </note>
    ///	</remarks>
    ///	<example>
    ///	  <para>
    ///	    The following example demonstrates how to use the generic
    ///	    <c>ReadBuffer&lt;T&gt;</c> and <c>WriteBuffer&lt;T&gt;</c>methods.
    ///	  </para>
    ///	  <code lang="Delphi">
    ///	procedure TestStreamHelper;
    ///	var
    ///	  stream: TStream;
    ///	  value: Integer;
    ///	begin
    ///	  stream := TMemoryStream.Create;
    ///	  try
    ///	    value := 2;
    ///	    stream.WriteBuffer(value);
    ///	    stream.Position := 0;
    ///	    stream.ReadBuffer&lt;Integer&gt;(value);
    ///	  finally
    ///	    stream.Free;
    ///	  end;
    ///	end;</code>
    ///	</example>
    procedure ReadBuffer<T: record>(var value: T); overload;

    ///	<summary>
    ///	  Writes a value of a value type to the stream.
    ///	</summary>
    procedure WriteBuffer<T: record>(const value: T); overload;
  end;

  TStringsHelper = class helper for TStrings
  private
    function GetIsEmpty: Boolean;
  public
    ///	<summary>
    ///	  Add an array of string to the list.
    ///	</summary>
    procedure AddStrings(const strings: array of string); overload;

    ///	<summary>
    ///	  Adds or updates a name-value pair.
    ///	</summary>
    ///	<remarks>
    ///	  <note type="warning">
    ///	    There is a <c>Values[name: string]</c>property in the TStrings
    ///	    class, but the entry will be removed if the value is empty.
    ///	  </note>
    ///	</remarks>
    procedure AddOrUpdate(const name, value: string);

//    procedure Remove(const s: string);

    ///	<summary>
    ///	  Executes a procedure during batch updating of the list.
    ///	</summary>
    ///	<exception cref="Spring|EArgumentNullException">
    ///	  Raised if the<paramref name="strings" /> is nil or the
    ///	  <paramref name="proc" /> is not assigned.
    ///	</exception>
    procedure ExecuteUpdate(proc: TProc);

    ///	<summary>
    ///	  Extract all name entries and add them to the
    ///	  <paramref name="strings" /> list.
    ///	</summary>
    ///	<exception cref="Spring|EArgumentNullException">
    ///	  Raised if the <paramref name="strings" /> is nil.
    ///	</exception>
    ///	<seealso cref="ExtractValues(TStrings)">
    ///	  ExtractValues
    ///	</seealso>
    procedure ExtractNames(strings: TStrings);

    ///	<summary>
    ///	  Extract all value entries and add them to the
    ///	  <paramref name="strings" /> list.
    ///	</summary>
    ///	<exception cref="Spring|EArgumentNullException">
    ///	  Raised if the <paramref name="strings" /> is nil.
    ///	</exception>
    ///	<seealso cref="ExtractNames(TStrings)" />
    procedure ExtractValues(strings: TStrings);

    ///	<summary>
    ///	  Returns a string array that contains all the <b>name</b>entries in
    ///	  the string list.
    ///	</summary>
    function GetNames: TStringDynArray;

    ///	<summary>
    ///	  Returns a string array that contains all the <b>value</b>entries in
    ///	  the string list.
    ///	</summary>
    function GetValues: TStringDynArray;

//    function GetValue(const name: string): string; overload;
//    function GetValue(const index: Integer): string; overload;

    ///	<summary>
    ///	  Gets the corresponding value of the name entry if there is such an
    ///	  entry and the value is not empty, otherwise, returns the default
    ///	  value specified by the <paramref name="default" />param.
    ///	</summary>
    function GetValueOrDefault<T>(const name: string; const default: T): T; experimental;

    ///	<summary>
    ///	  Try finding a name entry in the list.
    ///	</summary>
    function TryFindName(const name: string; var index: Integer): Boolean;

    ///	<summary>
    ///	  Try finding a value entry in the list.
    ///	</summary>
    function TryFindValue(const value: string; var index: Integer): Boolean;

    ///	<summary>
    ///	  Try finding an object in the list.
    ///	</summary>
    function TryFindObject(obj: TObject; var index: Integer): Boolean;

    ///	<summary>
    ///	  Determines whether the list contains the specified name entry.
    ///	</summary>
    function ContainsName(const name: string): Boolean;

    ///	<summary>
    ///	  Determines whether the list contains the specified value entry.
    ///	</summary>
    function ContainsValue(const value: string): Boolean;

    ///	<summary>
    ///	  Determines whether the list contains the specified object.
    ///	</summary>
    function ContainsObject(obj: TObject): Boolean;

    ///	<summary>
    ///	  Converts the string list to a dynamic string array.
    ///	</summary>
    function ToArray: TStringDynArray;

    ///	<summary>
    ///	  Gets a value indicates whether the strings is empty.
    ///	</summary>
    ///	<value>
    ///	  Returns true if the count of the list is zero, otherwise, returns
    ///	  false.
    ///	</value>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TCollectionHelper = class helper for TCollection
  public
    ///	<param name="proc">
    ///	  the anonymous method that will be executed within the batch update.
    ///	</param>
    procedure ExecuteUpdate(proc: TProc);
  end;

  TArrayHelper = class helper for TArray
  public
    class function CreateArray<T>(const values: array of T): TArray<T>;
  end;

  // TPointHelper, TSizeHelper, TRectHelper

  {TODO -oPaul -cGeneral : Add some non-generic implementation}

  TRttiObjectHelper = class helper for TRttiObject
  public
    ///	<summary>
    ///	  Gets an array which contains all custom attribute types which the
    ///	  type applies.
    ///	</summary>
    function GetCustomAttributes<T: TCustomAttribute>: TArray<T>;

    ///	<summary>
    ///	  Enumerates all applied custom attributes and returns the first one
    ///	  which is/inherits the specified type.
    ///	</summary>
    function GetCustomAttribute<T: TCustomAttribute>: T;

    ///	<summary>
    ///	  Try getting a custom attribute class which is applied by the type.
    ///	</summary>
    function TryGetCustomAttribute<T: TCustomAttribute>(out attribute: T): Boolean;

    ///	<summary>
    ///	  Determines whether the type applies the specified custom attribute
    ///	  class.
    ///	</summary>
    function HasCustomAttribute<T: TCustomAttribute>: Boolean;
  end;

  TRttiClassType = TRttiInstanceType;

  ///	<preliminary />
  TRttiTypeHelper =  class helper for TRttiType
  private
    function GetAsInterface: TRttiInterfaceType;
    function GetIsClass: Boolean;
    function GetIsInterface: Boolean;
    function GetIsClassOrInterface: Boolean;
    function GetAsClass: TRttiInstanceType;
    function GetIsGenericType: Boolean;
    function GetAsDynamicArray: TRttiDynamicArrayType;
    function GetIsDynamicArray: Boolean;
    function InternalGetConstructors(enumerateBaseType: Boolean = True): IEnumerable<TRttiMethod>;
    function InternalGetMethods(enumerateBaseType: Boolean = True): IEnumerable<TRttiMethod>;
    function InternalGetProperties(enumerateBaseType: Boolean = True): IEnumerable<TRttiProperty>;
    function InternalGetFields(enumerateBaseType: Boolean = True): IEnumerable<TRttiField>;
    function GetConstructors: IEnumerable<TRttiMethod>;
    function GetMethods: IEnumerable<TRttiMethod>;
    function GetProperties: IEnumerable<TRttiProperty>;
    function GetFields: IEnumerable<TRttiField>;
    function GetDefaultName: string;
  public
    // function GetMembers: IEnumerable<TRttiMember>;

    ///	<summary>
    ///	  Returns an enumerable collection which contains all the interface
    ///	  Rtti types that the target type implements.
    ///	  <note type="note">
    ///	    Only Guid interfaces will be enumerated.
    ///	  </note>
    ///	</summary>
    ///	<seealso cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;

    ///	<summary>
    ///	  Gets an array of types which contains all generic arguments.
    ///	</summary>
    ///	<remarks>
    ///	  This method extracts generic arguments from the name of the generic
    ///	  type. e.g. Invoking the method on the type
    ///	  <b><c>TDictionary&lt;Integer, string&gt;,</c></b> it will return an
    ///	  array?which contains two types: <c>System.Integer</c> and
    ///	  <c>System.String</c>.
    ///	</remarks>
    function GetGenericArguments: TArray<TRttiType>;

    ///	<summary>
    ///	  Gets an enumerable collection which contains all constructor methods
    ///	  of the type, including inherited.
    ///	</summary>
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Propoerties" />
    ///	<seealso cref="Fields" />
    property Constructors: IEnumerable<TRttiMethod> read GetConstructors;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all methods that the type
    ///	  contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Propoerties" />
    ///	<seealso cref="Fields" />
    property Methods: IEnumerable<TRttiMethod> read GetMethods;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all properties that the
    ///	  type contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Fields" />
    property Properties: IEnumerable<TRttiProperty> read GetProperties;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all fields that the type
    ///	  contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Propoerties" />
    property Fields: IEnumerable<TRttiField> read GetFields;

    property AsClass: TRttiInstanceType read GetAsClass;
    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property AsDynamicArray: TRttiDynamicArrayType read GetAsDynamicArray;
    property IsClass: Boolean read GetIsClass;
    property IsInterface: Boolean read GetIsInterface;
    property IsClassOrInterface: Boolean read GetIsClassOrInterface;
    property IsDynamicArray: Boolean read GetIsDynamicArray;

    ///	<summary>
    ///	  Gets a value indicates whether the current type is generic.
    ///	</summary>
    property IsGenericType: Boolean read GetIsGenericType;
    property DefaultName: string read GetDefaultName;
  end;

  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsProperty: Boolean;
    function GetIsMethod: Boolean;
    function GetIsField: Boolean;
    function GetAsMethod: TRttiMethod;
    function GetAsProperty: TRttiProperty;
    function GetAsField: TRttiField;
  public
//    procedure InvokeMember(instance: TValue; const arguments: array of TValue);
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
    property AsMethod: TRttiMethod read GetAsMethod;
    property AsProperty: TRttiProperty read GetAsProperty;
    property AsField: TRttiField read GetAsField;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsProperty: Boolean read GetIsProperty;
    property IsMethod: Boolean read GetIsMethod;
    property IsField: Boolean read GetIsField;
    property IsPrivate: Boolean read GetIsPrivate;
    property IsProtected: Boolean read GetIsProtected;
    property IsPublic: Boolean read GetIsPublic;
    property IsPublished: Boolean read GetIsPublished;
  end;

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  TRttiFieldHelper = class helper for TRttiField
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  TRttiInterfaceTypeHelper = class helper for TRttiInterfaceType
  private
    function GetHasGuid: Boolean;
  public
    ///	<summary>
    ///	  Gets a value indicates whether this interface type has a guid.
    ///	</summary>
    property HasGuid: Boolean read GetHasGuid;
  end;

implementation

uses
  StrUtils,
  TypInfo,
  Spring.ResourceStrings;


{$REGION 'TGuidHelper'}

class function TGuidHelper.Create(const guidString: string): TGuid;
begin
  Result := StringToGUID(guidString);
end;

class function TGuidHelper.Create(const bytes: TBytes): TGuid;
begin
  if Length(bytes) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  Move(bytes[0], Result, SizeOf(Result));
end;

class function TGuidHelper.Create(a: Integer; b, c: SmallInt;
  const d: TBytes): TGuid;
begin
  if Length(d) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [8]);
  Result.D1 := LongWord(a);
  Result.D2 := Word(b);
  Result.D3 := Word(c);
  Move(d[0], Result.D4, SizeOf(Result.D4));
end;

class function TGuidHelper.Create(a: Cardinal; b, c: Word; d, e, f, g, h, i, j,
  k: Byte): TGuid;
begin
  Result.D1 := LongWord(a);
  Result.D2 := Word(b);
  Result.D3 := Word(c);
  Result.D4[0] := d;
  Result.D4[1] := e;
  Result.D4[2] := f;
  Result.D4[3] := g;
  Result.D4[4] := h;
  Result.D4[5] := i;
  Result.D4[6] := j;
  Result.D4[7] := k;
end;

class function TGuidHelper.Create(a: Integer; b, c: SmallInt; d, e, f, g, h, i,
  j, k: Byte): TGuid;
begin
  Result.D1 := LongWord(a);
  Result.D2 := Word(b);
  Result.D3 := Word(c);
  Result.D4[0] := d;
  Result.D4[1] := e;
  Result.D4[2] := f;
  Result.D4[3] := g;
  Result.D4[4] := h;
  Result.D4[5] := i;
  Result.D4[6] := j;
  Result.D4[7] := k;
end;

class function TGuidHelper.NewGuid: TGuid;
begin
  if CreateGUID(Result) <> S_OK then
    RaiseLastOSError;
end;

function TGuidHelper.Equals(const guid: TGuid): Boolean;
begin
  Result := SysUtils.IsEqualGUID(Self, guid);
end;

function TGuidHelper.GetIsEmpty: Boolean;
begin
  {$WARNINGS OFF}
  Result := Self.Equals(TGuid.Empty);
  {$WARNINGS ON}
end;

function TGuidHelper.ToString: string;
begin
  Result := SysUtils.GUIDToString(Self);
end;

function TGuidHelper.ToByteArray: TBytes;
begin
  SetLength(Result, 16);
  Move(D1, Result[0], SizeOf(Self));
end;

function TGuidHelper.ToBytes: TBytes;
begin
  SetLength(Result, 16);
  Move(D1, Result[0], SizeOf(Self));
end;

function TGuidHelper.ToQuotedString: string;
begin
  Result := QuotedStr(Self.ToString);
end;

class function TGuidHelper.GetEmpty: TGuid;
begin
  FillChar(Result, Sizeof(Result), 0);
end;

//class operator TGuidHelper.Equal(const left, right: TGuid) : Boolean;
//begin
//  Result := left.Equals(right);
//end;

//class operator TGuidHelper.NotEqual(const left, right: TGuid) : Boolean;
//begin
//  Result := not left.Equals(right);
//end;

{$ENDREGION}


{$REGION 'TMethodHelper'}

class function TMethodHelper.Create(const instance,
  methodAddress: Pointer): TMethod;
begin
  Result.Code := methodAddress;
  Result.Data := instance;
end;

{$ENDREGION}


{$REGION 'Classes Helpers'}

{ TStreamHelper }

procedure TStreamHelper.ReadBuffer<T>(var value: T);
begin
  ReadBuffer(value, SizeOf(T));
end;

procedure TStreamHelper.WriteBuffer<T>(const value: T);
begin
  WriteBuffer(value, SizeOf(T));
end;

{ TStringsHelper }

procedure TStringsHelper.AddStrings(const strings: array of string);
var
  s: string;
begin
  BeginUpdate;
  try
    for s in strings do
    begin
      Add(s);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.AddOrUpdate(const name, value: string);
var
  index: Integer;
begin
  index := IndexOfName(name);
  if index <> -1 then
  begin
    Strings[index] := name + NameValueSeparator + value;
  end
  else
  begin
    Add(name + NameValueSeparator + value);
  end;
end;

procedure TStringsHelper.ExecuteUpdate(proc: TProc);
begin
  Guard.CheckNotNull(Assigned(proc), 'proc');

  BeginUpdate;
  try
    Clear;
    proc();
  finally
    EndUpdate;
  end;
end;

procedure TStringsHelper.ExtractNames(strings: TStrings);
var
  i: Integer;
begin
  Guard.CheckNotNull(strings, 'strings');

  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
    begin
      strings.Add(Self.Names[i]);
    end;
  finally
    strings.EndUpdate;
  end;
end;

procedure TStringsHelper.ExtractValues(strings: TStrings);
var
  i: Integer;
begin
  Guard.CheckNotNull(strings, 'strings');

  strings.BeginUpdate;
  try
    for i := 0 to Count - 1 do
    begin
      strings.Add(Self.ValueFromIndex[i]);
    end;
  finally
    strings.EndUpdate;
  end;
end;

function TStringsHelper.TryFindName(const name: string;
  var index: Integer): Boolean;
begin
  index := IndexOfName(name);
  Result := index > -1;
end;

function TStringsHelper.TryFindValue(const value: string;
  var index: Integer): Boolean;
var
  v: string;
  i: Integer;
begin
  index := -1;
  Result := False;
  for i := 0 to Count - 1 do
  begin
    v := ValueFromIndex[i];
    if SameText(v, value) then
    begin
      index := i;
      Exit(True);
    end;
  end;
end;

function TStringsHelper.TryFindObject(obj: TObject;
  var index: Integer): Boolean;
begin
  index := IndexOfObject(obj);
  Result := index > -1;
end;

function TStringsHelper.ContainsName(const name: string): Boolean;
begin
  Result := IndexOfName(name) > -1;
end;

function TStringsHelper.ContainsValue(const value: string): Boolean;
var
  index: Integer;
begin
  Result := TryFindValue(value, index);
end;

function TStringsHelper.ContainsObject(obj: TObject): Boolean;
begin
  Result := IndexOfObject(obj) > -1;
end;

function TStringsHelper.GetValueOrDefault<T>(const name: string;
  const default: T): T;
var
  index: Integer;
  value: string;
begin
  index := IndexOfName(name);
  if index > -1 then
  begin
    value := ValueFromIndex[index];
  end;
  if value <> '' then
  begin
    Result := TValue.From<string>(value).AsType<T>;  // TODO: Fix this ASAP because TValue.AsType<T> sucks...
  end
  else
  begin
    Result := default;
  end;
end;

function TStringsHelper.GetNames: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i] := Names[i];
  end;
end;

function TStringsHelper.GetValues: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i] := ValueFromIndex[i];
  end;
end;

function TStringsHelper.ToArray: TStringDynArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i] := Strings[i];
  end;
end;

function TStringsHelper.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

{ TCollectionHelper }

procedure TCollectionHelper.ExecuteUpdate(proc: TProc);
begin
  Guard.CheckNotNull(Assigned(proc), 'proc');

  BeginUpdate;
  try
    Clear;
    proc();
  finally
    EndUpdate;
  end;
end;

{$ENDREGION}


{$REGION 'TArray Helper'}

class function TArrayHelper.CreateArray<T>(const values: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Length(values));
  for i := 0 to High(values) do
  begin
    Result[i] := values[i];
  end;
end;

{$ENDREGION}


{$REGION 'RTTI Class Helpers'}

{ TRttiObjectHelper }

function TRttiObjectHelper.TryGetCustomAttribute<T>(out attribute: T): Boolean;
begin
  attribute := GetCustomAttribute<T>;
  Result := attribute <> nil;
end;

function TRttiObjectHelper.GetCustomAttribute<T>: T;
var
  attribute: TCustomAttribute;
begin
  Result := Default(T);
  for attribute in GetAttributes do
  begin
    if attribute.InheritsFrom(T) then
    begin
      Result := T(attribute);
      Break;
    end;
  end;
end;

function TRttiObjectHelper.GetCustomAttributes<T>: TArray<T>;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
  begin
    if attribute.InheritsFrom(T) then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := T(attribute);
    end;
  end;
end;

function TRttiObjectHelper.HasCustomAttribute<T>: Boolean;
var
  attribute: T;
begin
  Result := TryGetCustomAttribute<T>(attribute);
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.InternalGetConstructors(
  enumerateBaseType: Boolean): IEnumerable<TRttiMethod>;
var
  func: TGetRttiMembersFunc<TRttiMethod>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end;
  Result := TRttiMemberEnumerable<TRttiMethod>.Create(
    Self,
    func,
    enumerateBaseType,
    TMethodFilters.IsConstructor()
  );
end;

function TRttiTypeHelper.InternalGetMethods(
  enumerateBaseType: Boolean): IEnumerable<TRttiMethod>;
var
  func: TGetRttiMembersFunc<TRttiMethod>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end;
  Result := TRttiMemberEnumerable<TRttiMethod>.Create(
    Self,
    func,
    enumerateBaseType,
    nil
  );
end;

function TRttiTypeHelper.InternalGetProperties(
  enumerateBaseType: Boolean): IEnumerable<TRttiProperty>;
var
  func: TGetRttiMembersFunc<TRttiProperty>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiProperty>
    begin
      Result := targetType.GetDeclaredProperties;
    end;
  Result := TRttiMemberEnumerable<TRttiProperty>.Create(
    Self,
    func,
    enumerateBaseType,
    nil
  );
end;

function TRttiTypeHelper.InternalGetFields(
  enumerateBaseType: Boolean): IEnumerable<TRttiField>;
var
  func: TGetRttiMembersFunc<TRttiField>;
begin
  func :=
    function(targetType: TRttiType): TArray<TRttiField>
    begin
      Result := targetType.GetDeclaredFields;
    end;
  Result := TRttiMemberEnumerable<TRttiField>.Create(
    Self,
    func,
    enumerateBaseType,
    nil
  );
end;

function TRttiTypeHelper.GetConstructors: IEnumerable<TRttiMethod>;
begin
  Result := InternalGetConstructors;
end;

function TRttiTypeHelper.GetDefaultName: string;
begin
  if IsPublicType then
    Result := QualifiedName
  else
    Result := Name;
end;

function TRttiTypeHelper.GetMethods: IEnumerable<TRttiMethod>;
begin
  Result := InternalGetMethods;
end;

function TRttiTypeHelper.GetProperties: IEnumerable<TRttiProperty>;
begin
  Result := InternalGetProperties;
end;

function TRttiTypeHelper.GetFields: IEnumerable<TRttiField>;
begin
  Result := InternalGetFields;
end;

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

// Nullable<TDateTime>
// TDictionary<string, TObject>
// TDictionary<string, IDictionary<string, TObject>>
function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>; // TEMP
var
  p1, p2: Integer;
  args: string;
  elements: TStringDynArray;
  i: Integer;
begin
  p1 := Pos('<', Name);
  p2 := Pos('>', Name);
  if (p1 = 0) or (p2 = 0) or (p1 > p2) then
  begin
    Exit(nil);
  end;
  args := MidStr(Name, p1+1, p2-p1-1);
  elements := SplitString(args, ',');
  SetLength(Result, Length(elements));
  for i := 0 to High(elements) do
  begin
    Result[i] := TType.FindType(elements[i]);
  end;
end;

function TRttiTypeHelper.GetAsClass: TRttiInstanceType;
begin
  Result := Self as TRttiInstanceType;
end;

function TRttiTypeHelper.GetAsDynamicArray: TRttiDynamicArrayType;
begin
  Result := Self as TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetInterfaces: IEnumerable<TRttiInterfaceType>;
var
  list: IDictionary<TGUID, TRttiInterfaceType>;
  classType: TClass;
  table: PInterfaceTable;
  entry: TInterfaceEntry;
  aType: TRttiInterfaceType;
  i: Integer;
begin
  if Self.IsClass then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    classType := Self.AsInstance.MetaclassType;
    while Assigned(classType) do
    begin
      table := classType.GetInterfaceTable;
      if Assigned(table) then
      begin
        for i := 0 to table.EntryCount - 1 do
        begin
          entry := table.Entries[i];
          if not list.ContainsKey(entry.IID) and
        {$WARNINGS OFF}
            not entry.IID.IsEmpty and
        {$WARNINGS ON}
            TType.TryGetInterfaceType(entry.IID, aType) then
          begin
            list[entry.IID] := aType;
          end;
        end;
      end;
      classType := classType.ClassParent;
    end;
    Result := list.Values;
  end
  else
  if Self.IsInterface then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    aType := Self.AsInterface;
    while Assigned(aType) do
    begin
      if aType.HasGuid and not list.ContainsKey(aType.GUID)
        and not IsEqualGUID(aType.GUID, TGuid.Empty) then
      begin
        list[aType.GUID] := aType;
      end;
      aType := aType.BaseType;
    end;
    Result := list.Values;
  end;
end;

function TRttiTypeHelper.GetIsClass: Boolean;
begin
  Result := Self is TRttiInstanceType;
end;

function TRttiTypeHelper.GetIsClassOrInterface: Boolean;
begin
  Result := Self.IsClass or Self.IsInterface;
end;

function TRttiTypeHelper.GetIsDynamicArray: Boolean;
begin
  Result := Self is TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetIsGenericType: Boolean;
begin
  Result := (Pos('<', Name) > 0) and (Pos('>', Name) > 0);
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

{ TRttiInterfaceTypeHelper }

function TRttiInterfaceTypeHelper.GetHasGuid: Boolean;
begin
  Result := ifHasGuid in Self.IntfFlags;
end;

{ TRttiMemberHelper }

function TRttiMemberHelper.GetValue(const instance: TValue): TValue;
begin
  if IsProperty then
  begin
    Result := AsProperty.GetValue(instance);
  end
  else if IsField then
  begin
    Result := AsField.GetValue(instance);
  end
  else
  begin
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_GetValue);
  end;
end;

procedure TRttiMemberHelper.SetValue(const instance, value: TValue);
begin
  if IsProperty then
  begin
    AsProperty.SetValue(instance, value);
  end
  else if IsField then
  begin
    AsField.SetValue(instance, value);
  end
  else
  begin
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_SetValue);
  end;
end;

function TRttiMemberHelper.GetIsPrivate: Boolean;
begin
  Result := Visibility = mvPrivate;
end;

function TRttiMemberHelper.GetIsProtected: Boolean;
begin
  Result := Visibility = mvProtected;
end;

function TRttiMemberHelper.GetIsPublic: Boolean;
begin
  Result := Visibility = mvPublic;
end;

function TRttiMemberHelper.GetIsPublished: Boolean;
begin
  Result := Visibility = mvPublished;
end;

function TRttiMemberHelper.GetIsConstructor: Boolean;
begin
  Result := (Self is TRttiMethod) and TRttiMethod(Self).IsConstructor;
end;

function TRttiMemberHelper.GetIsProperty: Boolean;
begin
  Result := Self is TRttiProperty;
end;

function TRttiMemberHelper.GetIsMethod: Boolean;
begin
  Result := Self is TRttiMethod;
end;

function TRttiMemberHelper.GetIsField: Boolean;
begin
  Result := Self is TRttiField;
end;

function TRttiMemberHelper.GetAsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function TRttiMemberHelper.GetAsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function TRttiMemberHelper.GetAsField: TRttiField;
begin
  Result := Self as TRttiField;
end;

{ TRttiPropertyHelper }

function TRttiPropertyHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := GetValue(instance.AsObject)
  else
    Result := GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiPropertyHelper.SetValue(const instance, value: TValue);
begin
  if instance.IsObject then
    SetValue(instance.AsObject, value)
  else
    SetValue(instance.GetReferenceToRawData, value);
end;

{ TRttiFieldHelper }

function TRttiFieldHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := AsField.GetValue(instance.AsObject)
  else
    Result := AsField.GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiFieldHelper.SetValue(const instance, value: TValue);
begin
  if instance.IsObject then
    SetValue(instance.AsObject, value)
  else
    SetValue(instance.GetReferenceToRawData, value);
end;

{$ENDREGION}


end.
