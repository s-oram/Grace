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
///	  Provides the internal utilities for Spring4D.
///	</summary>
unit Spring.SystemUtils;

{$I 'Spring.inc'}

interface

uses
  Rtti,
  SysUtils,
  Types,
  TypInfo;

type
  ///	<summary>
  ///	  Provides static methods to manipulate an enumeration type.
  ///	</summary>
  TEnum = class
  private
    class function GetEnumTypeInfo<T>: PTypeInfo; static;
    class function GetEnumTypeData<T>: PTypeData; static;
    { Internal function without range check }
    class function ConvertToInteger<T>(const value: T): Integer; static;
  public
    class function IsValid<T>(const value: T): Boolean; overload; static;
    class function IsValid<T>(const value: Integer): Boolean; overload; static;
    class function GetName<T>(const value: T): string; overload; static;
    class function GetName<T>(const value: Integer): string; overload; static;
    class function GetNames<T>: TStringDynArray; static;
    class function GetValue<T>(const value: T): Integer; overload; static;
    class function GetValue<T>(const value: string): Integer; overload; static;
    class function GetValues<T>: TIntegerDynArray; static;
    class function GetValueStrings<T>: TStringDynArray; static;
    class function TryParse<T>(const value: Integer; out enum: T): Boolean; overload; static;
    class function TryParse<T>(const value: string; out enum: T): Boolean; overload; static;
    class function Parse<T>(const value: Integer): T; overload; static;
    class function Parse<T>(const value: string): T; overload; static;
  end;

  ///	<summary>
  ///	  Provides static methods to manipulate an Variant type.
  ///	</summary>
  TVariant = class
  public
    class function IsNull(const value: Variant): Boolean; static;
  end;

///	<summary>
///	  Retrieves the byte length of a unicode string.
///	</summary>
///	<param name="s">
///	  the unicode string.
///	</param>
///	<returns>
///	  The byte length of the unicode string.
///	</returns>
///	<remarks>
///	  Although there is already a routine <c>SysUtils.ByteLength(string)</c>
///	  function, it only supports unicode strings and doesn't provide overloads
///	  for WideStrings and AnsiStrings.
///	</remarks>
///	<seealso cref="GetByteLength(WideString)" />
///	<seealso cref="GetByteLength(RawByteString)" />
function GetByteLength(const s: string): Integer; overload; inline;

{$IFNDEF NEXTGEN}
///	<summary>
///	  Retrieves the byte length of a WideString.
///	</summary>
///	<param name="s">
///	  A wide string.
///	</param>
///	<returns>
///	  The byte length of the wide string.
///	</returns>
///	<seealso cref="GetByteLength(string)" />
///	<seealso cref="GetByteLength(RawByteString)" />
function GetByteLength(const s: WideString): Integer; overload; inline;

///	<summary>
///	  Retrieves the byte length of a <c>RawByteString</c> (AnsiString or
///	  UTF8String).
///	</summary>
///	<returns>
///	  The byte length of the raw byte string.
///	</returns>
///	<seealso cref="GetByteLength(string)" />
///	<seealso cref="GetByteLength(WideString)" />
function GetByteLength(const s: RawByteString): Integer; overload; inline;
{$ENDIF NEXTGEN}


///	<summary>
///	  Overloads. SplitString
///	</summary>
///	<remarks>
///	  Each element of separator defines a separate delimiter character. If two
///	  delimiters are adjacent, or a delimiter is found at the beginning or end
///	  of the buffer, the corresponding array element contains Empty.
///	</remarks>
function SplitString(const buffer: string; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;
function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;
function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
  removeEmptyEntries: Boolean = False): TStringDynArray; overload;

///	<summary>
///	  Returns a string array that contains the substrings in the buffer that
///	  are delimited by null char (#0) and ends with an additional null char.
///	</summary>
///	<example>
///	  <code lang="Delphi">
///	procedure TestSplitNullTerminatedStrings;
///	var
///	  buffer: string;
///	  strings: TStringDynArray;
///	  s: string;
///	begin
///	  buffer := 'C:'#0'D:'#0'E:'#0#0;
///	  strings := SplitString(PChar(buffer));
///	  for s in strings do
///	  begin
///	    Writeln(s);
///	  end;
///	end;</code>
///	</example>
function SplitString(const buffer: PChar): TStringDynArray; overload;

///	<summary>
///	  Returns a string array that contains the substrings in the buffer that
///	  are delimited by null char (#0) and ends with an additional null char.
///	</summary>
function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;
  deprecated 'Use the SplitString(PChar) function instead.';

///	<summary>
///	  Returns <c>True</c> if the type is a nullable type.
///	</summary>
function IsNullableType(typeInfo: PTypeInfo): Boolean;

///	<summary>
///	  Try getting the underlying type name of a nullable type.
///	</summary>
///	<remarks>
///	  For instance, the underlying type name of the type
///	  <c>Nullable&lt;System.Integer&gt;</c> is <c>System.Integer</c>.
///	</remarks>
function TryGetUnderlyingTypeName(typeInfo: PTypeInfo; out underlyingTypeName: string): Boolean;

///	<summary>
///	  Try getting the underlying type info of a nullable type.
///	</summary>
function TryGetUnderlyingTypeInfo(typeInfo: PTypeInfo; out underlyingTypeInfo: PTypeInfo): Boolean;

///	<summary>
///	  Try getting the underlying value of a nullable type.
///	</summary>
///	<param name="value">
///	  the value
///	</param>
///	<param name="underlyingValue">
///	  the underlying value.
///	</param>
///	<returns>
///	  Returns True if the value is a <c>Nullable&lt;T&gt;</c> and it has value.
///	</returns>
function TryGetUnderlyingValue(const value: TValue; out underlyingValue: TValue): Boolean;

///	<summary>
///	  Try setting the underlying value of a nullable type.
///	</summary>
///	<param name="value">
///	  the value
///	</param>
///	<param name="underlyingValue">
///	  the underlying value.
///	</param>
///	<returns>
///	  Returns True if the value is a <c>Nullable&lt;T&gt;</c> and it has value.
///	</returns>
function TrySetUnderlyingValue(const value: TValue; const underlyingValue: TValue): Boolean;

///	<summary>
///	  Uses this function to get an interface instance from a TValue.
///	</summary>
///	<remarks>
///	  <note type="warning">
///	    Rtti bugs: QC #82433 if
///	    value.TryAsType&lt;IPropertyNotification&gt;(propertyNotification) then
///	  </note>
///	</remarks>
function TryGetInterface(const instance: TValue; const guid: TGuid; out intf): Boolean; overload;

///	<seealso cref="Spring|Nullable{T}" />
function TryGetInterface(const instance: TValue; const guid: TGuid): Boolean; overload;

///	<summary>
///	  Try parsing a string to a datetime value based on the specified format.
///	  Returns True if the input string matches the format.
///	</summary>
///	<param name="s">
///	  the input string
///	</param>
///	<param name="format">
///	  the format of datetime
///	</param>
///	<param name="value">
///	  output datetime value
///	</param>
///	<returns>
///	  Returns True if the input string can be parsed.
///	</returns>
function TryConvertStrToDateTime(const s, format: string; out value: TDateTime): Boolean;

///	<summary>
///	  Parses a string to a datetime value based on the specified format. An
///	  EConvertError exception will be raised if failed to parse the string.
///	</summary>
///	<param name="s">
///	  the date time string.
///	</param>
///	<param name="format">
///	  the format of datetime.
///	</param>
function ConvertStrToDateTime(const s, format: string): TDateTime;

implementation

uses
  DateUtils,
  StrUtils,
  Variants,
  Spring,
  Spring.ResourceStrings;

{$REGION 'TEnum'}

class function TEnum.GetEnumTypeInfo<T>: PTypeInfo;
begin
  Result := TypeInfo(T);
  Guard.CheckTypeKind(Result, tkEnumeration, 'T');
end;

class function TEnum.GetEnumTypeData<T>: PTypeData;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := TEnum.GetEnumTypeInfo<T>;
  Result := GetTypeData(typeInfo);
end;

class function TEnum.ConvertToInteger<T>(const value: T): Integer;
begin
  Result := 0;  // *MUST* initialize Result
  Move(value, Result, SizeOf(T));
end;

class function TEnum.IsValid<T>(const value: Integer): Boolean;
var
  typeInfo: PTypeInfo;
  data: PTypeData;
begin
  typeInfo := System.TypeInfo(T);
  Guard.CheckTypeKind(typeInfo, [tkEnumeration], 'T');

  data := GetTypeData(typeInfo);
  Guard.CheckNotNull(data, 'data');
  Result := (value >= data.MinValue) and (value <= data.MaxValue);
end;

class function TEnum.IsValid<T>(const value: T): Boolean;
var
  intValue: Integer;
begin
  intValue := TEnum.ConvertToInteger<T>(value);
  Result := TEnum.IsValid<T>(intValue);
end;

class function TEnum.GetName<T>(const value: Integer): string;
var
  typeInfo: PTypeInfo;
begin
  Guard.CheckEnum<T>(value, 'value');

  typeInfo := GetEnumTypeInfo<T>;
  Result := GetEnumName(typeInfo, value);
end;

class function TEnum.GetName<T>(const value: T): string;
var
  intValue: Integer;
begin
  intValue := TEnum.ConvertToInteger<T>(value);
  Result := TEnum.GetName<T>(intValue);
end;

class function TEnum.GetNames<T>: TStringDynArray;
var
  typeData: PTypeData;
{$IFDEF NEXTGEN}
  p: TTypeInfoFieldAccessor;
{$ELSE}
  p: PShortString;
{$ENDIF}
  i: Integer;
begin
  typeData := TEnum.GetEnumTypeData<T>;
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
{$IFDEF NEXTGEN}
  p := typedata^.NameListFld;
{$ELSE}
  p := @typedata.NameList;
{$ENDIF}
  for i := 0 to High(Result) do
  begin
{$IFDEF NEXTGEN}
    Result[i] := p.ToString;
    p.SetData(p.Tail);
{$ELSE}
    Result[i] := UTF8ToString(p^);
    Inc(PByte(p), Length(p^)+1);
{$ENDIF}
  end;
end;

class function TEnum.GetValue<T>(const value: T): Integer;
begin
  Guard.CheckEnum<T>(value, 'value');

  Result := TEnum.ConvertToInteger<T>(value);
end;

class function TEnum.GetValue<T>(const value: string): Integer;
var
  temp: T;
begin
  temp := TEnum.Parse<T>(value);
  Result := TEnum.ConvertToInteger<T>(temp);
end;

class function TEnum.GetValues<T>: TIntegerDynArray;
var
  typeData: PTypeData;
  i: Integer;
begin
  typeData := TEnum.GetEnumTypeData<T>;
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  for i := 0 to High(Result) do
  begin
    Result[i] := i;
  end;
end;

class function TEnum.GetValueStrings<T>: TStringDynArray;
var
  typeData: PTypeData;
  i: Integer;
begin
  typeData := TEnum.GetEnumTypeData<T>;
  SetLength(Result, typeData.MaxValue - typeData.MinValue + 1);
  for i := 0 to High(Result) do
  begin
    Result[i] := IntToStr(i);
  end;
end;

class function TEnum.TryParse<T>(const value: Integer; out enum: T): Boolean;
begin
  Result := TEnum.IsValid<T>(value);
  if Result then
    Move(value, enum, SizeOf(T));
end;

class function TEnum.TryParse<T>(const value: string; out enum: T): Boolean;
var
  typeInfo: PTypeInfo;
  intValue: Integer;
begin
  typeInfo := TEnum.GetEnumTypeInfo<T>;
  intValue := GetEnumValue(typeInfo, value);
  Result := TEnum.TryParse<T>(intValue, enum);
end;

class function TEnum.Parse<T>(const value: Integer): T;
begin
  if not TEnum.TryParse<T>(value, Result) then
    raise EFormatException.CreateResFmt(@SIncorrectFormat, [IntToStr(value)]);
end;

class function TEnum.Parse<T>(const value: string): T;
begin
  if not TEnum.TryParse<T>(value, Result) then
    raise EFormatException.CreateResFmt(@SIncorrectFormat, [value]);
end;

{$ENDREGION}


{$REGION 'TVariant'}

class function TVariant.IsNull(const value: Variant): Boolean;
begin
  Result := VarIsNull(value);
end;

{$ENDREGION}


function GetByteLength(const s: string): Integer;
begin
  Result := Length(s) * SizeOf(Char);
end;

{$IFNDEF NEXTGEN}
function GetByteLength(const s: WideString): Integer;
begin
  Result := Length(s) * SizeOf(WideChar);
end;

function GetByteLength(const s: RawByteString): Integer;
begin
  Result := Length(s);
end;
{$ENDIF}

function SplitString(const buffer: string; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries);
end;

function SplitString(const buffer: TCharArray; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
begin
  Result := SplitString(PChar(buffer), Length(buffer), separators, removeEmptyEntries)
end;

function SplitString(const buffer: PChar; len: Integer; const separators: TSysCharSet;
  removeEmptyEntries: Boolean): TStringDynArray;
var
  head: PChar;
  tail: PChar;
  p: PChar;

  procedure AppendEntry(buffer: PChar; len: Integer; var strings: TStringDynArray);
  var
    entry: string;
  begin
    SetString(entry, buffer, len);
    if not removeEmptyEntries or (entry <> '') then
    begin
      SetLength(strings, Length(strings) + 1);
      strings[Length(strings) - 1] := entry;
    end;
  end;
begin
  Guard.CheckRange(len >= 0, 'len');

  if (buffer = nil) or (len = 0) then Exit;
  head := buffer;
  tail := head + len - 1;
  p := head;
  while p <= tail do
  begin
    if CharInSet(p^, separators) then
    begin
      AppendEntry(head, p - head, Result);
      head := StrNextChar(p);
    end;
    if p = tail then
    begin
      AppendEntry(head, p - head + 1, Result);
    end;
    p := StrNextChar(p);
  end;
end;

function SplitString(const buffer: PChar): TStringDynArray;
var
  p: PChar;
  entry: string;
begin
  if (buffer = nil) or (buffer^ = #0) then Exit;
  p := buffer;
  while p^ <> #0 do
  begin
    entry := p;
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result)-1] := entry;
    Inc(p, Length(entry) + 1);  // Jump to the next entry
  end;
end;

function SplitNullTerminatedStrings(const buffer: PChar): TStringDynArray;
begin
  Result := SplitString(buffer);
end;

function IsNullableType(typeInfo: PTypeInfo): Boolean;
const
  PrefixString = 'Nullable<';    // DO NOT LOCALIZE
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind = tkRecord)
    and StartsText(PrefixString, GetTypeName(typeInfo));
end;

function TryGetUnderlyingTypeName(typeInfo: PTypeInfo; out underlyingTypeName: string): Boolean;
const
  PrefixString = 'Nullable<';    // DO NOT LOCALIZE
  PrefixStringLength = Length(PrefixString);
var
  typeName: string;
begin
  Result := IsNullableType(typeInfo);
  if Result then
  begin
    typeName := GetTypeName(typeInfo);
    underlyingTypeName := Copy(typeName, PrefixStringLength + 1,
      Length(typeName) - PrefixStringLength - 1);
  end;
end;

function TryGetUnderlyingTypeInfo(typeInfo: PTypeInfo; out underlyingTypeInfo: PTypeInfo): Boolean;
var
  context: TRttiContext;
  rttiType: TRttiType;
  valueField: TRttiField;
begin
  Result := IsNullableType(typeInfo);
  if Result then
  begin
    rttiType := context.GetType(typeInfo);
    valueField := rttiType.GetField('fValue');
    Result := Assigned(valueField);
    if Result then
      underlyingTypeInfo := valueField.FieldType.Handle
    else
      underlyingTypeInfo := nil;
  end;
end;

function TryGetUnderlyingValue(const value: TValue; out underlyingValue: TValue): Boolean;
var
  typeInfo: PTypeInfo;
  context: TRttiContext;
  rttiType: TRttiType;
  hasValueField: TRttiField;
  instance: Pointer;
  valueField: TRttiField;
begin
  typeInfo := value.TypeInfo;
  Result := IsNullableType(typeInfo);
  if Result then
  begin
    rttiType := context.GetType(typeInfo);
    hasValueField := rttiType.GetField('fHasValue');
    if Assigned(hasValueField) then
    begin
      instance := value.GetReferenceToRawData;
      Result := hasValueField.GetValue(instance).AsString <> '';
      if Result then
      begin
        valueField := rttiType.GetField('fValue');
        Result := Assigned(valueField);
        if Result then
          underlyingValue := valueField.GetValue(instance);
      end;
    end;
  end;
end;

function TrySetUnderlyingValue(const value: TValue; const underlyingValue: TValue): Boolean;
var
  typeInfo: PTypeInfo;
  context: TRttiContext;
  rttiType: TRttiType;
  hasValueField: TRttiField;
  instance: Pointer;
  valueField: TRttiField;
begin
  typeInfo := value.TypeInfo;
  Result := IsNullableType(typeInfo);
  if Result then
  begin
    rttiType := context.GetType(typeInfo);
    valueField := rttiType.GetField('fValue');
    if Assigned(valueField) then
    begin
      hasValueField := rttiType.GetField('fHasValue');
      if Assigned(hasValueField) then
      begin
        instance := value.GetReferenceToRawData;
        valueField.SetValue(instance, underlyingValue);
        if underlyingValue.IsEmpty then
          hasValueField.SetValue(instance, '')
        else
          hasValueField.SetValue(instance, '@');
      end;
    end;
  end;
end;

function TryGetInterface(const instance: TValue; const guid: TGuid; out intf): Boolean;
var
  localInterface: IInterface;
begin
  if instance.IsEmpty then Exit(False);
  if instance.IsObject then
  begin
    Result := instance.AsObject.GetInterface(guid, intf);
  end
  else if instance.TryAsType<IInterface>(localInterface) then
  begin
    Result := localInterface.QueryInterface(guid, intf) = S_OK;
  end
  else
  begin
    Exit(False);
  end;
end;

function TryGetInterface(const instance: TValue; const guid: TGuid): Boolean;
var
  localInterface: IInterface;
begin
  if instance.IsEmpty then Exit(False);
  if instance.IsObject then
  begin
    Result := Supports(instance.AsObject, guid);
  end
  else if instance.TryAsType<IInterface>(localInterface) then
  begin
    Result := Supports(localInterface, guid);
  end
  else
  begin
    Exit(False);
  end;
end;

function TryConvertStrToDateTime(const s, format: string; out value: TDateTime): Boolean;
var
  localString: string;
  stringFormat: string;
  year, month, day: Word;
  hour, minute, second, milliSecond: Word;

  function ExtractElementDef(const element: string; const defaultValue: Integer = 0): Integer;
  var
    position: Integer;
  begin
    position := Pos(element, stringFormat);
    if position > 0 then
    begin
      Result := StrToInt(Copy(localString, position, Length(element)));
    end
    else
    begin
      Result := defaultValue;
    end;
  end;
begin
  localString := Trim(s);
  stringFormat := UpperCase(format);
  Result := Length(localString) = Length(stringFormat);
  if Result then
  try
    year := ExtractElementDef('YYYY', 0);
    if year = 0 then
    begin
      year := ExtractElementDef('YY', 1899);
      if year < 1899 then
      begin
        Inc(year, (YearOf(Today) div 100) * 100);
      end;
    end;
    month := ExtractElementDef('MM', 12);
    day := ExtractElementDef('DD', 30);
    hour := ExtractElementDef('HH');
    minute := ExtractElementDef('NN');
    second := ExtractElementDef('SS');
    milliSecond := ExtractElementDef('ZZZ');
    value := EncodeDateTime(year, month, day, hour, minute, second, milliSecond);
  except
    Result := False;
  end;
end;

function ConvertStrToDateTime(const s, format: string): TDateTime;
begin
  if not TryConvertStrToDateTime(s, format, Result) then
  begin
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [s]);
  end;
end;

end.
