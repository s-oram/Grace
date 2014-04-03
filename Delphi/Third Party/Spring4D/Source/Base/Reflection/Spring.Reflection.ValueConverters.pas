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

unit Spring.Reflection.ValueConverters;

{$I Spring.inc}

interface

uses
  TypInfo,
  Rtti,
  Generics.Collections,
  Spring,
  Spring.Reflection,
  Spring.Helpers;

type

  {$REGION 'IValueConverter'}

  ///	<summary>
  ///	  Base value converter interface
  ///	</summary>
  IValueConverter = interface
    ['{048EF3F0-41B5-4019-9BD6-00B88CAA7275}']

    ///	<param name="value">
    ///	  Rtti.TValue to convert
    ///	</param>
    ///	<param name="targetTypeInfo">
    ///	  Target Rtti.PTypeInfo structure
    ///	</param>
    ///	<returns>
    ///	  Returns <paramref name="value">converted</paramref> to type pointing
    ///	  by <paramref name="targetTypeInfo" /> parameter
    ///	</returns>
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; overload;

    ///	<param name="value">
    ///	  Rtti.TValue to convert
    ///	</param>
    ///	<param name="targetTypeInfo">
    ///	  Target Rtti.PTypeInfo structure
    ///	</param>
    ///	<param name="parameter">
    ///	  Additional Rtti.TValue formatting parameter, use when possible
    ///	</param>
    ///	<returns>
    ///	  Returns <paramref name="value" /> converted to type pointing by
    ///	  <paramref name="targetTypeInfo" />parameter
    ///	</returns>
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; overload;

    ///	<param name="value">
    ///	  Rtti.TValue to convert
    ///	</param>
    ///	<param name="targetTypeInfo">
    ///	  Target Rtti.PTypeInfo structure
    ///	</param>
    ///	<param name="targetValue">
    ///	  Target Rtti.TValue out parameter
    ///	</param>
    ///	<returns>
    ///	  Returns System.Boolean, True if converting with success
    ///	</returns>
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; overload;

    ///	<param name="value">
    ///	  Rtti.TValue to convert
    ///	</param>
    ///	<param name="targetTypeInfo">
    ///	  Target Rtti.PTypeInfo structure
    ///	</param>
    ///	<param name="targetValue">
    ///	  Target Rtti.TValue out parameter
    ///	</param>
    ///	<param name="parameter">
    ///	  Additional Rtti.TValue formatting parameter, use when possible
    ///	</param>
    ///	<returns>
    ///	  Returns System.Boolean, True if converting with success
    ///	</returns>
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; overload;
  end;

  {$ENDREGION}


  {$REGION 'TValueConverter'}

  ///	<summary>
  ///	  Base abstract class provides DefaultConverter as an entry point to the
  ///	  user side
  ///	</summary>
  TValueConverter = class abstract(TInterfacedObject, IValueConverter)
  private
    class var fDefaultConverter: IValueConverter;

    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo): TValue; overload;
    function ConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue): Boolean; overload;
    function TryConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      out targetValue: TValue;
      const parameter: TValue): Boolean; overload;
    class function GetDefault: IValueConverter; static;
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; virtual; abstract;
  public
    class constructor Create;
    constructor Create; virtual;

    class property Default: IValueConverter read GetDefault;
  end;

  TConverterClass = class of TValueConverter;

  {$ENDREGION}


  {$REGION 'TDefaultValueConverter'}

  ///	<summary>
  ///	  Provides default converter shared instance, TDefaultValueConverter is
  ///	  the master in the process of conversion
  ///	</summary>
  ///	<remarks>
  ///	  <para>
  ///	    There is three steps of doing so
  ///	  </para>
  ///	  <list type="number">
  ///	    <item>
  ///	      Find/lock "global" registry
  ///	    </item>
  ///	    <item>
  ///	      Use TValue.TryCast
  ///	    </item>
  ///	    <item>
  ///	      Use RTTI exploring and select apropriate converter.
  ///	    </item>
  ///	  </list>
  ///	  <para>
  ///	    There are four different internall converter types that can be
  ///	    selected to convert:
  ///	  </para>
  ///	  <list type="bullet">
  ///	    <item>
  ///	      TNullable&lt;T&gt; and T
  ///	    </item>
  ///	    <item>
  ///	      Enumeration and Integer/string
  ///	    </item>
  ///	    <item>
  ///	      TColor and Integer/string
  ///	    </item>
  ///	    <item>
  ///	      Integer and string
  ///	    </item>
  ///	    <item>
  ///	      Enumeration and Integer/string
  ///	    </item>
  ///	  </list>
  ///	</remarks>
  TDefaultValueConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TIntegerToStringConverter'}

  ///	<summary>
  ///	  Simply provides conversion routine beetwen Integer and
  ///	  string/UnicodeString
  ///	</summary>
  TIntegerToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToIntegerConverter'}

  ///	<summary>
  ///	  Simply provides conversion routine beetwen string and Integer
  ///	</summary>
  TStringToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TIntegerToBooleanConverter'}

  ///	<summary>
  ///	  Simply provides conversion routine beetwen Integer and Boolean
  ///	</summary>
  TIntegerToBooleanConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TBooleanToIntegerConverter'}

  ///	<summary>
  ///	  Simply provides conversion routine beetwen Boolean and Integer
  ///	</summary>
  TBooleanToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TBooleanToStringConverter'}

  ///	<summary>
  ///	  Simply provides conversion routine beetwen Boolean and string
  ///	</summary>
  TBooleanToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToBooleanConverter'}

  ///	<summary>
  ///	  Simply provides conversion routine beetwen string and Boolean
  ///	</summary>
  TStringToBooleanConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TNullableToTypeConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen Nullable(T) and T
  ///	</summary>
  ///	<remarks>
  ///	  Internally it use another Converter to delegate conversion routine if
  ///	  necessary
  ///	</remarks>
  TNullableToTypeConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TTypeToNullableConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen T and Nullable(T)
  ///	</summary>
  ///	<remarks>
  ///	  Internally it use another Converter to delegate conversion routine if
  ///	  necessary
  ///	</remarks>
  TTypeToNullableConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TEnumToIntegerConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen enumeration and Integer
  ///	</summary>
  TEnumToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TIntegerToEnumConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen Integer and enumeration
  ///	</summary>
  TIntegerToEnumConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TEnumToStringConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen enumeration and string
  ///	</summary>
  TEnumToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToEnumConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen string and enumeration
  ///	</summary>
  TStringToEnumConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TSetToStringConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen set and string
  ///	</summary>
  TSetToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToSetConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen string and enumeration
  ///	</summary>
  TStringToSetConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TFloatToStringConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen float and string
  ///	</summary>
  TFloatToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TFloatToIntegerConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen float and Integer
  ///	</summary>
  TFloatToIntegerConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToFloatConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen string and float
  ///	</summary>
  TStringToFloatConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TColorToStringConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen TColor and string
  ///	</summary>
  TColorToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToColorConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen string and TColor
  ///	</summary>
  TStringToColorConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TCurrencyToStringConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen Currency and string
  ///	</summary>
  TCurrencyToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToCurrencyConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen string and Currency
  ///	</summary>
  TStringToCurrencyConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToDateTimeConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen string and TDateTime
  ///	</summary>
  TStringToDateTimeConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TDateTimeToStringConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen TDateTime and string
  ///	</summary>
  TDateTimeToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TObjectToStringConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen TObject and string
  ///	</summary>
  TObjectToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TObjectToInterfaceConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen TObject and IInterface
  ///	</summary>
  ///	<remarks>
  ///	  acc. to #82433 TValue.TryAsType(T) raised an AV because ConvClass2Intf
  ///	  is wrong
  ///	</remarks>
  TObjectToInterfaceConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TInterfaceToObjectConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen TObject and IInterface
  ///	</summary>
  TInterfaceToObjectConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TInterfaceToInterfaceConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen Interface and Interface
  ///	</summary>
  TInterfaceToInterfaceConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TObjectToClassConverter'}

  ///	<summary>
  ///	  Provides conversion routine beetwen TObject and TClass
  ///	</summary>
  TObjectToClassConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;

  {$ENDREGION}


  {$REGION 'TStringToWStringConverter'}

{$IFNDEF NEXTGEN}
  ///	<summary>
  ///	  Provides conversion routine beetwen UnicodeString and WideString
  ///	</summary>
  ///	<remarks>
  ///	  acc. to #82487 Rtti.ConvStr2Str is wrong (when cast a unicode string to
  ///	  WideString)
  ///	</remarks>
  TStringToWStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TWStringToStringConverter'}

{$IFNDEF NEXTGEN}
  ///	<summary>
  ///	  Provides conversion routine beetwen UnicodeString and WideString
  ///	</summary>
  TWStringToStringConverter = class(TValueConverter)
  protected
    function DoConvertTo(const value: TValue;
      const targetTypeInfo: PTypeInfo;
      const parameter: TValue): TValue; override;
  end;
{$ENDIF}

  {$ENDREGION}


  {$REGION 'TValueConverterFactory'}

  ///	<summary>
  ///	  Factory class that brings to live converter which are registered within
  ///	  global converter registry scope
  ///	</summary>
  TValueConverterFactory = class
  strict private
    type
      ///	<summary>
      ///	  TypeInfo, TypeKinds registry structure
      ///	</summary>
      TConvertedTypeInfo = record
        SourceTypeInfo: PTypeInfo;
        SourceTypeKinds: TTypeKinds;
        TargetTypeInfo: PTypeInfo;
        TargetTypeKinds: TTypeKinds;
      end;

      IConverterPackage = interface
        ['{8388B821-13EC-4CA9-95A1-88D0E89FC46B}']
        function GetInstance: IValueConverter;

        property Instance: IValueConverter read GetInstance;
      end;

      TConverterPackage = class(TInterfacedObject, IConverterPackage)
      strict private
        fConverterClass: TConverterClass;
        fConverter: IValueConverter;

        function GetInstance: IValueConverter;
      public
        constructor Create(classType: TConverterClass);
        property Instance: IValueConverter read GetInstance;
      end;
    class var fTypeInfoToTypeInfoRegistry: TDictionary<TConvertedTypeInfo, IConverterPackage>;
    class var fTypeInfoToTypeKindsRegistry: TDictionary<TConvertedTypeInfo, IConverterPackage>;
    class var fTypeKindsToTypeInfoRegistry: TDictionary<TConvertedTypeInfo, IConverterPackage>;
    class var fTypeKindsToTypeKindsRegistry: TDictionary<TConvertedTypeInfo, IConverterPackage>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterConverter(const sourceTypeInfo, targetTypeInfo: PTypeInfo;
      converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeKinds, targetTypeKinds: TTypeKinds;
      converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeKinds: TTypeKinds;
      targetTypeInfo: PTypeInfo; converterClass: TConverterClass); overload;
    class procedure RegisterConverter(const sourceTypeInfo: PTypeInfo;
      targetTypeKinds: TTypeKinds; converterClass: TConverterClass); overload;

    class function CreateConverter(const sourceTypeInfo,
      targetTypeInfo: PTypeInfo): IValueConverter; deprecated 'Use GetConverter instead.';
    class function GetConverter(const sourceTypeInfo,
      targetTypeInfo: PTypeInfo): IValueConverter;
  end;

  {$ENDREGION}


implementation

uses
{$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UIConsts,
  System.UITypes,
{$ELSE}
  Graphics,
{$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  Math,
  StrUtils,
  SysUtils,
  Spring.SystemUtils,
  Spring.ResourceStrings;


  function CompareTypeInfo(const left, right: PTypeInfo): Boolean;
  begin
    Result := (left = right);
    if Assigned(left) and Assigned(right) then
      Result := Result or ((left.Kind = right.Kind)
        and (left.TypeName = right.TypeName));
  end;


{$REGION 'TValueConverter'}

class constructor TValueConverter.Create;
begin
  inherited;
  fDefaultConverter := TDefaultValueConverter.Create;
end;

constructor TValueConverter.Create;
begin
  inherited;
end;

class function TValueConverter.GetDefault: IValueConverter;
begin
  Result := fDefaultConverter;
end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo): TValue;
begin
  Result := ConvertTo(value, targetTypeInfo, TValue.Empty);
end;

function TValueConverter.ConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo;
  const parameter: TValue): TValue;
begin
  Guard.CheckNotNull(value.TypeInfo, 'value.TypeInfo');
  Guard.CheckNotNull(targetTypeInfo, 'targetTypeInfo');
  try
    Result := DoConvertTo(value, targetTypeInfo, parameter);
  except
    Exception.RaiseOuterException(Exception.CreateResFmt(@SCouldNotConvertValue,
      [value.TypeInfo.TypeName, targetTypeInfo.TypeName]));
  end;
  if Result.IsEmpty then
    raise Exception.CreateResFmt(@SCouldNotConvertValue,
      [value.TypeInfo.TypeName, targetTypeInfo.TypeName]);
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue): Boolean;
begin
  Result := TryConvertTo(value, targetTypeInfo, targetValue, TValue.Empty);
end;

function TValueConverter.TryConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; out targetValue: TValue;
  const parameter: TValue): Boolean;
begin
  Result := True;
  try
    targetValue := DoConvertTo(value, targetTypeInfo, parameter);
  except
    Result := False;
  end;
end;

{$ENDREGION}


{$REGION 'TDefaultValueConverter'}

function TDefaultValueConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  converter: IValueConverter;
begin
  converter := TValueConverterFactory.GetConverter(value.TypeInfo, targetTypeInfo);
  if Assigned(converter) then
    Result := converter.ConvertTo(value, targetTypeInfo, parameter)
  else
    value.TryCast(targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TIntegerToStringConverter'}

function TIntegerToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(IntToStr(value.AsInteger));
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(IntToStr(value.AsInteger)));
    tkWString:
      Result := TValue.From<WideString>(IntToStr(value.AsInteger));
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToIntegerConverter'}

function TStringToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo = TypeInfo(Integer) then
    Result := TValue.From<Integer>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(Cardinal) then
    Result := TValue.From<Cardinal>(StrToInt64(value.AsString))
  else if targetTypeInfo = TypeInfo(Int64) then
    Result := TValue.From<Int64>(StrToInt64(value.AsString))
  else if targetTypeInfo = TypeInfo(UInt64) then
    Result := TValue.From<UInt64>(StrToInt64(value.AsString))
  else if targetTypeInfo = TypeInfo(SmallInt) then
    Result := TValue.From<SmallInt>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(Word) then
    Result := TValue.From<Word>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(ShortInt) then
    Result := TValue.From<ShortInt>(StrToInt(value.AsString))
  else if targetTypeInfo = TypeInfo(Byte) then
    Result := TValue.From<Byte>(StrToInt(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToStringConverter'}

function TBooleanToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(BoolToStr(value.AsBoolean, True));
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(BoolToStr(value.AsBoolean, True)));
    tkWString:
      Result := TValue.From<WideString>(BoolToStr(value.AsBoolean, True));
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToBooleanConverter'}

function TStringToBooleanConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Boolean>(StrToBool(value.AsString));
end;

{$ENDREGION}


{$REGION 'TBooleanToIntegerConverter'}

function TBooleanToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo = TypeInfo(Integer) then
    Result := TValue.From<Integer>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Cardinal) then
    Result := TValue.From<Cardinal>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Int64) then
    Result := TValue.From<Int64>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(UInt64) then
    Result := TValue.From<UInt64>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(SmallInt) then
    Result := TValue.From<SmallInt>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Word) then
    Result := TValue.From<Word>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(ShortInt) then
    Result := TValue.From<ShortInt>(Integer(value.AsBoolean))
  else if targetTypeInfo = TypeInfo(Byte) then
    Result := TValue.From<Byte>(Integer(value.AsBoolean));
end;

{$ENDREGION}


{$REGION 'TIntegerToBooleanConverter'}

function TIntegerToBooleanConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Boolean>(Boolean(value.AsInteger));
end;

{$ENDREGION}


{$REGION 'TNullableToTypeConverter'}

function TNullableToTypeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  underlyingValue: TValue;
begin
  if TryGetUnderlyingValue(value, underlyingValue)
    and (underlyingValue.TypeInfo.TypeName <> targetTypeInfo.TypeName) then
  begin
    Result := TValueConverter.Default.ConvertTo(underlyingValue,
      targetTypeInfo, parameter)
  end
  else
    Result := underlyingValue;
end;

{$ENDREGION}


{$REGION 'TTypeToNullableConverter'}

function TTypeToNullableConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  underlyingTypeInfo: PTypeInfo;
  underlyingValue: TValue;
begin
  if TryGetUnderlyingTypeInfo(targetTypeInfo, underlyingTypeInfo) then
  begin
    underlyingValue := value;
    if underlyingTypeInfo.TypeName <> value.TypeInfo.TypeName then
      Result := TValueConverter.Default.TryConvertTo(value, underlyingTypeInfo,
        underlyingValue, parameter);

    TValue.Make(nil, targetTypeInfo, Result);
    TrySetUnderlyingValue(Result, underlyingValue);
  end;
end;

{$ENDREGION}


{$REGION 'TEnumToStringConverter'}

function TEnumToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
  enumName: string;
begin
  enumValue := PInteger(value.GetReferenceToRawData)^;
  enumName := GetEnumName(value.TypeInfo, enumValue);
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(enumName);
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(enumName));
    tkWString:
      Result := TValue.From<WideString>(enumName);
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToEnumConverter'}

function TStringToEnumConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
begin
  enumValue := GetEnumValue(targetTypeInfo, value.AsString);
  TValue.Make(enumValue, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TSetToStringConverter'}

function TSetToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  setAsString: string;
begin
  setAsString := SetToString(value.TypeInfo,
    PInteger(value.GetReferenceToRawData)^, True);
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(setAsString);
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(setAsString));
    tkWString:
      Result := TValue.From<WideString>(setAsString);
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToSetConverter'}

function TStringToSetConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  TValue.Make(StringToSet(targetTypeInfo, value.AsString),
    targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TEnumToIntegerConverter'}

function TEnumToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  enumValue: Integer;
begin
  enumValue := PInteger(value.GetReferenceToRawData)^;
  TValue.Make(enumValue, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TIntegerToEnumConverter'}

function TIntegerToEnumConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  TValue.Make(value.AsInteger, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TFloatToStringConverter'}

function TFloatToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatFloat(format, value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatFloat(format, value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FormatFloat(format, value.AsExtended));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FloatToStr(value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FloatToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FloatToStr(value.AsExtended));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TFloatToIntegerConverter'}

function TFloatToIntegerConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  if targetTypeInfo = TypeInfo(Integer) then
    Result := TValue.From<Integer>(Floor(value.AsExtended))
  else if targetTypeInfo = TypeInfo(Cardinal) then
    Result := TValue.From<Cardinal>(Floor(value.AsExtended))
  else if targetTypeInfo = TypeInfo(Int64) then
    Result := TValue.From<Int64>(Floor(value.AsExtended))
  else if targetTypeInfo = TypeInfo(UInt64) then
    Result := TValue.From<UInt64>(Floor(value.AsExtended))
  else if targetTypeInfo = TypeInfo(SmallInt) then
    Result := TValue.From<SmallInt>(Floor(value.AsExtended))
  else if targetTypeInfo = TypeInfo(Word) then
    Result := TValue.From<Word>(Floor(value.AsExtended))
  else if targetTypeInfo = TypeInfo(ShortInt) then
    Result := TValue.From<ShortInt>(Floor(value.AsExtended))
  else if targetTypeInfo = TypeInfo(Byte) then
    Result := TValue.From<Byte>(Floor(value.AsExtended));
end;

{$ENDREGION}


{$REGION 'TStringToFloatConverter'}

function TStringToFloatConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  targetTypeData: PTypeData;
begin
  if targetTypeInfo.Kind = tkFloat then
  begin
    targetTypeData := GetTypeData(targetTypeInfo);
    case targetTypeData.FloatType of
      ftExtended:
        Result := TValue.From<Extended>(StrToFloat(value.AsString));
      ftDouble:
        Result := TValue.From<Double>(StrToFloat(value.AsString));
      ftSingle:
        Result := TValue.From<Single>(StrToFloat(value.AsString));
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TColorToStringConverter'}

function TColorToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(ColorToString(value.AsType<TColor>));
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(ColorToString(value.AsType<TColor>)));
    tkWString:
      Result := TValue.From<WideString>(ColorToString(value.AsType<TColor>));
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TStringToColorConverter'}

function TStringToColorConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TColor>(StringToColor(value.AsString));
end;

{$ENDREGION}


{$REGION 'TCurrencyToStringConverter'}

function TCurrencyToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatCurr(format, value.AsType<Currency>));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatCurr(format, value.AsType<Currency>)));
      tkWString:
        Result := TValue.From<WideString>(FormatCurr(format, value.AsType<Currency>));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(CurrToStr(value.AsType<Currency>));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(CurrToStr(value.AsType<Currency>)));
      tkWString:
        Result := TValue.From<WideString>(CurrToStr(value.AsType<Currency>));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TStringToCurrencyConverter'}

function TStringToCurrencyConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<Currency>(StrToCurr(value.AsString));
end;

{$ENDREGION}


{$REGION 'TDateTimeToStringConverter'}

function TDateTimeToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(FormatDateTime(format, value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(FormatDateTime(format, value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(FormatDateTime(format, value.AsExtended));
{$ENDIF}
    end;
  end
  else
    case targetTypeInfo.Kind of
      tkString, tkUString:
        Result := TValue.From<string>(DateTimeToStr(value.AsExtended));
{$IFNDEF NEXTGEN}
      tkLString:
        Result := TValue.From<AnsiString>(AnsiString(DateTimeToStr(value.AsExtended)));
      tkWString:
        Result := TValue.From<WideString>(DateTimeToStr(value.AsExtended));
{$ENDIF}
    end;
end;

{$ENDREGION}


{$REGION 'TStringToDateTimeConverter'}

function TStringToDateTimeConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  format: string;
begin
  if not parameter.IsEmpty and
    parameter.TryAsType<string>(format) then
  begin
    Result := TValue.From<TDateTime>(ConvertStrToDateTime(value.AsString, format));
  end
  else
    Result := TValue.From<TDateTime>(StrToDateTime(value.AsString));
end;

{$ENDREGION}


{$REGION 'TObjectToStringConverter'}

function TObjectToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(value.AsObject.ToString);
{$IFNDEF NEXTGEN}
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(value.AsObject.ToString));
    tkWString:
      Result := TValue.From<WideString>(value.AsObject.ToString);
{$ENDIF}
  end;
end;

{$ENDREGION}


{$REGION 'TObjectToInterfaceConverter'}

function TObjectToInterfaceConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  guid: TGUID;
  p: Pointer;
begin
  guid := GetTypeData(targetTypeInfo)^.Guid;
  if value.AsObject.GetInterface(guid, p) then
    TValue.MakeWithoutCopy(@p, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TInterfaceToObjectConverter'}

function TInterfaceToObjectConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TObject>(TObject(value.AsInterface));
end;

{$ENDREGION}


{$REGION 'TObjectToClassConverter'}

function TObjectToClassConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<TClass>(value.AsObject.ClassType);
end;

{$ENDREGION}


{$REGION 'TStringToWStringConverter'}

{$IFNDEF NEXTGEN}
function TStringToWStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  Result := TValue.From<WideString>(value.AsString);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TWStringToStringConverter'}

{$IFNDEF NEXTGEN}
function TWStringToStringConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
begin
  case targetTypeInfo.Kind of
    tkString, tkUString:
      Result := TValue.From<string>(value.AsString);
    tkLString:
      Result := TValue.From<AnsiString>(AnsiString(value.AsString));
    tkWString:
      Result := TValue.From<WideString>(value.AsString);
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TInterfaceToInterfaceConverter'}

function TInterfaceToInterfaceConverter.DoConvertTo(const value: TValue;
  const targetTypeInfo: PTypeInfo; const parameter: TValue): TValue;
var
  intf: IInterface;
begin
  TryGetInterface(value, TType.GetType(targetTypeInfo).AsInterface.GUID, intf);
  TValue.Make(@intf, targetTypeInfo, Result);
end;

{$ENDREGION}


{$REGION 'TValueConverterFactory'}

class constructor TValueConverterFactory.Create;
begin
  fTypeInfoToTypeInfoRegistry := TDictionary<TConvertedTypeInfo, IConverterPackage>.Create;
  fTypeInfoToTypeKindsRegistry := TDictionary<TConvertedTypeInfo, IConverterPackage>.Create;
  fTypeKindsToTypeInfoRegistry := TDictionary<TConvertedTypeInfo, IConverterPackage>.Create;
  fTypeKindsToTypeKindsRegistry := TDictionary<TConvertedTypeInfo, IConverterPackage>.Create;

  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Integer>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.SmallInt>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.ShortInt>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.LongInt>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Cardinal>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Int64>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.UInt64>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Word>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Byte>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkClass, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.string>), TTypeToNullableConverter);
{$IFNDEF NEXTGEN}
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkClass, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.AnsiString>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkEnumeration, tkClass, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.WideString>), TTypeToNullableConverter);
{$ENDIF}
  RegisterConverter([tkInteger, tkInt64, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Extended>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Double>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TypeInfo(Nullable<System.Single>), TTypeToNullableConverter);
  RegisterConverter([tkInteger, tkInt64], TypeInfo(Boolean), TIntegerToBooleanConverter);
  RegisterConverter([tkInteger, tkInt64], [tkEnumeration], TIntegerToEnumConverter);
  RegisterConverter([tkInteger, tkInt64], [tkString, tkUString, tkLString, tkWString], TIntegerToStringConverter);

  RegisterConverter([tkFloat], [tkString, tkUString, tkLString, tkWString], TFloatToStringConverter);
  RegisterConverter([tkFloat], [tkInteger, tkInt64], TFloatToIntegerConverter);

  RegisterConverter(TypeInfo(TColor), TypeInfo(Nullable<TColor>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(TColor), [tkString, tkUString, tkLString, tkWString], TColorToStringConverter);

  RegisterConverter(TypeInfo(Currency), [tkString, tkUString, tkLString, tkWString], TCurrencyToStringConverter);

  RegisterConverter(TypeInfo(TDateTime), [tkString, tkUString, tkLString, tkWString], TDateTimeToStringConverter);

  RegisterConverter(TypeInfo(Boolean), TypeInfo(Nullable<System.Boolean>), TTypeToNullableConverter);
  RegisterConverter(TypeInfo(Boolean), [tkString, tkUString, tkLString, tkWString], TBooleanToStringConverter);
  RegisterConverter(TypeInfo(Boolean), [tkInteger], TBooleanToIntegerConverter);

  RegisterConverter(TypeInfo(Nullable<System.Integer>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.SmallInt>), [
    tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.ShortInt>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.LongInt>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Cardinal>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Int64>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.UInt64>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Word>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Byte>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.string>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

{$IFNDEF NEXTGEN}
  RegisterConverter(TypeInfo(Nullable<System.AnsiString>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.WideString>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);
{$ENDIF}

  RegisterConverter(TypeInfo(Nullable<System.Single>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Double>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Extended>),
    [tkInteger, tkInt64, tkFloat, tkString, tkUString, tkLString, tkWString],
    TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.Boolean>), TypeInfo(Boolean), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.Boolean>), [tkInteger, tkInt64], TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.Boolean>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<TColor>), [tkInteger, tkInt64], TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<TColor>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter(TypeInfo(Nullable<System.TDateTime>), TypeInfo(TDate), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.TDateTime>), TypeInfo(TDateTime), TNullableToTypeConverter);
  RegisterConverter(TypeInfo(Nullable<System.TDateTime>), [tkString, tkUString, tkLString, tkWString], TNullableToTypeConverter);

  RegisterConverter([tkEnumeration], [tkString, tkUString, tkLString, tkWString], TEnumToStringConverter);
  RegisterConverter([tkEnumeration], [tkInteger, tkInt64], TEnumToIntegerConverter);

  RegisterConverter([tkSet], [tkString, tkUString, tkLString, tkWString], TSetToStringConverter);

  RegisterConverter([tkClass], [tkString, tkUString, tkLString, tkWString], TObjectToStringConverter);
  RegisterConverter([tkClass], [tkInterface], TObjectToInterfaceConverter);
  RegisterConverter([tkClass], [tkClassRef], TObjectToClassConverter);

  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Boolean), TStringToBooleanConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TColor), TStringToColorConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Currency), TStringToCurrencyConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(TDateTime), TStringToDateTimeConverter);

  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<System.Currency>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<TColor>), TTypeToNullableConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], TypeInfo(Nullable<System.TDateTime>), TTypeToNullableConverter);

  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkInteger, tkInt64], TStringToIntegerConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkFloat], TStringToFloatConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkEnumeration], TStringToEnumConverter);
  RegisterConverter([tkString, tkUString, tkLString, tkWString], [tkSet], TStringToSetConverter);

{$IFNDEF NEXTGEN}
  RegisterConverter([tkString, tkUString, tkLString], [tkWString], TStringToWStringConverter);
  RegisterConverter([tkWString], [tkString, tkUString, tkLString], TWStringToStringConverter);
{$ENDIF}

  RegisterConverter([tkInterface], [tkInterface], TInterfaceToInterfaceConverter);
  RegisterConverter([tkInterface], [tkClass], TInterfaceToObjectConverter);
end;

class destructor TValueConverterFactory.Destroy;
begin
  fTypeInfoToTypeInfoRegistry.Free;
  fTypeInfoToTypeKindsRegistry.Free;
  fTypeKindsToTypeInfoRegistry.Free;
  fTypeKindsToTypeKindsRegistry.Free;
end;

class function TValueConverterFactory.GetConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo): IValueConverter;
var
  typeInfoPair: TPair<TConvertedTypeInfo, IConverterPackage>;
begin
  System.MonitorEnter(fTypeInfoToTypeInfoRegistry);
  try
    for typeInfoPair in fTypeInfoToTypeInfoRegistry do
    begin
      if CompareTypeInfo(typeInfoPair.Key.SourceTypeInfo, sourceTypeInfo) and
        CompareTypeInfo(typeInfoPair.Key.TargetTypeInfo, targetTypeInfo) then
      begin
        fTypeInfoToTypeInfoRegistry.AddOrSetValue(typeInfoPair.Key, typeInfoPair.Value);
        Exit(typeInfoPair.Value.Instance);
      end;
    end;
  finally
    System.MonitorExit(fTypeInfoToTypeInfoRegistry);
  end;

  System.MonitorEnter(fTypeInfoToTypeKindsRegistry);
  try
    for typeInfoPair in fTypeInfoToTypeKindsRegistry do
    begin
      if CompareTypeInfo(typeInfoPair.Key.SourceTypeInfo, sourceTypeInfo) and
        (targetTypeInfo.Kind in typeInfoPair.Key.TargetTypeKinds) then
      begin
        fTypeInfoToTypeKindsRegistry.AddOrSetValue(typeInfoPair.Key, typeInfoPair.Value);
        Exit(typeInfoPair.Value.Instance);
      end;
    end;
  finally
    System.MonitorExit(fTypeInfoToTypeKindsRegistry);
  end;

  System.MonitorEnter(fTypeKindsToTypeInfoRegistry);
  try
    for typeInfoPair in fTypeKindsToTypeInfoRegistry do
    begin
      if (sourceTypeInfo.Kind in typeInfoPair.Key.SourceTypeKinds) and
        CompareTypeInfo(typeInfoPair.Key.TargetTypeInfo, targetTypeInfo) then
      begin
        fTypeKindsToTypeInfoRegistry.AddOrSetValue(typeInfoPair.Key, typeInfoPair.Value);
        Exit(typeInfoPair.Value.Instance);
      end;
    end;
  finally
    System.MonitorExit(fTypeKindsToTypeInfoRegistry);
  end;

  System.MonitorEnter(fTypeKindsToTypeKindsRegistry);
  try
    for typeInfoPair in fTypeKindsToTypeKindsRegistry do
    begin
      if (sourceTypeInfo.Kind in typeInfoPair.Key.SourceTypeKinds) and
        (targetTypeInfo.Kind in typeInfoPair.Key.TargetTypeKinds) then
      begin
        fTypeKindsToTypeKindsRegistry.AddOrSetValue(typeInfoPair.Key, typeInfoPair.Value);
        Exit(typeInfoPair.Value.Instance);
      end;
    end;
  finally
    System.MonitorExit(fTypeKindsToTypeKindsRegistry);
  end;
end;

class function TValueConverterFactory.CreateConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo): IValueConverter;
begin
  Result := GetConverter(sourceTypeInfo, targetTypeInfo);
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeInfo,
  targetTypeInfo: PTypeInfo; converterClass: TConverterClass);
var
  key: TConvertedTypeInfo;
begin
  Guard.CheckNotNull(sourceTypeInfo, 'sourceTypeInfo');
  Guard.CheckNotNull(targetTypeInfo, 'targetTypeInfo');

  System.MonitorEnter(fTypeInfoToTypeInfoRegistry);
  try
    key.SourceTypeInfo := sourceTypeInfo;
    key.TargetTypeInfo := targetTypeInfo;
    fTypeInfoToTypeInfoRegistry.AddOrSetValue(key,
      TConverterPackage.Create(converterClass));
  finally
    System.MonitorExit(fTypeInfoToTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  const sourceTypeInfo: PTypeInfo; targetTypeKinds: TTypeKinds;
  converterClass: TConverterClass);
var
  key: TConvertedTypeInfo;
begin
  Guard.CheckFalse(SizeOf(targetTypeKinds) = 0, SEmptySourceTypeKind);
  Guard.CheckNotNull(sourceTypeInfo, 'targetTypeInfo');

  System.MonitorEnter(fTypeInfoToTypeKindsRegistry);
  try
    key.SourceTypeInfo := sourceTypeInfo;
    key.TargetTypeKinds := targetTypeKinds;
    fTypeInfoToTypeKindsRegistry.AddOrSetValue(key,
      TConverterPackage.Create(converterClass));
  finally
    System.MonitorExit(fTypeInfoToTypeKindsRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(
  const sourceTypeKinds: TTypeKinds; targetTypeInfo: PTypeInfo;
  converterClass: TConverterClass);
var
  key: TConvertedTypeInfo;
begin
  Guard.CheckFalse(SizeOf(sourceTypeKinds) = 0, SEmptySourceTypeKind);
  Guard.CheckNotNull(targetTypeInfo, 'targetTypeInfo');

  System.MonitorEnter(fTypeKindsToTypeInfoRegistry);
  try
    key.SourceTypeKinds := sourceTypeKinds;
    key.TargetTypeInfo := targetTypeInfo;
    fTypeKindsToTypeInfoRegistry.AddOrSetValue(key,
      TConverterPackage.Create(converterClass));
  finally
    System.MonitorExit(fTypeKindsToTypeInfoRegistry);
  end;
end;

class procedure TValueConverterFactory.RegisterConverter(const sourceTypeKinds,
  targetTypeKinds: TTypeKinds; converterClass: TConverterClass);
var
  key: TConvertedTypeInfo;
begin
  Guard.CheckFalse(SizeOf(sourceTypeKinds) = 0, SEmptySourceTypeKind);
  Guard.CheckFalse(SizeOf(targetTypeKinds) = 0, SEmptyTargetTypeKind);

  System.MonitorEnter(fTypeKindsToTypeKindsRegistry);
  try
    key.SourceTypeKinds := sourceTypeKinds;
    key.TargetTypeKinds := targetTypeKinds;
    fTypeKindsToTypeKindsRegistry.AddOrSetValue(key,
      TConverterPackage.Create(converterClass));
  finally
    System.MonitorExit(fTypeKindsToTypeKindsRegistry);
  end;
end;

{$ENDREGION}


{$REGION 'TValueConverterFactory.TConverterPackage'}

constructor TValueConverterFactory.TConverterPackage.Create(
  classType: TConverterClass);
begin
  fConverterClass := classType;
end;

function TValueConverterFactory.TConverterPackage.GetInstance: IValueConverter;
begin
  if not Assigned(fConverter) then
    fConverter := fConverterClass.Create;

  Result := fConverter;
end;

{$ENDREGION}


end.
