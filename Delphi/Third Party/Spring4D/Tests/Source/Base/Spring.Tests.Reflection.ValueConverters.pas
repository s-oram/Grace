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

unit Spring.Tests.Reflection.ValueConverters;

interface

uses
  SysUtils,
  TestFramework,
  Spring.Reflection.ValueConverters;

type
  TEnumeration = (teFirst, teSecond, teLast);

  TTestFromString = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestStringToInt64;
    procedure TestStringToUInt64;
    procedure TestStringToInteger;
    procedure TestStringToCardinal;
    procedure TestStringToSmallInt;
    procedure TestStringToWord;
    procedure TestStringToShortInt;
    procedure TestStringToByte;
    procedure TestStringToFloat;
    procedure TestStringToEnum;
    procedure TestStringToBoolean;
    procedure TestStringToColor;
    procedure TestStringToCurrency;
    procedure TestStringToDateTime;
    procedure TestStringToDateTimeF;
{$IFNDEF NEXTGEN}
    procedure TestStringToWideString;
    procedure TestStringToAnsiString;
{$ENDIF}
    procedure TestStringToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestStringToNullableAnsiString;
    procedure TestStringToNullableWideString;
{$ENDIF}
    procedure TestStringToNullableInteger;
    procedure TestStringToNullableCardinal;
    procedure TestStringToNullableInt64;
    procedure TestStringToNullableUInt64;
    procedure TestStringToNullableSmallInt;
    procedure TestStringToNullableWord;
    procedure TestStringToNullableShortInt;
    procedure TestStringToNullableByte;
    procedure TestStringToNullableFloat;
    procedure TestStringToNullableBoolean;
    procedure TestStringToNullableColor;
    procedure TestStringToNullableCurrency;
    procedure TestStringToNullableDateTime;
  end;

{$IFNDEF NEXTGEN}
  TTestFromWideString = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestWStringToInteger;
    procedure TestWStringToCardinal;
    procedure TestWStringToSmallInt;
    procedure TestWStringToShortInt;
    procedure TestWStringToFloat;
    procedure TestWStringToEnum;
    procedure TestWStringToBoolean;
    procedure TestWStringToColor;
    procedure TestWStringToCurrency;
    procedure TestWStringToDateTime;
    procedure TestWStringToDateTimeF;
    procedure TestWStringToString;
    procedure TestWStringToAnsiString;
    procedure TestWStringToNullableString;
    procedure TestWStringToNullableAnsiString;
    procedure TestWStringToNullableWideString;
    procedure TestWStringToNullableInteger;
    procedure TestWStringToNullableCardinal;
    procedure TestWStringToNullableInt64;
    procedure TestWStringToNullableUInt64;
    procedure TestWStringToNullableWord;
    procedure TestWStringToNullableByte;
    procedure TestWStringToNullableSmallInt;
    procedure TestWStringToNullableShortInt;
    procedure TestWStringToNullableFloat;
    procedure TestWStringToNullableBoolean;
    procedure TestWStringToNullableColor;
    procedure TestWStringToNullableCurrency;
    procedure TestWStringToNullableDateTime;
  end;
{$ENDIF}

  TTestFromInteger = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestIntegerToString;
{$IFNDEF NEXTGEN}
    procedure TestIntegerToWideString;
    procedure TestIntegerToAnsiString;
{$ENDIF}
    procedure TestIntegerToEnum;
    procedure TestIntegerToBoolean;
    procedure TestIntegerToFloat;
    procedure TestIntegerToNullableInteger;
    procedure TestIntegerToNullableCardinal;
    procedure TestIntegerToNullableInt64;
    procedure TestIntegerToNullableUInt64;
    procedure TestIntegerToNullableWord;
    procedure TestIntegerToNullableByte;
    procedure TestIntegerToNullableSmallInt;
    procedure TestIntegerToNullableShortInt;
    procedure TestIntegerToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestIntegerToNullableAnsiString;
    procedure TestIntegerToNullableWideString;
{$ENDIF}
    procedure TestIntegerToNullableBoolean;
    procedure TestIntegerToNullableFloat;
  end;

  TTestFromCardinal = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestCardinalToString;
{$IFNDEF NEXTGEN}
    procedure TestCardinalToWideString;
    procedure TestCardinalToAnsiString;
{$ENDIF}
    procedure TestCardinalToEnum;
    procedure TestCardinalToBoolean;
    procedure TestCardinalToFloat;
    procedure TestCardinalToNullableInteger;
    procedure TestCardinalToNullableCardinal;
    procedure TestCardinalToNullableInt64;
    procedure TestCardinalToNullableUInt64;
    procedure TestCardinalToNullableWord;
    procedure TestCardinalToNullableByte;
    procedure TestCardinalToNullableSmallInt;
    procedure TestCardinalToNullableShortInt;
    procedure TestCardinalToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestCardinalToNullableAnsiString;
    procedure TestCardinalToNullableWideString;
{$ENDIF}
    procedure TestCardinalToNullableBoolean;
    procedure TestCardinalToNullableFloat;
  end;

  TTestFromSmallInt = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestSmallIntToString;
{$IFNDEF NEXTGEN}
    procedure TestSmallIntToWideString;
    procedure TestSmallIntToAnsiString;
{$ENDIF}
    procedure TestSmallIntToEnum;
    procedure TestSmallIntToBoolean;
    procedure TestSmallIntToFloat;
    procedure TestSmallIntToNullableInteger;
    procedure TestSmallIntToNullableCardinal;
    procedure TestSmallIntToNullableSmallInt;
    procedure TestSmallIntToNullableShortInt;
    procedure TestSmallIntToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestSmallIntToNullableAnsiString;
    procedure TestSmallIntToNullableWideString;
{$ENDIF}
    procedure TestSmallIntToNullableBoolean;
    procedure TestSmallIntToNullableFloat;
  end;

  TTestFromShortInt = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestShortIntToString;
{$IFNDEF NEXTGEN}
    procedure TestShortIntToWideString;
    procedure TestShortIntToAnsiString;
{$ENDIF}
    procedure TestShortIntToEnum;
    procedure TestShortIntToBoolean;
    procedure TestShortIntToFloat;
    procedure TestShortIntToNullableInteger;
    procedure TestShortIntToNullableCardinal;
    procedure TestShortIntToNullableSmallInt;
    procedure TestShortIntToNullableShortInt;
    procedure TestShortIntToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestShortIntToNullableAnsiString;
    procedure TestShortIntToNullableWideString;
{$ENDIF}
    procedure TestShortIntToNullableBoolean;
    procedure TestShortIntToNullableFloat;
  end;

  TTestFromBoolean = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestBooleanToInteger;
    procedure TestBooleanToCardinal;
    procedure TestBooleanToSmallInt;
    procedure TestBooleanToShortInt;
    procedure TestBooleanToString;
{$IFNDEF NEXTGEN}
    procedure TestBooleanToWideString;
    procedure TestBooleanToAnsiString;
{$ENDIF}
    procedure TestBooleanToNullableBoolean;
    procedure TestBooleanToNullableInteger;
    procedure TestBooleanToNullableCardinal;
    procedure TestBooleanToNullableSmallInt;
    procedure TestBooleanToNullableShortInt;
    procedure TestBooleanToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestBooleanToNullableAnsiString;
    procedure TestBooleanToNullableWideString;
{$ENDIF}
  end;

  TTestFromEnum = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestEnumToInteger;
    procedure TestEnumToCardinal;
    procedure TestEnumToSmallInt;
    procedure TestEnumToShortInt;
    procedure TestEnumToString;
{$IFNDEF NEXTGEN}
    procedure TestEnumToAnsiString;
    procedure TestEnumToWideString;
{$ENDIF}
    procedure TestEnumToNullableInteger;
    procedure TestEnumToNullableCardinal;
    procedure TestEnumToNullableSmallInt;
    procedure TestEnumToNullableShortInt;
    procedure TestEnumToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestEnumToNullableAnsiString;
    procedure TestEnumToNullableWideString;
{$ENDIF}
  end;

  TTestFromFloat = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestFloatToInteger;
    procedure TestFloatToCardinal;
    procedure TestFloatToSmallInt;
    procedure TestFloatToShortInt;
    procedure TestFloatToString;
{$IFNDEF NEXTGEN}
    procedure TestFloatToAnsiString;
    procedure TestFloatToWideString;
{$ENDIF}
    procedure TestFloatToStringF;
    procedure TestFloatToNullableInteger;
    procedure TestFloatToNullableCardinal;
    procedure TestFloatToNullableSmallInt;
    procedure TestFloatToNullableShortInt;
    procedure TestFloatToNullableFloat;
    procedure TestFloatToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestFloatToNullableAnsiString;
    procedure TestFloatToNullableWideString;
{$ENDIF}
  end;

  TTestFromColor = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestColorToInteger;
    procedure TestColorToCardinal;
    procedure TestColorToSmallInt;
    procedure TestColorToString;
{$IFNDEF NEXTGEN}
    procedure TestColorToAnsiString;
    procedure TestColorToWideString;
{$ENDIF}
    procedure TestColorToNullableColor;
    procedure TestColorToNullableInteger;
    procedure TestColorToNullableCardinal;
    procedure TestColorToNullableSmallInt;
    procedure TestColorToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestColorToNullableAnsiString;
    procedure TestColorToNullableWideString;
{$ENDIF}
  end;

  TTestFromCurrency = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestCurrencyToString;
{$IFNDEF NEXTGEN}
    procedure TestCurrencyToAnsiString;
    procedure TestCurrencyToWideString;
{$ENDIF}
    procedure TestCurrencyToStringF;
    procedure TestCurrencyToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestCurrencyToNullableAnsiString;
    procedure TestCurrencyToNullableWideString;
{$ENDIF}
  end;

  TTestFromDateTime = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestDateTimeToString;
{$IFNDEF NEXTGEN}
    procedure TestDateTimeToAnsiString;
    procedure TestDateTimeToWideString;
{$ENDIF}
    procedure TestDateTimeToStringF;
    procedure TestDateTimeToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestDateTimeToNullableAnsiString;
    procedure TestDateTimeToNullableWideString;
{$ENDIF}
  end;

  TTestFromObject = class(TTestCase)
  strict private
    fConverter: IValueConverter;

    type
      ITestInterface = interface
      ['{F98EAF66-33B6-4687-8052-4B76471D978B}']
        procedure Test;
      end;

      TTestObject = class(TInterfacedObject, ITestInterface)
      strict protected
        procedure Test;
      public
        function ToString: string; override;
      end;
  strict protected
    procedure SetUp; override;
  published
    procedure TestObjectToString;
{$IFNDEF NEXTGEN}
    procedure TestObjectToAnsiString;
    procedure TestObjectToWideString;
{$ENDIF}
    procedure TestObjectToInterface;
    procedure TestObjectToClass;
    procedure TestObjectToNullableString;
{$IFNDEF NEXTGEN}
    procedure TestObjectToNullableAnsiString;
    procedure TestObjectToNullableWideString;
{$ENDIF}
  end;

  TTestFromInterface = class(TTestCase)
  strict private
    fConverter: IValueConverter;

    type
      ITestInterface = interface
      ['{F98EAF66-33B6-4687-8052-4B76471D978B}']
        function ToString: string;
      end;

      TTestObject = class(TInterfacedObject, ITestInterface)
      public
        function ToString: string; override;
      end;
  strict protected
    procedure SetUp; override;
  published
    procedure TestInterfaceToObject;
  end;

  TTestFromNullable = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestNullableIntegerToInteger;
    procedure TestNullableIntegerToCardinal;
    procedure TestNullableIntegerToSmallInt;
    procedure TestNullableIntegerToShortInt;
    procedure TestNullableIntegerToFloat;
    procedure TestNullableIntegerToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableIntegerToWideString;
    procedure TestNullableIntegerToAnsiString;
{$ENDIF}
    procedure TestNullableCardinalToInteger;
    procedure TestNullableCardinalToCardinal;
    procedure TestNullableCardinalToSmallInt;
    procedure TestNullableCardinalToShortInt;
    procedure TestNullableCardinalToFloat;
    procedure TestNullableCardinalToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableCardinalToWideString;
    procedure TestNullableCardinalToAnsiString;
{$ENDIF}
    procedure TestNullableSmallIntToInteger;
    procedure TestNullableSmallIntToCardinal;
    procedure TestNullableSmallIntToSmallInt;
    procedure TestNullableSmallIntToShortInt;
    procedure TestNullableSmallIntToFloat;
    procedure TestNullableSmallIntToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableSmallIntToWideString;
    procedure TestNullableSmallIntToAnsiString;
{$ENDIF}
    procedure TestNullableShortIntToInteger;
    procedure TestNullableShortIntToCardinal;
    procedure TestNullableShortIntToSmallInt;
    procedure TestNullableShortIntToShortInt;
    procedure TestNullableShortIntToFloat;
    procedure TestNullableShortIntToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableShortIntToWideString;
    procedure TestNullableShortIntToAnsiString;
{$ENDIF}
    procedure TestNullableFloatToInteger;
    procedure TestNullableFloatToCardinal;
    procedure TestNullableFloatToSmallInt;
    procedure TestNullableFloatToShortInt;
    procedure TestNullableFloatToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableFloatToAnsiString;
    procedure TestNullableFloatToWideString;
{$ENDIF}
    procedure TestNullableStringToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableAnsiStringToString;
    procedure TestNullableWideStringToWideString;
{$ENDIF}
    procedure TestNullableStringToInteger;
    procedure TestNullableStringToCardinal;
    procedure TestNullableStringToSmallInt;
    procedure TestNullableStringToShortInt;
{$IFNDEF NEXTGEN}
    procedure TestNullableAnsiStringToInteger;
    procedure TestNullableAnsiStringToCardinal;
    procedure TestNullableAnsiStringToSmallInt;
    procedure TestNullableAnsiStringToShortInt;
    procedure TestNullableWideStringToInteger;
    procedure TestNullableWideStringToCardinal;
    procedure TestNullableWideStringToSmallInt;
    procedure TestNullableWideStringToShortInt;
{$ENDIF}
    procedure TestNullableStringToFloat;
{$IFNDEF NEXTGEN}
    procedure TestNullableAnsiStringToFloat;
    procedure TestNullableWideStringToFloat;
{$ENDIF}
    procedure TestNullableDateTimeToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableDateTimeToAnsiString;
    procedure TestNullableDateTimeToWideString;
{$ENDIF}
    procedure TestNullableColorToString;
{$IFNDEF NEXTGEN}
    procedure TestNullableColorToAnsiString;
    procedure TestNullableColorToWideString;
{$ENDIF}
  end;

  TTestCustomTypes = class(TTestCase)
  strict private
    fConverter: IValueConverter;
  strict protected
    procedure SetUp; override;
  published
    procedure TestCustomFloatToString;
    procedure TestStringToCustomFloat;
  end;

type
  CustomFloat = type Extended;

implementation

{$I 'spring.inc'}

{$DEFINE UseGenericUI} // for MACOS and other non Windows platforms
{$IFDEF MSWINDOWS}
  {$UNDEF UseGenericUI}
{$ENDIF MSWINDOWS}

uses
{$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  System.UIConsts,
{$ELSE}
  Graphics,
{$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  DateUtils,
  Spring;

const
// both UseGenericUI and non-UseGenericUI internally use
// function System.UIConsts.ColorToString(Color: TColor): string;
// which maps old style VCL color names to new-style System.UITypes.TColors consts,
// or old-style Vcl.Graphics TColor consts.
  RedString = 'clRed';
  BlueString = 'clBlue';
{$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  RedValue = TColors.Red;
  BlueValue = TColors.Blue;
{$ELSE}
  RedValue = clRed;
  BlueValue = clBlue;
{$ENDIF HAS_UNIT_SYSTEM_UITYPES}

{$IFDEF HAS_UNIT_SYSTEM_UITYPES}
function ColorToRGB(i : TColor) : Longint;
begin
  if (Assigned(TColors.ColorToRGB)) then
    Result := TColors.ColorToRGB(i)
  else Result := i;
end;
{$ENDIF}


{$REGION 'TTestFromString'}

procedure TTestFromString.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromString.TestStringToInt64;
var
  outValue: TValue;
  outInt: Int64;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Int64));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Int64>(outInt));
  CheckEquals(1, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('9223372036854775807'),
    TypeInfo(Int64));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Int64>(outInt));
  CheckEquals(9223372036854775807, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('-9223372036854775808'),
    TypeInfo(Int64));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Int64>(outInt));
  CheckEquals(-9223372036854775808, outInt);
end;

procedure TTestFromString.TestStringToUInt64;
var
  outValue: TValue;
  outInt: UInt64;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(UInt64));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<UInt64>(outInt));
  CheckEquals(1, outInt);

  {outValue := fConverter.ConvertTo(TValue.From<string>('18446744073709551615'),
    TypeInfo(UInt64));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<UInt64>(outInt));
  CheckTrue(outInt = 18446744073709551615);}
end;

procedure TTestFromString.TestStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('2147483647'),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(2147483647, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('-2147483648'),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(-2147483648, outInt);
end;

procedure TTestFromString.TestStringToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('4294967295'),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(4294967295, outInt);
end;

procedure TTestFromString.TestStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(1, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('32767'),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(32767, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('-32768'),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(-32768, outInt);
end;

procedure TTestFromString.TestStringToWord;
var
  outValue: TValue;
  outInt: Word;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Word));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Word>(outInt));
  CheckEquals(1, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('65535'),
    TypeInfo(Word));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Word>(outInt));
  CheckEquals(65535, outInt);
end;

procedure TTestFromString.TestStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('127'),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(127, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('-128'),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(-128, outInt);
end;

procedure TTestFromString.TestStringToByte;
var
  outValue: TValue;
  outInt: Byte;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('1'),
    TypeInfo(Byte));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Byte>(outInt));
  CheckEquals(1, outInt);

  outValue := fConverter.ConvertTo(TValue.From<string>('255'),
    TypeInfo(Byte));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Byte>(outInt));
  CheckEquals(255, outInt);
end;

procedure TTestFromString.TestStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.11, outFloat);
end;

procedure TTestFromString.TestStringToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('teLast'),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teLast);
end;

procedure TTestFromString.TestStringToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('True'),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(True, outBool);
end;

procedure TTestFromString.TestStringToColor;
var
  outValue: TValue;
  outColor: TColor;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(BlueString),
    TypeInfo(TColor));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TColor>(outColor));
  CheckEquals(BlueValue, outColor);
end;

procedure TTestFromString.TestStringToCurrency;
var
  outValue: TValue;
  outCurrency: Currency;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Currency));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Currency>(outCurrency));
  CheckEquals(1.11, outCurrency);
end;

procedure TTestFromString.TestStringToDateTime;
var
  outValue: TValue;
  outStamp: TDateTime;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<string>(DateTimeToStr(stamp)),
    TypeInfo(TDateTime));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString(DateTimeToStr(stamp), DateTimeToStr(outStamp));
end;

procedure TTestFromString.TestStringToDateTimeF;
var
  outValue: TValue;
  outStamp: TDateTime;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('10.10.2010'),
    TypeInfo(TDateTime), TValue.From<string>('dd.mm.yyyy'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString('10.10.2010', FormatDateTime('dd.mm.yyyy', outStamp));
end;

{$IFNDEF NEXTGEN}
procedure TTestFromString.TestStringToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test AnsiString'),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEqualsString('Test AnsiString', string(outAStr));
end;

procedure TTestFromString.TestStringToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<UnicodeString>('Test WideString'),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString('Test WideString'), outWStr);
end;
{$ENDIF NEXTGEN}

procedure TTestFromString.TestStringToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals('Test', outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromString.TestStringToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(AnsiString('Test'), outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('Test'),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(WideString('Test'), outNullable.Value);
end;
{$ENDIF}

procedure TTestFromString.TestStringToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableInt64;
var
  outValue: TValue;
  outNullable: Nullable<Int64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableUInt64;
var
  outValue: TValue;
  outNullable: Nullable<UInt64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<UInt64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<UInt64>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableWord;
var
  outValue: TValue;
  outNullable: Nullable<Word>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<Word>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Word>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableByte;
var
  outValue: TValue;
  outNullable: Nullable<Byte>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('15'),
    TypeInfo(Nullable<Byte>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Byte>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(1.11, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>('True'),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckEquals(True, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableColor;
var
  outValue: TValue;
  outNullable: Nullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(ColorToString(RedValue)),
    TypeInfo(Nullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TColor>>(outNullable));
  CheckEquals(RedString, ColorToString(outNullable.Value));
end;

procedure TTestFromString.TestStringToNullableCurrency;
var
  outValue: TValue;
  outNullable: Nullable<Currency>;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(Nullable<Currency>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Currency>>(outNullable));
  CheckEquals(1.11, outNullable.Value);
end;

procedure TTestFromString.TestStringToNullableDateTime;
var
  outValue: TValue;
  outNullable: Nullable<TDateTime>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<string>(DateTimeToStr(stamp)),
    TypeInfo(Nullable<TDateTime>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TDateTime>>(outNullable));
  CheckEqualsString(DateTimeToStr(stamp), DateTimeToStr(outNullable.Value));
end;

{$ENDREGION}


{$REGION 'TTestFromInteger'}

procedure TTestFromInteger.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromInteger.TestIntegerToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(True, outBool);
end;

procedure TTestFromInteger.TestIntegerToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromInteger.TestIntegerToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.0, outValue.AsExtended);
end;

procedure TTestFromInteger.TestIntegerToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromInteger.TestIntegerToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;

procedure TTestFromInteger.TestIntegerToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;
{$ENDIF}

procedure TTestFromInteger.TestIntegerToNullableInt64;
var
  outValue: TValue;
  outNullable: Nullable<Int64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(1, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<Integer>(2147483647),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(2147483647, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<Integer>(-2147483647),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(-2147483647, outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString('1', outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableUInt64;
var
  outValue: TValue;
  outNullable: Nullable<UInt64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<UInt64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<UInt64>>(outNullable));
  CheckEquals(1, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<Integer>(2147483647),
    TypeInfo(Nullable<UInt64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<UInt64>>(outNullable));
  CheckEquals(2147483647, outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromInteger.TestIntegerToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString('1'), outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals('1', outNullable.Value);
end;
{$ENDIF}

procedure TTestFromInteger.TestIntegerToNullableWord;
var
  outValue: TValue;
  outInt: Nullable<Word>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<Word>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Word>>(outInt));
  CheckEquals(1, outInt.Value);

  outValue := fConverter.ConvertTo(TValue.From<Integer>(65535),
    TypeInfo(Nullable<Word>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Word>>(outInt));
  CheckEquals(65535, outInt.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableByte;
var
  outValue: TValue;
  outInt: Nullable<Byte>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(1),
    TypeInfo(Nullable<Byte>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Byte>>(outInt));
  CheckEquals(1, outInt.Value);

  outValue := fConverter.ConvertTo(TValue.From<Integer>(255),
    TypeInfo(Nullable<Byte>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Byte>>(outInt));
  CheckEquals(255, outInt.Value);
end;

procedure TTestFromInteger.TestIntegerToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Integer>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(0.0, outNullable.Value);
end;

{$ENDREGION}


{$REGION 'TTestFromCardinal'}

procedure TTestFromCardinal.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromCardinal.TestCardinalToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(True, outBool);
end;

procedure TTestFromCardinal.TestCardinalToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromCardinal.TestCardinalToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.0, outValue.AsExtended);
end;

procedure TTestFromCardinal.TestCardinalToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromCardinal.TestCardinalToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;

procedure TTestFromCardinal.TestCardinalToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;
{$ENDIF}

procedure TTestFromCardinal.TestCardinalToNullableInt64;
var
  outValue: TValue;
  outNullable: Nullable<Int64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(1, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(2147483647),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(2147483647, outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString('1', outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableUInt64;
var
  outValue: TValue;
  outNullable: Nullable<UInt64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<UInt64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<UInt64>>(outNullable));
  CheckEquals(1, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(2147483647),
    TypeInfo(Nullable<UInt64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<UInt64>>(outNullable));
  CheckEquals(2147483647, outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromCardinal.TestCardinalToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString('1'), outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals('1', outNullable.Value);
end;
{$ENDIF}

procedure TTestFromCardinal.TestCardinalToNullableWord;
var
  outValue: TValue;
  outInt: Nullable<Word>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<Word>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Word>>(outInt));
  CheckEquals(1, outInt.Value);

  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(65535),
    TypeInfo(Nullable<Word>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Word>>(outInt));
  CheckEquals(65535, outInt.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableByte;
var
  outValue: TValue;
  outInt: Nullable<Byte>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(1),
    TypeInfo(Nullable<Byte>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Byte>>(outInt));
  CheckEquals(1, outInt.Value);

  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(255),
    TypeInfo(Nullable<Byte>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Byte>>(outInt));
  CheckEquals(255, outInt.Value);
end;

procedure TTestFromCardinal.TestCardinalToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Cardinal>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(0.0, outNullable.Value);
end;

{$ENDREGION}


{$REGION 'TTestFromSmallInt'}

procedure TTestFromSmallInt.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromSmallInt.TestSmallIntToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(True, outBool);
end;

procedure TTestFromSmallInt.TestSmallIntToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromSmallInt.TestSmallIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.0, outValue.AsExtended);
end;

procedure TTestFromSmallInt.TestSmallIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromSmallInt.TestSmallIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;

procedure TTestFromSmallInt.TestSmallIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;
{$ENDIF}

procedure TTestFromSmallInt.TestSmallIntToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString('1', outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromSmallInt.TestSmallIntToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString('1'), outNullable.Value);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals('1', outNullable.Value);
end;
{$ENDIF}

procedure TTestFromSmallInt.TestSmallIntToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromSmallInt.TestSmallIntToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<SmallInt>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(0.0, outNullable.Value);
end;

{$ENDREGION}


{$REGION 'TTestFromShortInt'}

procedure TTestFromShortInt.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromShortInt.TestShortIntToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(True, outBool);
end;

procedure TTestFromShortInt.TestShortIntToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teSecond);
end;

procedure TTestFromShortInt.TestShortIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.0, outValue.AsExtended);
end;

procedure TTestFromShortInt.TestShortIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromShortInt.TestShortIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;

procedure TTestFromShortInt.TestShortIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;
{$ENDIF}

procedure TTestFromShortInt.TestShortIntToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromShortInt.TestShortIntToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromShortInt.TestShortIntToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromShortInt.TestShortIntToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromShortInt.TestShortIntToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString('1', outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromShortInt.TestShortIntToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString('1'), outNullable.Value);
end;

procedure TTestFromShortInt.TestShortIntToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(1),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals('1', outNullable.Value);
end;
{$ENDIF}

procedure TTestFromShortInt.TestShortIntToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(0),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromShortInt.TestShortIntToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<ShortInt>(0),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(0.0, outNullable.Value);
end;

{$ENDREGION}


{$REGION 'TTestFromBoolean'}

procedure TTestFromBoolean.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromBoolean.TestBooleanToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(0, outInt);
end;

procedure TTestFromBoolean.TestBooleanToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(0, outInt);
end;

procedure TTestFromBoolean.TestBooleanToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(0, outInt);
end;

procedure TTestFromBoolean.TestBooleanToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(0, outInt);
end;

procedure TTestFromBoolean.TestBooleanToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString('False', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromBoolean.TestBooleanToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('False'), outAStr);
end;

procedure TTestFromBoolean.TestBooleanToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString('False'), outWStr);
end;
{$ENDIF}

procedure TTestFromBoolean.TestBooleanToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckFalse(outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(0, outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(0, outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(0, outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(0, outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString('False', outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromBoolean.TestBooleanToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString('False'), outNullable.Value);
end;

procedure TTestFromBoolean.TestBooleanToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Boolean>(False),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEqualsString('False', outNullable.Value);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestFromEnum'}

procedure TTestFromEnum.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromEnum.TestEnumToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('teFirst', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromEnum.TestEnumToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('teFirst'), outAStr);
end;

procedure TTestFromEnum.TestEnumToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString('teFirst'), outWStr);
end;
{$ENDIF}

procedure TTestFromEnum.TestEnumToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromEnum.TestEnumToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromEnum.TestEnumToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromEnum.TestEnumToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teSecond),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromEnum.TestEnumToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString('teFirst', outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromEnum.TestEnumToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString('teFirst'), outNullable.Value);
end;

procedure TTestFromEnum.TestEnumToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teFirst),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(WideString('teFirst'), outNullable.Value);
end;
{$ENDIF}

procedure TTestFromEnum.TestEnumToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(2, outNullable.Value);
end;

procedure TTestFromEnum.TestEnumToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(2, outNullable.Value);
end;

procedure TTestFromEnum.TestEnumToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(2, outNullable.Value);
end;

procedure TTestFromEnum.TestEnumToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TEnumeration>(teLast),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(2, outNullable.Value);
end;

{$ENDREGION}


{$REGION 'TTestFromFloat'}

procedure TTestFromFloat.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromFloat.TestFloatToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromFloat.TestFloatToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromFloat.TestFloatToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromFloat.TestFloatToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromFloat.TestFloatToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(FloatToStr(1.11), outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromFloat.TestFloatToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(FloatToStr(1.11)), outAStr);
end;

procedure TTestFromFloat.TestFloatToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString(FloatToStr(1.11)), outWStr);
end;
{$ENDIF}

procedure TTestFromFloat.TestFloatToStringF;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(string), TValue.From<string>('#.000'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(FormatFloat('#.000', 1.11), outStr);
end;

procedure TTestFromFloat.TestFloatToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(1.11, outNullable.Value);
end;

procedure TTestFromFloat.TestFloatToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(FloatToStr(1.11), outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromFloat.TestFloatToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString(FloatToStr(1.11)), outNullable.Value);
end;

procedure TTestFromFloat.TestFloatToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(WideString(FloatToStr(1.11)), outNullable.Value);
end;
{$ENDIF}

procedure TTestFromFloat.TestFloatToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromFloat.TestFloatToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromFloat.TestFloatToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

procedure TTestFromFloat.TestFloatToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Extended>(1.11),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckEquals(1, outNullable.Value);
end;

{$ENDREGION}


{$REGION 'TTestFromColor'}

procedure TTestFromColor.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromColor.TestColorToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(RedString, outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromColor.TestColorToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(RedString), outAStr);
end;

procedure TTestFromColor.TestColorToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEqualsString(RedString, outWStr);
end;
{$ENDIF}

procedure TTestFromColor.TestColorToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(ColorToRGB(RedValue), outInt);
end;

procedure TTestFromColor.TestColorToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(ColorToRGB(RedValue), outInt);
end;

procedure TTestFromColor.TestColorToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(ColorToRGB(RedValue), outInt);
end;

procedure TTestFromColor.TestColorToNullableColor;
var
  outValue: TValue;
  outNullable: Nullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Nullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TColor>>(outNullable));
  CheckEquals(RedValue, outNullable.Value);
end;

procedure TTestFromColor.TestColorToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(RedString, outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromColor.TestColorToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString(RedString), outNullable.Value);
end;

procedure TTestFromColor.TestColorToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEqualsString(RedString, outNullable.Value);
end;
{$ENDIF}

procedure TTestFromColor.TestColorToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckEquals(ColorToRGB(RedValue), outNullable.Value);
end;

procedure TTestFromColor.TestColorToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckEquals(ColorToRGB(RedValue), outNullable.Value);
end;

procedure TTestFromColor.TestColorToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<TColor>(RedValue),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckEquals(ColorToRGB(RedValue), outNullable.Value);
end;

{$ENDREGION}


{$REGION 'TTestFromCurrency'}

procedure TTestFromCurrency.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromCurrency.TestCurrencyToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(FloatToStr(1.11), outStr);
end;

procedure TTestFromCurrency.TestCurrencyToStringF;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.112),
    TypeInfo(string), TValue.From<string>('#.00 $'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(FormatCurr('#.00 $', 1.11), outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromCurrency.TestCurrencyToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(FloatToStr(1.11)), outAStr);
end;

procedure TTestFromCurrency.TestCurrencyToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEqualsString(FloatToStr(1.11), outWStr);
end;
{$ENDIF}

procedure TTestFromCurrency.TestCurrencyToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(CurrToStr(1.11), outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromCurrency.TestCurrencyToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(outNullable.Value, AnsiString(CurrToStr(1.11)));
end;

procedure TTestFromCurrency.TestCurrencyToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<Currency>(1.11),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEqualsString(CurrToStr(1.11), outNullable.Value);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestFromDateTime'}

procedure TTestFromDateTime.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromDateTime.TestDateTimeToString;
var
  outValue: TValue;
  outStr: string;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(DateTimeToStr(stamp), outStr);
end;

procedure TTestFromDateTime.TestDateTimeToStringF;
var
  outValue: TValue;
  outStr: string;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(string), TValue.From<string>('dd-mm-yyyy'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(FormatDateTime('dd-mm-yyyy', stamp), outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromDateTime.TestDateTimeToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(DateTimeToStr(stamp)), outAStr);
end;

procedure TTestFromDateTime.TestDateTimeToWideString;
var
  outValue: TValue;
  outWStr: WideString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEqualsString(DateTimeToStr(stamp), outWStr);
end;
{$ENDIF}

procedure TTestFromDateTime.TestDateTimeToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(DateTimeToStr(stamp), outNullable.Value);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromDateTime.TestDateTimeToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString(DateTimeToStr(stamp)), outNullable.Value);
end;

procedure TTestFromDateTime.TestDateTimeToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<TDateTime>(stamp),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEqualsString(DateTimeToStr(stamp), outNullable.Value);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestFromObject'}

  {$REGION 'TTestFromObject.TTestObject'}

  procedure TTestFromObject.TTestObject.Test;
  begin

  end;

  function TTestFromObject.TTestObject.ToString: string;
  begin
    inherited;
    Result := 'ObjectToString test case.';
  end;

  {$ENDREGION}

procedure TTestFromObject.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromObject.TestObjectToString;
var
  outValue: TValue;
  obj: TObject;
  outStr: string;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEqualsString(obj.ToString, outStr);
  obj.Free;
end;

{$IFNDEF NEXTGEN}
procedure TTestFromObject.TestObjectToAnsiString;
var
  outValue: TValue;
  obj: TObject;
  outAStr: AnsiString;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(obj.ToString), outAStr);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToWideString;
var
  outValue: TValue;
  obj: TObject;
  outWStr: WideString;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString(obj.ToString), outWStr);
  obj.Free;
end;
{$ENDIF}

procedure TTestFromObject.TestObjectToClass;
var
  outValue: TValue;
  obj: TObject;
  outClass: TClass;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(TClass));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TClass>(outClass));
  CheckTrue(outClass = obj.ClassType);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToInterface;
var
  outValue: TValue;
  obj: TTestObject;
  outIntf, objIntf: ITestInterface;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(ITestInterface));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ITestInterface>(outIntf));
  obj.QueryInterface(ITestInterface, objIntf);
  CheckSame(outIntf, objIntf);
end;

procedure TTestFromObject.TestObjectToNullableString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: Nullable<string>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckEqualsString(obj.ToString, outNullable.Value);
  obj.Free;
end;

{$IFNDEF NEXTGEN}
procedure TTestFromObject.TestObjectToNullableWideString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: Nullable<WideString>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckEquals(WideString(obj.ToString), outNullable.Value);
  obj.Free;
end;

procedure TTestFromObject.TestObjectToNullableAnsiString;
var
  outValue: TValue;
  obj: TObject;
  outNullable: Nullable<AnsiString>;
begin
  obj := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<TObject>(obj),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckEquals(AnsiString(obj.ToString), outNullable.Value);
  obj.Free;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestFromNullable'}

procedure TTestFromNullable.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromNullable.TestNullableIntegerToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(outInt, 1);
end;

procedure TTestFromNullable.TestNullableIntegerToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableIntegerToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableIntegerToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableIntegerToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.00, 1, outFloat);
end;

procedure TTestFromNullable.TestNullableIntegerToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableIntegerToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;

procedure TTestFromNullable.TestNullableIntegerToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Integer>>(Nullable<Integer>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;
{$ENDIF}

procedure TTestFromNullable.TestNullableFloatToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(FloatToStr(1.11), outStr);
end;

procedure TTestFromNullable.TestNullableFloatToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableFloatToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableFloatToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableFloatToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableFloatToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(FloatToStr(1.11)), outAStr);
end;

procedure TTestFromNullable.TestNullableFloatToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Extended>>(Nullable<Extended>.Create(1.11)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString(FloatToStr(1.11)), outWStr);
end;

procedure TTestFromNullable.TestNullableSmallIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;
{$ENDIF}

procedure TTestFromNullable.TestNullableSmallIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.00, 1, outFloat);
end;

procedure TTestFromNullable.TestNullableSmallIntToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableSmallIntToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableSmallIntToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableSmallIntToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableSmallIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableSmallIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<SmallInt>>(Nullable<SmallInt>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;

procedure TTestFromNullable.TestNullableShortIntToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;
{$ENDIF}

procedure TTestFromNullable.TestNullableShortIntToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.00, 1, outFloat);
end;

procedure TTestFromNullable.TestNullableShortIntToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableShortIntToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableShortIntToSmallInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableShortIntToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableShortIntToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableShortIntToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<ShortInt>>(Nullable<ShortInt>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;

procedure TTestFromNullable.TestNullableCardinalToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('1'), outAStr);
end;
{$ENDIF}

procedure TTestFromNullable.TestNullableCardinalToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.00, 1, outFloat);
end;

procedure TTestFromNullable.TestNullableCardinalToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableCardinalToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableCardinalToSmallInt;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableCardinalToShortInt;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromNullable.TestNullableCardinalToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('1', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableCardinalToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<Cardinal>>(Nullable<Cardinal>.Create(1)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals('1', outWStr);
end;
{$ENDIF}

procedure TTestFromNullable.TestNullableStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create(FloatToStr(1.11))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.11, outFloat);
end;

procedure TTestFromNullable.TestNullableStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableStringToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('2')),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableStringToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('Test')),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('Test', outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableAnsiStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableAnsiStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableAnsiStringToString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('Test')),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('Test'), outAStr);
end;

procedure TTestFromNullable.TestNullableWideStringToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<string>>(Nullable<string>.Create('Test')),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString('Test'), outWStr);
end;

procedure TTestFromNullable.TestNullableAnsiStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create(AnsiString(FloatToStr(1.11)))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.11, outFloat);
end;

procedure TTestFromNullable.TestNullableAnsiStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableAnsiStringToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<AnsiString>>(Nullable<AnsiString>.Create('2')),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableWideStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create(FloatToStr(1.11))),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.11, outFloat);
end;

procedure TTestFromNullable.TestNullableWideStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableWideStringToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableWideStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(2, outInt);
end;

procedure TTestFromNullable.TestNullableWideStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<WideString>>(Nullable<WideString>.Create('2')),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(2, outInt);
end;
{$ENDIF !NEXTGEN}


procedure TTestFromNullable.TestNullableDateTimeToString;
var
  outValue: TValue;
  outStr: string;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TDateTime>>(Nullable<TDateTime>.Create(stamp)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(DateTimeToStr(stamp), outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableDateTimeToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TDateTime>>(Nullable<TDateTime>.Create(stamp)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(DateTimeToStr(stamp)), outAStr);
end;

procedure TTestFromNullable.TestNullableDateTimeToWideString;
var
  outValue: TValue;
  outWStr: WideString;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TDateTime>>(Nullable<TDateTime>.Create(stamp)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString(DateTimeToStr(stamp)), outWStr);
end;
{$ENDIF}

procedure TTestFromNullable.TestNullableColorToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TColor>>(Nullable<TColor>.Create(RedValue)),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(RedString, outStr);
end;

{$IFNDEF NEXTGEN}
procedure TTestFromNullable.TestNullableColorToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TColor>>(Nullable<TColor>.Create(RedValue)),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString(RedString), outAStr);
end;

procedure TTestFromNullable.TestNullableColorToWideString;
var
  outValue: TValue;
  outWStr: WideString;
begin
  outValue := fConverter.ConvertTo(TValue.From<Nullable<TColor>>(Nullable<TColor>.Create(RedValue)),
    TypeInfo(WideString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<WideString>(outWStr));
  CheckEquals(WideString('clRed'), outWStr);
end;
{$ENDIF}

{$ENDREGION}


{$IFNDEF NEXTGEN}
{$REGION 'TTestFromWideString'}

procedure TTestFromWideString.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromWideString.TestWStringToAnsiString;
var
  outValue: TValue;
  outAStr: AnsiString;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test WideString'),
    TypeInfo(AnsiString));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<AnsiString>(outAStr));
  CheckEquals(AnsiString('Test WideString'), outAStr);
end;

procedure TTestFromWideString.TestWStringToBoolean;
var
  outValue: TValue;
  outBool: Boolean;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('True'),
    TypeInfo(Boolean));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Boolean>(outBool));
  CheckEquals(True, outBool);
end;

procedure TTestFromWideString.TestWStringToColor;
var
  outValue: TValue;
  outColor: TColor;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(BlueString),
    TypeInfo(TColor));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TColor>(outColor));
  CheckEquals(BlueValue, outColor);
end;

procedure TTestFromWideString.TestWStringToCurrency;
var
  outValue: TValue;
  outCurrency: Currency;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Currency));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Currency>(outCurrency));
  CheckEquals(1.11, outCurrency);
end;

procedure TTestFromWideString.TestWStringToDateTime;
var
  outValue: TValue;
  outStamp: TDateTime;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<WideString>(DateTimeToStr(stamp)),
    TypeInfo(TDateTime));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString(DateTimeToStr(stamp), DateTimeToStr(outStamp));
end;

procedure TTestFromWideString.TestWStringToDateTimeF;
var
  outValue: TValue;
  outStamp: TDateTime;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('10.10.2010'),
    TypeInfo(TDateTime), TValue.From<string>('dd.mm.yyyy'));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TDateTime>(outStamp));
  CheckEqualsString('10.10.2010', FormatDateTime('dd.mm.yyyy', outStamp));
end;

procedure TTestFromWideString.TestWStringToEnum;
var
  outValue: TValue;
  outEnum: TEnumeration;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('teLast'),
    TypeInfo(TEnumeration));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TEnumeration>(outEnum));
  CheckTrue(outEnum = teLast);
end;

procedure TTestFromWideString.TestWStringToFloat;
var
  outValue: TValue;
  outFloat: Extended;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Extended));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Extended>(outFloat));
  CheckEquals(1.11, outFloat);
end;

procedure TTestFromWideString.TestWStringToInteger;
var
  outValue: TValue;
  outInt: Integer;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(Integer));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Integer>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromWideString.TestWStringToCardinal;
var
  outValue: TValue;
  outInt: Cardinal;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(Cardinal));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Cardinal>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromWideString.TestWStringToSmallInt;
var
  outValue: TValue;
  outInt: SmallInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(SmallInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<SmallInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromWideString.TestWStringToShortInt;
var
  outValue: TValue;
  outInt: ShortInt;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(ShortInt));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<ShortInt>(outInt));
  CheckEquals(1, outInt);
end;

procedure TTestFromWideString.TestWStringToNullableAnsiString;
var
  outValue: TValue;
  outNullable: Nullable<AnsiString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(Nullable<AnsiString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<AnsiString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(AnsiString('Test'), outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableBoolean;
var
  outValue: TValue;
  outNullable: Nullable<Boolean>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('True'),
    TypeInfo(Nullable<Boolean>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Boolean>>(outNullable));
  CheckEquals(True, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableByte;
var
  outValue: TValue;
  outInt: Nullable<Byte>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(Nullable<Byte>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Byte>>(outInt));
  CheckEquals(1, outInt.Value);

  outValue := fConverter.ConvertTo(TValue.From<WideString>('255'),
    TypeInfo(Nullable<Byte>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Byte>>(outInt));
  CheckEquals(255, outInt.Value);
end;

procedure TTestFromWideString.TestWStringToNullableColor;
var
  outValue: TValue;
  outNullable: Nullable<TColor>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(ColorToString(RedValue)),
    TypeInfo(Nullable<TColor>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TColor>>(outNullable));
  CheckEquals(RedString, ColorToString(outNullable.Value));
end;

procedure TTestFromWideString.TestWStringToNullableCurrency;
var
  outValue: TValue;
  outNullable: Nullable<Currency>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Nullable<Currency>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Currency>>(outNullable));
  CheckEquals(1.11, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableDateTime;
var
  outValue: TValue;
  outNullable: Nullable<TDateTime>;
  stamp: TDateTime;
begin
  stamp := Now;
  outValue := fConverter.ConvertTo(TValue.From<WideString>(DateTimeToStr(stamp)),
    TypeInfo(Nullable<TDateTime>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<TDateTime>>(outNullable));
  CheckEqualsString(DateTimeToStr(stamp), DateTimeToStr(outNullable.Value));
end;

procedure TTestFromWideString.TestWStringToNullableFloat;
var
  outValue: TValue;
  outNullable: Nullable<Extended>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>(FloatToStr(1.11)),
    TypeInfo(Nullable<Extended>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Extended>>(outNullable));
  CheckEquals(1.11, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableInt64;
var
  outValue: TValue;
  outNullable: Nullable<Int64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(1, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<WideString>('2147483647'),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(2147483647, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<WideString>('-2147483647'),
    TypeInfo(Nullable<Int64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Int64>>(outNullable));
  CheckEquals(-2147483647, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableInteger;
var
  outValue: TValue;
  outNullable: Nullable<Integer>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<Integer>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Integer>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableCardinal;
var
  outValue: TValue;
  outNullable: Nullable<Cardinal>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<Cardinal>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Cardinal>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableSmallInt;
var
  outValue: TValue;
  outNullable: Nullable<SmallInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<SmallInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<SmallInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableShortInt;
var
  outValue: TValue;
  outNullable: Nullable<ShortInt>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('15'),
    TypeInfo(Nullable<ShortInt>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<ShortInt>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(15, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableString;
var
  outValue: TValue;
  outNullable: Nullable<string>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(Nullable<string>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<string>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals('Test', outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableUInt64;
var
  outValue: TValue;
  outNullable: Nullable<UInt64>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(Nullable<UInt64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<UInt64>>(outNullable));
  CheckEquals(1, outNullable.Value);

  outValue := fConverter.ConvertTo(TValue.From<WideString>('2147483647'),
    TypeInfo(Nullable<UInt64>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<UInt64>>(outNullable));
  CheckEquals(2147483647, outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableWideString;
var
  outValue: TValue;
  outNullable: Nullable<WideString>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test'),
    TypeInfo(Nullable<WideString>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<WideString>>(outNullable));
  CheckTrue(outNullable.HasValue);
  CheckEquals(WideString('Test'), outNullable.Value);
end;

procedure TTestFromWideString.TestWStringToNullableWord;
var
  outValue: TValue;
  outInt: Nullable<Word>;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('1'),
    TypeInfo(Nullable<Word>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Word>>(outInt));
  CheckEquals(1, outInt.Value);

  outValue := fConverter.ConvertTo(TValue.From<WideString>('65535'),
    TypeInfo(Nullable<Word>));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<Nullable<Word>>(outInt));
  CheckEquals(65535, outInt.Value);
end;

procedure TTestFromWideString.TestWStringToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<WideString>('Test WideString'),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals('Test WideString', outStr);
end;

{$ENDREGION}
{$ENDIF}


{$REGION 'TTestFromInterface'}

  {$REGION 'TTestFromObject.TTestObject'}

  function TTestFromInterface.TTestObject.ToString: string;
  begin
    inherited;
    Result := 'InterfaceToObject test case.';
  end;

  {$ENDREGION}

procedure TTestFromInterface.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestFromInterface.TestInterfaceToObject;
var
  outValue: TValue;
  intf: ITestInterface;
  obj: TTestObject;
begin
  intf := TTestObject.Create;
  outValue := fConverter.ConvertTo(TValue.From<ITestInterface>(intf),
    TypeInfo(TTestObject));

  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<TTestObject>(obj));
  CheckEqualsString(obj.ToString, intf.ToString);
end;

{$ENDREGION}


{$REGION 'TTestCustomTypes'}

procedure TTestCustomTypes.SetUp;
begin
  inherited;
  fConverter := TValueConverter.Default;
end;

procedure TTestCustomTypes.TestCustomFloatToString;
var
  outValue: TValue;
  outStr: string;
begin
  outValue := fConverter.ConvertTo(TValue.From<CustomFloat>(1.11),
    TypeInfo(string));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<string>(outStr));
  CheckEquals(FloatToStr(1.11), outStr);
end;

procedure TTestCustomTypes.TestStringToCustomFloat;
var
  outValue: TValue;
  outFloat: CustomFloat;
begin
  outValue := fConverter.ConvertTo(TValue.From<string>(FloatToStr(1.11)),
    TypeInfo(CustomFloat));
  CheckFalse(outValue.IsEmpty);
  CheckTrue(outValue.TryAsType<CustomFloat>(outFloat));
  CheckEquals(1.11, outFloat);
end;

{$ENDREGION}

end.
