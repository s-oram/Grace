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

unit Spring.Tests.SysUtils;

interface

uses
  SysUtils,
  TestFramework,
  Spring,
  Spring.SystemUtils;

type
  TTestSplitString = class(TTestCase)
  private
    fStrings: TStringDynArray;
  published
    procedure TestEmptyString;
    procedure TestOneEntry;
    procedure TestEmptyEntry;
    procedure TestMoreEntries;
    procedure TestRemoveEmptyEntries;
  end;

  TTestTryConvertStrToDateTime = class(TTestCase)
  published
    procedure TestParseDate;
    procedure TestParseTime;
    procedure TestParseDateTime;
    procedure TestFailedCases;
  end;

  TTestSplitNullTerminatedStrings = class(TTestCase)
  private
    fStrings: TStringDynArray;
    fBuffer: TCharArray;
  published
    procedure TestNil;
    procedure TestEmpty;
    procedure TestOneEntry;
    procedure TestThreeEntries;
    procedure TestVariousStrings;
  end;

  TTestEnum = class(TTestCase)
  published
    procedure TestGetNameByEnum;
    procedure TestGetNames;
    procedure TestGetNameByInteger;
    procedure TestGetValueByEnum;
    procedure TestGetValueByName;
    procedure TestIsValid;
    procedure TestTryParse;
    procedure TestParse;
    procedure TestParseIntegerException;
    procedure TestParseStringException;
  end;

implementation

uses
  Classes,
  DateUtils,
  TypInfo;


{$REGION 'TTestSplitString'}

procedure TTestSplitString.TestEmptyString;
begin
  fStrings := SplitString('', []);
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitString.TestOneEntry;
begin
  fStrings := SplitString('word', [' ']);
  CheckEquals(1, Length(fStrings));
  CheckEquals('word', fStrings[0]);

  fStrings := SplitString('2', [' ']);
  CheckEquals(1, Length(fStrings));
  CheckEquals('2', fStrings[0]);
end;

procedure TTestSplitString.TestMoreEntries;
begin
  fStrings := SplitString('one word', [' ']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('word', fStrings[1]);

  fStrings := SplitString('one two three four', [' ']);
  CheckEquals(4, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('two', fStrings[1]);
  CheckEquals('three', fStrings[2]);
  CheckEquals('four', fStrings[3]);

  fStrings := SplitString('2.0', ['.']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('2', fStrings[0]);
  CheckEquals('0', fStrings[1]);
end;

procedure TTestSplitString.TestEmptyEntry;
begin
  fStrings := SplitString('one  word', [' ']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('one', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('word', fStrings[2]);

  fStrings := SplitString('1..2', ['.']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('1', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('2', fStrings[2]);

  fStrings := SplitString('12..3..456', ['.']);
  CheckEquals(5, Length(fStrings));
  CheckEquals('12', fStrings[0]);
  CheckEquals('', fStrings[1]);
  CheckEquals('3', fStrings[2]);
  CheckEquals('', fStrings[3]);
  CheckEquals('456', fStrings[4]);

  fStrings := SplitString('.', ['.']);
  CheckEquals(2, Length(fStrings));
  CheckEquals('', fStrings[0]);
  CheckEquals('', fStrings[1]);

  fStrings := SplitString('.1.', ['.']);
  CheckEquals(3, Length(fStrings));
  CheckEquals('', fStrings[0]);
  CheckEquals('1', fStrings[1]);
  CheckEquals('', fStrings[2]);
end;

procedure TTestSplitString.TestRemoveEmptyEntries;
begin
  fStrings := SplitString('1..2', ['.'], True);
  CheckEquals(2, Length(fStrings));
  CheckEquals('1', fStrings[0]);
  CheckEquals('2', fStrings[1]);

  fStrings := SplitString('.', ['.'], True);
  CheckEquals(0, Length(fStrings));
end;

{$ENDREGION}


{$REGION 'TTestSplitNullTerminatedStrings'}

procedure TTestSplitNullTerminatedStrings.TestEmpty;
begin
  fBuffer := TCharArray.Create(#0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitNullTerminatedStrings.TestNil;
begin
  fStrings := SplitString(nil);
  CheckEquals(0, Length(fStrings));
end;

procedure TTestSplitNullTerminatedStrings.TestOneEntry;
begin
  fBuffer := TCharArray.Create('C', ':', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(1, Length(fStrings));
  CheckEquals('C:', fStrings[0]);
end;

procedure TTestSplitNullTerminatedStrings.TestThreeEntries;
begin
  fBuffer := TCharArray.Create('C', ':', #0, 'D', ':', #0, 'E', ':', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(3, Length(fStrings));
  CheckEquals('C:', fStrings[0]);
  CheckEquals('D:', fStrings[1]);
  CheckEquals('E:', fStrings[2]);
end;

procedure TTestSplitNullTerminatedStrings.TestVariousStrings;
begin
  fBuffer := TCharArray.Create('A', 'B', 'C', #0, 'D', 'E', #0, 'F', #0, #0);
  fStrings := SplitString(PChar(fBuffer));
  CheckEquals(3, Length(fStrings));
  CheckEquals('ABC', fStrings[0]);
  CheckEquals('DE', fStrings[1]);
  CheckEquals('F', fStrings[2]);
end;

{$ENDREGION}


{$REGION 'TTestEnum'}

procedure TTestEnum.TestGetNameByEnum;
var
  expectedName: string;
  actualName: string;
  item: TSeekOrigin;
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(TSeekOrigin);
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedName := GetEnumName(pInfo, Integer(item));
    actualName := TEnum.GetName<TSeekOrigin>(item);
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetNameByInteger;
var
  expectedName: string;
  actualName: string;
  item: TSeekOrigin;
  pInfo: PTypeInfo;
begin
  pInfo := TypeInfo(TSeekOrigin);
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedName := GetEnumName(pInfo, Integer(item));
    actualName := TEnum.GetName<TSeekOrigin>(Integer(item));
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetNames;
var
  expectedName: string;
  actualName: string;
  item: TSeekOrigin;
  pInfo: PTypeInfo;
  names: TStringDynArray;
begin
  pInfo := TypeInfo(TSeekOrigin);
  names := TEnum.GetNames<TSeekOrigin>;
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedName := GetEnumName(pInfo, Ord(item));
    actualName := names[Ord(item)];
    CheckEquals(expectedName, actualName);
  end;
end;

procedure TTestEnum.TestGetValueByEnum;
var
  expectedValue: Integer;
  actualValue: Integer;
  item: TSeekOrigin;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedValue := Integer(item);
    actualValue := TEnum.GetValue<TSeekOrigin>(item);
    CheckEquals(expectedValue, actualValue);
  end;
end;

procedure TTestEnum.TestGetValueByName;
var
  expectedValue: Integer;
  actualValue: Integer;
  item: TSeekOrigin;
  name: string;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    expectedValue := Integer(item);
    name := GetEnumName(TypeInfo(TSeekOrigin), expectedValue);
    actualValue := TEnum.GetValue<TSeekOrigin>(name);
    CheckEquals(expectedValue, actualValue);
  end;
end;

procedure TTestEnum.TestIsValid;
var
  item: TSeekOrigin;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    Check(TEnum.IsValid<TSeekOrigin>(item));
    Check(TEnum.IsValid<TSeekOrigin>(Integer(item)));
  end;
  CheckFalse(TEnum.IsValid<TSeekOrigin>(Integer(Low(TSeekOrigin)) - 1));
  CheckFalse(TEnum.IsValid<TSeekOrigin>(Integer(High(TSeekOrigin)) + 1));
end;

procedure TTestEnum.TestParse;
var
  item: TSeekOrigin;
  actual: TSeekOrigin;
begin
  for item := Low(TSeekOrigin) to High(TSeekOrigin) do
  begin
    actual := TEnum.Parse<TSeekOrigin>(Integer(item));
    CheckEquals(Integer(item), Integer(actual));
    actual := TEnum.Parse<TSeekOrigin>(GetEnumName(TypeInfo(TSeekOrigin), Integer(item)));
    CheckEquals(Integer(item), Integer(actual));
  end;
end;

procedure TTestEnum.TestTryParse;
var
  item: TSeekOrigin;
begin
  Check(TEnum.TryParse<TSeekOrigin>(Integer(soBeginning), item));
  CheckEquals(Integer(soBeginning), Integer(item));
  Check(TEnum.TryParse<TSeekOrigin>('soBeginning', item));
  CheckEquals(Integer(soBeginning), Integer(item));

  CheckFalse(TEnum.TryParse<TSeekOrigin>(Integer(Low(TSeekOrigin)) - 1, item));
  CheckFalse(TEnum.TryParse<TSeekOrigin>('dummy', item));
end;

procedure TTestEnum.TestParseIntegerException;
begin
  ExpectedException := EFormatException;
  TEnum.Parse<TSeekOrigin>(Integer(Low(TSeekOrigin))-1);
end;

procedure TTestEnum.TestParseStringException;
begin
  ExpectedException := EFormatException;
  TEnum.Parse<TSeekOrigin>('dummy');
end;

{$ENDREGION}


{$REGION 'TTestTryConvertStrToDateTime'}

procedure TTestTryConvertStrToDateTime.TestParseDate;
var
  actual, expected: TDateTime;
begin
  expected := EncodeDate(2009, 10, 18);

  CheckTrue(TryConvertStrToDateTime('20091018', 'YYYYMMDD', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryConvertStrToDateTime('091018', 'YYMMDD', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryConvertStrToDateTime('10-18-2009', 'MM-DD-YYYY', actual));
  CheckTrue(SameDateTime(actual, expected));

  CheckTrue(TryConvertStrToDateTime(' 2009-10-18 ', 'YYYY-MM-DD', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryConvertStrToDateTime.TestParseTime;
var
  actual, expected: TDateTime;
begin
  expected := EncodeTime(12, 10, 18, 35);
  CheckTrue(TryConvertStrToDateTime('12:10:18.035', 'hh:nn:ss.zzz', actual));
  CheckTrue(SameDateTime(actual, expected));

  expected := EncodeTime(12, 10, 0, 0);
  CheckTrue(TryConvertStrToDateTime('12:10 ', 'hh:nn', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryConvertStrToDateTime.TestParseDateTime;
var
  actual, expected: TDateTime;
begin
  expected := EncodeDateTime(2009, 10, 18, 12, 30, 59, 200);
  CheckTrue(TryConvertStrToDateTime('2009-10-18 12:30:59.200', 'YYYY-MM-DD HH:NN:SS.ZZZ', actual));
  CheckTrue(SameDateTime(actual, expected));

  expected := EncodeDateTime(2009, 10, 18, 12, 30, 59, 200);
  CheckTrue(TryConvertStrToDateTime('20091018123059200', 'YYYYMMDDHHNNSSZZZ', actual));
  CheckTrue(SameDateTime(actual, expected));
end;

procedure TTestTryConvertStrToDateTime.TestFailedCases;
var
  value: TDateTime;
begin
  CheckFalse(TryConvertStrToDateTime('', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime(' ', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('2009', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('2009080', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('2009080A', 'YYYYMMDD', value));
  CheckFalse(TryConvertStrToDateTime('200908011230', 'YYYYMMDDHHNNSS', value));
  CheckFalse(TryConvertStrToDateTime('20090801123007', 'YYYYMMDDHHNNSSZZZ', value));
end;

{$ENDREGION}

end.
