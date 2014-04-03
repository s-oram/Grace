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

unit Spring.Tests.Utils;

{$I Spring.inc}

interface

uses
  TypInfo,
  Types,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Utils,
  Spring.Utils.IO;

type
  TTestVersion = class(TTestCase)
  published
    procedure TestCompareTo;
    procedure TestFromString;
    procedure TestToString;
    procedure TestToStringException;
    procedure TestArgumentException;
    procedure TestArgumentOutOfRangeException;
    procedure TestFormatException;
  end;

  TTestEnvironment = class(TTestCase)
  published
    /// <summary>
    ///   We're currently testing only the most common case where we set
    ///   environmental variable to our own process.
    /// </summary>
    procedure TestSetEnvironmentVariableProcess;
    /// <summary>
    ///   Also test the case when we assign regional characters to both name
    ///   and value, basically test that we convert encoding correctly.
    /// </summary>
    procedure TestSetEnvironmentVariableRegional;
  end;

implementation

uses
  DateUtils;


{$REGION 'TTestVersion'}

//  Version 1.1 is older than version 1.1.0.
//  Version 1.1 is older than version 1.1.1.
//  Version 1.1 is older than version 1.1.2.3.
//  Version 1.1.2 is older than version 1.1.2.4.
//  Version 1.2.5 is newer than version 1.2.3.4.

procedure TTestVersion.TestCompareTo;
var
  v1, v2: TVersion;
begin
  v1 := TVersion.Create('1.1');
  v2 := TVersion.Create('1.1.0');
  CheckTrue(v1.CompareTo(v2) < 0);    // v1 is older than v2
  CheckTrue(v2.CompareTo(v1) > 0);    // v2 is newer than v1

  v1 := TVersion.Create('1.1');
  v2 := TVersion.Create('1.1.2.3');
  CheckTrue(v1.CompareTo(v2) < 0);
  CheckTrue(v2.CompareTo(v1) > 0);

  v1 := TVersion.Create('1.1.2');
  v2 := TVersion.Create('1.1.2.4');
  CheckTrue(v1.CompareTo(v2) < 0);
  CheckTrue(v2.CompareTo(v1) > 0);

  v1 := TVersion.Create('1.1.5');
  v2 := TVersion.Create('1.1.3.4');
  CheckTrue(v1.CompareTo(v2) > 0);
  CheckTrue(v2.CompareTo(v1) < 0);

  v1 := TVersion.Create('1.1.5');
  v2 := TVersion.Create('1.1.5');
  CheckTrue(v1.CompareTo(v2) = 0);
  CheckTrue(v2.CompareTo(v1) = 0);
end;

procedure TTestVersion.TestFromString;
var
  ver: TVersion;
begin
  ver := TVersion.Create('1.0');
  CheckEquals(1, ver.Major);
  CheckEquals(0, ver.Minor);
  CheckEquals(-1, ver.Build);
  CheckEquals(-1, ver.Reversion);

  ver := TVersion.Create('1.1.0');
  CheckEquals(1, ver.Major);
  CheckEquals(1, ver.Minor);
  CheckEquals(0, ver.Build);
  CheckEquals(-1, ver.Reversion);

  ver := TVersion.Create('1.1.1.0');
  CheckEquals(1, ver.Major);
  CheckEquals(1, ver.Minor);
  CheckEquals(1, ver.Build);
  CheckEquals(0, ver.Reversion);
end;

procedure TTestVersion.TestToString;
var
  ver: TVersion;
begin
  ver := TVersion.Create(1, 0);
  CheckEquals('1.0', ver.ToString);

  ver := TVersion.Create(1, 0, 6);
  CheckEquals('1.0.6', ver.ToString);
  CheckEquals('1.0', ver.ToString(2));
  CheckEquals('1.0.6', ver.ToString(3));

  ver := TVersion.Create(10, 8, 2608, 8);
  CheckEquals('10.8.2608.8', ver.ToString);
  CheckEquals('10.8', ver.ToString(2));
  CheckEquals('10.8.2608', ver.ToString(3));
  CheckEquals('10.8.2608.8', ver.ToString(4));
end;

procedure TTestVersion.TestToStringException;
var
  ver: TVersion;
begin
  ExpectedException := EArgumentException;
  ver := TVersion.Create('1.0');
  ver.ToString(3);
end;

procedure TTestVersion.TestArgumentException;
begin
  ExpectedException := EArgumentException;
  TVersion.Create('1');
end;

procedure TTestVersion.TestFormatException;
begin
  ExpectedException := EFormatException;
  TVersion.Create('1.c.d');
end;

procedure TTestVersion.TestArgumentOutOfRangeException;
begin
  ExpectedException := EArgumentOutOfRangeException;
  TVersion.Create('1.-1.0');
end;

{$ENDREGION}


{$REGION 'TTestEnvironment'}

procedure TTestEnvironment.TestSetEnvironmentVariableProcess;
var
  name,
  value,
  actual: string;
begin
  // Test name and value with plain ASCII in it so we can
  // use SysUtils.GetEnvironmentVariable to test basic functionality
  name := 'MyVar';
  value := 'Value';

  TEnvironment.SetEnvironmentVariable(name, value);

  actual := SysUtils.GetEnvironmentVariable(name);

  CheckEquals(value, actual);
end;

procedure TTestEnvironment.TestSetEnvironmentVariableRegional;
var
  name,
  value,
  actual: string;
begin
  // Test name and value with regional characters in it not just ASCII
  name := 'MyVar'#$00E1;
  value := 'Value'#$00E1;

  TEnvironment.SetEnvironmentVariable(name, value);

  // Do not use SysUtils, it is broken for regional characters, this will actually
  // test both GetEnvironmentVariable and SetEnvironmentVariable which makes this an integration test
  actual := TEnvironment.GetEnvironmentVariable(name);

  CheckEquals(value, actual);
end;

{$ENDREGION}


end.
