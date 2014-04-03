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

unit Spring.Tests.Helpers;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Helpers;

type
  TTestGuidHelper = class(TTestCase)
  published
    procedure TestNewGUID;
    procedure TestEmpty;
    procedure TestEquals;
    procedure TestToString;
    procedure TestToQuotedString;
  end;


implementation


{$REGION 'TTestGuidHelper'}

{$WARNINGS OFF}

procedure TTestGuidHelper.TestNewGUID;
var
  guid: TGUID;
begin
  guid := TGUID.NewGUID;
  CheckEquals(38, Length(guid.ToString));
end;

procedure TTestGuidHelper.TestEmpty;
var
  empty: TGUID;
const
  EmptyGuidString = '{00000000-0000-0000-0000-000000000000}';
begin
  empty := TGUID.Empty;
  CheckEquals(EmptyGuidString, empty.ToString);
  CheckTrue(empty.IsEmpty);
end;

procedure TTestGuidHelper.TestEquals;
var
  guid: TGUID;
const
  GuidString = '{93585BA2-B43B-4C55-AAAB-6DE6EB4C0E57}';
begin
  guid := TGUID.Create(GuidString);
  Check(guid.Equals(guid));
  CheckFalse(guid.Equals(TGUID.Empty));
end;

procedure TTestGuidHelper.TestToString;
var
  guid: TGUID;
const
  GuidString = '{93585BA2-B43B-4C55-AAAB-6DE6EB4C0E57}';
begin
  guid := TGuid.Create(GuidString);
  CheckEquals(GuidString, guid.ToString);
end;

procedure TTestGuidHelper.TestToQuotedString;
var
  guid: TGUID;
const
  GuidString = '{93585BA2-B43B-4C55-AAAB-6DE6EB4C0E57}';
begin
  guid := TGuid.Create(GuidString);
  CheckEquals(QuotedStr(GuidString), guid.ToQuotedString);
end;

{$WARNINGS ON}

{$ENDREGION}

end.
