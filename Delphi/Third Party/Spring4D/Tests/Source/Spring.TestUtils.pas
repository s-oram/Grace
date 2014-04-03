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

unit Spring.TestUtils;

{$I Spring.Tests.inc}

interface

uses
  Classes,
  IniFiles,
  SysUtils,
  TestFramework;

type
  TAbstractTestHelper = class helper for TAbstractTest
  public
    procedure CheckException(AExceptionType: ExceptionClass; ACode: TProc; const AMessage: string = '');
  end;

  TTestDecorator = class(TAbstractTest)
  private
    fTest: ITest;
    fTests: IInterfaceList;
  protected
    procedure RunTest(ATestResult: TTestResult); override;
  public
    constructor Create(const ATest: ITest; const AName: string = '');

    function CountEnabledTestCases: Integer; override;
    function CountTestCases: Integer; override;

    function GetName: string; override;
    function Tests: IInterfaceList; override;

    procedure LoadConfiguration(const iniFile: TCustomIniFile; const section: string); override;
    procedure SaveConfiguration(const iniFile: TCustomIniFile; const section: string); override;

    property Test: ITest read fTest;
  end;

  TRepeatedTest = class(TTestDecorator)
  private
    fCount: Integer;
  protected
    procedure RunTest(ATestResult: TTestResult); override;
  public
    constructor Create(const ATest: ITest; ACount: Integer; const AName: string = '');
    function GetName: string; override;

    function CountEnabledTestCases: Integer; override;
    function CountTestCases: Integer; override;
  end;

procedure ProcessTestResult(const ATestResult: TTestResult);

implementation

procedure ProcessTestResult(const ATestResult: TTestResult);
begin
{$IFNDEF AUTOREFCOUNT}
  ATestResult.Free();
{$ENDIF}
end;

{$REGION 'TAbstractTestHelper'}

procedure TAbstractTestHelper.CheckException(
  AExceptionType: ExceptionClass; ACode: TProc; const AMessage: string);
var
  WasException: Boolean;
begin
  WasException := False;
  try
    aCode;
  except
    on E: Exception do
    begin
      if E is aExceptionType then
      begin
        WasException := True;
      end;
    end;
  end;
  Check(WasException, aMessage);
end;

{$ENDREGION}


{$REGION 'TTestDecorator'}

constructor TTestDecorator.Create(const ATest: ITest; const AName: string);
begin
  if AName = '' then
    inherited Create(ATest.Name)
  else
    inherited Create(AName);
  fTest := ATest;
  fTests := TInterfaceList.Create;
  fTests.Add(fTest);
end;

function TTestDecorator.GetName: string;
begin
  Result := fTest.Name;
end;

function TTestDecorator.CountEnabledTestCases: Integer;
begin
  if Enabled then
    Result := fTest.CountEnabledTestCases
  else
    Result := 0;
end;

function TTestDecorator.CountTestCases: Integer;
begin
  if Enabled then
    Result := fTest.CountTestCases
  else
    Result := 0;
end;

procedure TTestDecorator.RunTest(ATestResult: TTestResult);
begin
  fTest.RunWithFixture(ATestResult);
end;

function TTestDecorator.Tests: IInterfaceList;
begin
  Result := fTests;
end;

procedure TTestDecorator.LoadConfiguration(const iniFile: TCustomIniFile;
  const section: string);
var
  i: Integer;
begin
  inherited LoadConfiguration(iniFile, section);
  for i := 0 to fTests.Count - 1 do
    ITest(fTests[i]).LoadConfiguration(iniFile, section + '.' + Name);
end;

procedure TTestDecorator.SaveConfiguration(const iniFile: TCustomIniFile;
  const section: string);
var
  i: integer;
begin
  inherited SaveConfiguration(iniFile, section);
  for i := 0 to fTests.Count - 1 do
    ITest(fTests[i]).SaveConfiguration(iniFile, section + '.' + Name);
end;

{$ENDREGION}


{$REGION 'TRepeatedTest'}

constructor TRepeatedTest.Create(const ATest: ITest; ACount: Integer;
  const AName: string);
begin
  inherited Create(ATest, AName);
  fCount := ACount;
end;

function TRepeatedTest.CountEnabledTestCases: Integer;
begin
  Result := inherited CountEnabledTestCases * fCount;
end;

function TRepeatedTest.CountTestCases: Integer;
begin
  Result := inherited CountTestCases * fCount;
end;

function TRepeatedTest.GetName: string;
begin
  Result := Format('%d x %s', [fCount, Test.Name]);
end;

procedure TRepeatedTest.RunTest(ATestResult: TTestResult);
var
  i: Integer;
  errorCount: Integer;
  failureCount: integer;
begin
  errorCount := ATestResult.ErrorCount;
  failureCount := ATestResult.FailureCount;

  for i := 0 to fCount - 1 do
  begin
    if ATestResult.ShouldStop
      or (ATestResult.ErrorCount > errorCount)
      or (ATestResult.FailureCount > failureCount) then
      Break;
    inherited RunTest(ATestResult);
  end;
end;

{$ENDREGION}


end.
