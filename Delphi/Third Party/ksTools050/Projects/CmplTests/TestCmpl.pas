{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit TestCmpl;

interface

uses
  TestFramework, Windows, Forms, Dialogs, Controls, Classes, SysUtils, Variants,
  Graphics, Messages, Math, ksMath;

type
  TTestCmpl = class(TTestCase)
  private
    FValue1: TksComplex;
    FValue2: TksComplex;
    FValue3: TksComplex;
//  protected
//    procedure SetUp; override;
    procedure TestLn(const Re, Im: Extended);
    procedure TestCos(const Re, Im: Extended);
    procedure TestSin(const AValue: TksComplex);
    procedure TestSqr(const AValue: TksComplex);
    procedure TestSqrt(const AValue: TksComplex);
    procedure TestTan(const AValue: TksComplex);
  published
    procedure TestLn_0;
    procedure TestCos_0;
    procedure TestSin_0;
    procedure TestSqr_0;
    procedure TestSqrt_0;
    procedure TestTan_0;
  end;

implementation

{ TestCmpl }

const
  TestValues: array[0..5] of TksComplex = (
   (Re:10; Im:0),
   (Re:-10; Im:0),
   (Re:0; Im:10),
   (Re:0; Im:-10),
   (Re:5; Im:10),
   (Re:5; Im:5)
   );

{
procedure TTestInt.SetUp;
begin
end;
}

procedure TTestCmpl.TestCos(const Re, Im: Extended);
var
  I: TksComplex;

begin
  FValue1.Assign(Re, Im);
  FValue2:= TksComplex.Cos(FValue1);
  I.Assign(0, 1);
  FValue3:= (TksComplex.Exp(FValue1 * I) + TksComplex.Exp(-FValue1 * I)) / 2;
  CheckEquals(FValue2.Re, FValue3.Re, 1.0E-15);
  CheckEquals(FValue2.Im, FValue3.Im, 1.0E-15);
end;

procedure TTestCmpl.TestSin(const AValue: TksComplex);
var
  I: TksComplex;

begin
  FValue1:= AValue;
  FValue2:= TksComplex.Sin(FValue1);
  I.Assign(0, 1);
  FValue3:= (TksComplex.Exp(FValue1 * I) - TksComplex.Exp(-FValue1 * I)) / (2 * I);
  CheckEquals(FValue2.Re, FValue3.Re, 1.0E-15);
  CheckEquals(FValue2.Im, FValue3.Im, 1.0E-15);
end;

procedure TTestCmpl.TestLn(const Re, Im: Extended);
begin
  FValue1.Assign(Re, Im);
  FValue2:= TksComplex.Ln(FValue1);
  FValue3:= TksComplex.Exp(FValue2);
  CheckEquals(FValue1.Re, FValue3.Re, 1.0E-15);
  CheckEquals(FValue1.Im, FValue3.Im, 1.0E-15);
end;

procedure TTestCmpl.TestSqr(const AValue: TksComplex);
begin
  FValue1:= AValue;
  FValue2:= TksComplex.Sqr(FValue1);
  FValue3:= FValue1 * FValue1;
  CheckEquals(FValue3.Re, FValue2.Re, 1.0E-15);
  CheckEquals(FValue3.Im, FValue2.Im, 1.0E-15);
end;

procedure TTestCmpl.TestSqrt(const AValue: TksComplex);
begin
  FValue1:= AValue;
  FValue2:= TksComplex.Sqrt(FValue1);
  FValue3:= FValue2 * FValue2;
  CheckEquals(FValue1.Re, FValue3.Re, 1.0E-15);
  CheckEquals(FValue1.Im, FValue3.Im, 1.0E-15);
end;

procedure TTestCmpl.TestTan(const AValue: TksComplex);
begin
  FValue1:= AValue;
  FValue2:= TksComplex.Tan(FValue1);
  FValue3:= TksComplex.Sin(FValue1) / TksComplex.Cos(FValue1);
  CheckEquals(FValue3.Re, FValue2.Re, 1.0E-15);
  CheckEquals(FValue3.Im, FValue2.Im, 1.0E-15);
end;

procedure TTestCmpl.TestLn_0;
begin
  TestLn(10, 0);
  TestLn(-10, 0);
  TestLn(0, 10);
  TestLn(0, -10);
  TestLn(5, 10);
end;

procedure TTestCmpl.TestCos_0;
begin
  TestCos(10, 0);
  TestCos(-10, 0);
  TestCos(0, 10);
  TestCos(0, -10);
  TestCos(5, 10);
end;

procedure TTestCmpl.TestSin_0;
var
  Value: TksComplex;

begin
  for Value in TestValues do
    TestSin(Value);
end;

procedure TTestCmpl.TestSqr_0;
var
  Value: TksComplex;

begin
  for Value in TestValues do
    TestSqr(Value);
end;

procedure TTestCmpl.TestSqrt_0;
var
  Value: TksComplex;

begin
  for Value in TestValues do
    TestSqrt(Value);
end;

procedure TTestCmpl.TestTan_0;
var
  Value: TksComplex;

begin
  for Value in TestValues do
    TestTan(Value);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestCmpl.Suite);
end.

