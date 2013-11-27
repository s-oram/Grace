unit eeDsp.Interpolation.Tests;

interface

uses
  TestFrameWork;

type
  {$M+}
  TInterpolationUnitTest = class(TTestCase)
  published
    procedure Linear_Test;
  end;

implementation

uses
  eeFunctions, eeDsp.Interpolation;

{ TInterpolationUnitTest }

procedure TInterpolationUnitTest.Linear_Test;
var
  f, y0, y1 : double;
  tr : double;
begin
  f := 0;
  y0 := 1;
  y1 := 2;
  tr := Linear(f, y0, y1);
  Check(tr = 1);


  f := 0.25;
  y0 := 1;
  y1 := 2;
  tr := Linear(f, y0, y1);
  Check(tr = 1.25);

  f := 0.5;
  y0 := 1;
  y1 := 2;
  tr := Linear(f, y0, y1);
  Check(tr = 1.5);

  f := 0.75;
  y0 := 1;
  y1 := 2;
  tr := Linear(f, y0, y1);
  Check(tr = 1.75);

  f := 1;
  y0 := 1;
  y1 := 2;
  tr := Linear(f, y0, y1);
  Check(tr = 2);
end;




initialization
 TestFramework.RegisterTest(TInterpolationUnitTest.Suite);


end.
