unit Test.FarScape.Positioning;

interface

uses
  FarScape.CustomControl,
  WatchTower,
  WatchTower.Confirm;

type
  TPositioningTest = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure ChildPosition;
  end;

  TSimpleControl = class(TFarScapeControl)
  end;

implementation

uses
  Types;

{ TPositioningTest }

procedure TPositioningTest.ChildPosition;
var
  ControlA, ControlB, ControlC : TSimpleControl;
  r : TRect;
  Expected : TRect;
begin
  ControlA := TSimpleControl.Create;
  ControlB := TSimpleControl.Create;
  ControlC := TSimpleControl.Create;
  try
    ControlA.SetBounds(10,20,100,150);
    r := ControlA.GetAbsoluteRect;
    Confirm.IsTrue(r = rect(0,0,100,150), 'AbsoluteRect is not correct.');

    ControlB.Parent := ControlA;
    ControlB.SetBounds(10,20,30,40);
    r := ControlB.GetAbsoluteRect;
    Expected.TopLeft := Point(10,20);
    Expected.Width := 30;
    Expected.Height := 40;
    Confirm.IsTrue(r = Expected, 'AbsoluteRect is not correct.');


    ControlC.SetBounds(5, 15, 40, 30);
    ControlC.Parent := ControlB;
    r := ControlC.GetAbsoluteRect;
    Expected.TopLeft := Point(15,35);
    Expected.Width := 40;
    Expected.Height := 30;
    Confirm.IsTrue(r = Expected, 'AbsoluteRect is not correct.');


    ControlB.SetPosition(-10,-20);
    r := ControlB.GetAbsoluteRect;
    Expected.TopLeft := Point(-10,-20);
    Expected.Width := 30;
    Expected.Height := 40;
    Confirm.IsTrue(r = Expected, 'AbsoluteRect is not correct.');

  finally
    ControlA.Free;
    ControlB.Free;
    ControlC.Free;
  end;
end;

end.
