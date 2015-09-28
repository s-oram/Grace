unit Test.FarScape.Parentage;

interface

uses
  FarScape.CustomControl,
  WatchTower;

type
  TControlParentTest = class(TWatchTowerTest)
  public
    [Test]
    procedure NoParentOnCreate;

    [Test]
    procedure TestParentHierarchy;

    [Test]
    procedure ContainsControl;

    [Test]
    procedure FreeParentControl;
  end;

  TSimpleControl = class(TFarScapeControl)
  end;

implementation

uses
  WatchTower.Confirm;

{ TControlParentTest }

procedure TControlParentTest.NoParentOnCreate;
var
  c : TSimpleControl;
begin
  c := TSimpleControl.Create;
  try
    if assigned(c.Parent) then self.ReportError('Control parent is assigned.');
  finally
    c.Free;
  end;
end;

procedure TControlParentTest.TestParentHierarchy;
var
  ControlA, ControlB, ControlC : TSimpleControl;
begin
  ControlA := TSimpleControl.Create;
  ControlB := TSimpleControl.Create;
  ControlC := TSimpleControl.Create;
  try
    ControlB.Parent := ControlA;
    if not assigned(ControlB.Parent) then self.ReportError('Parent is not assigned.');
    if ControlB.Parent <> ControlA then self.ReportError('Parent is not correct.');

    ControlC.Parent := ControlB;
    if not assigned(ControlC.Parent) then self.ReportError('Parent is not assigned.');
    if ControlC.Parent <> ControlB then self.ReportError('Parent is not correct.');

    ControlB.Parent := nil;
    if assigned(ControlB.Parent) then self.ReportError('Control parent is still assigned.');

  finally
    ControlA.Free;
    ControlB.Free;
    ControlC.Free;
  end;
end;

procedure TControlParentTest.ContainsControl;
var
  ControlA, ControlB, ControlC : TSimpleControl;
begin
  ControlA := TSimpleControl.Create;
  ControlB := TSimpleControl.Create;
  ControlC := TSimpleControl.Create;
  try
    Confirm.IsFalse(ControlA.ContainsControl(ControlC), 'The control shouldn''t be contained.');

    ControlB.Parent := ControlA;
    ControlC.Parent := ControlB;
    Confirm.IsTrue(ControlA.ContainsControl(ControlC), 'The control should be contained.');

    ControlB.Parent := nil;
    Confirm.IsFalse(ControlA.ContainsControl(ControlC), 'The control shouldn''t be contained.');
  finally
    ControlA.Free;
    ControlB.Free;
    ControlC.Free;
  end;
end;

procedure TControlParentTest.FreeParentControl;
var
  ControlA, ControlB : TSimpleControl;
begin
  ControlA := TSimpleControl.Create;
  ControlB := TSimpleControl.Create;
  try
    ControlB.Parent := ControlA;
    Confirm.IsTrue(ControlB.Parent = ControlA);

    ControlA.Free;

    Confirm.IsTrue(not assigned(ControlB.Parent));
  finally
    //ControlA.Free;
    ControlB.Free;
  end;
end;





end.
