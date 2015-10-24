unit Test.FarScape.Parentage;

interface

uses
  FarScape.CustomControl,
  WatchTower;

type
  TControlParentTest = class(TWatchTowerTest)
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure NoParentOnCreate;

    [Test]
    procedure TestParentHierarchy;

    [Test]
    procedure ContainsControl;

    [Test]
    procedure FreeParentControl;

    [Test]
    procedure FreeParentControlWhileOwned;
  end;

  TSimpleControl = class(TFarScapeControl)
  end;

implementation

uses
  SysUtils,
  WatchTower.Confirm;

{ TControlParentTest }

procedure TControlParentTest.Setup;
begin
  inherited;

end;

procedure TControlParentTest.TearDown;
begin
  inherited;

end;



procedure TControlParentTest.NoParentOnCreate;
var
  c : TSimpleControl;
begin
  c := TSimpleControl.Create;
  c.IsOwnedByParent := false;
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

  ControlA.IsOwnedByParent := false;
  ControlB.IsOwnedByParent := false;
  ControlC.IsOwnedByParent := false;
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

  ControlA.IsOwnedByParent := false;
  ControlB.IsOwnedByParent := false;
  ControlC.IsOwnedByParent := false;

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

  ControlA.IsOwnedByParent := false;
  ControlB.IsOwnedByParent := false;

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

procedure TControlParentTest.FreeParentControlWhileOwned;
var
  ControlA, ControlB, ControlC : TSimpleControl;
begin
  ControlA := TSimpleControl.Create;
  ControlB := TSimpleControl.Create;
  ControlC := TSimpleControl.Create;

  Confirm.IsTrue( ControlA.IsOwnedByParent );
  Confirm.IsTrue( ControlB.IsOwnedByParent );
  Confirm.IsTrue( ControlC.IsOwnedByParent );

  ControlB.Parent := ControlA;
  ControlC.Parent := ControlB;

  ControlA.Free;

  // TODO:MED should confirm that the child controls have
  // be deleted. Right now I'm relying on FastMM to report
  // a memory leak if the controls haven't been freed.
end;

end.
