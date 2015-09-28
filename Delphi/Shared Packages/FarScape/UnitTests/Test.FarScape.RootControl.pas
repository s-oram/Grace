unit Test.FarScape.RootControl;

interface

uses
  TestMocks.FarScapeControls,
  FarScape.CustomControl,
  WatchTower;

type
  TRootControlTest = class(TWatchTowerTest)
  private
  protected
    RootControl : TMockRootControl;
    fsc1 : TMockControl;
    fsc2 : TMockControl;
    fsc3 : TMockControl;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure NoRootForStandardControl;

    [Test]
    procedure RootAssignmentA;

    [Test]
    procedure RootAssignmentB;

    [Test]
    procedure RootAssignmentC;

    [Test]
    procedure IsSceneUpdated_TestA;
  end;



implementation

uses
  SysUtils,
  Types,
  WatchTower.Confirm;

{ TRootControlTest }

procedure TRootControlTest.Setup;
begin
  inherited;

  RootControl := TMockRootControl.Create;
  fsc1 := TMockControl.Create;
  fsc2 := TMockControl.Create;
  fsc3 := TMockControl.Create;
end;

procedure TRootControlTest.TearDown;
begin
  inherited;

  if assigned(RootControl) then RootControl.Free;
  if assigned(fsc1) then fsc1.Free;
  if assigned(fsc2) then fsc2.Free;
  if assigned(fsc3) then fsc3.Free;
end;

procedure TRootControlTest.NoRootForStandardControl;
begin
  Confirm.IsTrue(assigned(RootControl.Root));

  Confirm.IsTrue(not assigned(fsc1.Root));
  Confirm.IsTrue(not assigned(fsc2.Root));
  Confirm.IsTrue(not assigned(fsc3.Root));
end;

procedure TRootControlTest.RootAssignmentA;
begin
  // Check to see if the root value is set after assigning to parent.
  fsc1.Parent := RootControl;
  Confirm.IsTrue(fsc1.Root = RootControl);

  // check to see if the root value is cleared after unassigning parent.
  fsc1.Parent := nil;
  Confirm.IsTrue(fsc1.Root <> RootControl);
  Confirm.IsTrue(not assigned(fsc1.Root));
end;



procedure TRootControlTest.RootAssignmentB;
begin
  fsc2.Parent := fsc1;
  fsc1.Parent := RootControl;

  Confirm.IsTrue(fsc1.Root = RootControl);
  Confirm.IsTrue(fsc2.Root = RootControl);
end;

procedure TRootControlTest.RootAssignmentC;
begin
  // test if root reference is cleared if RootControl is free'd.

  fsc2.Parent := fsc1;
  fsc1.Parent := RootControl;

  Confirm.IsTrue(fsc1.Root = RootControl);
  Confirm.IsTrue(fsc2.Root = RootControl);

  FreeAndNil(RootControl);

  Confirm.IsTrue(not assigned(fsc1.Root));
  Confirm.IsTrue(not assigned(fsc2.Root));
end;

procedure TRootControlTest.IsSceneUpdated_TestA;
var
  b1 : TRect;
  b2 : TRect;
begin
  Confirm.IsTrue(RootControl.Scene.ElementCount = 0);

  fsc1.Parent := RootControl;

  Confirm.IsTrue(RootControl.Scene.ElementCount = 1);
  Confirm.IsTrue(RootControl.Scene.Element[0].Control = fsc1 );

  fsc1.SetBounds(10,20,30,40);
  b1 := fsc1.GetAbsoluteRect;
  b2 := RootControl.Scene.Element[0].AbsoluteBoundsRect;

  Confirm.IsTrue(b1.Left   = b2.Left);
  Confirm.IsTrue(b1.Right  = b2.Right);
  Confirm.IsTrue(b1.Top    = b2.Top);
  Confirm.IsTrue(b1.Bottom = b2.Bottom);

end;



end.
