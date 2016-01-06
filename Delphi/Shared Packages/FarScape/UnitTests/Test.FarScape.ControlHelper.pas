unit Test.FarScape.ControlHelper;

interface

uses
  TestMocks.FarScapeControls,
  FarScape.CustomControl,
  WatchTower;

type
  TControlHelperTest = class(TWatchTowerTest)
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
    procedure IsControlChildOf_Test;

    [Test]
    procedure IsShowing;
  end;

implementation

uses
  SysUtils,
  Types,
  WatchTower.Confirm,
  FarScape.ControlHelper;

{ TControlFunctionsTest }

procedure TControlHelperTest.Setup;
begin
  inherited;

  RootControl := TMockRootControl.Create;
  fsc1 := TMockControl.Create;
  fsc2 := TMockControl.Create;
  fsc3 := TMockControl.Create;

  RootControl.IsOwnedByParent := false;
  fsc1.IsOwnedByParent := false;
  fsc2.IsOwnedByParent := false;
  fsc3.IsOwnedByParent := false;
end;

procedure TControlHelperTest.TearDown;
begin
  inherited;

  if assigned(RootControl) then RootControl.Free;
  if assigned(fsc1) then fsc1.Free;
  if assigned(fsc2) then fsc2.Free;
  if assigned(fsc3) then fsc3.Free;
end;

procedure TControlHelperTest.IsControlChildOf_Test;
begin
  Confirm.IsFalse(fsc1.IsChildOf(RootControl));

  fsc1.Parent := RootControl;
  Confirm.IsFalse(RootControl.IsChildOf(fsc1));
  Confirm.IsTrue(fsc1.IsChildOf(RootControl));


  fsc2.Parent := fsc1;
  Confirm.IsTrue(fsc2.IsChildOf(RootControl));

  fsc3.Parent := fsc2;
  Confirm.IsTrue(fsc3.IsChildOf(RootControl));
  Confirm.IsTrue(fsc3.IsChildOf(fsc1));


  fsc2.Parent := nil;
  Confirm.IsFalse(fsc3.IsChildOf(RootControl));
end;


procedure TControlHelperTest.IsShowing;
begin
  fsc2.Parent := fsc1;
  fsc3.Parent := fsc2;
  Confirm.IsTrue(fsc3.IsShowing);
  fsc1.Visible := false;
  Confirm.IsFalse(fsc3.IsShowing);
end;



end.
