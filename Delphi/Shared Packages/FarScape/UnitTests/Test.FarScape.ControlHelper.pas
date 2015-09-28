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
    procedure GetControlBoundsInReferenceTo_TestA;

    [Test]
    procedure GetControlBoundsInReferenceTo_TestB;

    [Test]
    procedure GetControlBoundsInReferenceTo_TestC;

    [Test]
    procedure GetControlBoundsInReferenceTo_TestD;

    [Test]
    procedure GetControlBoundsWithChildren_TestA;

    [Test]
    procedure GetControlBoundsWithChildren_TestB;

    [Test]
    procedure GetControlBoundsWithChildren_TestC;

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

procedure TControlHelperTest.GetControlBoundsInReferenceTo_TestA;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0,0,400,400);
  fsc1.SetBounds(25,25,150,100);

  ExpectedLeft := 0;
  ExpectedTop := 0;
  ExpectedWidth := 150;
  ExpectedHeight := 100;

  Bounds := fsc1.GetBoundsInReferenceTo(fsc1);
  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlHelperTest.GetControlBoundsInReferenceTo_TestB;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0,0,400,400);
  fsc1.SetBounds(25,25,150,100);
  fsc1.Parent := RootControl;

  Bounds := fsc1.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft := 25;
  ExpectedTop := 25;
  ExpectedWidth := 150;
  ExpectedHeight := 100;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlHelperTest.GetControlBoundsInReferenceTo_TestC;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0,0,400,400);
  fsc1.SetBounds(20,10, 300,300);
  fsc2.SetBounds(25,25,150,100);

  fsc1.Parent := RootControl;
  fsc2.Parent := fsc1;

  ExpectedLeft   := 45;
  ExpectedTop    := 35;
  ExpectedWidth  := 150;
  ExpectedHeight := 100;

  Bounds := fsc2.GetBoundsInReferenceTo(RootControl);
  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlHelperTest.GetControlBoundsInReferenceTo_TestD;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0,0,400,400);
  fsc1.SetBounds(-40,-30, 300,300);
  fsc2.SetBounds(25,25,150,100);

  fsc1.Parent := RootControl;
  fsc2.Parent := fsc1;

  ExpectedLeft   := -15;
  ExpectedTop    := -5;
  ExpectedWidth  := 150;
  ExpectedHeight := 100;

  Bounds := fsc2.GetBoundsInReferenceTo(RootControl);
  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlHelperTest.GetControlBoundsWithChildren_TestA;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  fsc1.SetBounds(-40,-30, 300,350);
  fsc2.SetBounds(25,25,150,100);

  ExpectedLeft   := 0;
  ExpectedTop    := 0;
  ExpectedWidth  := 300;
  ExpectedHeight := 350;

  Bounds := fsc1.GetBoundsWithChildren;
  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlHelperTest.GetControlBoundsWithChildren_TestB;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  fsc1.SetBounds(200,100, 300,350);
  fsc2.SetBounds(-10,-20,320,370);
  fsc2.Parent := fsc1;

  ExpectedLeft   := -10;
  ExpectedTop    := -20;
  ExpectedWidth  := 320;
  ExpectedHeight := 370;

  Bounds := fsc1.GetBoundsWithChildren;
  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlHelperTest.GetControlBoundsWithChildren_TestC;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  fsc1.SetBounds(200,100, 300,350);
  fsc2.SetBounds(-10,-20,320,370);
  fsc2.Parent := fsc1;
  fsc3.SetBounds(-20,-30,10,10);
  fsc3.Parent := fsc2;

  ExpectedLeft   := -30;
  ExpectedTop    := -50;
  ExpectedWidth  := 340;
  ExpectedHeight := 400;

  Bounds := fsc1.GetBoundsWithChildren;
  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
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
