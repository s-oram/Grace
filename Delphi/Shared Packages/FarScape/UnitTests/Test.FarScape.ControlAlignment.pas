unit Test.FarScape.ControlAlignment;

interface

uses
  TestMocks.FarScapeControls,
  FarScape.CustomControl,
  FarScape.ControlHelper,
  WatchTower;

type
  TControlAlignmentTest = class(TWatchTowerTest)
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
    procedure GetLastAlignmentTarget_Test;

    [Test]
    procedure TopAlignment_TestA;

    [Test]
    procedure TopAlignment_TestB;

    [Test]
    procedure TopAlignment_TestC;

    [Test]
    procedure TopAlignment_TestD;

    [Test]
    procedure BottomAlignment_TestA;

    [Test]
    procedure LeftAlignment;

    [Test]
    procedure RightAlignment;

    [Test]
    procedure CenterAlignment;

    [Test]
    procedure ClientAlignment;

    [Test]
    procedure GridAlignment_TestA;

    [Test]
    procedure GridAlignment_TestB;

    [Test]
    procedure NoAlignment;
  end;


implementation

uses
  FarScape.Assistant.AlignControls,
  WatchTower.Confirm,
  Types;

{ TControlAlignmentTest }

procedure TControlAlignmentTest.Setup;
begin
  inherited;

  RootControl := TMockRootControl.Create;
  fsc1 := TMockControl.Create;
  fsc2 := TMockControl.Create;
  fsc3 := TMockControl.Create;

  fsc1.IsOwnedByParent := false;
  fsc2.IsOwnedByParent := false;
  fsc3.IsOwnedByParent := false;
end;

procedure TControlAlignmentTest.TearDown;
begin
  inherited;

  if assigned(RootControl) then RootControl.Free;
  if assigned(fsc1) then fsc1.Free;
  if assigned(fsc2) then fsc2.Free;
  if assigned(fsc3) then fsc3.Free;
end;

procedure TControlAlignmentTest.GetLastAlignmentTarget_Test;
var
  LastControl : TFarScapeControl;
begin
  fsc1.Parent := RootControl;
  fsc2.Parent := RootControl;
  fsc3.Parent := RootControl;

  //===== Part 1 =====

  fsc1.Align := TControlAlignment.caRight;
  fsc2.Align := TControlAlignment.caRight;
  fsc3.Align := TControlAlignment.caRight;

  LastControl := ControlAlignmentAssistant.FindLastAlignmentTarget(RootControl, TControlAlignment.caRight);

  Confirm.IsTrue(LastControl = fsc3);


  //===== Part 2 =====

  fsc1.Align := TControlAlignment.caTop;
  fsc2.Align := TControlAlignment.caTop;
  fsc3.Align := TControlAlignment.caTop;

  LastControl := ControlAlignmentAssistant.FindLastAlignmentTarget(RootControl, TControlAlignment.caTop);

  Confirm.IsTrue(LastControl = fsc3);


  //===== Part 3 =====

  fsc1.Align := TControlAlignment.caTop;
  fsc2.Align := TControlAlignment.caTop;
  fsc3.Align := TControlAlignment.caBottom;

  LastControl := ControlAlignmentAssistant.FindLastAlignmentTarget(RootControl, TControlAlignment.caTop);

  Confirm.IsTrue(LastControl = fsc2);

  //===== Part 4 =====

  fsc1.Align := TControlAlignment.caTop;
  fsc2.Align := TControlAlignment.caTop;
  fsc3.Align := TControlAlignment.caBottom;

  fsc2.Visible := false;

  LastControl := ControlAlignmentAssistant.FindLastAlignmentTarget(RootControl, TControlAlignment.caTop);

  Confirm.IsTrue(LastControl = fsc1);
end;

procedure TControlAlignmentTest.TopAlignment_TestA;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc1.Align := TControlAlignment.caTop;
  fsc1.Parent := RootControl;

  Bounds := fsc1.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft := 0;
  ExpectedTop := 0;
  ExpectedWidth := 100;
  ExpectedHeight := 10;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.TopAlignment_TestB;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc1.Align := TControlAlignment.caTop;
  fsc1.Parent := RootControl;

  fsc2.SetBounds(0, 0, 25, 30);
  fsc2.Align := TControlAlignment.caTop;
  fsc2.Parent := RootControl;

  fsc3.SetBounds(0, 0, 25, 30);
  fsc3.Align := TControlAlignment.caTop;
  fsc3.Parent := RootControl;
  fsc3.SetMargins(0, 15, 0, 0);

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft := 0;
  ExpectedTop := 55;
  ExpectedWidth := 100;
  ExpectedHeight := 30;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.TopAlignment_TestC;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc1.Align := TControlAlignment.caTop;
  fsc1.Parent := RootControl;

  fsc2.SetBounds(0, 0, 25, 30);
  fsc2.Align := TControlAlignment.caTop;
  fsc2.Parent := RootControl;

  fsc3.SetBounds(0, 0, 25, 30);
  fsc3.Align := TControlAlignment.caTop;
  fsc3.Parent := RootControl;
  fsc3.SetMargins(0, 15, 0, 0);

  RootControl.SetPadding(10,10,10,10);

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft := 10;
  ExpectedTop := 65;
  ExpectedWidth := 80;
  ExpectedHeight := 30;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.TopAlignment_TestD;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc1.Align := TControlAlignment.caTop;
  fsc1.Parent := RootControl;

  fsc2.SetBounds(0, 0, 25, 30);
  fsc2.Align := TControlAlignment.caTop;
  fsc2.Parent := RootControl;

  fsc3.SetBounds(0, 0, 25, 30);
  fsc3.Align := TControlAlignment.caTop;
  fsc3.Parent := RootControl;
  fsc3.SetMargins(0, 15, 0, 0);

  RootControl.SetPadding(10,10,10,10);

  fsc2.Visible := false;

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft := 10;
  ExpectedTop := 35;
  ExpectedWidth := 80;
  ExpectedHeight := 30;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.BottomAlignment_TestA;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetMargins(666,666,666,666);

  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc2.SetBounds(0, 0, 10, 10);
  fsc3.SetBounds(0, 0, 10, 10);

  fsc1.Align := TControlAlignment.caBottom;
  fsc2.Align := TControlAlignment.caBottom;
  fsc3.Align := TControlAlignment.caBottom;

  fsc1.SetMargins(10,12,10,12);

  RootControl.SetPadding(10,5,10,7);

  fsc1.Parent := RootControl;
  fsc2.Parent := RootControl;
  fsc3.Parent := RootControl;

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft := 10;
  ExpectedTop := 100 - RootControl.Padding.Bottom;
  ExpectedTop := ExpectedTop - fsc1.Margins.Top - fsc1.Margins.Bottom - fsc1.Height;
  ExpectedTop := ExpectedTop - fsc2.Margins.Top - fsc2.Margins.Bottom - fsc2.Height;
  ExpectedTop := ExpectedTop - fsc3.Margins.Bottom - fsc3.Height;
  ExpectedWidth := 80;
  ExpectedHeight := 10;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.LeftAlignment;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc2.SetBounds(0, 0, 10, 10);
  fsc3.SetBounds(0, 0, 10, 10);

  fsc1.Align := TControlAlignment.caLeft;
  fsc2.Align := TControlAlignment.caLeft;
  fsc3.Align := TControlAlignment.caLeft;

  fsc1.Parent := RootControl;
  fsc2.Parent := RootControl;
  fsc3.Parent := RootControl;

  RootControl.SetPadding(3,5,7,13);
  fsc2.SetMargins(17,23,33,37);

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 20 + 3 + 17 + 33;
  ExpectedTop    := 5;
  ExpectedWidth  := 10;
  ExpectedHeight := 100 - 5 - 13;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.RightAlignment;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc2.SetBounds(0, 0, 10, 10);
  fsc3.SetBounds(0, 0, 10, 10);

  fsc1.Align := TControlAlignment.caRight;
  fsc2.Align := TControlAlignment.caRight;
  fsc3.Align := TControlAlignment.caRight;

  fsc1.Parent := RootControl;
  fsc2.Parent := RootControl;
  fsc3.Parent := RootControl;

  RootControl.SetPadding(3,5,7,13);
  fsc2.SetMargins(17,23,33,37);

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 100 - 30 - 7 - 17 - 33;
  ExpectedTop    := 5;
  ExpectedWidth  := 10;
  ExpectedHeight := 100 - 5 - 13;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.CenterAlignment;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc2.SetBounds(0, 0, 10, 10);
  fsc3.SetBounds(0, 0, 10, 10);

  fsc1.Align := TControlAlignment.caTop;
  fsc2.Align := TControlAlignment.caTop;
  fsc3.Align := TControlAlignment.caCenter;

  fsc1.Parent := RootControl;
  fsc2.Parent := RootControl;
  fsc3.Parent := RootControl;

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 45;
  ExpectedTop    := 55;
  ExpectedWidth  := 10;
  ExpectedHeight := 10;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.ClientAlignment;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(0, 0, 10, 10);
  fsc2.SetBounds(0, 0, 10, 10);
  fsc3.SetBounds(0, 0, 10, 10);

  fsc1.Align := TControlAlignment.caTop;
  fsc2.Align := TControlAlignment.caTop;
  fsc3.Align := TControlAlignment.caClient;

  fsc1.Parent := RootControl;
  fsc2.Parent := RootControl;
  fsc3.Parent := RootControl;

  Bounds := fsc3.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 0;
  ExpectedTop    := 20;
  ExpectedWidth  := 100;
  ExpectedHeight := 80;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.NoAlignment;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetBounds(25, 10, 12, 15);

  // Margins and padding should have no effect on the control position when not aligned..
  fsc1.SetPadding(666,666,666,666);
  fsc1.SetMargins(666,666,666,666);

  fsc1.Align := TControlAlignment.caNone;

  fsc1.Parent := RootControl;

  Bounds := fsc1.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 25;
  ExpectedTop    := 10;
  ExpectedWidth  := 12;
  ExpectedHeight := 15;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.GridAlignment_TestA;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 100, 100);
  fsc1.SetGridDivisions(24, 24);

  fsc1.Align := TControlAlignment.caGrid;
  fsc1.Parent := RootControl;

  fsc1.SetGridBounds(1,1,1,1);

  Bounds := fsc1.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 4;
  ExpectedTop    := 4;
  ExpectedWidth  := 4;
  ExpectedHeight := 4;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

procedure TControlAlignmentTest.GridAlignment_TestB;
var
  ExpectedTop    : integer;
  ExpectedLeft   : integer;
  ExpectedWidth  : integer;
  ExpectedHeight : integer;
  Bounds : TRect;
begin
  RootControl.SetBounds(0, 0, 200, 200);
  fsc1.SetGridDivisions(24, 24);
  fsc2.SetGridDivisions(24, 24);

  fsc1.Align := TControlAlignment.caGrid;
  fsc1.Parent := RootControl;

  fsc2.Align := TControlAlignment.caGrid;
  fsc2.Parent := RootControl;


  //==== Part 1 ====
  fsc1.SetGridBounds(1,1,1,1);

  Bounds := fsc1.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 8;
  ExpectedTop    := 8;
  ExpectedWidth  := 9;
  ExpectedHeight := 9;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);



  //==== Part 2 ====
  fsc2.SetGridBounds(2,2,1,1);

  Bounds := fsc2.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 8+9;
  ExpectedTop    := 8+9;
  ExpectedWidth  := 8;
  ExpectedHeight := 8;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);


  //==== Part 3 ====
  fsc2.SetGridBounds(2,2,1,1);
  fsc2.SetMargins(2,2,2,2);

  Bounds := fsc2.GetBoundsInReferenceTo(RootControl);

  ExpectedLeft   := 8+9+2;
  ExpectedTop    := 8+9+2;
  ExpectedWidth  := 8-4;
  ExpectedHeight := 8-4;

  Confirm.IsTrue(Bounds.Left   = ExpectedLeft);
  Confirm.IsTrue(Bounds.Top    = ExpectedTop);
  Confirm.IsTrue(Bounds.Width  = ExpectedWidth);
  Confirm.IsTrue(Bounds.Height = ExpectedHeight);
end;

end.
