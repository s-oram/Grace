unit uModSystem2Frame;

interface

uses
  uConstants, eePlugin, eeGuiStandard,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamModSelector, VamTextBox;

type
  TModSystem2Frame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    VamTextBox1: TVamTextBox;
  private
    ModSelectors : array[0..kModSlots-1] of TVamModSelector;
    MainSelector : TVamTextBox;
    ForwardSelector : TVamTextBox;
    BackwardSelector : TVamTextBox;
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    procedure ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  protected
    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
  end;

implementation

uses
  RedFox, RedFoxColor;

{$R *.dfm}

{ TModSystem2Frame }

constructor TModSystem2Frame.Create(AOwner: TComponent);
var
  c1: Integer;
  PosX, PosY : integer;
  OffsetX : integer;

begin
  inherited;


  //560 - width of mod slots

  OffsetX := 64 + 4 + 4;
  PosY := 4;


  for c1 := 0 to kModSlots-1 do
  begin
    ModSelectors[c1] := TVamModSelector.Create(AOwner);
    ModSelectors[c1].Parent := BackgroundPanel;
    ModSelectors[c1].Visible := true;

    PosX := OffsetX + c1 * (62 + 4);

    ModSelectors[c1].Layout.SetSize(62,62);
    ModSelectors[c1].Layout.SetPos(PosX, PosY);

    ModSelectors[c1].Color := kColor_LcdDark1;
    ModSelectors[c1].ColorMouseOver  := kColor_ButtonMouseOver;


  end;


  MainSelector     := TVamTextBox.Create(AOwner);
  MainSelector.Parent := BackgroundPanel;
  MainSelector.Visible := true;
  MainSelector.Layout.SetSize(64, 28);
  MainSelector.Layout.SetPos(4,4);

  BackwardSelector := TVamTextBox.Create(AOwner);
  BackwardSelector.Parent := BackgroundPanel;
  BackwardSelector.Visible := true;
  BackwardSelector.Layout.SetSize(29, 29);
  BackwardSelector.Layout.SetPos(4,36);

  ForwardSelector  := TVamTextBox.Create(AOwner);
  ForwardSelector.Parent := BackgroundPanel;
  ForwardSelector.Visible := true;
  ForwardSelector.Layout.SetSize(29, 29);
  ForwardSelector.Layout.SetPos(38,36);



  for c1 := 0 to kModSlots-1 do
  begin
    ModSelectors[c1].Tag := c1;
    ModSelectors[c1].OnMouseDown := ModSelectorMouseDown;
  end;

  MainSelector.OnMouseDown := MainSelectorMouseDown;
  BackwardSelector.OnMouseDown := BackwardSelectorMouseDown;
  ForwardSelector.OnMouseDown  := ForwardSelectorMouseDown;


  // Apply Colours

  MainSelector.Text := 'Main';
  MainSelector.TextAlign := TRedFoxAlign.AlignCenter;
  MainSelector.TextVAlign := TRedFoxAlign.AlignCenter;

  ForwardSelector.Text := '>';
  ForwardSelector.TextAlign := TRedFoxAlign.AlignCenter;
  ForwardSelector.TextVAlign := TRedFoxAlign.AlignCenter;

  BackwardSelector.Text := '<';
  BackwardSelector.TextAlign := TRedFoxAlign.AlignCenter;
  BackwardSelector.TextVAlign := TRedFoxAlign.AlignCenter;


  MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  BackwardSelector.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  ForwardSelector.Font.Color  := GetRedFoxColor(kColor_LcdDark5);

  MainSelector.Color     := kColor_LcdDark1;
  BackwardSelector.Color := kColor_LcdDark1;
  ForwardSelector.Color  := kColor_LcdDark1;

  MainSelector.ColorMouseOver     := kColor_ButtonMouseOver;
  BackwardSelector.ColorMouseOver := kColor_ButtonMouseOver;
  ForwardSelector.ColorMouseOver  := kColor_ButtonMouseOver;
end;

destructor TModSystem2Frame.Destroy;
begin

  inherited;
end;

procedure TModSystem2Frame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;
end;


procedure TModSystem2Frame.ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := (Sender as TVamTextBox).Tag;
    Plugin.SelectedModSlot := Index;
  end;
end;

procedure TModSystem2Frame.MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    Plugin.SelectedModSlot := -1;
  end;
end;

procedure TModSystem2Frame.BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := Plugin.SelectedModSlot - 1;
    if Index < -1 then Index := 7;
    Plugin.SelectedModSlot := Index;
  end;
end;

procedure TModSystem2Frame.ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := Plugin.SelectedModSlot + 1;
    if Index > 7 then Index := -1;
    Plugin.SelectedModSlot := Index;
  end;
end;


end.
