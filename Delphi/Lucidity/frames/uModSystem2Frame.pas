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
  private
    ModSelectors : array[0..kModSlots-1] of TVamModSelector;
    MainSelector : TVamTextBox;
    ForwardSelector : TVamTextBox;
    BackwardSelector : TVamTextBox;
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    MsgHandle : hwnd;
    procedure MessageHandler(var Message : TMessage);

    procedure ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);


    procedure UpdateModulation;


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

  // NOTE: AFAIK TFrame should be receive a windows handle at some stage.
  // for whatever reason this TFrame instance wasn't receiving a handle and
  // i couldn't figure out why. This is a work-around so that the frame
  // can receive messages posted by the EasyEffect Globals class.
  MsgHandle := AllocateHWND(MessageHandler);


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
  if (MsgHandle <> 0) and (assigned(Plugin)) then
  begin
    Plugin.Globals.RemoveWindowsMessageListener(MsgHandle);
  end;
  DeallocateHWnd(MsgHandle);

  inherited;
end;

procedure TModSystem2Frame.MessageHandler(var Message: TMessage);
begin
  if Message.Msg = UM_MOD_SLOT_CHANGED then UpdateModulation;
end;


procedure TModSystem2Frame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  if MsgHandle <> 0 then
  begin
    Plugin.Globals.AddWindowsMessageListener(MsgHandle);
  end;


  // finally
  UpdateModulation;
end;


procedure TModSystem2Frame.ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := (Sender as TVamModSelector).Tag;
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


procedure TModSystem2Frame.UpdateModulation;
var
  c1 : integer;
begin
  for c1 := 0 to kModSlots-1 do
  begin
    ModSelectors[c1].Color := kColor_LcdDark1;
    ModSelectors[c1].ColorMouseOver  := kColor_ButtonMouseOver;
  end;

  MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  MainSelector.Color          := kColor_LcdDark1;
  MainSelector.ColorMouseOver := kColor_ButtonMouseOver;

  case Plugin.SelectedModSlot of
    -1 :
    begin
      MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark1);
      MainSelector.Color          := kColor_LcdDark5;
      MainSelector.ColorMouseOver := kColor_LcdDark6;
    end;

    0..7:
    begin
      c1 := Plugin.SelectedModSlot;
      ModSelectors[c1].Color           := kColor_LcdDark5;
      ModSelectors[c1].ColorMouseOver  := kColor_LcdDark6;
    end;
  end;




end;




end.
