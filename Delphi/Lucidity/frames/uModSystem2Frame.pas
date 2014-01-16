unit uModSystem2Frame;

interface

uses
  uLucidityEnums, uLucidityKeyGroupInterface,
  uConstants, eePlugin, eeGuiStandard, eeEnumMenu,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamModSelector, VamTextBox;

type
  TModSystem2Frame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
  private
    ModSourceMenu : TEnumMenu<TModSource>;
    ModViaMenu    : TEnumMenu<TModSource>;

    MenuModSlot : integer;
    MenuKeyGroup : IKeyGroup;

    ModSelectors : array[0..kModSlotCount-1] of TVamModSelector;
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


    procedure ModSourceSelected(Sender : TObject; aSource : TModSource);
    procedure ModViaSelected(Sender : TObject; aSource : TModSource);

    procedure Handle_ShowModSourceMenu(Sender:TObject);
    procedure Handle_ShowModViaMenu(Sender:TObject);

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
  LucidityModConnections,

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

  ModSourceMenu := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModSourceMenu.OnItemSelected := ModSourceSelected;
  //ModSourceMenu.OnClose := EventHandler_ModMenuClosed;

  ModViaMenu    := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModViaMenu.OnItemSelected := ModViaSelected;
  //ModViaMenu.OnClose := EventHandler_ModMenuClosed;





  for c1 := 0 to kModSlotCount-1 do
  begin
    ModSelectors[c1] := TVamModSelector.Create(AOwner);
    ModSelectors[c1].Parent := BackgroundPanel;
    ModSelectors[c1].Visible := true;
    ModSelectors[c1].Tag := c1;
    ModSelectors[c1].OnMouseDown := ModSelectorMouseDown;
    ModSelectors[c1].OnShowModSourceMenu := Handle_ShowModSourceMenu;
    ModSelectors[c1].OnShowModViaMenu    := Handle_ShowModViaMenu;
  end;

  MainSelector     := TVamTextBox.Create(AOwner);
  MainSelector.Parent := BackgroundPanel;
  MainSelector.Visible := true;
  MainSelector.Layout.SetSize(64, 28);
  MainSelector.Layout.SetPos(4,4);
  MainSelector.TextPadding.Bottom := 2;

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

  MainSelector.OnMouseDown := MainSelectorMouseDown;
  BackwardSelector.OnMouseDown := BackwardSelectorMouseDown;
  ForwardSelector.OnMouseDown  := ForwardSelectorMouseDown;


  //======= GUI Stylings ===============

  OffsetX := 64 + 4 + 4;
  PosY := 4;

  for c1 := 0 to kModSlotCount-1 do
  begin
    PosX := OffsetX + c1 * (62 + 4);

    ModSelectors[c1].Layout.SetSize(62,62);
    ModSelectors[c1].Layout.SetPos(PosX, PosY);

    ModSelectors[c1].ColorBorder     := kColor_LcdDark1;
    ModSelectors[c1].Color           := kColor_LcdDark1;
    ModSelectors[c1].ColorMouseOver  := kColor_ButtonMouseOver;
    ModSelectors[c1].TextPadding.Top    := 5;
    ModSelectors[c1].TextPadding.Bottom := 5;
  end;

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

  ModViaMenu.Free;
  ModSourceMenu.Free;

  MenuKeyGroup := nil;

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
    Plugin.Globals.SelectedModSlot := Index;
  end;
end;

procedure TModSystem2Frame.MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    Plugin.Globals.SelectedModSlot := -1;
  end;
end;


procedure TModSystem2Frame.BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := Plugin.Globals.SelectedModSlot - 1;
    if Index < -1 then Index := 7;
    Plugin.Globals.SelectedModSlot := Index;
  end;
end;

procedure TModSystem2Frame.ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := Plugin.Globals.SelectedModSlot + 1;
    if Index > 7 then Index := -1;
    Plugin.Globals.SelectedModSlot := Index;
  end;
end;


procedure TModSystem2Frame.UpdateModulation;
var
  c1 : integer;
  ModSource : TModSource;
  ModSourceText : string;
  kg : IKeyGroup;
  ModConnections : TModConnections;
begin
  //=== update mod slot text ===
  for c1 := 0 to kModSlotCount-1 do
  begin
    kg := Plugin.ActiveKeyGroup;
    ModConnections := kg.GetModConnections;

    ModSource := ModConnections.ModSource[c1];
    ModSourceText := TModSourceHelper.ToShortGuiString(ModSource);
    ModSelectors[c1].Text := ModSourceText;

    ModSource := ModConnections.ModVia[c1];
    ModSourceText := TModSourceHelper.ToShortGuiString(ModSource);
    ModSelectors[c1].TextB := ModSourceText;
  end;



  //=== Update selected slot colors ====
  for c1 := 0 to kModSlotCount-1 do
  begin
    ModSelectors[c1].Color := kColor_LcdDark1;
    ModSelectors[c1].ColorMouseOver  := kColor_ButtonMouseOver;
  end;

  MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  MainSelector.Color          := kColor_LcdDark1;
  MainSelector.ColorMouseOver := kColor_ButtonMouseOver;

  case Plugin.Globals.SelectedModSlot of
    -1 :
    begin
      MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark1);
      MainSelector.Color          := kColor_LcdDark5;
      MainSelector.ColorMouseOver := kColor_LcdDark6;
    end;

    0..7:
    begin
      c1 := Plugin.Globals.SelectedModSlot;
      ModSelectors[c1].Color           := kColor_LcdDark5;
      ModSelectors[c1].ColorMouseOver  := kColor_LcdDark6;
    end;
  end;

end;

procedure TModSystem2Frame.Handle_ShowModSourceMenu(Sender: TObject);
begin
  MenuModSlot := (Sender as TVamModSelector).Tag;
  MenuKeyGroup := Plugin.ActiveKeyGroup;
  ModSourceMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TModSystem2Frame.Handle_ShowModViaMenu(Sender: TObject);
begin
  MenuModSlot := (Sender as TVamModSelector).Tag;
  MenuKeyGroup := Plugin.ActiveKeyGroup;
  ModViaMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TModSystem2Frame.ModSourceSelected(Sender: TObject; aSource: TModSource);
var
  ModConnections : TModConnections;
begin
  ModConnections := MenuKeyGroup.GetModConnections;
  ModConnections.ModSource[MenuModSlot] := aSource;
  Plugin.Globals.SendWindowsMessage(UM_MOD_SLOT_CHANGED);
end;

procedure TModSystem2Frame.ModViaSelected(Sender: TObject; aSource: TModSource);
var
  ModConnections : TModConnections;
begin
  ModConnections := MenuKeyGroup.GetModConnections;
  ModConnections.ModVia[MenuModSlot] := aSource;
  Plugin.Globals.SendWindowsMessage(UM_MOD_SLOT_CHANGED);
end;






end.
