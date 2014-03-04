unit uModSystem2Frame;

interface

uses
  uDialogDisplayArea,
  Menu.ModSelectorContextMenu,
  VamLib.ZeroObject,
  uLucidityEnums, uLucidityKeyGroupInterface,
  uConstants, eePlugin, eeGuiStandard, eeEnumMenu,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamModSelector, VamTextBox;

type
  TModSystem2Frame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
  private
    // ActiveModParIndex determines what modulation amounts the modulation
    // selectors show.
    ActiveModParIndex : integer;

    ModContextMenu : TModSelectorContextMenu;

    MenuModSlot : integer;
    MenuKeyGroup : IKeyGroup;

    ModSelectors : array[0..kModSlotCount-1] of TVamModSelector;
    MainSelector : TVamTextBox;
    ForwardSelector : TVamTextBox;
    BackwardSelector : TVamTextBox;
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    CurrentMouseOverControl : TControl;

    MsgHandle : hwnd;
    procedure MessageHandler(var Message : TMessage);

    procedure UpdateModulation;
    procedure UpdateModSelector_ModulationAmounts;


    procedure Handle_ActiveModParIndexChanged(Data : Pointer);

    procedure Handle_ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_ModSelectorMouseEnter(Sender : TObject);
    procedure Handle_ModSelectorMouseLeave(Sender : TObject);

    procedure Handle_ShowModContextMenu(Sender : TObject);
  private
    FMotherShip : IMothership;
    function GetMotherShipReference:IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
  end;

implementation

uses
  LucidityModConnections,
  eeVstParameterEx,
  VamLib.Utils,
  VamLib.Threads,
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

  CurrentMouseOverControl := nil;

  // NOTE: AFAIK TFrame should be receive a windows handle at some stage.
  // for whatever reason this TFrame instance wasn't receiving a handle and
  // i couldn't figure out why. This is a work-around so that the frame
  // can receive messages posted by the EasyEffect Globals class.
  MsgHandle := AllocateHWND(MessageHandler);

  ModContextMenu := TModSelectorContextMenu.Create;

  ActiveModParIndex := -1;

  for c1 := 0 to kModSlotCount-1 do
  begin
    ModSelectors[c1] := TVamModSelector.Create(AOwner);
    ModSelectors[c1].Parent := BackgroundPanel;
    ModSelectors[c1].Visible := true;
    ModSelectors[c1].Tag := c1;
    ModSelectors[c1].OnMouseDown := Handle_ModSelectorMouseDown;
    ModSelectors[c1].OnShowContextMenu := Handle_ShowModContextMenu;
    ModSelectors[c1].OnMouseEnter := self.Handle_ModSelectorMouseEnter;
    ModSelectors[c1].OnMouseLeave := self.Handle_ModSelectorMouseLeave;
  end;

  MainSelector     := TVamTextBox.Create(AOwner);
  MainSelector.OnMouseEnter := self.Handle_ModSelectorMouseEnter;
  MainSelector.OnMouseLeave := self.Handle_ModSelectorMouseLeave;
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

  MainSelector.OnMouseDown := Handle_MainSelectorMouseDown;
  BackwardSelector.OnMouseDown := Handle_BackwardSelectorMouseDown;
  ForwardSelector.OnMouseDown  := Handle_ForwardSelectorMouseDown;


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
  MainSelector.ShowBorder := true;
  MainSelector.ColorBorder := kColor_LcdDark1;

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

  if (assigned(FMotherShip))
    then FMotherShip.DeregisterZeroObject(self);

  ModContextMenu.Free;

  MenuKeyGroup := nil;

  inherited;
end;

function TModSystem2Frame.GetMotherShipReference: IMotherShip;
begin
  result := FMotherShip;
end;

procedure TModSystem2Frame.MessageHandler(var Message: TMessage);
begin
  //TODO: Delete
end;


procedure TModSystem2Frame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  if MsgID = TLucidMsgID.ModSlotChanged           then UpdateModulation;
  if MsgID = TLucidMsgID.ActiveModParIndexChanged then Handle_ActiveModParIndexChanged(Data);
  if MsgID = TLucidMsgID.ModAmountChanged         then UpdateModSelector_ModulationAmounts;
end;

procedure TModSystem2Frame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMothership;
end;

procedure TModSystem2Frame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  if MsgHandle <> 0 then
  begin
    Plugin.Globals.AddWindowsMessageListener(MsgHandle);
  end;

  ModContextMenu.Initialize(aPlugin, aDialogDisplayArea);

  // finally
  UpdateModulation;
end;


procedure TModSystem2Frame.Handle_ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := (Sender as TVamModSelector).Tag;
    Plugin.Globals.SelectedModSlot := Index;
  end;
end;

procedure TModSystem2Frame.Handle_MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    Plugin.Globals.SelectedModSlot := -1;
  end;
end;


procedure TModSystem2Frame.Handle_BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TModSystem2Frame.Handle_ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
  IsMute    : boolean;
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

    ModSource := ModConnections.GetModSource(c1);
    ModSourceText := TModSourceHelper.ToShortGuiString(ModSource);
    ModSelectors[c1].Text := ModSourceText;

    ModSource := ModConnections.GetModVia(c1);
    ModSourceText := TModSourceHelper.ToShortGuiString(ModSource);
    ModSelectors[c1].TextB := ModSourceText;

    IsMute := ModConnections.GetModMute(c1);
    ModSelectors[c1].ShowMuteIcon := IsMute;
  end;



  //=== Update selected slot colors ====
  MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  MainSelector.Color          := kColor_LcdDark1;
  MainSelector.ColorMouseOver := kColor_ButtonMouseOver;

  for c1 := 0 to kModSlotCount-1 do
  begin
    ModSelectors[c1].Font.Color := GetRedFoxColor(kColor_LcdDark5);
    ModSelectors[c1].Color := kColor_LcdDark1;
    ModSelectors[c1].ColorMouseOver  := kColor_ButtonMouseOver;
  end;



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
      ModSelectors[c1].Font.Color     := GetRedFoxColor(kColor_LcdDark1);
      ModSelectors[c1].Color           := kColor_LcdDark5;
      ModSelectors[c1].ColorMouseOver  := kColor_LcdDark6;
    end;
  end;

  // Call the next update method.
  UpdateModSelector_ModulationAmounts;
end;

procedure TModSystem2Frame.UpdateModSelector_ModulationAmounts;
var
  c1 : integer;
  Par : TVstParameterEx;
  x1, x2 : single;
  ModAmount : single;
begin
  if ActiveModParIndex = -1 then
  begin
    for c1 := 0 to kModSlotCount-1 do
    begin
      ModSelectors[c1].ShowModAmount := false;
    end;
  end else
  begin

    for c1 := 0 to kModSlotCount-1 do
    begin
      //x1 := Plugin.ActiveKeyGroup.GetModParValue(ActiveModParIndex);
      x1 := 0.5;
      ModAmount := Plugin.ActiveKeyGroup.GetModParModAmount(ActiveModParIndex, c1);
      ModAmount := ModAmount * 0.5;
      x2 := x1 + ModAmount;
      x2 := Clamp(x2, 0, 1);
      ModSelectors[c1].ModAmountX1   := x1;
      ModSelectors[c1].ModAmountX2   := x2;

      if abs(x1 - x2) <> 0
        then ModSelectors[c1].ShowModAmount := true
        else ModSelectors[c1].ShowModAmount := false;
    end;
  end;

end;

procedure TModSystem2Frame.Handle_ShowModContextMenu(Sender: TObject);
var
  ModSlotIndex : integer;
begin
  ModSlotIndex := (Sender as TVamModSelector).Tag;
  ModContextMenu.Popup(ModSlotIndex, Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TModSystem2Frame.Handle_ModSelectorMouseEnter(Sender: TObject);
var
  Index : integer;
begin
  CurrentMouseOverControl := Sender as TControl;

  if CurrentMouseOverControl = MainSelector
    then Index := -1
    else Index := (Sender as TVamModSelector).Tag;

  Plugin.Globals.MouseOverModSlot := Index;
  Plugin.Globals.IsMouseOverModSlot := true;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
end;

procedure TModSystem2Frame.Handle_ModSelectorMouseLeave(Sender: TObject);
var
  LeaveControl : TProc;
begin
  LeaveControl := procedure
  begin
    if CurrentMouseOverControl = Sender then
    begin
      CurrentMouseOverControl := nil;

      Plugin.Globals.IsMouseOverModSlot := false;
      Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
    end;
  end;

  RunTask(LeaveControl, 150, nil);
end;


procedure TModSystem2Frame.Handle_ActiveModParIndexChanged(Data: Pointer);
begin
  ActiveModParIndex := Integer(Data^);
  UpdateModSelector_ModulationAmounts
end;









end.
