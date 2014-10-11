unit uModSystemFrame;

interface

uses
  Menu.ModSelectorContextMenu,
  VamLib.ZeroObject,
  uLucidityEnums, Lucidity.Interfaces,
  uConstants, eePlugin, eeGuiStandardv2, eeEnumMenu,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamModSelector, VamTextBox;

type
  TModSystemFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
  private
    ModContextMenu : TModSelectorContextMenu;
    ActiveParameter : string;
    MenuKeyGroup : IKeyGroup;

    ModSelectors : array[0..kModSlotCount-1] of TVamModSelector;
    MainSelector : TVamTextBox;
    ForwardSelector : TVamTextBox;
    BackwardSelector : TVamTextBox;
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    CurrentMouseOverControl : TControl;

    procedure UpdateModulation;
    procedure UpdateModSelector_ModulationAmounts;

    procedure Handle_ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_ModSelectorMouseEnter(Sender : TObject);
    procedure Handle_ModSelectorMouseLeave(Sender : TObject);

    procedure Handle_ShowModContextMenu(Sender : TObject);
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);
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
  OtlParallel,
  Lucidity.PluginParameters,
  LucidityModConnections,
  VamLib.Utils,
  VamLib.Threads,
  Lucidity.Types,
  RedFox, RedFoxColor;

{$R *.dfm}

{ TModSystem2Frame }

constructor TModSystemFrame.Create(AOwner: TComponent);
var
  c1: Integer;
  PosX, PosY : integer;
  OffsetX : integer;
begin
  inherited;

  CurrentMouseOverControl := nil;

  ModContextMenu := TModSelectorContextMenu.Create;

  ActiveParameter := '';

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

    ModSelectors[c1].Color_ModAmountOff := kColor_LcdDark3;
    ModSelectors[c1].Color_ModAmountOn  := kColor_LcdDark5;
  end;

  MainSelector.Text := 'Main';
  MainSelector.TextAlign := TRedFoxAlign.AlignCenter;
  MainSelector.TextVAlign := TRedFoxAlign.AlignCenter;
  MainSelector.ShowBorder := true;
  MainSelector.ColorBorder := kColor_LcdDark1;
  MainSelector.CornerRadius[0] := 2;
  MainSelector.CornerRadius[1] := 2;
  MainSelector.CornerRadius[2] := 2;
  MainSelector.CornerRadius[3] := 2;

  ForwardSelector.Text := '>';
  ForwardSelector.TextAlign := TRedFoxAlign.AlignCenter;
  ForwardSelector.TextVAlign := TRedFoxAlign.AlignCenter;
  ForwardSelector.CornerRadius[0] := 2;
  ForwardSelector.CornerRadius[1] := 2;
  ForwardSelector.CornerRadius[2] := 2;
  ForwardSelector.CornerRadius[3] := 2;

  BackwardSelector.Text := '<';
  BackwardSelector.TextAlign := TRedFoxAlign.AlignCenter;
  BackwardSelector.TextVAlign := TRedFoxAlign.AlignCenter;
  BackwardSelector.CornerRadius[0] := 2;
  BackwardSelector.CornerRadius[1] := 2;
  BackwardSelector.CornerRadius[2] := 2;
  BackwardSelector.CornerRadius[3] := 2;

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

destructor TModSystemFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  ModContextMenu.Free;

  MenuKeyGroup := nil;

  inherited;
end;

procedure TModSystemFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IInterface);
var
  s : string;
begin
  if MsgID = TLucidMsgID.NewPatchLoaded           then UpdateModulation;
  if MsgID = TLucidMsgID.SampleFocusChanged       then UpdateModulation;
  if MsgID = TLucidMsgID.Command_UpdateGUI        then UpdateModulation;
  if MsgID = TLucidMsgID.ModSlotChanged           then UpdateModulation;
  if MsgID = TLucidMsgID.ModAmountChanged         then UpdateModSelector_ModulationAmounts;

  if MsgID = TLucidMsgID.OnActiveParameterChanged then
  begin
    s := string(Data^);
    ActiveParameter := s;
    UpdateModSelector_ModulationAmounts;
  end;

  if MsgID = TLucidMsgID.Command_DisposeKeyGroup then
  begin
    MenuKeyGroup := nil;
  end;
end;

procedure TModSystemFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMothership;
end;

procedure TModSystemFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  ModContextMenu.Initialize(aPlugin);



  // finally
  UpdateModulation;
end;


procedure TModSystemFrame.Handle_ModSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := (Sender as TVamModSelector).Tag;
    Plugin.Globals.GuiState.SelectedModSlot := Index;
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;

procedure TModSystemFrame.Handle_MainSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    Plugin.Globals.GuiState.SelectedModSlot := -1;
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;


procedure TModSystemFrame.Handle_BackwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := Plugin.Globals.GuiState.SelectedModSlot - 1;
    if Index < -1 then Index := 7;
    Plugin.Globals.GuiState.SelectedModSlot := Index;
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;

procedure TModSystemFrame.Handle_ForwardSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  if Button = mbLeft then
  begin
    Index := Plugin.Globals.GuiState.SelectedModSlot + 1;
    if Index > 7 then Index := -1;
    Plugin.Globals.GuiState.SelectedModSlot := Index;
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
  end;
end;


procedure TModSystemFrame.UpdateModulation;
var
  c1 : integer;
  IsMute    : boolean;
  ModSource : TModSource;
  ModSourceText : string;
  kg : IKeyGroup;
  ModConnections : TModConnections;
  ModSourcePolarity : TModSourcePolarity;
begin
  kg := Plugin.ActiveKeyGroup;
  if not assigned(kg) then exit;

  //=== update mod slot text ===
  for c1 := 0 to kModSlotCount-1 do
  begin
    ModConnections := kg.GetModConnections;

    ModSourcePolarity := kg.GetModConnections.GetModSourcePolarity(c1);

    ModSource := ModConnections.GetModSource(c1);
    ModSourceText := TModSourceHelper.ToShortGuiString(ModSource);
    if ModSource <> TModSource.None then
    begin
      if ModSourcePolarity = TModSourcePolarity.Unipolar
        then ModSourceText := ModSourceText + '+'
        else ModSourceText := ModSourceText + '+/-';
    end;
    ModSelectors[c1].Text := ModSourceText;

    ModSource := ModConnections.GetModVia(c1);
    ModSourceText := TModSourceHelper.ToShortGuiString(ModSource);
    ModSelectors[c1].TextB := ModSourceText;

    IsMute := ModConnections.GetModMute(c1);
    ModSelectors[c1].ShowMuteIcon := IsMute;

    // TODO:HIGH find a way to dectect when this property should be true.
    ModSelectors[c1].ShowModulationActiveIcon := false;
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



  case Plugin.Globals.GuiState.SelectedModSlot of
    -1 :
    begin
      MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark1);
      MainSelector.Color          := kColor_LcdDark5;
      MainSelector.ColorMouseOver := kColor_LcdDark6;
    end;

    0..7:
    begin
      c1 := Plugin.Globals.GuiState.SelectedModSlot;
      ModSelectors[c1].Font.Color     := GetRedFoxColor(kColor_LcdDark1);
      ModSelectors[c1].Color           := kColor_LcdDark5;
      ModSelectors[c1].ColorMouseOver  := kColor_LcdDark6;
    end;
  end;

  // Call the next update method.
  UpdateModSelector_ModulationAmounts;
end;



procedure TModSystemFrame.Handle_ShowModContextMenu(Sender: TObject);
var
  ModSlotIndex : integer;
begin
  ModSlotIndex := (Sender as TVamModSelector).Tag;
  ModContextMenu.Popup(ModSlotIndex, Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TModSystemFrame.Handle_ModSelectorMouseEnter(Sender: TObject);
var
  Index : integer;
begin
  CurrentMouseOverControl := Sender as TControl;

  if CurrentMouseOverControl = MainSelector
    then Index := -1
    else Index := (Sender as TVamModSelector).Tag;

  Plugin.Globals.GuiState.MouseOverModSlot := Index;
  Plugin.Globals.GuiState.IsMouseOverModSlot := true;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
end;

procedure TModSystemFrame.Handle_ModSelectorMouseLeave(Sender: TObject);
var
  LeaveControl : TProc;
begin
  LeaveControl := procedure
  begin
    if CurrentMouseOverControl = Sender then
    begin
      CurrentMouseOverControl := nil;

      Plugin.Globals.GuiState.IsMouseOverModSlot := false;
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
    end;
  end;


  DelayedAction(150, LeaveControl);
end;



procedure TModSystemFrame.UpdateModSelector_ModulationAmounts;
var
  c1 : integer;
  x1, x2 : single;
  ModAmount : single;
  kg : IKeyGroup;
  New_ActiveModParIndex : integer;
  Par : TPluginParameter;
begin
  kg := Plugin.ActiveKeyGroup;
  if not assigned(kg) then exit;

  if ActiveParameter = '' then
  begin
    New_ActiveModParIndex := -1;
  end else
  begin
    Par := PluginParFromName(ActiveParameter);
    if IsModPar(Par)
      then New_ActiveModParIndex := GetModParIndex(Par)
      else New_ActiveModParIndex := -1;
  end;


  if New_ActiveModParIndex = -1 then
  begin
    for c1 := 0 to kModSlotCount-1 do
    begin
      ModSelectors[c1].ShowModAmount := false;
    end;
  end else
  begin
    for c1 := 0 to kModSlotCount-1 do
    begin
      x1 := 0.5;
      ModAmount := kg.GetModParModAmount(New_ActiveModParIndex, c1);
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





end.
