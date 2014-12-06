unit uMenuBarFrame;

interface

{$INCLUDE Defines.inc}

uses
  eeTypes,
  VamLib.ZeroObject,
  Lucidity.GuiStandard, eePlugin, uGuiFeedbackData,  Menu.KeyGroupsMenu, Menu.SamplesMenu,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamDiv, RedFoxGraphicControl,
  VamGraphicControl, VamTextBox, Vcl.Menus, Menu.MainMenu, VamLabel, VamButton;

type
  TMenuBarFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    ContainerDiv: TVamDiv;
    SampleMenuButton: TVamTextBox;
    GroupMenuButton: TVamTextBox;
    MainMenuButton: TVamTextBox;
    MapEditButton: TVamTextBox;
    InfoDisplay: TVamLabel;
    AutoSelectButton: TVamButton;
    procedure SampleMapButtonClick(Sender: TObject);
    procedure SampleEditButtonClick(Sender: TObject);
    procedure ContainerDivResize(Sender: TObject);
    procedure MainMenuButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MapEditButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GroupMenuButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleMenuButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AutoSelectButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    MainMenu : TMainMenu;
    GroupsMenu : TGroupsMenu;
    SamplesMenu : TSamplesMenu;
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface);

    procedure SampleFocusChanged; // Called when the sample focus changes...

    procedure ShowParameterChangeInfo(const ParameterID : TPluginParameterID);
    procedure UpdateParameterChangeInfo(const ParameterID : TPluginParameterID);
    procedure HideParameterChangeInfo;

    procedure RefreshParDisplay;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  RedFox,
  RedFoxColor,
  Lucidity.Enums,
  VamQuery,
  VamLayoutWizard,
  Lucidity.GuiUtils,
  uConstants,
  Menu.AutoSelectMenu,
  Lucidity.PluginParameters,
  Lucidity.SampleMap,
  Lucidity.Interfaces;

{ TMenuBarFrame }

constructor TMenuBarFrame.Create(AOwner: TComponent);
begin
  inherited;

  ContainerDiv.Align := TAlign.alClient;
  ContainerDiv.AlignWithMargins := true;
  ContainerDiv.Margins.SetBounds(4,4,4,4);

  GroupsMenu := TGroupsMenu.Create(AOwner);
  SamplesMenu := TSamplesMenu.Create(AOwner, 0);
  MainMenu := TMainMenu.Create(AOwner);

  AddDisplayClass(MainMenuButton, dcGUIMenuButton);
  AddDisplayClass(MapEditButton, dcGUIMenuButton);
  AddDisplayClass(GroupMenuButton, dcGUIMenuButton);
  AddDisplayClass(SampleMenuButton, dcGUIMenuButton);


end;

destructor TMenuBarFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  GroupsMenu.Free;
  SamplesMenu.Free;
  MainMenu.Free;
  inherited;
end;

procedure TMenuBarFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
begin
  fPlugin            := aPlugin;
  fGuiStandard       := aGuiStandard;

  GroupsMenu.Initialize(aPlugin);
  SamplesMenu.Initialize(aPlugin);
  MainMenu.Initialize(aPlugin);

  GuiSetup.StyleButton_SelectorButton(GroupMenuButton);
  GuiSetup.StyleButton_SelectorButton(SampleMenuButton);

  GuiSetup.StyleButton_OnOffButton(AutoSelectButton);

  GuiSetup.StyleButton_CommandButton(MainMenuButton);
  MainMenuButton.Width := 86;

  GuiSetup.StyleButton_CommandButton(MapEditButton);

  MapEditButton.AlignWithMargins := true;
  MapEditButton.Margins.SetBounds(4,0,0,0);
  MapEditButton.Width := 86;
  MapEditButton.Text  := 'Sample Map';
  MapEditButton.TextPadding.Left := 14;

  MapEditButton.ImageOverlay := Plugin.Globals.SkinImageLoader.GetImage('Menu_ProgramIcon');
  MapEditButton.ImageOverlayHorzAlign := TRedFoxAlign.AlignNear;
  MapEditButton.ImageOverlayOffsetX := 2;


  AutoSelectButton.Align := alNone;
  GroupMenuButton.Align  := alNone;
  SampleMenuButton.Align := alNone;

  AutoSelectButton.Width  := 18;
  GroupMenuButton.Width := 116;
  SampleMenuButton.Width := 136;

  {

  AutoSelectButton.Align := alRight;
  SampleMenuButton.Align := alRight;
  GroupMenuButton.Align  := alRight;

  InfoDisplay.Align := alClient;
  InfoDisplay.Text := '';
  }

  GuiSetup.StyleButton_CommandButton_Bright(MainMenuButton);
  GuiSetup.StyleButton_CommandButton_Bright(MapEditButton);

  //===

  SampleFocusChanged;

end;

procedure TMenuBarFrame.AutoSelectButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Menu : TAutoSelectMenu;
begin
  Plugin.Globals.GuiState.HotkeyContext := THotKeyContext.None;

  if Button = mbLeft then
  begin
    Menu := TAutoSelectMenu.Create(self.Owner);
    Menu.AutoFreeMenu := true;
    Menu.Initialize(self.Plugin);
    Menu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TMenuBarFrame.ContainerDivResize(Sender: TObject);
begin
  AutoSelectButton.Height := ContainerDiv.Height;
  SampleMenuButton.Height := ContainerDiv.Height;
  GroupMenuButton.Height := ContainerDiv.Height;

  AutoSelectButton.Top := 0;
  SampleMenuButton.Top := 0;
  GroupMenuButton.Top := 0;

  AutoSelectButton.Width := ContainerDiv.Height;

  AutoSelectButton.Layout.SnapToParentEdge(TControlFeature.RightEdge);
  SampleMenuButton.Layout.Anchor(AutoSelectButton).SnapToEdge(TControlFeature.LeftEdge).Move(-4, 0);
  GroupMenuButton.Layout.Anchor(SampleMenuButton).SnapToEdge(TControlFeature.LeftEdge).Move(-4, 0);

  InfoDisplay.Layout.Anchor(MapEditButton).SnapToEdge(TControlFeature.RightEdge).Move(4,0);
  InfoDisplay.Top := 0;
  InfoDisplay.Height := ContainerDiv.Height;
  InfoDisplay.Width := GroupMenuButton.Left - InfoDisplay.Left - 4;
  InfoDisplay.Text := '';
end;





procedure TMenuBarFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

procedure TMenuBarFrame.SampleMapButtonClick(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleMapVisibility(Plugin);
end;

procedure TMenuBarFrame.SampleMenuButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Plugin.Globals.GuiState.HotkeyContext := THotKeyContext.None;

  if Button = mbLeft then
  begin
    SamplesMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TMenuBarFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMothership;
end;

procedure TMenuBarFrame.GroupMenuButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Plugin.Globals.GuiState.HotkeyContext := THotKeyContext.None;

  if Button = mbLeft then
  begin
    GroupsMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TMenuBarFrame.SampleFocusChanged;
var
  kg : IKeyGroup;
  Region : IRegion;
  Text : string;
  rd:TRegionDisplayResult;
begin
  if not assigned(Plugin) then exit;


  rd := FindRegionToDisplay(Plugin);
  Region := rd.Region;

  if assigned(Region) then
  begin
    kg := Region.GetKeyGroup;
  end else
  begin
    kg := Plugin.FocusedKeyGroup;
  end;

  //sg := Plugin.FocusedEngine;
  //Region := Plugin.FocusedRegion;

  if assigned(kg)
    then Text := kg.GetName
    else Text := '---';

  if GroupMenuButton.Text <> Text then GroupMenuButton.Text := Text;

  if assigned(Region)
    then Text := ExtractFileName(Region.GetProperties^.SampleFileName)
    else Text := '---';

  if SampleMenuButton.Text <> Text then SampleMenuButton.Text := Text;
end;

procedure TMenuBarFrame.MainMenuButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Plugin.Globals.GuiState.HotkeyContext := THotKeyContext.None;

  if Button = mbLeft then
  begin
    MainMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TMenuBarFrame.SampleEditButtonClick(Sender: TObject);
begin
  //TODO:MED I don't think this method is needed anymore.
  if not assigned(Plugin) then exit;
  Plugin.Globals.GuiState.MainGuiLayout := TMainGuiLayout.SampleZoom;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.GUILayoutChanged);
end;

procedure TMenuBarFrame.MapEditButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Plugin.Globals.GuiState.HotkeyContext := THotKeyContext.None;
  if not assigned(Plugin) then exit;
  if Button = mbLeft then
  begin
    Command.ToggleSampleMapVisibility(Plugin);
  end;
end;

procedure TMenuBarFrame.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB:IInterface);
var
  ParID : TPluginParameterID;
begin
  if MsgID = TLucidmsgID.Command_UpdateGUI then
  begin
    SampleFocusChanged;
    RefreshParDisplay;
  end;

  if MsgID = TLucidmsgID.NewProgramLoaded             then SampleFocusChanged;
  if MsgID = TLucidmsgID.SampleFocusChanged           then SampleFocusChanged;
  if MsgID = TLucidmsgID.MouseOverSampleRegionChanged then SampleFocusChanged;
  if MsgID = TLucidmsgID.SampleRegionChanged          then SampleFocusChanged;

  if MsgID = TLucidMsgID.Command_ShowParChangeInfo then
  begin
    ParID := TPluginParameterID(DataA^);
    ShowParameterChangeInfo(ParID);
  end;

  if MsgID = TLucidMsgID.Command_UpdateParChangeInfo then
  begin
    ParID := TPluginParameterID(DataA^);
    UpdateParameterChangeInfo(ParID);
  end;

  if MsgID = TLucidMsgID.Command_HideParChangeInfo
    then HideParameterChangeInfo;

  if MsgID = TLucidMsgID.Cmd_RefreshParDisplay then RefreshParDisplay;

end;

procedure TMenuBarFrame.ShowParameterChangeInfo(const ParameterID: TPluginParameterID);
begin
  InfoDisplay.Text := Command.GetParDisplayInfo(self.Plugin, ParameterID);
  InfoDisplay.Invalidate;
  InfoDisplay.Visible := true;
end;

procedure TMenuBarFrame.UpdateParameterChangeInfo(const ParameterID: TPluginParameterID);
begin
  InfoDisplay.Text := Command.GetParDisplayInfo(self.Plugin, ParameterID);
  InfoDisplay.Invalidate;
end;

procedure TMenuBarFrame.HideParameterChangeInfo;
begin
  InfoDisplay.Visible := false;
end;

procedure TMenuBarFrame.RefreshParDisplay;
begin
  if Plugin.Globals.GuiState.IsAutoSelectActive
    then AutoSelectButton.IsOn := true
    else AutoSelectButton.IsOn := false;
end;





end.
