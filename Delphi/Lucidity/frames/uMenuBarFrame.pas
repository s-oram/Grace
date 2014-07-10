unit uMenuBarFrame;

interface

uses
  VamLib.ZeroObject,
  uDialogDisplayArea,
  eeGuiStandardv2, eePlugin, uGuiFeedbackData,  Menu.KeyGroupsMenu, Menu.SamplesMenu,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamDiv, RedFoxGraphicControl,
  VamGraphicControl, VamTextBox, Vcl.Menus, Menu.MainMenu, VamLabel;

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
    procedure SampleMapButtonClick(Sender: TObject);
    procedure GroupMenuButtonClick(Sender: TObject);
    procedure SampleMenuButtonClick(Sender: TObject);
    procedure MainMenuButtonClick(Sender: TObject);
    procedure SampleEditButtonClick(Sender: TObject);
    procedure MapEditButtonClick(Sender: TObject);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
    fDialogDisplayArea : TDialogDisplayArea;

    MainMenu : TMainMenu;
    GroupsMenu : TGroupsMenu;
    SamplesMenu : TSamplesMenu;
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);

    procedure SampleFocusChanged; // Called when the sample focus changes...
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;



    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;
  end;

implementation

{$R *.dfm}

uses
  RedFox,
  RedFoxColor,
  eeTypes,
  uLucidityEnums,
  VamQuery,
  uGuiUtils,
  uConstants,
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

  GroupsMenu := TGroupsMenu.Create;

  SamplesMenu := TSamplesMenu.Create(0);

  MainMenu := TMainMenu.Create;

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

procedure TMenuBarFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
begin
  fPlugin            := aPlugin;
  fGuiStandard       := aGuiStandard;
  fDialogDisplayArea := aDialogDisplayArea;

  GroupsMenu.Initialize(aPlugin, aDialogDisplayArea);
  SamplesMenu.Initialize(aPlugin, aDialogDisplayArea);
  MainMenu.Initialize(aPlugin, aDialogDisplayArea);

  GuiSetup.StyleButton_SelectorButton(GroupMenuButton);
  GuiSetup.StyleButton_SelectorButton(SampleMenuButton);

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



  GroupMenuButton.Width := 116;
  SampleMenuButton.Width := 136;

  InfoDisplay.Align := alClient;
  InfoDisplay.Text := '';


  //===

  SampleFocusChanged;

end;



procedure TMenuBarFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

procedure TMenuBarFrame.SampleMapButtonClick(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleMapVisibility(Plugin);
end;

procedure TMenuBarFrame.SampleMenuButtonClick(Sender: TObject);
begin
  SamplesMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;


procedure TMenuBarFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMothership;
end;

procedure TMenuBarFrame.GroupMenuButtonClick(Sender: TObject);
begin
  GroupsMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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

procedure TMenuBarFrame.MainMenuButtonClick(Sender: TObject);
begin
  MainMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TMenuBarFrame.SampleEditButtonClick(Sender: TObject);
begin
  //TODO:MED I don't think this method is needed anymore.
  if not assigned(Plugin) then exit;
  Plugin.Globals.GuiState.MainGuiLayout := TMainGuiLayout.SampleZoom;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.GUILayoutChanged);
end;

procedure TMenuBarFrame.MapEditButtonClick(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleMapVisibility(Plugin);
end;

procedure TMenuBarFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  ParID : TPluginParameterID;
  ParValue : single;
begin
  if MsgID = TLucidmsgID.SampleFocusChanged then
  begin
    SampleFocusChanged;
  end;

  if MsgID = TLucidmsgID.MouseOverSampleRegionChanged then
  begin
    SampleFocusChanged;
  end;

  if MsgID = TLucidmsgID.SampleRegionChanged then
  begin
    SampleFocusChanged;
  end;


  if MsgID = TLucidMsgID.Command_ShowParChangeInfo then
  begin
    ParID := TPluginParameterID(Data^);

    ParValue := Plugin.GetPluginParameter(ParID);

    InfoDisplay.Text := FloatToStr(ParValue);

    InfoDisplay.Visible := true;
  end;

  if MsgID = TLucidMsgID.Command_HideParChangeInfo then
  begin

    InfoDisplay.Visible := false;
  end;

  //TODO: will maybe need to respond when hiding the sample map edit.

end;

end.
