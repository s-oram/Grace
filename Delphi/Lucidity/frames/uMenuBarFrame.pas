unit uMenuBarFrame;

interface

uses
  uDialogDisplayArea,
  eeGuiStandard, eePlugin, uGuiFeedbackData,  Menu.KeyGroupsMenu, Menu.SamplesMenu,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamDiv, RedFoxGraphicControl,
  VamGraphicControl, VamTextBox, Vcl.Menus, Menu.MainMenu;

type
  TMenuBarFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    ContainerDiv: TVamDiv;
    SampleMenuButton: TVamTextBox;
    GroupMenuButton: TVamTextBox;
    MainMenuButton: TVamTextBox;
    MapEditButton: TVamTextBox;
    SampleEditButton: TVamTextBox;
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
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SampleFocusChanged; // Called when the sample focus changes...

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard; aDialogDisplayArea : TDialogDisplayArea);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;
  end;

implementation

{$R *.dfm}

uses
  VamQuery,
  uGuiUtils,
  RedFoxColor,
  uConstants,
  Lucidity.SampleMap,
  uLucidityKeyGroupInterface;

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
  AddDisplayClass(SampleEditButton, dcGUIMenuButton);
  AddDisplayClass(MapEditButton, dcGUIMenuButton);
  AddDisplayClass(GroupMenuButton, dcGUIMenuButton);
  AddDisplayClass(SampleMenuButton, dcGUIMenuButton);
end;

destructor TMenuBarFrame.Destroy;
begin
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

  MainMenuButton.Color          := kColor_LcdDark1;
  MainMenuButton.ColorMouseOver := kColor_ButtonMouseOver;
  MainMenuButton.Font.Color     := GetRedFoxColor(kColor_LcdDark5);

  GroupMenuButton.Color          := kColor_LcdDark1;
  GroupMenuButton.ColorMouseOver := kColor_ButtonMouseOver;
  GroupMenuButton.Font.Color     := GetRedFoxColor(kColor_LcdDark5);

  SampleMenuButton.Color          := kColor_LcdDark1;
  SampleMenuButton.ColorMouseOver := kColor_ButtonMouseOver;
  SampleMenuButton.Font.Color     := GetRedFoxColor(kColor_LcdDark5);

  SampleEditButton.Color          := kColor_LcdDark1;
  SampleEditButton.ColorMouseOver := kColor_ButtonMouseOver;
  SampleEditButton.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  SampleEditButton.AlignWithMargins := true;
  SampleEditButton.Margins.SetBounds(4,0,0,0);
  SampleEditButton.Width := 20;
  SampleEditButton.Text  := '';
  SampleEditButton.ImageOverlay := Plugin.Globals.SkinImageLoader.GetImage('Menu_SampleEditIcon');

  MapEditButton.Color          := kColor_LcdDark1;
  MapEditButton.ColorMouseOver := kColor_ButtonMouseOver;
  MapEditButton.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  MapEditButton.AlignWithMargins := true;
  MapEditButton.Margins.SetBounds(4,0,0,0);
  MapEditButton.Width := 20;
  MapEditButton.Text  := '';
  MapEditButton.ImageOverlay := Plugin.Globals.SkinImageLoader.GetImage('Menu_ProgramIcon');

end;



procedure TMenuBarFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

procedure TMenuBarFrame.SampleMapButtonClick(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  Plugin.GuiState.IsSampleMapVisible := not(Plugin.GuiState.IsSampleMapVisible);
end;

procedure TMenuBarFrame.SampleMenuButtonClick(Sender: TObject);
begin
  SamplesMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TMenuBarFrame.GroupMenuButtonClick(Sender: TObject);
begin
  GroupsMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;



procedure TMenuBarFrame.SampleFocusChanged;
var
  sg : IKeyGroup;
  Region : IRegion;
  Text : string;
  rd:TRegionDisplayResult;
begin
  if not assigned(Plugin) then exit;


  rd := FindRegionToDisplay(Plugin);
  Region := rd.Region;

  if assigned(Region) then
  begin
    sg := Region.GetKeyGroup;
  end else
  begin
    sg := Plugin.FocusedKeyGroup;
  end;

  //sg := Plugin.FocusedEngine;
  //Region := Plugin.FocusedRegion;

  if assigned(sg)
    then Text := sg.GetName
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
  if not assigned(Plugin) then exit;
  Plugin.Globals.SendWindowsMessage(UM_SHOW_LOOP_EDIT_FRAME);
end;

procedure TMenuBarFrame.MapEditButtonClick(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  if Plugin.GuiState.IsSampleMapVisible
    then Plugin.Globals.SendWindowsMessage(UM_HIDE_SAMPLE_MAP_EDIT)
    else Plugin.Globals.SendWindowsMessage(UM_SHOW_SAMPLE_MAP_EDIT);
end;




end.
