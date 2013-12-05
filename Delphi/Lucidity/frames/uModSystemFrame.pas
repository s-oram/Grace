unit uModSystemFrame;

interface

uses
  uLucidityKeyGroup, uLucidityKeyGroupInterface, LucidityModConnections, uLucidityEnums,
  eePlugin, eeGuiStandard, uGuiFeedbackData, eeEnumMenu,
  LucidityControl.ModSection,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamLabel, VamDiv, VamTextBox,
  VamSlider, VamKnob, VamButton, VamXYPad;

type
  TModSystemFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    FrameLabel: TVamLabel;
    ControlsContainer: TVamDiv;
    ModSectionPanel: TVamPanel;
    ModTargetButton: TVamTextBox;
    ModTargetLabel: TVamLabel;
    SourceLabel: TVamLabel;
    DepthLabel: TVamLabel;
    ViaLabel: TVamLabel;
    x1Knob: TVamKnob;
    x1Label: TVamLabel;
    x2Label: TVamLabel;
    x2Knob: TVamKnob;
    x3Knob: TVamKnob;
    x3Label: TVamLabel;
    x4Label: TVamLabel;
    x4Knob: TVamKnob;
    y4Knob: TVamKnob;
    y4Label: TVamLabel;
    y3Label: TVamLabel;
    y3Knob: TVamKnob;
    y2Knob: TVamKnob;
    y2Label: TVamLabel;
    y1Label: TVamLabel;
    y1Knob: TVamKnob;
    ModTargetAutoSelectButton: TVamButton;
    XYPad1: TVamXYPad;
    PadLabel1: TVamLabel;
    PadLabel2: TVamLabel;
    XYPad2: TVamXYPad;
    XYPad3: TVamXYPad;
    PadLabel3: TVamLabel;
    PadLabel4: TVamLabel;
    XYPad4: TVamXYPad;
    procedure ControlsContainerResize(Sender: TObject);
    procedure ModTargetButtonClick(Sender: TObject);
    procedure ModTargetAutoSelectButtonChanged(Sender: TObject);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    MsgHandle : hwnd;
    procedure MessageHandler(var Message : TMessage);
  protected
    //ModSections : array[0..3] of TModSection;
    ModSections : TArray<TModSection>;

    ModSourceMenu : TEnumMenu<TModSource>;
    ModDestMenu   : TEnumMenu<TModDest>;
    ModViaMenu    : TEnumMenu<TModSource>;

    RefModDest : TModDest;
    RefOffset  : integer;

    procedure ModSourceSelected(Sender : TObject; aSource : TModSource);
    procedure ModDestSelected(Sender : TObject; aSource : TModDest);
    procedure ModViaSelected(Sender : TObject; aSource : TModSource);

    procedure EventHandler_ModMenuClosed(Sender: TObject; Cancelled: Boolean);

    procedure EventHandler_ShowModSouceMenu(Sender : TObject);
    procedure EventHandler_ShowModViaMenu(Sender : TObject);
    procedure EventHandler_ModSliderChanged(Sender : TObject);

    procedure EventHandle_FocusedControlHasChanged;

    procedure UpdateModMatrix;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property Plugin:TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;
  end;

implementation

uses
  Menus,
  RedFox, RedFoxColor,
  VamLayoutWizard,
  uConstants,
  uGuiUtils,
  Math;

{$R *.dfm}


procedure ClearModSection(ModSection:TModSection);
begin
end;

procedure UpdateModSection(ModSection : TModSection; ModLink:PModLink);
begin
  ModSection.TextboxA.Text := TModSourceHelper.ToShortGuiString(ModLink^.Source);
  ModSection.TextboxC.Text := TModSourceHelper.ToShortGuiString(ModLink^.Via);

  ModSection.Slider.Pos := (ModLink^.Amount * 0.5) + 0.5;
end;

{ TModSystemFrame }


constructor TModSystemFrame.Create(AOwner: TComponent);
var
  c1: Integer;
begin
  inherited;

  setLength(ModSections, 4);

  for c1 := 0 to 3 do
  begin
    ModSections[c1] := TModSection.Create(AOwner);
    ModSections[c1].Tag        := c1;
    ModSections[c1].Slider.Tag := c1;

    ModSections[c1].OnShowModSourceMenu := EventHandler_ShowModSouceMenu;
    ModSections[c1].OnShowModViaMenu    := EventHandler_ShowModViaMenu;
    ModSections[c1].Slider.OnChanged    := EventHandler_ModSliderChanged;
  end;


  ModTargetButton.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  ModTargetButton.Color          := kColor_LcdDark1;
  ModTargetButton.ColorMouseOver := kColor_ButtonMouseOver;
  ModTargetButton.TextVAlign     := TRedFoxAlign.AlignCenter;
  ModTargetButton.TextAlign      := TRedFoxAlign.AlignCenter;
  ModTargetButton.Text := 'bang';


  // NOTE: AFAIK TFrame should be receive a windows handle at some stage.
  // for whatever reason this TFrame instance wasn't receiving a handle and
  // i couldn't figure out why. This is a work-around so that the frame
  // can receive messages posted by the EasyEffect Globals class.
  MsgHandle := AllocateHWND(MessageHandler);



  ModSourceMenu := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModSourceMenu.OnItemSelected := ModSourceSelected;
  ModSourceMenu.OnClose := EventHandler_ModMenuClosed;

  ModDestMenu   := TEnumMenu<TModDest>.Create(TModDestHelper);
  ModDestMenu.OnItemSelected := ModDestSelected;
  ModDestMenu.OnClose := EventHandler_ModMenuClosed;

  ModViaMenu    := TEnumMenu<TModSource>.Create(TModSourceHelper);
  ModViaMenu.OnItemSelected := ModViaSelected;
  ModViaMenu.OnClose := EventHandler_ModMenuClosed;
end;

destructor TModSystemFrame.Destroy;
var
  c1 : integer;
begin
  if (MsgHandle <> 0) and (assigned(Plugin)) then
  begin
    Plugin.Globals.RemoveWindowsMessageListener(MsgHandle);
  end;
  DeallocateHWnd(MsgHandle);


  for c1 := 0 to 3 do
  begin
    FreeAndNil(ModSections[c1]);
  end;
  setLength(ModSections, 0);



  ModSourceMenu.Free;
  ModDestMenu.Free;
  ModViaMenu.Free;



  inherited;
end;

procedure TModSystemFrame.MessageHandler(var Message: TMessage);
begin
  if Message.Msg = UM_Update_Mod_Matrix       then UpdateModMatrix;
  if Message.Msg = UM_SAMPLE_FOCUS_CHANGED    then UpdateModMatrix;
  if Message.Msg = UM_SAMPLE_REGION_CHANGED   then UpdateModMatrix;
  if Message.Msg = UM_Focused_Control_Changed then EventHandle_FocusedControlHasChanged;
end;




procedure TModSystemFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  assert(not assigned(fPlugin), 'InitializeFrame() must only be called once.');

  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  if MsgHandle <> 0 then
  begin
    Plugin.Globals.AddWindowsMessageListener(MsgHandle);
  end;



  //TODO: change PadX1 to X1, etc.
  // altertively, remove the knobs and add 4 small xy pads.

  x1Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadX1');
  x2Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadX2');
  x3Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadX3');
  x4Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadX4');

  y1Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadY1');
  y2Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadY2');
  y3Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadY3');
  y4Knob.Tag := aPlugin.Globals.VstParameters.FindParameterIndexByName('PadY4');

  fGuiStandard.RegisterControlForAutoUpdate(x1Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(x2Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(x3Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(x4Knob, true);

  fGuiStandard.RegisterControlForAutoUpdate(y1Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(y2Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(y3Knob, true);
  fGuiStandard.RegisterControlForAutoUpdate(y4Knob, true);




  XYPad1.PadX_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadX1');
  XYPad1.PadY_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadY1');

  XYPad2.PadX_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadX2');
  XYPad2.PadY_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadY2');

  XYPad3.PadX_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadX3');
  XYPad3.PadY_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadY3');

  XYPad4.PadX_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadX4');
  XYPad4.PadY_VstParameterIndex := Plugin.Globals.VstParameters.FindParameterIndexByName('PadY4');


  fGuiStandard.RegisterControlForAutoUpdate(XYPad1, true);
  fGuiStandard.RegisterControlForAutoUpdate(XYPad2, true);
  fGuiStandard.RegisterControlForAutoUpdate(XYPad3, true);
  fGuiStandard.RegisterControlForAutoUpdate(XYPad4, true);


  //===== Update control state ========
  if ModTargetAutoSelectButton.IsOn <> Plugin.GuiState.IsModDestAutoSelectEnabled
    then ModTargetAutoSelectButton.IsOn := Plugin.GuiState.IsModDestAutoSelectEnabled;



  // finally...
  UpdateModMatrix;
end;


procedure TModSystemFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin



end;

procedure TModSystemFrame.ControlsContainerResize(Sender: TObject);
var
  c1 : integer;
  WidthOfKnobs : integer;
  KnobLeft : integer;
  MacroKnobWidth : integer;
  PadSpace : integer;
  PadLeft : integer;
  PadSize : integer;
begin
  ModTargetAutoSelectButton.Text         := '';
  ModTargetAutoSelectButton.ImageOff     := Plugin.Globals.SkinImageLoader.GetImage('Locked_Icon');
  ModTargetAutoSelectButton.ImageOn      := Plugin.Globals.SkinImageLoader.GetImage('Unlocked_Icon');
  ModTargetAutoSelectButton.Color_Border := kColor_ToggleButtonBorder;
  ModTargetAutoSelectButton.ColorOnA     := kColor_ToggleButtonOn;
  ModTargetAutoSelectButton.ColorOnB     := kColor_ToggleButtonOnMouseOver;
  ModTargetAutoSelectButton.ColorOffA    := kColor_ToggleButtonOff;
  ModTargetAutoSelectButton.ColorOffB    := kColor_ToggleButtonOffMouseOver;



  //============================================================================
  ModSectionPanel.Width := round(ControlsContainer.Width * 0.66);
  ModSectionPanel.Height := 88;
  ModSectionPanel.Top := 4;
  ModSectionPanel.Left := ControlsContainer.Width - ModSectionPanel.Width;
  ModSectionPanel.Color := kColor_LcdDark1;
  ModSectionPanel.CornerRadius1 := 3;
  ModSectionPanel.CornerRadius2 := 3;
  ModSectionPanel.CornerRadius3 := 3;
  ModSectionPanel.CornerRadius4 := 3;


  for c1 := 0 to 3 do
  begin
    ModSections[c1].Parent := ModSectionPanel;
    ModSections[c1].Width  := ModSectionPanel.Width - 8;
    ModSections[c1].Height := 20;
    ModSections[c1].Left   := 4;
    ModSections[c1].Top    := 4 + c1 * 20;
    ModSections[c1].Visible := true;
  end;


  ModTargetButton.Height := 18;
  ModTargetButton.Width  := (ModSectionPanel.Width - 16) div 3;
  ModTargetButton.Top := ModSectionPanel.Top + 4;
  ModTargetButton.Left := (ControlsContainer.Width - ModSectionPanel.Width - ModTargetButton.Width) div 2 - 8;
  //ModTargetButton.Layout.Anchor(ModSectionPanel).SnapToEdge(TControlFeature.BottomEdge).AlignEdge(TControlFeature.LeftEdge).Move(4,20);
  ModTargetButton.Layout.Move(-6, 0);
  ModTargetLabel.Layout.Anchor(ModTargetButton).SnapToEdge(TControlFeature.BottomEdge);

  ModTargetAutoSelectButton.Width  := 18;
  ModTargetAutoSelectButton.Height := 18;

  ModTargetAutoSelectButton.Layout.Anchor(ModTargetButton).SnapToEdge(TControlFeature.RightEdge).Move(4, 0);


  SourceLabel.Width := ModSectionPanel.Width div 3;
  DepthLabel.Width := ModSectionPanel.Width div 3;
  ViaLabel.Width := ModSectionPanel.Width div 3;

  SourceLabel.Layout.Anchor(ModSectionPanel).SnapToEdge(TControlFeature.BottomEdge);
  DepthLabel.Layout.Anchor(ModSectionPanel).SnapToEdge(TControlFeature.BottomEdge);
  ViaLabel.Layout.Anchor(ModSectionPanel).SnapToEdge(TControlFeature.BottomEdge);

  SourceLabel.Left := ModSectionPanel.Left + 4;
  DepthLabel.Left  := ModSectionPanel.Left + ModSectionPanel.Width div 3 + 3;
  ViaLabel.Left    := ModSectionPanel.Left + ModSectionPanel.Width div 3 * 2 + 2;


  //=========
  MacroKnobWidth := 45;

  WidthOfKnobs := 4 * MacroKnobWidth;
  //KnobLeft := (ControlsContainer.Width - ModSectionPanel.Width - WidthOfKnobs) div 2 - 8;
  //knobLeft := (ControlsContainer.Width div 2 - WidthOfKnobs) div 2;
  //KnobLeft := ModSectionPanel.Left + 4;

  KnobLeft := (ControlsContainer.Width - WidthOfKnobs * 2 - MacroKnobWidth) div 2;

  x1Knob.Layout.SetSize(MacroKnobWidth,32);
  x2Knob.Layout.SetSize(MacroKnobWidth,32);
  x3Knob.Layout.SetSize(MacroKnobWidth,32);
  x4Knob.Layout.SetSize(MacroKnobWidth,32);

  //x1Knob.Layout.Anchor(ModTargetButton).SnapToEdge(TControlFeature.BottomEdge).Move(-40,30);
  x1Knob.Layout.SetPos(KnobLeft,130);
  x2Knob.Layout.Anchor(x1Knob).SnapToEdge(TControlFeature.RightEdge);
  x3Knob.Layout.Anchor(x2Knob).SnapToEdge(TControlFeature.RightEdge);
  x4Knob.Layout.Anchor(x3Knob).SnapToEdge(TControlFeature.RightEdge);



  //knobLeft := (ControlsContainer.Width div 2 - WidthOfKnobs) div 2 + (ControlsContainer.Width div 2);

  //KnobLeft := ModSectionPanel.Left + ModSectionPanel.Width - 4 - WidthOfKnobs;

  KnobLeft := (ControlsContainer.Width - WidthOfKnobs * 2 - 40) div 2  + WidthOfKnobs + 40;

  y1Knob.Layout.SetSize(MacroKnobWidth,32);
  y2Knob.Layout.SetSize(MacroKnobWidth,32);
  y3Knob.Layout.SetSize(MacroKnobWidth,32);
  y4Knob.Layout.SetSize(MacroKnobWidth,32);

  y1Knob.Layout.SetPos(KnobLeft,130);
  y2Knob.Layout.Anchor(y1Knob).SnapToEdge(TControlFeature.RightEdge);
  y3Knob.Layout.Anchor(y2Knob).SnapToEdge(TControlFeature.RightEdge);
  y4Knob.Layout.Anchor(y3Knob).SnapToEdge(TControlFeature.RightEdge);

  x1Label.Layout.Anchor(x1Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  x2Label.Layout.Anchor(x2Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  x3Label.Layout.Anchor(x3Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  x4Label.Layout.Anchor(x4Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);

  y1Label.Layout.Anchor(y1Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  y2Label.Layout.Anchor(y2Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  y3Label.Layout.Anchor(y3Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);
  y4Label.Layout.Anchor(y4Knob).MatchWidth.SnapToEdge(TControlFeature.BottomEdge);

  x1Label.Height := 16;
  x2Label.Height := 16;
  x3Label.Height := 16;
  x4Label.Height := 16;
  y1Label.Height := 16;
  y2Label.Height := 16;
  y3Label.Height := 16;
  y4Label.Height := 16;


  x1Knob.Visible := false;
  x2Knob.Visible := false;
  x3Knob.Visible := false;
  x4Knob.Visible := false;

  y1Knob.Visible := false;
  y2Knob.Visible := false;
  y3Knob.Visible := false;
  y4Knob.Visible := false;

  x1Label.Visible := false;
  x2Label.Visible := false;
  x3Label.Visible := false;
  x4Label.Visible := false;

  y1Label.Visible := false;
  y2Label.Visible := false;
  y3Label.Visible := false;
  y4Label.Visible := false;


  PadSize  := 80;
  PadSpace := 26;

  PadLeft := (ControlsContainer.Width div 2) - (PadSize * 2) - (PadSpace * 3);
  xyPad1.Layout.SetSize(PadSize,PadSize).SetPos(PadLeft,125);

  PadLeft := (ControlsContainer.Width div 2) - PadSize - PadSpace;
  xyPad2.Layout.SetSize(PadSize,PadSize).SetPos(PadLeft,125);

  PadLeft := (ControlsContainer.Width div 2) + PadSpace;
  xyPad3.Layout.SetSize(PadSize,PadSize).SetPos(PadLeft,125);

  PadLeft := (ControlsContainer.Width div 2) + PadSize + (PadSpace * 3);
  xyPad4.Layout.SetSize(PadSize,PadSize).SetPos(PadLeft,125);

  PadLabel1.Text := 'XY1';
  PadLabel1.Layout.Anchor(xyPad1).SnapToEdge(TControlFeature.BottomEdge);

  PadLabel2.Text := 'XY2';
  PadLabel2.Layout.Anchor(xyPad2).SnapToEdge(TControlFeature.BottomEdge);

  PadLabel3.Text := 'XY3';
  PadLabel3.Layout.Anchor(xyPad3).SnapToEdge(TControlFeature.BottomEdge);

  PadLabel4.Text := 'XY4';
  PadLabel4.Layout.Anchor(xyPad4).SnapToEdge(TControlFeature.BottomEdge);

  //xyPad2.Layout.SetSize(80,80).SetPos(0,0);
  //xyPad3.Layout.SetSize(80,80).SetPos(0,0);
  //xyPad4.Layout.SetSize(80,80).SetPos(0,0);

end;

procedure TModSystemFrame.UpdateModMatrix;
var
  c1 : integer;
  kg:IKeyGroup;
  ModConnections : TModConnections;
  ModDest : TModDest;
  ModLinkData : PModLink;
begin
  if not assigned(Plugin) then exit;

  kg := Plugin.ActiveKeyGroup;
  ModConnections := kg.GetModConnections;

  ModDest := Plugin.GuiState.ModDestTarget;

  if ModDest  = TModDest.None then
  begin
    ModTargetButton.Text := 'None';
    ClearModSection(ModSections[0]);
    ClearModSection(ModSections[1]);
    ClearModSection(ModSections[2]);
    ClearModSection(ModSections[3]);
    for c1 := 0 to 3 do
    begin
      ModSections[c1].IsEnabled := false;
    end;

  end else
  begin
    ModTargetButton.Text := TModDestHelper.ToShortGuiString(ModDest);

    ModLinkData := ModConnections.FindModLink(ModDest, 0);
    UpdateModSection(ModSections[0], ModLinkData);

    ModLinkData := ModConnections.FindModLink(ModDest, 1);
    UpdateModSection(ModSections[1], ModLinkData);

    ModLinkData := ModConnections.FindModLink(ModDest, 2);
    UpdateModSection(ModSections[2], ModLinkData);

    ModLinkData := ModConnections.FindModLink(ModDest, 3);
    UpdateModSection(ModSections[3], ModLinkData);

    for c1 := 0 to 3 do
    begin
      ModSections[c1].IsEnabled := true;
    end;
  end;

end;

procedure TModSystemFrame.ModTargetButtonClick(Sender: TObject);
var
  kg:IKeyGroup;
  ModDest : TModDest;
  mi : TMenuItem;
begin
  if not assigned(Plugin) then exit;
  kg := Plugin.ActiveKeyGroup;
  ModDest := Plugin.GuiState.ModDestTarget;

  mi := ModDestMenu.FindMenuItemByName('ModOutA');
  mi.Visible := false;

  mi := ModDestMenu.FindMenuItemByName('ModOutB');
  mi.Visible := false;

  //TODO: mod destinations should be renamed to match parameter names etc.
  ModDestMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y, ModDest);
end;



procedure TModSystemFrame.ModDestSelected(Sender: TObject; aSource: TModDest);
begin
  Plugin.GuiState.ModDestTarget := aSource;
  UpdateModMatrix;
end;



procedure TModSystemFrame.EventHandler_ShowModSouceMenu(Sender: TObject);
var
  kg:IKeyGroup;
  ModConnections : TModConnections;
  ModLinkData : PModLink;
begin
  RefModDest := Plugin.GuiState.ModDestTarget;
  RefOffset  := (Sender as TModSection).Tag;

  kg := Plugin.ActiveKeyGroup;
  ModConnections := kg.GetModConnections;
  ModLinkData := ModConnections.FindModLink(RefModDest, RefOffset);

  ModSourceMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y, ModLinkData^.Source);
end;

procedure TModSystemFrame.EventHandler_ShowModViaMenu(Sender: TObject);
var
  kg:IKeyGroup;
  ModConnections : TModConnections;
  ModLinkData : PModLink;
begin
  RefModDest := Plugin.GuiState.ModDestTarget;
  RefOffset  := (Sender as TModSection).Tag;

  kg := Plugin.ActiveKeyGroup;
  ModConnections := kg.GetModConnections;
  ModLinkData := ModConnections.FindModLink(RefModDest, RefOffset);

  ModViaMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y, ModLinkData^.Via);
end;

procedure TModSystemFrame.ModSourceSelected(Sender: TObject; aSource: TModSource);
var
  kg:IKeyGroup;
  ModConnections : TModConnections;
  ModLinkData : PModLink;
begin
  if not assigned(Plugin) then exit;

  kg := Plugin.ActiveKeyGroup;
  ModConnections := kg.GetModConnections;
  ModLinkData := ModConnections.FindModLink(RefModDest, RefOffset);

  ModLinkData^.Source := aSource;
  ModLinkData^.Dest   := RefModDest;

  ModConnections.UpdateModLink(RefModDest, RefOffset, ModLinkData);

  UpdateModMatrix;
  (kg.GetObject as TKeyGroup).VoiceParameters.UpdateAllModLinks(ModConnections);
end;


procedure TModSystemFrame.ModViaSelected(Sender: TObject; aSource: TModSource);
var
  kg:IKeyGroup;
  ModConnections : TModConnections;
  ModLinkData : PModLink;
begin
  if not assigned(Plugin) then exit;

  kg := Plugin.ActiveKeyGroup;
  ModConnections := kg.GetModConnections;

  ModLinkData := ModConnections.FindModLink(RefModDest, RefOffset);

  ModLinkData^.Via  := aSource;
  ModLinkData^.Dest := RefModDest;

  ModConnections.UpdateModLink(RefModDest, RefOffset, ModLinkData);

  UpdateModMatrix;
  (kg.GetObject as TKeyGroup).VoiceParameters.UpdateAllModLinks(ModConnections);
end;



procedure TModSystemFrame.EventHandler_ModMenuClosed(Sender: TObject; Cancelled: Boolean);
var
  c1 : integer;
begin
  for c1 := 0 to 3 do
  begin
    ModSections[c1].ResetDisplayState;
  end;
end;





procedure TModSystemFrame.EventHandler_ModSliderChanged(Sender: TObject);
var
  kg:IKeyGroup;
  ModConnections : TModConnections;
  ModDest : TModDest;
  ModOffset : integer;
  ModLinkData : PModLink;

  SliderValue : single;
  Tag : integer;
begin
  if not assigned(Plugin) then exit;

  Tag := (Sender as TVamSlider).Tag;
  SliderValue := (Sender as TVamSlider).Pos;

  ModDest   := Plugin.GuiState.ModDestTarget;
  ModOffset := Tag;

  kg := Plugin.ActiveKeyGroup;
  ModConnections := kg.GetModConnections;

  ModLinkData := ModConnections.FindModLink(ModDest, ModOffset);

  ModLinkData^.Amount := SliderValue * 2 - 1;

  ModConnections.UpdateModLink(ModDest, ModOffset, ModLinkData);

  //TODO: need a more efficient update method here.
  //(kg.GetObject as TKeyGroup).VoiceParameters.UpdateAllModLinks(ModConnections);
  (kg.GetObject as TKeyGroup).VoiceParameters.UpdateModLink(ModLinkData);
end;

procedure TModSystemFrame.EventHandle_FocusedControlHasChanged;
var
  aControl : TControl;
  ModDest : TModDest;
begin
  if not assigned(Plugin) then exit;

  aControl := Plugin.GuiState.FocusedControl;

  ModDest := TModDest.None;

  if SameText(aControl.Name, 'Filter1Par1Knob') then ModDest := TModDest.Filter1_Par1;
  if SameText(aControl.Name, 'Filter1Par2Knob') then ModDest := TModDest.Filter1_Par2;
  if SameText(aControl.Name, 'Filter1Par3Knob') then ModDest := TModDest.Filter1_Par3;
  if SameText(aControl.Name, 'Filter1Par4Knob') then ModDest := TModDest.Filter1_Par4;
  if SameText(aControl.Name, 'Filter2Par1Knob') then ModDest := TModDest.Filter2_Par1;
  if SameText(aControl.Name, 'Filter2Par2Knob') then ModDest := TModDest.Filter2_Par2;
  if SameText(aControl.Name, 'Filter2Par3Knob') then ModDest := TModDest.Filter2_Par3;
  if SameText(aControl.Name, 'Filter2Par4Knob') then ModDest := TModDest.Filter2_Par4;
  if SameText(aControl.Name, 'MainOutputKnob')  then ModDest := TModDest.VoiceAmplitude;
  if SameText(aControl.Name, 'MainPanKnob')     then ModDest := TModDest.VoicePan;
  if SameText(aControl.Name, 'LfoSpeedKnob1')   then ModDest := TModDest.Lfo1_Rate;
  if SameText(aControl.Name, 'LfoDepthKnob1')   then ModDest := TModDest.Lfo1_ParB;
  if SameText(aControl.Name, 'LfoSpeedKnob2')   then ModDest := TModDest.Lfo2_Rate;
  if SameText(aControl.Name, 'LfoDepthKnob2')   then ModDest := TModDest.Lfo2_ParB;


  if (ModDest <> TModDest.None) and (Plugin.GuiState.IsModDestAutoSelectEnabled) then
  begin
    Plugin.GuiState.ModDestTarget := ModDest;
    UpdateModMatrix;
  end;

end;


procedure TModSystemFrame.ModTargetAutoSelectButtonChanged(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  Plugin.GuiState.IsModDestAutoSelectEnabled := (Sender as TVamButton).IsOn;
end;



end.
