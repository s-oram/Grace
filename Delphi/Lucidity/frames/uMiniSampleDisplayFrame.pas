unit uMiniSampleDisplayFrame;

interface

{$INCLUDE Defines.inc}

{$SCOPEDENUMS ON}

uses
  eeGuiStandardv2,
  VamLib.UniqueID,
  VamLib.ZeroObject, Math,
  VamVisibleControl, Lucidity.SampleImageRenderer,
  Lucidity.Types,
  Lucidity.Interfaces,
  Lucidity.SampleMap, Menu.SampleContextMenu,
  Lucidity.FlexSampleRenderer,
  LucidityGui.Scope,
  Menu.MissingSampleContextMenu,
  eePlugin, uGuiFeedbackData, LuciditySampleOverlay,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, VamSampleDisplay, Vcl.Menus,
  RedFoxGraphicControl, VamGraphicControl, VamLabel, VamDiv, VamCompoundLabel,
  VamCompoundNumericKnob, VamScrollBar, VamTextBox, Vcl.ExtCtrls;

type
  TDialogSampleMarker = (SampleStart, SampleEnd, LoopStart, LoopEnd);


  TSampleDisplayFrameInfo = record
    Region     : IRegion;
    Info : TSampleDisplayInfo;
  end;


  TMiniSampleDisplayFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    SampleDisplay: TVamSampleDisplay;
    SampleInfoBox: TVamDiv;
    InfoDiv: TVamDiv;
    SampleBeatsKnob: TVamCompoundNumericKnob;
    SampleVolumeKnob: TVamCompoundNumericKnob;
    SamplePanKnob: TVamCompoundNumericKnob;
    InsidePanel: TVamPanel;
    SampleNameLabel: TVamLabel;
    SampleFineKnob: TVamCompoundNumericKnob;
    SampleTuneKnob: TVamCompoundNumericKnob;
    Timer1: TTimer;
    ScrollBarDiv: TVamDiv;
    ZoomScrollBar: TVamScrollBar;
    ZoomInButton: TVamTextBox;
    ZoomOutButton: TVamTextBox;
    ZoomOutFullButton: TVamTextBox;
    procedure SampleKnobChanged(Sender: TObject);
    procedure SampleDisplayResize(Sender: TObject);
    procedure InfoDivResize(Sender: TObject);
    procedure InsidePanelResize(Sender: TObject);
    procedure ZoomScrollBarChanged(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    UpdateSampleDisplayThottleToken : TUniqueID;

    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    procedure UpdateControlVisibility;
    procedure UpdateModulation;

    procedure SetGuiStandard(const Value: TGuiStandard);
    procedure SetPlugin(const Value: TeePlugin);

    procedure Handle_SampleOverlay_ModAmountsChanged(Sender:TObject);
    procedure SampleOverlay_MarkerChanged(Sender:TObject; Marker:TSampleMarker; NewPosition : integer);
    procedure SampleOverlay_ZoomChanged(Sender : TObject; Zoom, Offset : single);
  private
    FMotherShip : IMothership;

    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);

    procedure ZoomButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ZoomToSampleMarker(Sender : TObject; Marker:TDialogSampleMarker);

  protected
    CurrentSample : TSampleDisplayFrameInfo;

    fSampleOverlay : TLuciditySampleOverlay;

    SampleOverlayClickPos : TPoint;
    SampleContextMenu : TSampleContextMenu;
    MissingSampleContextMenu : TMissingSampleContextMenu;

    StoredImage : ISampleImageBuffer;
    SampleRenderer : TSampleImageRenderer;

    Scope : TLucidityScope;

    procedure InternalUpdateSampleDisplay(const Region : IRegion; const NoRegionMessage : string);
    procedure InternalUpdateSampleInfo(const Region : IRegion; const NoRegionMessage : string);

    procedure UpdateSampleDisplayInfo;

    procedure SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleOverlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleOverlayMouseOverMarkerChanged(Sender : TObject);

    procedure SampleDisplayOleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
    procedure SampleDisplayOleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
    procedure SampleDisplayOleDragLeave(Sender: TObject);
    procedure SampleMarkerChanged(Sender:TObject; Marker:TSampleMarker; NewPosition : integer);

    property Plugin:TeePlugin read fPlugin write SetPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard write SetGuiStandard;

    procedure UpdateZoomSlider;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GuiEvent_SampleMakersChanged;

    procedure UpdateSampleDisplay;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);

    property SampleOverlay : TLuciditySampleOverlay read fSampleOverlay;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eeTypes,
  VamLib.Utils,
  VamLib.Graphics,
  Lucidity.PluginParameters,
  VamLib.Throttler,
  eeDsp,
  eeVstXml, uLucidityEnums,
  GuidEx, RedFoxColor, uLucidityExtra,
  uGuiUtils, VamLayoutWizard,
  uConstants, Lucidity.KeyGroup;

{$R *.dfm}

{ TMiniSampleDisplayFrame }

constructor TMiniSampleDisplayFrame.Create(AOwner: TComponent);
begin
  inherited;

  UpdateSampleDisplayThottleToken.Init;

  fSampleOverlay := TLuciditySampleOverlay.Create(AOwner);
  fSampleOverlay.Parent  := SampleDisplay;
  fSampleOverlay.Align := alClient;
  fSampleOverlay.Visible := true;

  fSampleOverlay.OnModAmountsChanged := Handle_SampleOverlay_ModAmountsChanged;
  fSampleOverlay.OnMouseDown := SampleOverlayMouseDown;
  fSampleOverlay.OnMouseUp   := SampleOverlayMouseUp;
  fSampleOverlay.OnSampleMarkerChanged := SampleMarkerChanged;
  fSampleOverlay.OnMouseOverMakerChanged := SampleOverlayMouseOverMarkerChanged;
  fSampleOverlay.OnSampleMarkerChanged := SampleOverlay_MarkerChanged;
  fSampleOverlay.OnZoomChanged         := SampleOverlay_ZoomChanged;

  fSampleOverlay.OnOleDragDrop  := SampleDisplayOleDragDrop;
  fSampleOverlay.OnOleDragOver  := SampleDisplayOleDragOver;
  fSampleOverlay.OnOleDragLeave := SampleDisplayOleDragLeave;

  CurrentSample.Info.IsValid := false;

  SampleContextMenu := TSampleContextMenu.Create;

  // TODO:MED instead of creating the menu at the start, maybe it would
  // be better to create the menu when needed and free when finish. It
  // would be more dynmaic.
  MissingSampleContextMenu := TMissingSampleContextMenu.Create;


  StoredImage := TSampleImageBuffer.Create;

  SampleRenderer := TSampleImageRenderer.Create;

  ZoomOutButton.OnMouseDown         := ZoomButtonMouseDown;
  ZoomInButton.OnMouseDown          := ZoomButtonMouseDown;
  ZoomOutFullButton.OnMouseDown     := ZoomButtonMouseDown;

  Scope := TLucidityScope.Create(AOwner);
  Scope.Visible := true;
  Scope.Parent := InsidePanel;
  Scope.HitTest := false;
  Scope.BringToFront;
  Scope.Name := 'Scope';

  Scope.Font.Name := 'Tahoma';
  Scope.Font.Style := [];
end;

destructor TMiniSampleDisplayFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  MissingSampleContextMenu.Free;
  SampleContextMenu.Free;
  SampleRenderer.Free;

  CurrentSample.Region := nil;

  inherited;
end;

procedure TMiniSampleDisplayFrame.InfoDivResize(Sender: TObject);
begin
  self.UpdateControlVisibility;
end;

procedure TMiniSampleDisplayFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
const
  kZoomButtonWidth = 74;
begin
  Plugin := aPlugin;
  GuiStandard := aGuiStandard;

  fSampleOverlay.Initialize(aPlugin);

  //TODO: This should be tied to the active voice group, or the active voice.
  // not the global scope.
  Scope.SignalRecorder  := Plugin.SignalRecorder;
  Scope.FreqAnalyzer    := Plugin.FreqAnalyzer;
  Scope.Font.Color      := GetRedFoxColor(kColor_LcdDark5);
  Scope.ColorBackground := kColor_LcdDark1;
  Scope.ColorForeground := GetRedFoxColor(kColor_LcdDark5);
  Scope.ColorForeground := '$FFCCDFFF';

  SampleInfoBox.Height := 25;
  InfoDiv.Align := alClient;

  SampleContextMenu.Initialize(aPlugin);
  MissingSampleContextMenu.Initialize(Plugin, nil);

  SampleNameLabel.AutoSize := false;
  SampleNameLabel.Align := alLeft;
  SampleNameLabel.AlignWithMargins := true;
  SampleNameLabel.Margins.SetBounds(0, 0, 4, 0);
  SampleNameLabel.Width := 220;

  SampleNameLabel.Font.Color := GetRedFoxColor(kColor_LcdDark5);

  SampleVolumeKnob.Color_Label   := GetRedFoxColor(kColor_LcdDark4);
  SampleVolumeKnob.Color_Numeric := GetRedFoxColor(kColor_LcdDark5);

  SamplePanKnob.Color_Label      := GetRedFoxColor(kColor_LcdDark4);
  SamplePanKnob.Color_Numeric    := GetRedFoxColor(kColor_LcdDark5);

  SampleBeatsKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  SampleBeatsKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);

  SampleTuneKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  SampleTuneKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);

  SampleFineKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  SampleFineKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);

  SampleDisplay.LineColor        := kColor_SampleDisplayLine;

  SampleOverlay.Font.Color := GetTColor(kColor_LcdDark5);
  SampleOverlay.ShowMarkerTags := true;

  SampleVolumeKnob.Width := 90;
  SamplePanKnob.Width    := 75;
  SampleTuneKnob.Width   := 75;
  SampleFineKnob.Width   := 75;
  SampleBeatsKnob.Width  := 85;

  SampleVolumeKnob.Margins.SetBounds(0,0,20,0);
  SamplePanKnob.Margins.SetBounds(0,0,20,0);

  StoredImage.GetObject.Resize(kSampleImageWidth, kSampleImageHeight);

  ScrollBarDiv.Align := alBottom;
  ScrollBarDiv.Height := 18;

  ZoomInButton.Width := 18;
  ZoomOutButton.Width := 18;
  ZoomOutFullButton.Width := 18;

  GuiSetup.StyleButton_SliderButton(ZoomOutButton);
  GuiSetup.StyleButton_SliderButton(ZoomInButton);
  GuiSetup.StyleButton_SliderButton(ZoomOutFullButton);

  ZoomOutButton.AlignWithMargins := true;
  ZoomOutButton.Margins.SetBounds(0,0,-1,0);

  ZoomInButton.CornerRadius[2]      := 3;
  ZoomOutFullButton.CornerRadius[3] := 3;

  Timer1.Enabled := true;
  Timer1.Interval := 25;

  SampleDisplay.Align := TAlign.alClient;

  InsidePanel.AlignWithMargins := true;
  InsidePanel.Align := TAlign.alClient;
  InsidePanel.Margins.Bottom := 0;
  InsidePanel.CornerRadius3 := 0;
  InsidePanel.CornerRadius4 := 0;

  ZoomScrollBar.Align := alClient;
  ZoomScrollBar.AlignWithMargins := true;
  ZoomScrollBar.Margins.SetBounds(-1, 0 , -1, 0);
  ZoomScrollBar.Visible := true;
  ZoomScrollBar.BringToFront;

  SampleOverlay.BringToFront;
  Scope.BringToFront;

  //== finally, call the message handlers to ensure everything is up to date ===
  UpdateZoomSlider;
  UpdateControlVisibility;
  UpdateModulation;
  UpdateSampleDisplay;
end;

procedure TMiniSampleDisplayFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IInterface);
var
  SamplePos : integer;
  zx : single;
  Zoom, Offset : single;
begin
  if MsgID = TLucidMsgId.SampleRegionChanged then
  begin
    // TODO:HIGH I'm not sure if the sample display needs to be updated on SampleRegionChanged messages.
    //UpdateSampleDisplay;
  end;

  if MsgID = TLucidMsgID.SampleFocusChanged_OLD then
  begin
    UpdateControlVisibility;
    UpdateSampleDisplay;
    UpdateSampleDisplayInfo;
  end;

  if MsgID = TLucidMsgId.MouseOverSampleRegionChanged then
  begin
    UpdateSampleDisplay;
  end;

  if MsgID = TLucidMsgID.Command_UpdateControlVisibility then UpdateControlVisibility;
  if MsgID = TLucidMsgID.Command_UpdateModMatrix         then UpdateModulation;
  if MsgID = TLucidMsgID.ModSlotChanged                  then UpdateModulation;
  if MsgID = TLucidMsgID.ModAmountChanged                then UpdateModulation;
  if MsgID = TLucidMsgID.SampleOscTypeChanged            then UpdateControlVisibility;
  if MsgID = TLucidMsgID.LoopTypeChanged                 then UpdateSampleDisplayInfo;
  if MsgID = TLucidMsgID.Command_UpdateSampleDisplay     then UpdateSampleDisplay;
  if MsgID = TLucidMsgID.Command_UpdateSampleInfo        then UpdateSampleDisplayInfo;

  if MsgID = TLucidMsgID.Command_ShowReplaceRegionMessage then SampleOverlay.LargeTextMessage := 'Replace Sample';
  if MsgID = TLucidMsgID.Command_HideReplaceRegionMessage then SampleOverlay.LargeTextMessage := '';

  if MsgID = TLucidMsgID.Command_Sample_ZoomToSampleStart  then ZoomToSampleMarker(self, TDialogSampleMarker.SampleStart);
  if MsgID = TLucidMsgID.Command_Sample_ZoomToSampleEnd    then ZoomToSampleMarker(self, TDialogSampleMarker.SampleEnd);
  if MsgID = TLucidMsgID.Command_Sample_ZoomToLoopStart    then ZoomToSampleMarker(self, TDialogSampleMarker.LoopStart);
  if MsgID = TLucidMsgID.Command_Sample_ZoomToLoopEnd      then ZoomToSampleMarker(self, TDialogSampleMarker.LoopEnd);


  if MsgID = TLucidMsgID.Command_Sample_ZoomIn then
  begin
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
    try
      Zoom   := Plugin.Globals.GuiState.SampleDisplayZoom;
      //Offset := Plugin.Globals.GuiState.SampleDisplayOffset;

      SamplePos := Integer(Data^);

      //Zoom In!
      zx := Zoom  + 0.2;
      Zoom := Clamp(zx, 0, 1);

      // Update the offset.
      zx := SamplePos / CurrentSample.Info.SampleFrames;
      Offset := Clamp(zx, 0, 1);

      Plugin.Globals.GuiState.SampleDisplayZoom   := Zoom;
      Plugin.Globals.GuiState.SampleDisplayOffset := Offset;

      // Update the GUI.
      UpdateSampleDisplay;
      UpdateZoomSlider;
    finally
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
    end;
  end;

  if MsgID = TLucidMsgID.Command_Sample_ZoomOut then
  begin
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
    try
      Zoom   := Plugin.Globals.GuiState.SampleDisplayZoom;
      //Offset := Plugin.Globals.GuiState.SampleDisplayOffset;

      SamplePos := Integer(Data^);

      //Zoom In!
      zx := Zoom  - 0.2;
      if zx < 0.05 then zx := 0;
      Zoom := Clamp(zx, 0, 1);

      // Update the offset.
      zx := SamplePos / CurrentSample.Info.SampleFrames;
      Offset := Clamp(zx, 0, 1);

      Plugin.Globals.GuiState.SampleDisplayZoom   := Zoom;
      Plugin.Globals.GuiState.SampleDisplayOffset := Offset;

      UpdateSampleDisplay;
      UpdateZoomSlider;
    finally
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
    end;
  end;

  if MsgID = TLucidMsgID.Command_Sample_ZoomOutFull then
  begin
    Plugin.Globals.GuiState.SampleDisplayZoom   := 0;

    UpdateSampleDisplay;
    UpdateZoomSlider;
  end;

end;

procedure TMiniSampleDisplayFrame.SetPlugin(const Value: TeePlugin);
begin
  fPlugin := Value;

  if fPlugin <> nil then
  begin
    UpdateSampleDisplay;
  end;
end;

procedure TMiniSampleDisplayFrame.SetGuiStandard(const Value: TGuiStandard);
begin
  fGuiStandard := Value;
end;

procedure TMiniSampleDisplayFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TMiniSampleDisplayFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin
  // TODO: The sample overlay will need to be updated some how.
  // Currently it's being updated on timer and it's causing a lot of
  // unecessary drawing operations.
  fSampleOverlay.LinkToGuiFeedbackData(Feedback);
  fSampleOverlay.Invalidate;
end;

procedure TMiniSampleDisplayFrame.UpdateSampleDisplay;
var
  rd:TRegionDisplayResult;
begin
  if not assigned(Plugin) then exit;

  LogMain.EnterMethod('TMiniSampleDisplayFrame.UpdateSampleDisplay');

  rd := FindRegionToDisplay(Plugin);

  InternalUpdateSampleDisplay(rd.Region, rd.Message);
  InternalUpdateSampleInfo(rd.Region, rd.Message);

  LogMain.LeaveMethod('TMiniSampleDisplayFrame.UpdateSampleDisplay');
end;

procedure TMiniSampleDisplayFrame.UpdateSampleDisplayInfo;
var
  rd:TRegionDisplayResult;
begin
  if not assigned(Plugin) then exit;
  rd := FindRegionToDisplay(Plugin);

  InternalUpdateSampleInfo(rd.Region, rd.Message);
end;


procedure TMiniSampleDisplayFrame.InternalUpdateSampleDisplay(const Region: IRegion; const NoRegionMessage: string);
var
  //aOffset, aZoom : single;
  xSampleImage : IInterfacedBitmap;
  Par:TSampleRenderParameters;
  FlexPar:TFlexRenderPar;
  Zoom, Offset : single;
begin
  Zoom   := Plugin.Globals.GuiState.SampleDisplayZoom;
  Offset := Plugin.Globals.GuiState.SampleDisplayOffset;
  CurrentSample.Region := Region;

  if (assigned(Region)) then
  begin
    if (Region.GetSample^.Properties.IsValid) and (Zoom > 0) then
    begin
      FlexPar.BackgroundColor := kColor_LcdDark1;
      FlexPar.LineColor       := kColor_SampleDisplayLine;
      FlexPar.ImageWidth      := SampleDisplay.ClientRect.Width;
      FlexPar.ImageHeight     := SampleDisplay.ClientRect.Height;
      FlexPar.Zoom            := Zoom;
      FlexPar.Offset          := Offset;
      FlexPar.VertGain        := DecibelsToLinear(Region.GetProperties^.SampleVolume);

      xSampleImage := TFlexSampleImageRenderer.RenderSample(Region, FlexPar);
      SampleDisplay.DrawSample(xSampleImage);

      SampleOverlay.SetZoomOffset(Zoom, Offset);
    end else
    if (Region.GetSample^.Properties.IsValid) and (Zoom = 0) then
    begin
      Par.BackgroundColor := kColor_LcdDark1;
      Par.LineColor       := kColor_SampleDisplayLine;
      Par.Zoom            := 0;
      Par.Offset          := 0;
      Par.ImageWidth      := kSampleImageWidth;
      Par.ImageHeight     := kSampleImageHeight;
      Par.VertGain        := DecibelsToLinear(Region.GetProperties^.SampleVolume);

      xSampleImage := SampleRenderer.RenderSample(Par, Region, Region.GetPeakBuffer);
      SampleDisplay.DrawSample(xSampleImage);

      SampleOverlay.SetZoomOffset(0, 0);

      // TODO:LOW Look at the two sample renderers used above.
      // a) xSampleImage := TFlexSampleImageRenderer.RenderSample(Region, FlexPar);
      // b) xSampleImage := SampleRenderer.RenderSample(Par, Region, Region.GetPeakBuffer);
      // Notice one is using class functions while the other needs to initiate the class.
      // It would be better if both were the same. Additionally, FlexSampleImageRenderer
      // is a more descriptive name than SampleRenderer. This needs to be updated
      // so the difference between the two sample renderers appear to be deliberate
      // instead of looking like a fogotten left-over after refactoring some functionality
      // somewhere.
    end else
    if (Region.GetSample^.Properties.IsValid = false) then
    begin
      SampleDisplay.ClearSample(true);
    end else
    begin
      SampleDisplay.DrawSample(Region.GetSampleImage);
    end;
  end else
  begin
    SampleDisplay.ClearSample(true);
  end;
end;

procedure TMiniSampleDisplayFrame.InternalUpdateSampleInfo(const Region: IRegion; const NoRegionMessage: string);
  function EncloseWithBrackets(const Text : string):string;
  begin
    result := '(' + Text + ')';
  end;
var
  px : PRegionProperties;
  s : string;
  RegionLoopStart, RegionLoopEnd : integer;
begin
  if (assigned(Region)) then
  begin
    if (Region.GetProperties^.SampleDataLoaded) and (Region.GetSample^.Properties.IsValid) then
    begin
      CurrentSample.Info.IsValid := true;
      CurrentSample.Info.ChannelCount := Region.GetSample^.Properties.ChannelCount;
      CurrentSample.Info.SampleFrames := Region.GetSample^.Properties.SampleFrames;

      //== Sample Overlay ==
      fSampleOverlay.SetSampleInfo(true, CurrentSample.Info.SampleFrames);

      fSampleOverlay.SampleStart := Region.GetProperties^.SampleStart;
      fSampleOverlay.SampleEnd   := Region.GetProperties^.SampleEnd;

      if ShowLoopMarkers(Region, RegionLoopStart, RegionLoopEnd) then
      begin
        fSampleOverlay.ShowLoopPoints := true;
        fSampleOverlay.LoopStart   := RegionLoopStart;
        fSampleOverlay.LoopEnd     := RegionLoopEnd;
      end else
      begin
        fSampleOverlay.ShowLoopPoints := false;
        fSampleOverlay.LoopStart      := RegionLoopStart;
        fSampleOverlay.LoopEnd        := RegionLoopEnd;
      end;

      fSampleOverlay.IsCurrentSampleMissing := false;
    end else
    begin
      //== Sample Display Overlay ==
      fSampleOverlay.SetSampleInfo(false, 0);
      fSampleOverlay.ShowLoopPoints := false;
      fSampleOverlay.NoSampleMessage :=  EncloseWithBrackets(Region.GetProperties.ErrorMessage);
      fSampleOverlay.IsCurrentSampleMissing := true;
    end;

    //== Sample / Region Properties =============================
    s := ExtractFilename(Region.GetProperties^.SampleFileName);
    if SampleNameLabel.Text <> s then SampleNameLabel.Text := s;

    px := Region.GetProperties;
    SampleVolumeKnob.KnobValue := px^.SampleVolume;
    SamplePanKnob.KnobValue    := px^.SamplePan;
    SampleTuneKnob.KnobValue   := px^.sampleTune;
    SampleFineKnob.KnobValue   := px^.sampleFine;
    SampleBeatsKnob.KnobValue  := px^.SampleBeats;

    InfoDiv.Visible := true;
  end else
  begin
    CurrentSample.Info.IsValid := false;
    SampleOverlay.NoSampleMessage := NoRegionMessage;
    fSampleOverlay.IsCurrentSampleMissing := false;

    //===========================================
    InfoDiv.Visible := false;

    //== Sample Display Overlay ==
    fSampleOverlay.SetSampleInfo(false, 0);
    fSampleOverlay.ShowLoopPoints := false;

    //== Sample / Region Properties =============================
    s := '';
    if SampleNameLabel.Text <> s then SampleNameLabel.Text := s;

    SampleVolumeKnob.KnobValue := 0;
    SamplePanKnob.KnobValue    := 0;
    SampleTuneKnob.KnobValue   := 0;
    SampleFineKnob.KnobValue   := 0;
    SampleBeatsKnob.KnobValue  := 4;
  end;
end;


procedure TMiniSampleDisplayFrame.SampleDisplayOleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
var
  RegionCreateInfo : TRegionCreateInfo;
  curRG : IRegion;
  newRG : IRegion;
  kg : IKeyGroup;
  fn : string;
  XmlRegionCount : integer;
  XmlRegionInfo : TVstXmlRegion;
begin
  SampleOverlay.LargeTextMessage := '';

  Plugin.StopPreview;

  fn := '';

  // Check dropped Text for presence of VST-XML data from Cubase.
  XmlRegionCount := GetVstXmlRegionCount(Data.GetText);
  if XmlRegionCount > 0 then
  begin
    XmlRegionInfo := GetVstXmlRegionInfo(0, Data.GetText);
    fn := XmlRegionInfo.FileName;

    // TODO:MED when loading from cubase we also have access to the sample
    // start and sample end points. I think this is related to chopping
    // the sample. It would be good to support this somehow.
    // perhaps the load sample will be trimmed to match the
    // same sample data in Cubase.
  end;

  // Check for dropped files...
  if (fn = '') and (Data.GetFiles.Count > 0) then
  begin
    fn := Data.GetFiles[0];
  end;


  // check if the dropped file is a lucidity program..
  if (fn <> '') and (IsSupportedProgramFormat(fn)) then
  begin
    Plugin.ImportProgram(fn);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged_OLD);
    exit; //====================================================exit======>>
  end;


  // Assume file is an audio sample and attempt to load. TODO: should also check if file is supported audio format here...
  if (fn <> '') and (IsSupportedAudioFormat(fn)) then
  begin
    kg := Plugin.FocusedKeyGroup;
    curRG := Plugin.FocusedRegion;

    if assigned(curRG) then
    begin
      newRG := Plugin.ReplaceSample(curRG, fn);
      if assigned(newRG) then
      begin
        Plugin.FocusRegion(newRG.GetProperties^.UniqueID);
        Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged_OLD);
      end;
    end else
    begin
      RegionCreateInfo.KeyGroup      := kg;
      RegionCreateInfo.AudioFileName := fn;
      RegionCreateInfo.LowNote       := 0;
      RegionCreateInfo.HighNote      := 127;
      RegionCreateInfo.LowVelocity   := 0;
      RegionCreateInfo.HighVelocity  := 127;
      RegionCreateInfo.RootNote      := 60; //MIDI c4.

      newRG := Plugin.NewRegion(RegionCreateInfo);
      if (assigned(newRG)) and (newRG.GetProperties^.IsSampleError = false) then
      begin
        Plugin.FocusRegion(newRG.GetProperties^.UniqueID);
      end else
      if (assigned(newRG)) and (newRG.GetProperties^.IsSampleError = true) then
      begin
        Plugin.SampleMap.DeleteRegion(newRG);
        newRG := nil;
      end else
      begin
        // TODO:HIGH Need to show a message here to say that
        // the sample couldn't be loaded.
      end;
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged_OLD);
    end;


  end;




  {
  // Assume file is an audio sample and attempt to load. TODO: should also check if file is supported audio format here...
  if (fn <> '') then
  begin
    // TODO:HIGH This code requires a few changes.
    // - current region should only be deleted if new sample is loaded successfully.
    // - new region bounds should duplicate the old region bounds if it is replacing an older region.
    // - need some sort of response if the sample doesn't load.

    kg := Plugin.FocusedKeyGroup;
    CurRegion := Plugin.FocusedRegion;

    if assigned(CurRegion) then
    begin
      OwningSampleGroup := CurRegion.GetKeyGroup;

      RegionCreateInfo.KeyGroup      := OwningSampleGroup;
      RegionCreateInfo.AudioFileName := fn;
      RegionCreateInfo.LowNote       := CurRegion.GetProperties^.LowNote;
      RegionCreateInfo.HighNote      := CurRegion.GetProperties^.HighNote;
      RegionCreateInfo.LowVelocity   := CurRegion.GetProperties^.LowVelocity;
      RegionCreateInfo.HighVelocity  := CurRegion.GetProperties^.HighVelocity;
      RegionCreateInfo.RootNote      := CurRegion.GetProperties^.RootNote;

      // replace this code with some replace region code.
      aRegion := Plugin.NewRegion(RegionCreateInfo);
      if (assigned(aRegion)) and (aRegion.GetProperties^.IsSampleError = false) then
      begin
        Plugin.FocusRegion(aRegion.GetProperties^.UniqueID);
        Plugin.SampleMap.DeleteRegion(CurRegion);
      end else
      if (assigned(aRegion)) and (aRegion.GetProperties^.IsSampleError = true) then
      begin
        Plugin.SampleMap.DeleteRegion(aRegion);
        aRegion := nil;
      end else
      begin
        // TODO:HIGH Need to show a message here to say that
        // the sample couldn't be loaded.
      end;
    end else
    begin
      OwningSampleGroup := Plugin.FocusedKeyGroup;

      RegionCreateInfo.KeyGroup      := OwningSampleGroup;
      RegionCreateInfo.AudioFileName := fn;
      RegionCreateInfo.LowNote       := 0;
      RegionCreateInfo.HighNote      := 127;
      RegionCreateInfo.LowVelocity   := 0;
      RegionCreateInfo.HighVelocity  := 127;
      RegionCreateInfo.RootNote      := 60; //MIDI c4.

      aRegion := Plugin.NewRegion(RegionCreateInfo);
      if (assigned(aRegion)) and (aRegion.GetProperties^.IsSampleError = false) then
      begin
        Plugin.FocusRegion(aRegion.GetProperties^.UniqueID);
      end else
      if (assigned(aRegion)) and (aRegion.GetProperties^.IsSampleError = true) then
      begin
        Plugin.SampleMap.DeleteRegion(aRegion);
        aRegion := nil;
      end else
      begin
        // TODO:HIGH Need to show a message here to say that
        // the sample couldn't be loaded.
      end;
    end;

    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
  end;
  }

end;

procedure TMiniSampleDisplayFrame.SampleDisplayOleDragLeave(Sender: TObject);
begin
  SampleOverlay.LargeTextMessage := '';
end;

procedure TMiniSampleDisplayFrame.SampleDisplayOleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
var
  fn : string;
begin
  if not assigned(Plugin) then exit;

  if Data.GetFiles.Count > 0 then
  begin
    fn := Data.GetFiles[0];

    if IsSupportedAudioFormat(fn)   then SampleOverlay.LargeTextMessage := 'Replace Sample' else
    if IsSupportedProgramFormat(fn) then SampleOverlay.LargeTextMessage := 'Load Program'
    else SampleOverlay.LargeTextMessage := '';
  end;

end;

procedure TMiniSampleDisplayFrame.SampleDisplayResize(Sender: TObject);
begin
  UpdateControlVisibility;
  UpdateSampleDisplay;
  UpdateSampleDisplayInfo;
end;

procedure TMiniSampleDisplayFrame.SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurRegion : IRegion;
  MouseDownSamplePos : integer;
begin
  CurRegion := Plugin.FocusedRegion;
  if CurRegion = nil then exit;

  if (Button = mbRight) and (assigned(CurrentSample.Region)) then
  begin
    if CurrentSample.Region.GetProperties^.SampleDataLoaded then
    begin
      SampleContextMenu.LoopPointsVisible := fSampleOverlay.ShowLoopPoints;
      MouseDownSamplePos := SampleOverlay.PixelPosToSamplePos(x, CurrentSample.Info.SampleFrames);
      SampleContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, MouseDownSamplePos, CurRegion);
    end else
    if (CurrentSample.Region.GetProperties^.IsSampleError) and (CurrentSample.Region.GetProperties^.SampleErrorType = TSampleError.FileNotFound) then
    begin
      MissingSampleContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end;

end;

procedure TMiniSampleDisplayFrame.SampleOverlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // NOTE: Send a ModSlotChanged message here to force the sample overlay to update
  // it's modulation values. The current mod slot hasn't changed so
  // the message isn't strictly UM_MOD_SLOT_CHANGED appropiate.
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
end;

procedure TMiniSampleDisplayFrame.SampleMarkerChanged(Sender: TObject; Marker: TSampleMarker; NewPosition: integer);
var
  CurRegion : IRegion;
  sf : integer;
begin
  if not assigned(Plugin) then exit;

  CurRegion := Plugin.FocusedRegion;

  sf := CurRegion.GetSample^.Properties.SampleFrames;
  NewPosition := Clamp(NewPosition, 0, sf-1);

  case Marker of
    TSampleMarker.smSampleStartMarker: CurRegion.GetProperties^.SampleStart   := NewPosition;
    TSampleMarker.smSampleEndMarker:   CurRegion.GetProperties^.SampleEnd     := NewPosition;
    TSampleMarker.smLoopStartMarker:   CurRegion.GetProperties^.UserLoopStart := NewPosition;
    TSampleMarker.smLoopEndMarker:     CurRegion.GetProperties^.UserLoopEnd   := NewPosition;
  end;

  UpdateSampleDisplayInfo;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleMarkersChanged);
end;



procedure TMiniSampleDisplayFrame.SampleKnobChanged(Sender: TObject);
var
  Tag : integer;
  KnobValue : single;
  CurRegion : IRegion;
begin
  if not assigned(Plugin) then exit;

  Tag       := (Sender as TVamCompoundNumericKnob).Tag;
  KnobValue := (Sender as TVamCompoundNumericKnob).KnobValue;

  CurRegion := Plugin.FocusedRegion;

  if (assigned(CurRegion)) and (CurRegion.GetSample^.Properties.IsValid) then
  begin
    case Tag of
      1: CurRegion.GetProperties^.SampleVolume := KnobValue;
      2: CurRegion.GetProperties^.SamplePan    := KnobValue;
      4: CurRegion.GetProperties^.SampleBeats  := round(KnobValue);
      5: CurRegion.GetProperties^.SampleTune   := round(KnobValue);
      6: CurRegion.GetProperties^.SampleFine   := round(KnobValue);
    else
      raise Exception.Create('Unexpected tag value.');
    end;
  end;


  Throttle(UpdateSampleDisplayThottleToken, 25, procedure
  begin
    UpdateSampleDisplay;
  end);
end;


procedure TMiniSampleDisplayFrame.GuiEvent_SampleMakersChanged;
begin
  UpdateSampleDisplayInfo;
end;

procedure TMiniSampleDisplayFrame.UpdateControlVisibility;
var
  Target : TControl;
  mode : TPitchTracking;
  xpos : integer;
  ParName : string;
  ParID    : TPluginParameterID;
  ParValue : single;
begin
  SampleVolumeKnob.Align := alNone;
  SamplePanKnob.Align := alNone;
  SampleFineKnob.Align := alNone;
  SampleTuneKnob.Align := alNone;
  SampleBeatsKnob.Align := alNone;

  ParName := PluginParToName(TPluginParameter.PitchTracking);
  ParID   := PluginParToID(TPluginParameter.PitchTracking);
  ParValue := Plugin.GetPluginParameter(ParID);
  Mode := TPitchTrackingHelper.ToEnum(ParValue);

  case Mode of
    TPitchTracking.Note,
    TPitchTracking.Off:
    begin
      SampleTuneKnob.Visible := true;
      SampleFineKnob.Visible := true;
      SampleBeatsKnob.Visible := false;

      Target := SampleFineKnob;
      xPos := Target.Parent.Width - Target.Width;
      SampleFineKnob.Layout.SetPos(xPos,0);
      SampleTuneKnob.Layout.Anchor(SampleFineKnob).SnapToEdge(TControlFeature.LeftEdge).Move(-8,0);
      SamplePanKnob.Layout.Anchor(SampleTuneKnob).SnapToEdge(TControlFeature.LeftEdge).Move(-8,0);
      SampleVolumeKnob.Layout.Anchor(SamplePanKnob).SnapToEdge(TControlFeature.LeftEdge).Move(-8,0);
    end;

    TPitchTracking.BPM:
    begin
      SampleTuneKnob.Visible := false;
      SampleFineKnob.Visible := false;
      SampleBeatsKnob.Visible := true;

      Target := SampleBeatsKnob;
      xPos := Target.Parent.Width - Target.Width;
      SampleBeatsKnob.Layout.SetPos(xPos,0);
      SamplePanKnob.Layout.Anchor(SampleBeatsKnob).SnapToEdge(TControlFeature.LeftEdge).Move(-8,0);
      SampleVolumeKnob.Layout.Anchor(SamplePanKnob).SnapToEdge(TControlFeature.LeftEdge).Move(-8,0);
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

end;


procedure TMiniSampleDisplayFrame.InsidePanelResize(Sender: TObject);
begin
  Scope.Top := 4;
  Scope.Left := 4;
  Scope.Width := InsidePanel.Width-8;
  Scope.Height := InsidePanel.Height-8;
end;

procedure TMiniSampleDisplayFrame.UpdateModulation;
var
  ModSlot : integer;
  ModAmount : single;
  ModMin, ModMax : single;
  kg : IKeyGroup;
  Index1 : integer;
  Index2 : integer;
  Index3 : integer;
  Index4 : integer;
begin
  if Plugin.Globals.IsMouseOverModSlot
    then ModSlot := Plugin.Globals.MouseOverModSlot
    else ModSlot := Plugin.Globals.SelectedModSlot;

  kg := Plugin.ActiveKeyGroup;
  if not assigned(kg) then exit;

  //== Update min-max modulation amounts ==
  Index1 := GetModParIndex(TPluginParameter.SampleStart);
  Index2 := GetModParIndex(TPluginParameter.SampleEnd);
  Index3 := GetModParIndex(TPluginParameter.LoopStart);
  Index4 := GetModParIndex(TPluginParameter.LoopEnd);

  kg.GetModParModMinMax(Index1, ModMin, ModMax);
  SampleOverlay.SampleStartModMin := ModMin;
  SampleOverlay.SampleStartModMax := ModMax;

  kg.GetModParModMinMax(Index2, ModMin, ModMax);
  SampleOverlay.SampleEndModMin := ModMin;
  SampleOverlay.SampleEndModMax := ModMax;

  kg.GetModParModMinMax(Index3, ModMin, ModMax);
  SampleOverlay.LoopStartModMin := ModMin;
  SampleOverlay.LoopStartModMax := ModMax;

  kg.GetModParModMinMax(Index4, ModMin, ModMax);
  SampleOverlay.LoopEndModMin := ModMin;
  SampleOverlay.LoopEndModMax := ModMax;

  SampleOverlay.IsModEditActive := false;
  SampleOverlay.ShowModPoints := false;

  if ModSlot <> -1 then
  begin
    ModAmount := kg.GetModulatedParameters^[Index1].ModAmount[ModSlot];
    SampleOverlay.SampleStartMod := ModAmount;

    ModAmount := kg.GetModulatedParameters^[Index2].ModAmount[ModSlot];
    SampleOverlay.SampleEndMod := ModAmount;

    ModAmount := kg.GetModulatedParameters^[Index3].ModAmount[ModSlot];
    SampleOverlay.LoopStartMod := ModAmount;

    ModAmount := kg.GetModulatedParameters^[Index4].ModAmount[ModSlot];
    SampleOverlay.LoopEndMod := ModAmount;

    SampleOverlay.IsModEditActive := true;
    SampleOverlay.ShowModPoints := true;
  end;

  SampleOverlay.Invalidate;

end;

procedure TMiniSampleDisplayFrame.Handle_SampleOverlay_ModAmountsChanged(Sender: TObject);
var
  ModSlot : integer;
  ModAmount : single;
  kg : IKeyGroup;
  Index : integer;
begin
  if Plugin.Globals.IsMouseOverModSlot
    then ModSlot := Plugin.Globals.MouseOverModSlot
    else ModSlot := Plugin.Globals.SelectedModSlot;

  if ModSlot <> -1 then
  begin
    kg := Plugin.ActiveKeyGroup;

    Index     := GetModParIndex(TPluginParameter.SampleStart);
    ModAmount := SampleOverlay.SampleStartMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);

    Index     := GetModParIndex(TPluginParameter.SampleEnd);
    ModAmount := SampleOverlay.SampleEndMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);

    Index     := GetModParIndex(TPluginParameter.LoopStart);
    ModAmount := SampleOverlay.LoopStartMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);

    Index     := GetModParIndex(TPluginParameter.LoopEnd);
    ModAmount := SampleOverlay.LoopEndMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);
  end;

end;

procedure TMiniSampleDisplayFrame.SampleOverlayMouseOverMarkerChanged(Sender: TObject);
var
  Marker : TSampleMarker;
  ParName : string;
begin
  Marker := (Sender as TLuciditySampleOverlay).MouseOverMarker;

  case Marker  of
    TSampleMarker.smNone:                  ParName := '';
    TSampleMarker.smSampleStartMarker:     ParName := PluginParToName(TPluginParameter.SampleStart);
    TSampleMarker.smSampleEndMarker:       ParName := PluginParToName(TPluginParameter.SampleEnd);
    TSampleMarker.smLoopStartMarker:       ParName := PluginParToName(TPluginParameter.LoopStart);
    TSampleMarker.smLoopEndMarker:         ParName := PluginParToName(TPluginParameter.LoopEnd);
    TSampleMarker.smSampleStartModMarker:  ParName := PluginParToName(TPluginParameter.SampleStart);
    TSampleMarker.smSampleEndModMarker:    ParName := PluginParToName(TPluginParameter.SampleEnd);
    TSampleMarker.smLoopStartModMarker:    ParName := PluginParToName(TPluginParameter.LoopStart);
    TSampleMarker.smLoopEndModMarker:      ParName := PluginParToName(TPluginParameter.LoopEnd);
  else
    raise Exception.Create('Type not handled.');
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.OnParControlEnter, @ParName, nil);
end;


procedure TMiniSampleDisplayFrame.Timer1Timer(Sender: TObject);
begin
  //TODO:MED delete this timer.
  Scope.Invalidate;
end;

procedure TMiniSampleDisplayFrame.ZoomButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;

  //TODO:HIGH code clean up required here. the xZoom and xOffset variables needs to be deleted.
  Zoom, Offset : single;
  xZoom, xOffset : double;
  //SampleFrames, DisplayPixelWidth : integer;
  //IndexA, IndexB : single;
begin
  if not CurrentSample.Info.IsValid then exit;
  if not assigned(CurrentSample.Region) then exit;

  Tag := (Sender as TVamTextBox).Tag;

  Zoom   := Plugin.Globals.GuiState.SampleDisplayZoom;
  Offset := Plugin.Globals.GuiState.SampleDisplayOffset;

  if Button = mbLeft then
  begin
    if Tag = 1 then
    begin
      //Zoom in.
      xZoom   := Zoom + ((1 - Zoom) * 0.25);
      xOffset := Offset;

      assert(xZoom >= 0);
      assert(xZoom <= 1);
      assert(xOffset >= 0);
      assert(xOffset <= 1);

      Zoom := xZoom;
      Offset := xOffset;

      Plugin.Globals.GuiState.SampleDisplayZoom   := Zoom;
      Plugin.Globals.GuiState.SampleDisplayOffset := Offset;

      UpdateSampleDisplay;
    end;

    if Tag = 2 then
    begin
      //Zoom out.
      xZoom   := Zoom - ((1 - Zoom) * 0.5);
      if xZoom <= 0.15 then xZoom := 0;
      xOffset := Offset;

      assert(xZoom >= 0);
      assert(xZoom <= 1);
      assert(xOffset >= 0);
      assert(xOffset <= 1);

      Zoom := xZoom;
      Offset := xOffset;

      Plugin.Globals.GuiState.SampleDisplayZoom   := Zoom;
      Plugin.Globals.GuiState.SampleDisplayOffset := Offset;

      UpdateSampleDisplay;
    end;

    if Tag = 3 then
    begin
      //Zoom out full
      Plugin.Globals.GuiState.SampleDisplayZoom   := 0;
      UpdateSampleDisplay;
    end;

    // Finally....
    UpdateZoomSlider;
  end;

end;


procedure TMiniSampleDisplayFrame.ZoomToSampleMarker(Sender: TObject; Marker: TDialogSampleMarker);
var
  CurRegion : IRegion;
  TargetSampleFrame : integer;
  SampleFrames : integer;
  DisplayPixelWidth : integer;
  ZoomPos : TZoomPos;
  Zoom, Offset : single;
  RegionLoopStart, RegionLoopEnd : integer;
begin
  if not assigned(Plugin)         then exit;

  CurRegion := Plugin.FocusedRegion;
  if CurRegion = nil then exit;

  // Safty check: Check the current sample is loaded and is valid.
  assert(CurRegion.GetSample^.Properties.IsValid);

  CurRegion.GetProperties^.GetRegionLoopPoints(RegionLoopStart, RegionLoopEnd);

  case Marker of
    TDialogSampleMarker.SampleStart: TargetSampleFrame := CurRegion.GetProperties^.SampleStart;
    TDialogSampleMarker.SampleEnd:   TargetSampleFrame := CurRegion.GetProperties^.SampleEnd;
    TDialogSampleMarker.LoopStart:   TargetSampleFrame := RegionLoopStart;
    TDialogSampleMarker.LoopEnd:     TargetSampleFrame := RegionLoopEnd;
  else
    raise Exception.Create('Marker type not handled.');
  end;

  SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;
  DisplayPixelWidth := SampleDisplay.Width;

  ZoomPos := CalcZoomPos(SampleFrames, DisplayPixelWidth, TargetSampleFrame);


  //== Set a bunch of zoom/offset values ==
  Zoom   := ZoomPos.Zoom;
  Offset := ZoomPos.Offset;

  Plugin.Globals.GuiState.SampleDisplayZoom   := Zoom;
  Plugin.Globals.GuiState.SampleDisplayOffset := Offset;

  UpdateSampleDisplay;
end;

procedure TMiniSampleDisplayFrame.UpdateZoomSlider;
var
  ZoomSize : single;
  Zoom, Offset : single;
begin
  Zoom   := Plugin.Globals.GuiState.SampleDisplayZoom;
  Offset := Plugin.Globals.GuiState.SampleDisplayOffset;

  ZoomSize := 1 - (Zoom * 0.9);

  ZoomScrollBar.IndexSize := ZoomSize;
  ZoomScrollBar.IndexPos  := Offset;
end;

procedure TMiniSampleDisplayFrame.ZoomScrollBarChanged(Sender: TObject);
var
  Offset : single;
begin
  Offset := (Sender as TVamScrollBar).IndexPos;
  Plugin.Globals.GuiState.SampleDisplayOffset := Offset;

  Throttle(UpdateSampleDisplayThottleToken, 25, procedure
  begin
    UpdateSampleDisplay;
  end);
end;

procedure TMiniSampleDisplayFrame.SampleOverlay_MarkerChanged(Sender: TObject; Marker: TSampleMarker; NewPosition: integer);
var
  CurRegion : IRegion;
  sf : integer;
begin
  if not assigned(Plugin) then exit;

  CurRegion := Plugin.FocusedRegion;

  sf := CurRegion.GetSample^.Properties.SampleFrames;
  NewPosition := Clamp(NewPosition, 0, sf-1);

  case Marker of
    TSampleMarker.smSampleStartMarker: CurRegion.GetProperties^.SampleStart   := NewPosition;
    TSampleMarker.smSampleEndMarker:   CurRegion.GetProperties^.SampleEnd     := NewPosition;
    TSampleMarker.smLoopStartMarker:   CurRegion.GetProperties^.UserLoopStart := NewPosition;
    TSampleMarker.smLoopEndMarker:     CurRegion.GetProperties^.UserLoopEnd   := NewPosition;
  end;

  GuiEvent_SampleMakersChanged;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleMarkersChanged);
end;

procedure TMiniSampleDisplayFrame.SampleOverlay_ZoomChanged(Sender: TObject; Zoom, Offset: single);
begin
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
  try
    Plugin.Globals.GuiState.SampleDisplayZoom   := Zoom;
    Plugin.Globals.GuiState.SampleDisplayOffset := Offset;

    UpdateSampleDisplay;
    UpdateZoomSlider;
  finally
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
  end;
end;


end.
