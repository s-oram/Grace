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
  eePlugin, uGuiFeedbackData, LuciditySampleOverlay,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, VamSampleDisplay, Vcl.Menus,
  RedFoxGraphicControl, VamGraphicControl, VamLabel, VamDiv, VamCompoundLabel,
  VamCompoundNumericKnob, VamScrollBar, VamTextBox, Vcl.ExtCtrls;

type
  TUsageContext = (General, SampleZoom);

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
    ZoomScrollBar: TVamScrollBar;
    ZoomControlsDiv: TVamDiv;
    ZoomOutButton: TVamTextBox;
    ZoomInButton: TVamTextBox;
    ZoomApplyButton: TVamTextBox;
    ZoomLoopStartButton: TVamTextBox;
    ZoomSampleEndButton: TVamTextBox;
    ZoomSampleStartButton: TVamTextBox;
    VamLabel2: TVamLabel;
    Zoom100Button: TVamTextBox;
    ZoomLoopEndButton: TVamTextBox;
    Timer1: TTimer;
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
    fSampleDisplayContext: TUsageContext;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
    procedure SetSampleDisplayContext(const Value: TUsageContext);

    procedure ZoomButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ZoomToSampleMarker(Sender : TObject; Marker:TDialogSampleMarker);
  protected
    CurrentSample : TSampleDisplayFrameInfo;

    Zoom, Offset : single;
    fSampleOverlay : TLuciditySampleOverlay;

    SampleOverlayClickPos : TPoint;
    SampleContextMenu : TSampleContextMenu;

    StoredImage : ISampleImageBuffer;
    SampleRenderer : TSampleImageRenderer;

    Scope : TLucidityScope;

    procedure InternalUpdateSampleDisplay(const Region : IRegion; const NoRegionMessage : string);
    procedure InternalUpdateSampleInfo(const Region : IRegion; const NoRegionMessage : string);

    procedure UpdateSampleDisplayInfo;

    procedure SampleOverlayDblClicked(Sender : TObject);

    procedure SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleOverlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleOverlayZoomChanged(Sender : TObject; aZoom, aOffset : single);
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



    property UsageContext : TUsageContext read fSampleDisplayContext write SetSampleDisplayContext;
  end;

implementation

uses
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

  Zoom   := 0;
  Offset := 0;

  fSampleOverlay := TLuciditySampleOverlay.Create(AOwner);
  fSampleOverlay.Parent  := SampleDisplay;
  fSampleOverlay.Align := alClient;
  fSampleOverlay.Visible := true;

  fSampleOverlay.OnModAmountsChanged := Handle_SampleOverlay_ModAmountsChanged;
  fSampleOverlay.OnMouseDown := SampleOverlayMouseDown;
  fSampleOverlay.OnMouseUp   := SampleOverlayMouseUp;
  fSampleOverlay.OnSampleMarkerChanged := SampleMarkerChanged;
  fSampleOverlay.OnZoomChanged := SampleOverlayZoomChanged;
  fSampleOverlay.OnMouseOverMakerChanged := SampleOverlayMouseOverMarkerChanged;
  fSampleOverlay.OnSampleMarkerChanged := SampleOverlay_MarkerChanged;
  fSampleOverlay.OnZoomChanged         := SampleOverlay_ZoomChanged;

  fSampleOverlay.OnOleDragDrop  := SampleDisplayOleDragDrop;
  fSampleOverlay.OnOleDragOver  := SampleDisplayOleDragOver;
  fSampleOverlay.OnOleDragLeave := SampleDisplayOleDragLeave;

  fSampleOverlay.OnDblClick := SampleOverlayDblClicked;

  CurrentSample.Info.IsValid := false;

  SampleContextMenu := TSampleContextMenu.Create;


  StoredImage := TSampleImageBuffer.Create;

  SampleRenderer := TSampleImageRenderer.Create;

  Zoom100Button.OnMouseDown         := ZoomButtonMouseDown;
  ZoomSampleStartButton.OnMouseDown := ZoomButtonMouseDown;
  ZoomSampleEndButton.OnMouseDown   := ZoomButtonMouseDown;
  ZoomLoopStartButton.OnMouseDown   := ZoomButtonMouseDown;
  ZoomLoopEndButton.OnMouseDown     := ZoomButtonMouseDown;
  ZoomOutButton.OnMouseDown         := ZoomButtonMouseDown;
  ZoomInButton.OnMouseDown          := ZoomButtonMouseDown;
  ZoomApplyButton.OnMouseDown       := ZoomButtonMouseDown;


  ZoomScrollBar.SliderStyle := TVamScrollBarStyle.RoundCornersBottom;


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


  //TODO: This should be tied to the active voice group, or the active voice.
  // not the global scope.
  Scope.SignalRecorder  := Plugin.SignalRecorder;
  Scope.FreqAnalyzer    := Plugin.FreqAnalyzer;
  Scope.Font.Color      := GetRedFoxColor(kColor_LcdDark5);
  Scope.ColorBackground := kColor_LcdDark1;
  Scope.ColorForeground := GetRedFoxColor(kColor_LcdDark5);
  Scope.ColorForeground := '$FFCCDFFF';

  UsageContext := TUsageContext.General;

  SampleInfoBox.Height := 25;
  InfoDiv.Align := alClient;

  SampleContextMenu.Initialize(aPlugin);

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
  //SampleDisplay.Layout.SetPos(0,0).SetSize(kSampleImageWidth, kSampleImageHeight);



  GuiSetup.StyleButton_CommandButton(Zoom100Button);
  GuiSetup.StyleButton_CommandButton(ZoomSampleStartButton);
  GuiSetup.StyleButton_CommandButton(ZoomSampleEndButton);
  GuiSetup.StyleButton_CommandButton(ZoomLoopStartButton);
  GuiSetup.StyleButton_CommandButton(ZoomLoopEndButton);
  GuiSetup.StyleButton_CommandButton(ZoomOutButton);
  GuiSetup.StyleButton_CommandButton(ZoomInButton);
  GuiSetup.StyleButton_CommandButton(ZoomInButton);
  GuiSetup.StyleButton_CommandButton_Bright(ZoomApplyButton);

  Zoom100Button.Width := kZoomButtonWidth;
  ZoomSampleStartButton.Width := kZoomButtonWidth;
  ZoomSampleEndButton.Width := kZoomButtonWidth;
  ZoomLoopStartButton.Width := kZoomButtonWidth;
  ZoomLoopEndButton.Width := kZoomButtonWidth;
  ZoomApplyButton.Align := TAlign.alClient;



  Timer1.Enabled := true;
  Timer1.Interval := 25;

  //== finally, call the message handlers to ensure everything is up to date ===
  UpdateControlVisibility;
  UpdateModulation;
  UpdateSampleDisplay;
end;

procedure TMiniSampleDisplayFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  SamplePos : integer;
  zx : single;
begin
  if MsgID = TLucidMsgID.Command_UpdateControlVisibility then UpdateControlVisibility;
  if MsgID = TLucidMsgID.Command_UpdateModMatrix         then UpdateModulation;
  if MsgID = TLucidMsgID.ModSlotChanged                  then UpdateModulation;
  if MsgID = TLucidMsgID.ModAmountChanged                then UpdateModulation;
  if MsgID = TLucidMsgID.SampleFocusChanged              then UpdateControlVisibility;
  if MsgID = TLucidMsgID.SampleOscTypeChanged            then UpdateControlVisibility;
  if MsgID = TLucidMsgID.LoopTypeChanged                 then UpdateSampleDisplayInfo;
  if MsgID = TLucidMsgID.Command_UpdateSampleDisplay     then UpdateSampleDisplay;
  if MsgID = TLucidMsgID.Command_UpdateSampleInfo        then UpdateSampleDisplayInfo;

  if MsgID = TLucidMsgID.Command_ShowReplaceRegionMessage then SampleOverlay.ShowReplaceMessage := true;
  if MsgID = TLucidMsgID.Command_HideReplaceRegionMessage then SampleOverlay.ShowReplaceMessage := false;

  if MsgID = TLucidMsgID.Command_Sample_ZoomIn then
  begin
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
    try
      SamplePos := Integer(Data^);

      if not Command.AreSampleZoomControlsVisible(Plugin)
        then Command.ToggleSampleZoom(Plugin);

      //Zoom In!
      zx := Zoom  + 0.2;
      Zoom := Clamp(zx, 0, 1);

      // Update the offset.
      zx := SamplePos / CurrentSample.Info.SampleFrames;
      Offset := Clamp(zx, 0, 1);

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
      SamplePos := Integer(Data^);
      //Zoom In!
      zx := Zoom  - 0.2;
      Zoom := Clamp(zx, 0, 1);

      // Update the offset.
      zx := SamplePos / CurrentSample.Info.SampleFrames;
      Offset := Clamp(zx, 0, 1);

      if Zoom > 0 then
      begin
        // Update the GUI.
        UpdateSampleDisplay;
        UpdateZoomSlider;
      end else
      begin
        Command.ToggleSampleZoom(Plugin);
      end;
    finally
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
    end;
  end;

  if MsgID = TLucidMsgID.Command_Sample_ZoomOutFull then
  begin
    if Command.AreSampleZoomControlsVisible(Plugin)
      then Command.ToggleSampleZoom(Plugin);
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
  rd := FindRegionToDisplay(Plugin);

  InternalUpdateSampleDisplay(rd.Region, rd.Message);
  InternalUpdateSampleInfo(rd.Region, rd.Message);
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
begin
  CurrentSample.Region := Region;

  if (assigned(Region)) then
  begin
    if (Region.GetSample^.Properties.IsValid) and (UsageContext = TUsageContext.SampleZoom) then
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
    if (Region.GetSample^.Properties.IsValid) and (UsageContext = TUsageContext.General) then
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




      {
      // Provide sample info for real-time drawing of sample.
      aZoom   := Zoom;
      aOffset := Offset;

      SampleOverlay.SetZoomOffset(aZoom, aOffset);
      SampleDisplay.Zoom := aZoom;
      SampleDisplay.Offset := aOffset;


      CurrentSample.SampleInfo.IsValid := true;
      CurrentSample.SampleInfo.ChannelCount := Region.GetSample^.Properties.ChannelCount;
      CurrentSample.SampleInfo.SampleFrames := Region.GetSample^.Properties.SampleFrames;

      if Region.GetSample^.Properties.ChannelCount = 1 then
      begin
        CurrentSample.SampleInfo.Ch1 := Region.GetSample^.Properties.Ch1;
      end;

      if Region.GetSample^.Properties.ChannelCount = 2 then
      begin
        CurrentSample.SampleInfo.Ch1 := Region.GetSample^.Properties.Ch1;
        CurrentSample.SampleInfo.Ch2 := Region.GetSample^.Properties.Ch2;
      end;

      SampleDisplay.DrawSample(CurrentSample.SampleInfo);
      SampleDisplay.Invalidate;
      }
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
begin
  if (assigned(Region)) then
  begin
    if Region.GetSample^.Properties.IsValid then
    begin
      CurrentSample.Info.IsValid := true;
      CurrentSample.Info.ChannelCount := Region.GetSample^.Properties.ChannelCount;
      CurrentSample.Info.SampleFrames := Region.GetSample^.Properties.SampleFrames;

      //== Sample Overlay ==
      fSampleOverlay.SetSampleInfo(true, CurrentSample.Info.SampleFrames);
      fSampleOverlay.SampleStart := Region.GetProperties^.SampleStart;
      fSampleOverlay.SampleEnd   := Region.GetProperties^.SampleEnd;
      fSampleOverlay.LoopStart   := Region.GetProperties^.LoopStart;
      fSampleOverlay.LoopEnd     := Region.GetProperties^.LoopEnd;

      fSampleOverlay.ShowLoopPoints := ShowLoopMarkers(Region);
    end else
    begin
      //== Sample Display Overlay ==
      fSampleOverlay.SetSampleInfo(false, 0);
      fSampleOverlay.ShowLoopPoints := false;
      fSampleOverlay.NoSampleMessage :=  EncloseWithBrackets(Region.GetSample^.LastErrorMessage);
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
  aRegion : IRegion;
  CurRegion : IRegion;
  kg : IKeyGroup;
  OwningSampleGroup : IKeyGroup;
  fn : string;
  XmlRegionCount : integer;
  XmlRegionInfo : TVstXmlRegion;
begin
  SampleOverlay.ShowReplaceMessage := false;

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
  if (fn <> '') and (IsLucidityProgramFile(fn)) then
  begin
    Plugin.LoadProgramFromFile(fn);
    Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleFocusChanged);
    exit; //====================================================exit======>>
  end;



  // Asume file is an audio sample and attempt to load. TODO: should also check if file is supported audio format here...
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

    Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleFocusChanged);




    {
    SG := Plugin.FocusedKeyGroup;
    CurRegion := Plugin.FocusedRegion;
    if assigned(CurRegion) then
    begin
      Plugin.SampleMap.DeleteRegion(CurRegion);
    end;

    if assigned(CurRegion)
      then OwningSampleGroup := CurRegion.GetKeyGroup
      else OwningSampleGroup := Plugin.FocusedKeyGroup;

    RegionCreateInfo.KeyGroup   := OwningSampleGroup;
    RegionCreateInfo.AudioFileName := fn;
    RegionCreateInfo.LowNote       := 0;
    RegionCreateInfo.HighNote      := 127;
    RegionCreateInfo.LowVelocity   := 0;
    RegionCreateInfo.HighVelocity  := 127;
    RegionCreateInfo.RootNote      := 60; //MIDI c4.

    aRegion := Plugin.NewRegion(RegionCreateInfo);

    if assigned(aRegion) then
    begin
      Plugin.FocusRegion(aRegion.GetProperties^.UniqueID);
    end;

    Plugin.Globals.SendGuiMessage(UM_SAMPLE_FOCUS_CHANGED);
    }
  end;







  {
  if (Data.GetFiles.Count = 1) then
  begin
    fn := Data.GetFiles[0];

    if IsFileLucidityProgram(fn) then
    begin
      Plugin.LoadProgramFromFile(fn);
      Plugin.Globals.SendGuiMessage(UM_SAMPLE_FOCUS_CHANGED);
      exit; //====================================================exit======>>
    end;
  end;


  if (Data.GetFiles.Count > 0) then
  begin
    SG := Plugin.FocusedKeyGroup;
    CurRegion := Plugin.FocusedRegion;
    if assigned(CurRegion) then
    begin
      Plugin.SampleMap.DeleteRegion(CurRegion);
    end;

    if assigned(CurRegion)
      then OwningSampleGroup := CurRegion.GetKeyGroup
      else OwningSampleGroup := Plugin.FocusedKeyGroup;

    RegionCreateInfo.KeyGroup   := OwningSampleGroup;
    RegionCreateInfo.AudioFileName := Data.GetFiles[0];
    RegionCreateInfo.LowNote       := 0;
    RegionCreateInfo.HighNote      := 127;
    RegionCreateInfo.LowVelocity   := 0;
    RegionCreateInfo.HighVelocity  := 127;
    RegionCreateInfo.RootNote      := 60; //MIDI c4.

    aRegion := Plugin.SampleMap.NewRegion(RegionCreateInfo);

    if assigned(aRegion) then
    begin
      Plugin.FocusRegion(aRegion.GetProperties^.UniqueID);
    end;

    Plugin.Globals.SendGuiMessage(UM_SAMPLE_FOCUS_CHANGED);
  end;
  }
end;

procedure TMiniSampleDisplayFrame.SampleDisplayOleDragLeave(Sender: TObject);
begin
  SampleOverlay.ShowReplaceMessage := false;
end;

procedure TMiniSampleDisplayFrame.SampleDisplayOleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
begin
  //TODO: Nothing here yet.

  if not assigned(Plugin) then exit;

  if assigned(Plugin.FocusedRegion)
    then SampleOverlay.ShowReplaceMessage := true
    else SampleOverlay.ShowReplaceMessage := false;

end;

procedure TMiniSampleDisplayFrame.SampleDisplayResize(Sender: TObject);
begin
  self.UpdateControlVisibility;
  UpdateSampleDisplay;
  UpdateSampleDisplayInfo;
end;

procedure TMiniSampleDisplayFrame.SampleOverlayDblClicked(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleZoom(Plugin);
end;

procedure TMiniSampleDisplayFrame.SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurRegion : IRegion;
  MouseDownSamplePos : integer;
begin
  CurRegion := Plugin.FocusedRegion;
  if CurRegion = nil then exit;

  if (Button = mbRight) and (CurrentSample.Info.IsValid) then
  begin
    SampleContextMenu.LoopPointsVisible := fSampleOverlay.ShowLoopPoints;

    MouseDownSamplePos := SampleOverlay.PixelPosToSamplePos(x, CurrentSample.Info.SampleFrames);
    SampleContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, MouseDownSamplePos);
  end;
end;

procedure TMiniSampleDisplayFrame.SampleOverlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // NOTE: Send a ModSlotChanged message here to force the sample overlay to update
  // it's modulation values. The current mod slot hasn't changed so
  // the message isn't strictly UM_MOD_SLOT_CHANGED appropiate.
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
end;

procedure TMiniSampleDisplayFrame.SampleMarkerChanged(Sender: TObject; Marker: TSampleMarker; NewPosition: integer);
var
  CurRegion : IRegion;
begin
  if not assigned(Plugin) then exit;

  CurRegion := Plugin.FocusedRegion;

  case Marker of
    smSampleStartMarker: CurRegion.GetProperties^.SampleStart := NewPosition;
    smSampleEndMarker:   CurRegion.GetProperties^.SampleEnd   := NewPosition;
    smLoopStartMarker:   CurRegion.GetProperties^.LoopStart   := NewPosition;
    smLoopEndMarker:     CurRegion.GetProperties^.LoopEnd     := NewPosition;
  end;

  UpdateSampleDisplayInfo;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleMarkersChanged);
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


procedure TMiniSampleDisplayFrame.SampleOverlayZoomChanged(Sender: TObject; aZoom, aOffset: single);
begin
  Zoom   := aZoom;
  Offset := aOffset;
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
    smNone:                  ParName := '';
    smSampleStartMarker:     ParName := PluginParToName(TPluginParameter.SampleStart);
    smSampleEndMarker:       ParName := PluginParToName(TPluginParameter.SampleEnd);
    smLoopStartMarker:       ParName := PluginParToName(TPluginParameter.LoopStart);
    smLoopEndMarker:         ParName := PluginParToName(TPluginParameter.LoopEnd);
    smSampleStartModMarker:  ParName := PluginParToName(TPluginParameter.SampleStart);
    smSampleEndModMarker:    ParName := PluginParToName(TPluginParameter.SampleEnd);
    smLoopStartModMarker:    ParName := PluginParToName(TPluginParameter.LoopStart);
    smLoopEndModMarker:      ParName := PluginParToName(TPluginParameter.LoopEnd);
  else
    raise Exception.Create('Type not handled.');
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.OnParControlEnter, @ParName);
end;



procedure TMiniSampleDisplayFrame.SetSampleDisplayContext(const Value: TUsageContext);
begin
  fSampleDisplayContext := Value;

  SampleDisplay.Align := TAlign.alClient;
  InsidePanel.AlignWithMargins := true;



  case Value of
    TUsageContext.General:
    begin
      InsidePanel.Align := TAlign.alClient;
      InsidePanel.Margins.Bottom := 0;
      InsidePanel.CornerRadius3 := 3;
      InsidePanel.CornerRadius4 := 3;

      ZoomScrollBar.Visible := false;
      ZoomControlsDiv.Visible := false;

      SampleOverlay.BringToFront;

      Zoom := 0;
      //Offset
    end;

    TUsageContext.SampleZoom:
    begin
      InsidePanel.Align := TAlign.alClient;
      InsidePanel.CornerRadius3 := 0;
      InsidePanel.CornerRadius4 := 0;

      ZoomControlsDiv.Align := TAlign.alBottom;
      ZoomControlsDiv.Visible := true;


      ZoomScrollBar.Align := TAlign.alBottom;
      ZoomScrollBar.AlignWithMargins := true;
      ZoomScrollBar.Margins.Bottom := 4;
      ZoomScrollBar.Height := 22;
      ZoomScrollBar.Top := 0;
      ZoomScrollBar.Visible := true;
      ZoomScrollBar.BringToFront;

      UpdateZoomSlider;

      SampleOverlay.BringToFront;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

  //Scope.Layout.
  Scope.BringToFront;


  UpdateSampleDisplay;
end;


procedure TMiniSampleDisplayFrame.Timer1Timer(Sender: TObject);
begin
  //TODO:MED delete this timer.
  Scope.Invalidate;
end;

procedure TMiniSampleDisplayFrame.ZoomButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;
  xZoom, xOffset : double;
  //SampleFrames, DisplayPixelWidth : integer;
  //IndexA, IndexB : single;
begin
  if not CurrentSample.Info.IsValid then exit;
  if not assigned(CurrentSample.Region) then exit;

  Tag := (Sender as TVamTextBox).Tag;

  if Button = mbLeft then
  begin
    //SampleFrames      := CurrentSample.Info.SampleFrames;
    //DisplayPixelWidth := SampleDisplay.Width;

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
      UpdateSampleDisplay;


      //CalcZoomBounds(xZoom, xOffset, SampleFrames, DisplayPixelWidth, IndexA, IndexB);
      //SampleZoomControl.IndexA := IndexA;
      //SampleZoomControl.IndexB := IndexB;
      //SampleZoomControl.Invalidate;
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
      UpdateSampleDisplay;

      //CalcZoomBounds(xZoom, xOffset, SampleFrames, DisplayPixelWidth, IndexA, IndexB);
      //SampleZoomControl.IndexA := IndexA;
      //SampleZoomControl.IndexB := IndexB;
      //SampleZoomControl.Invalidate;
    end;


    if Tag = 3 then ZoomToSampleMarker(self, TDialogSampleMarker.SampleStart);
    if Tag = 4 then ZoomToSampleMarker(self, TDialogSampleMarker.SampleEnd);
    if Tag = 5 then ZoomToSampleMarker(self, TDialogSampleMarker.LoopStart);
    if Tag = 6 then ZoomToSampleMarker(self, TDialogSampleMarker.LoopEnd);
    //if Tag = 7 then ZoomMarkerMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);

    if Tag = 8 then
    begin
      Zoom := 0;
      UpdateSampleDisplay;

      //CalcZoomBounds(0, 0, SampleFrames, DisplayPixelWidth, IndexA, IndexB);
      //SampleZoomControl.IndexA := IndexA;
      //SampleZoomControl.IndexB := IndexB;
      //SampleZoomControl.Invalidate;
    end;


    if Tag = 10 then
    begin
      // Hide the sample zoom controls here.
      if Plugin.Globals.GuiState.MainGuiLayout = TMainGuiLayout.SampleZoom then
      begin
        Plugin.Globals.GuiState.MainGuiLayout := TMainGuiLayout.Default;
        Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.GUILayoutChanged);
      end;
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
begin
  if not assigned(Plugin)         then exit;

  CurRegion := Plugin.FocusedRegion;
  if CurRegion = nil then exit;

  // Safty check: Check the current sample is loaded and is valid.
  assert(CurRegion.GetSample^.Properties.IsValid);

  case Marker of
    TDialogSampleMarker.SampleStart: TargetSampleFrame := CurRegion.GetProperties^.SampleStart;
    TDialogSampleMarker.SampleEnd:   TargetSampleFrame := CurRegion.GetProperties^.SampleEnd;
    TDialogSampleMarker.LoopStart:   TargetSampleFrame := CurRegion.GetProperties^.LoopStart;
    TDialogSampleMarker.LoopEnd:     TargetSampleFrame := CurRegion.GetProperties^.LoopEnd;
  else
    raise Exception.Create('Marker type not handled.');
  end;

  SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;
  DisplayPixelWidth := SampleDisplay.Width;

  ZoomPos := CalcZoomPos(SampleFrames, DisplayPixelWidth, TargetSampleFrame);


  //== Set a bunch of zoom/offset values ==
  Zoom   := ZoomPos.Zoom;
  Offset := ZoomPos.Offset;
  UpdateSampleDisplay;

  //Debounce(SampleUpdateDebounceID, 25, deLeading, UpdateSampleDisplay);
  //SampleZoomControl.IndexA := ZoomPos.IndexA;
  //SampleZoomControl.IndexB := ZoomPos.IndexB;
  //SampleZoomControl.Invalidate;
end;

procedure TMiniSampleDisplayFrame.UpdateZoomSlider;
var
  ZoomSize : single;
begin
  ZoomSize := 1 - (Zoom * 0.9);
  ZoomScrollBar.IndexSize := ZoomSize;
  ZoomScrollBar.IndexPos  := Offset;
end;

procedure TMiniSampleDisplayFrame.ZoomScrollBarChanged(Sender: TObject);
begin
  Offset := (Sender as TVamScrollBar).IndexPos;

  Throttle(UpdateSampleDisplayThottleToken, 25, procedure
  begin
    UpdateSampleDisplay;
  end);
end;

procedure TMiniSampleDisplayFrame.SampleOverlay_MarkerChanged(Sender: TObject; Marker: TSampleMarker; NewPosition: integer);
var
  CurRegion : IRegion;
begin
  if not assigned(Plugin) then exit;

  CurRegion := Plugin.FocusedRegion;

  case Marker of
    smSampleStartMarker: CurRegion.GetProperties^.SampleStart := NewPosition;
    smSampleEndMarker:   CurRegion.GetProperties^.SampleEnd   := NewPosition;
    smLoopStartMarker:   CurRegion.GetProperties^.LoopStart   := NewPosition;
    smLoopEndMarker:     CurRegion.GetProperties^.LoopEnd     := NewPosition;
  end;

  GuiEvent_SampleMakersChanged;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleMarkersChanged);
end;

procedure TMiniSampleDisplayFrame.SampleOverlay_ZoomChanged(Sender: TObject; Zoom, Offset: single);
begin
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_BeginGuiUpdate);
  try
    Command.ShowSampleZoom(Plugin);
    self.Zoom := Zoom;
    self.Offset := Offset;
    UpdateSampleDisplay;
    UpdateZoomSlider;
  finally
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_EndGuiUpdate);
  end;
end;

end.
