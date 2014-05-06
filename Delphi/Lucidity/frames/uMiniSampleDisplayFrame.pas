unit uMiniSampleDisplayFrame;

interface

uses
  eeGuiStandardv2,
  VamLib.UniqueID,
  VamLib.ZeroObject, Math,
  VamVisibleControl, Lucidity.SampleImageRenderer,
  Lucidity.Types,
  Lucidity.Interfaces,
  Lucidity.SampleMap, Menu.SampleDisplayMenu,
  eePlugin, uGuiFeedbackData, LuciditySampleOverlay,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, VamSampleDisplay, Vcl.Menus,
  RedFoxGraphicControl, VamGraphicControl, VamLabel, VamDiv, VamCompoundLabel,
  VamCompoundNumericKnob;

type
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
    procedure SampleKnobChanged(Sender: TObject);
    procedure SampleDisplayResize(Sender: TObject);
    procedure InfoDivResize(Sender: TObject);
    procedure InsidePanelResize(Sender: TObject);
  private
    UpdateSampleDisplayThottleToken : TUniqueID;

    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    procedure UpdateControlVisibility;
    procedure UpdateModulation;

    procedure SetGuiStandard(const Value: TGuiStandard);
    procedure SetPlugin(const Value: TeePlugin);

    procedure Handle_SampleOverlay_ModAmountsChanged(Sender:TObject);
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    CurrentSample : TSampleDisplayFrameInfo;

    Zoom, Offset : single;
    fSampleOverlay : TLuciditySampleOverlay;

    SampleOverlayClickPos : TPoint;
    SampleDisplayMenu : TSampleDisplayMenu;

    StoredImage : ISampleImageBuffer;
    SampleRenderer : TSampleImageRenderer;

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
  Lucidity.PluginParameters,
  VamLib.Throttler,
  eeDsp,
  VamLib.Graphics,
  eeVstXml, eeVstParameter, uLucidityEnums,
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

  fSampleOverlay.OnOleDragDrop  := SampleDisplayOleDragDrop;
  fSampleOverlay.OnOleDragOver  := SampleDisplayOleDragOver;
  fSampleOverlay.OnOleDragLeave := SampleDisplayOleDragLeave;

  fSampleOverlay.OnDblClick := SampleOverlayDblClicked;

  CurrentSample.Info.IsValid := false;

  SampleDisplayMenu := TSampleDisplayMenu.Create;


  StoredImage := TSampleImageBuffer.Create;

  SampleRenderer := TSampleImageRenderer.Create;
end;

destructor TMiniSampleDisplayFrame.Destroy;
begin
  if (assigned(FMotherShip))
    then FMotherShip.DeregisterZeroObject(Pointer(IZeroObject(Self)));

  SampleDisplayMenu.Free;
  SampleRenderer.Free;

  CurrentSample.Region := nil;

  inherited;
end;

procedure TMiniSampleDisplayFrame.InfoDivResize(Sender: TObject);
begin
  self.UpdateControlVisibility;
end;

procedure TMiniSampleDisplayFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
begin
  SampleInfoBox.Height := 25;
  InfoDiv.Align := alClient;

  Plugin := aPlugin;
  GuiStandard := aGuiStandard;

  SampleDisplayMenu.Initialize(aPlugin);

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


  //== finally, call the message handlers to ensure everything is up to date ===
  UpdateControlVisibility;
  UpdateModulation;
end;

procedure TMiniSampleDisplayFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
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
  xSampleImage : IInterfacedBitmap;
  Par:TSampleRenderParameters;
begin
  CurrentSample.Region := Region;

  if (assigned(Region)) then
  begin
    if Region.GetSample^.Properties.IsValid then
    begin
      Par.BackgroundColor := kColor_LcdDark1;
      Par.LineColor       := kColor_SampleDisplayLine;
      Par.ImageWidth      := kSampleImageWidth;
      Par.ImageHeight     := kSampleImageHeight;
      Par.Zoom            := 0;
      Par.Offset          := 0;
      Par.VertGain        := DecibelsToLinear(Region.GetProperties^.SampleVolume);

      xSampleImage := SampleRenderer.RenderSample(Par, Region, Region.GetPeakBuffer);
      SampleDisplay.DrawSample(xSampleImage);




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
    begin
      SampleDisplay.DrawSample(Region.GetSampleImage);
    end;
  end else
  begin
    SampleDisplay.ClearSample(true);
  end;
end;

procedure TMiniSampleDisplayFrame.InternalUpdateSampleInfo(const Region: IRegion; const NoRegionMessage: string);
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
      SampleOverlay.NoSampleMessage := NoRegionMessage;
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

    // TODO: when loading from cubase we also have access to the sample
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
    // TODO: This code requires a few changes.
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
        // TODO: Need to show a message here to say that
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
        // TODO: Need to show a message here to say that
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
  UpdateSampleDisplayInfo;
end;

procedure TMiniSampleDisplayFrame.SampleOverlayDblClicked(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  if Plugin.Globals.GuiState.IsSampleMapVisible
    then Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_HideSampleMapEdit)
    else Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_ShowSampleMapEdit);

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
    SampleDisplayMenu.LoopPointsVisible := fSampleOverlay.ShowLoopPoints;

    MouseDownSamplePos := SampleOverlay.PixelPosToSamplePos(x, CurrentSample.Info.SampleFrames);
    SampleDisplayMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, MouseDownSamplePos);
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
  //SampleDisplay.Zoom   := Zoom;
  //SampleDisplay.Offset := Zoom;
  //SampleOverlay.SetZoomOffset(Zoom, Offset);

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
  ParValue : single;
begin
  SampleVolumeKnob.Align := alNone;
  SamplePanKnob.Align := alNone;
  SampleFineKnob.Align := alNone;
  SampleTuneKnob.Align := alNone;
  SampleBeatsKnob.Align := alNone;

  ParName := PluginParToName(TPluginParameter.PitchTracking);
  ParValue := Plugin.GetPluginParameter(ParName);
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
  //
end;

procedure TMiniSampleDisplayFrame.UpdateModulation;
var
  ModSlot : integer;
  ModAmount : single;
  ModMin, ModMax : single;
  kg : IKeyGroup;
begin
  if Plugin.Globals.IsMouseOverModSlot
    then ModSlot := Plugin.Globals.MouseOverModSlot
    else ModSlot := Plugin.Globals.SelectedModSlot;

  kg := Plugin.ActiveKeyGroup;
  if not assigned(kg) then exit;

  //== Update min-max modulation amounts ==
  kg.GetModParModMinMax(TModParIndex.SampleStart, ModMin, ModMax);
  SampleOverlay.SampleStartModMin := ModMin;
  SampleOverlay.SampleStartModMax := ModMax;

  kg.GetModParModMinMax(TModParIndex.SampleEnd, ModMin, ModMax);
  SampleOverlay.SampleEndModMin := ModMin;
  SampleOverlay.SampleEndModMax := ModMax;

  kg.GetModParModMinMax(TModParIndex.LoopStart, ModMin, ModMax);
  SampleOverlay.LoopStartModMin := ModMin;
  SampleOverlay.LoopStartModMax := ModMax;

  kg.GetModParModMinMax(TModParIndex.LoopEnd, ModMin, ModMax);
  SampleOverlay.LoopEndModMin := ModMin;
  SampleOverlay.LoopEndModMax := ModMax;

  SampleOverlay.IsModEditActive := false;
  SampleOverlay.ShowModPoints := false;

  if ModSlot <> -1 then
  begin
    ModAmount := kg.GetModulatedParameters^[TModParIndex.SampleStart].ModAmount[ModSlot];
    SampleOverlay.SampleStartMod := ModAmount;

    ModAmount := kg.GetModulatedParameters^[TModParIndex.SampleEnd].ModAmount[ModSlot];
    SampleOverlay.SampleEndMod := ModAmount;

    ModAmount := kg.GetModulatedParameters^[TModParIndex.LoopStart].ModAmount[ModSlot];
    SampleOverlay.LoopStartMod := ModAmount;

    ModAmount := kg.GetModulatedParameters^[TModParIndex.LoopEnd].ModAmount[ModSlot];
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

    Index     := TModParIndex.SampleStart;
    ModAmount := SampleOverlay.SampleStartMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);

    Index     := TModParIndex.SampleEnd;
    ModAmount := SampleOverlay.SampleEndMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);

    Index     := TModParIndex.LoopStart;
    ModAmount := SampleOverlay.LoopStartMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);

    Index     := TModParIndex.LoopEnd;
    ModAmount := SampleOverlay.LoopEndMod;
    kg.SetModParModAmount(Index, ModSlot, ModAmount);
  end;

end;

procedure TMiniSampleDisplayFrame.SampleOverlayMouseOverMarkerChanged(Sender: TObject);
var
  Marker : TSampleMarker;
  ActiveModParIndex : integer;
begin
  Marker := (Sender as TLuciditySampleOverlay).MouseOverMarker;

  case Marker  of
    smNone:                  ActiveModParIndex := -1;
    smSampleStartMarker:     ActiveModParIndex := TModParIndex.SampleStart;
    smSampleEndMarker:       ActiveModParIndex := TModParIndex.SampleEnd;
    smLoopStartMarker:       ActiveModParIndex := TModParIndex.LoopStart;
    smLoopEndMarker:         ActiveModParIndex := TModParIndex.LoopEnd;
    smSampleStartModMarker:  ActiveModParIndex := TModParIndex.SampleStart;
    smSampleEndModMarker:    ActiveModParIndex := TModParIndex.SampleEnd;
    smLoopStartModMarker:    ActiveModParIndex := TModParIndex.LoopStart;
    smLoopEndModMarker:      ActiveModParIndex := TModParIndex.LoopEnd;
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ActiveModParIndexChanged, @ActiveModParIndex);
end;










end.
