unit uMiniSampleDisplayFrame;

interface

uses
  VamVisibleControl,
  Lucidity.SampleMap, uLucidityKeyGroupInterface, Menu.SampleDisplayMenu,
  eePlugin, eeGuiStandard, uGuiFeedbackData, LuciditySampleOverlay,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer,
  RedFoxWinControl, VamWinControl, VamPanel, VamSampleDisplay, Vcl.Menus,
  RedFoxGraphicControl, VamGraphicControl, VamLabel, VamDiv, VamCompoundLabel,
  VamCompoundNumericKnob;

type
  TMiniSampleDisplayFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    SampleDisplay: TVamSampleDisplay;
    SampleInfoBox: TVamDiv;
    InfoDiv: TVamDiv;
    SampleBeatsKnob: TVamCompoundNumericKnob;
    SamplePitchKnob: TVamCompoundNumericKnob;
    SampleVolumeKnob: TVamCompoundNumericKnob;
    SamplePanKnob: TVamCompoundNumericKnob;
    InsidePanel: TVamPanel;
    SampleNameLabel: TVamLabel;
    procedure SampleKnobChanged(Sender: TObject);
    procedure SampleDisplayResize(Sender: TObject);
    procedure InfoDivResize(Sender: TObject);
    procedure InsidePanelResize(Sender: TObject);
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;

    MsgHandle : hwnd;
    procedure MessageHandler(var Message : TMessage);
    procedure UpdateControlVisibility;

    procedure SetGuiStandard(const Value: TGuiStandard);
    procedure SetPlugin(const Value: TeePlugin);
  protected
    Zoom, Offset : single;

    fSampleOverlay : TLuciditySampleOverlay;
    SampleInfo : TSampleDisplayInfo;
    SampleOverlayClickPos : TPoint;
    SampleDisplayMenu : TSampleDisplayMenu;

    StoredImage : ISampleImageBuffer;

    procedure InternalUpdateSampleDisplay(const Region : IRegion; const NoRegionMessage : string);
    procedure InternalUpdateSampleInfo(const Region : IRegion; const NoRegionMessage : string);

    procedure UpdateSampleDisplayInfo;

    procedure SampleOverlayDblClicked(Sender : TObject);

    procedure SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleOverlayZoomChanged(Sender : TObject; aZoom, aOffset : single);

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
  eeVstXml, eeVstParameter, uLucidityEnums,
  GuidEx, RedFoxColor, uLucidityExtra,
  uGuiUtils, VamLayoutWizard,
  uConstants, uLucidityKeyGroup;

{$R *.dfm}

{ TMiniSampleDisplayFrame }

constructor TMiniSampleDisplayFrame.Create(AOwner: TComponent);
begin
  inherited;

  MsgHandle := AllocateHWND(MessageHandler);

  Zoom   := 0;
  Offset := 0;

  fSampleOverlay := TLuciditySampleOverlay.Create(AOwner);
  fSampleOverlay.Parent  := SampleDisplay;
  fSampleOverlay.Align := alClient;
  fSampleOverlay.Visible := true;

  fSampleOverlay.OnMouseDown := SampleOverlayMouseDown;
  fSampleOverlay.OnSampleMarkerChanged := SampleMarkerChanged;
  fSampleOverlay.OnZoomChanged := SampleOverlayZoomChanged;

  fSampleOverlay.OnOleDragDrop  := SampleDisplayOleDragDrop;
  fSampleOverlay.OnOleDragOver  := SampleDisplayOleDragOver;
  fSampleOverlay.OnOleDragLeave := SampleDisplayOleDragLeave;

  fSampleOverlay.OnDblClick := SampleOverlayDblClicked;

  SampleInfo.IsValid := false;

  SampleDisplayMenu := TSampleDisplayMenu.Create;


  StoredImage := TSampleImageBuffer.Create;
end;

destructor TMiniSampleDisplayFrame.Destroy;
begin
  if (MsgHandle <> 0) and (assigned(Plugin)) then
  begin
    Plugin.Globals.RemoveWindowsMessageListener(MsgHandle);
  end;
  DeallocateHWnd(MsgHandle);

  SampleDisplayMenu.Free;
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


  if MsgHandle <> 0 then Plugin.Globals.AddWindowsMessageListener(MsgHandle);
  UpdateControlVisibility;


  SampleDisplayMenu.Initialize(aPlugin);

  SampleNameLabel.Font.Color := GetRedFoxColor(kColor_LcdDark5);

  SampleVolumeKnob.Color_Label   := GetRedFoxColor(kColor_LcdDark4);
  SampleVolumeKnob.Color_Numeric := GetRedFoxColor(kColor_LcdDark5);

  SamplePanKnob.Color_Label      := GetRedFoxColor(kColor_LcdDark4);
  SamplePanKnob.Color_Numeric    := GetRedFoxColor(kColor_LcdDark5);

  SamplePitchKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  SamplePitchKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);

  SampleBeatsKnob.Color_Label    := GetRedFoxColor(kColor_LcdDark4);
  SampleBeatsKnob.Color_Numeric  := GetRedFoxColor(kColor_LcdDark5);

  SampleDisplay.LineColor        := kColor_SampleDisplayLine;

  SampleOverlay.Font.Color := GetTColor(kColor_LcdDark5);
  SampleOverlay.ShowMarkerTags := true;

  SampleVolumeKnob.Width := 90;
  SamplePanKnob.Width    := 75;
  SamplePitchKnob.Width  := 85;
  SampleBeatsKnob.Width  := 85;

  SampleVolumeKnob.Margins.SetBounds(0,0,20,0);
  SamplePanKnob.Margins.SetBounds(0,0,20,0);


  StoredImage.GetObject.Resize(kSampleImageWidth, kSampleImageHeight);
end;

procedure TMiniSampleDisplayFrame.MessageHandler(var Message: TMessage);
begin
  if Message.Msg = UM_Update_Control_Visibility then UpdateControlVisibility;
  if Message.Msg = UM_SAMPLE_OSC_TYPE_CHANGED   then UpdateControlVisibility;
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
begin
  if (assigned(Region)) then
  begin
    if Region.GetSample^.Properties.IsValid then
    begin
      if (Region.GetSample^.Properties.SampleFrames > kSampleImageWidth) then
      begin
        //Update the stored image.
        StoredImage.GetObject.LineColor := SampleDisplay.LineColor;
        StoredImage.GetObject.DrawSample(Region.GetPeakBuffer);

        // draw the stored image on the control.
        SampleDisplay.DrawSample(StoredImage);
      end else
      begin
        // Draw pre-rendered sample.
        SampleDisplay.DrawSample(Region.GetSampleImage);
      end;





      {
      // Provide sample info for real-time drawing of sample.
      aZoom   := Zoom;
      aOffset := Offset;

      SampleOverlay.SetZoomOffset(aZoom, aOffset);
      SampleDisplay.Zoom := aZoom;
      SampleDisplay.Offset := aOffset;


      SampleInfo.IsValid := true;
      SampleInfo.ChannelCount := Region.GetSample^.Properties.ChannelCount;
      SampleInfo.SampleFrames := Region.GetSample^.Properties.SampleFrames;

      if Region.GetSample^.Properties.ChannelCount = 1 then
      begin
        SampleInfo.Ch1 := Region.GetSample^.Properties.Ch1;
      end;

      if Region.GetSample^.Properties.ChannelCount = 2 then
      begin
        SampleInfo.Ch1 := Region.GetSample^.Properties.Ch1;
        SampleInfo.Ch2 := Region.GetSample^.Properties.Ch2;
      end;

      SampleDisplay.DrawSample(SampleInfo);
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
  aZoom, aOffset : single;
begin
  if (assigned(Region)) then
  begin
    if Region.GetSample^.Properties.IsValid then
    begin
      SampleInfo.IsValid := true;
      SampleInfo.ChannelCount := Region.GetSample^.Properties.ChannelCount;
      SampleInfo.SampleFrames := Region.GetSample^.Properties.SampleFrames;

      //== Sample Overlay ==
      fSampleOverlay.SetSampleInfo(true, SampleInfo.SampleFrames);
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
    SamplePitchKnob.KnobValue  := px^.SamplePitch;
    SampleBeatsKnob.KnobValue  := px^.SampleBeats;

    InfoDiv.Visible := true;
  end else
  begin
    SampleInfo.IsValid := false;
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
    SamplePitchKnob.KnobValue  := 0;
    SampleBeatsKnob.KnobValue  := 4;
  end;


end;



procedure TMiniSampleDisplayFrame.SampleDisplayOleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
var
  RegionCreateInfo : TRegionCreateInfo;
  kg : IKeyGroup;
  aRegion : IRegion;
  CurRegion : IRegion;
  SG : IKeyGroup;
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
    Plugin.Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
    exit; //====================================================exit======>>
  end;



  // Asume file is an audio sample and attempt to load. TODO: should also check if file is supported audio format here...
  if (fn <> '') then
  begin
    // TODO: This code requires a few changes.
    // - current region should only be deleted if new sample is loaded successfully.
    // - new region bounds should duplicate the old region bounds if it is replacing an older region.
    // - need some sort of response if the sample doesn't load.

    SG := Plugin.FocusedKeyGroup;
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

    Plugin.Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);




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

  if Plugin.GuiState.IsSampleMapVisible
    then Plugin.Globals.SendWindowsMessage(UM_HIDE_SAMPLE_MAP_EDIT)
    else Plugin.Globals.SendWindowsMessage(UM_SHOW_SAMPLE_MAP_EDIT);

  //Plugin.Globals.SendGuiMessage(UM_SHOW_LOOP_EDIT_FRAME);
end;

procedure TMiniSampleDisplayFrame.SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurRegion : IRegion;
begin
  CurRegion := Plugin.FocusedRegion;
  if CurRegion = nil then exit;

  if (Button = mbRight) then
  begin
    //SampleOverlayClickPos := Point(X, Y);
    SampleDisplayMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
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

  Plugin.Globals.SendWindowsMessage(UM_SAMPLE_MARKERS_CHANGED);
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
      3: CurRegion.GetProperties^.SamplePitch  := KnobValue;
      4: CurRegion.GetProperties^.SampleBeats  := round(KnobValue);
    else
      raise Exception.Create('Unexpected tag value.');
    end;
  end;



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
  Par : TVstParameter;
  Ref, Target : TControl;
begin
  Par := Plugin.Globals.VstParameters.Par('SamplePlaybackType');
  case TSamplePlaybackTypeHelper.ToEnum(Par.ValueVST) of
    TSamplePlaybackType.NoteSampler:
    begin
      SamplePitchKnob.Visible := true;
      SampleBeatsKnob.Visible := false;
    end;

    TSamplePlaybackType.LoopSampler:
    begin
      SamplePitchKnob.Visible := false;
      SampleBeatsKnob.Visible := true;
    end;

    TSamplePlaybackType.OneShotSampler:
    begin
      SamplePitchKnob.Visible := true;
      SampleBeatsKnob.Visible := false;
    end;
    //TSamplePlaybackType.GrainStretch
    //TSamplePlaybackType.WaveOsc;:
  else
    raise Exception.Create('Type not handled.');
  end;


  if SampleBeatsKnob.Visible
    then Target := SampleBeatsKnob
    else Target := SamplePitchKnob;

  Target.Align := alNone;
  Target.Top   := 0;
  Target.Left  := Target.Parent.Width - Target.Width;

  ref := Target;
  SamplePanKnob.Align := alNone;
  SamplePanKnob.Layout.Anchor(ref).SnapToEdge(TControlFeature.LeftEdge).Move(-8,0);

  ref := SamplePanKnob;
  SampleVolumeKnob.Align := alNone;
  SampleVolumeKnob.Layout.Anchor(ref).SnapToEdge(TControlFeature.LeftEdge).Move(-8,0);
end;


procedure TMiniSampleDisplayFrame.InsidePanelResize(Sender: TObject);
begin
  //
end;








end.
