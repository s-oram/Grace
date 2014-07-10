unit uConstants;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.MoreTypes,
  Lucidity.PluginParameters,
  SysUtils,
  eeEnumHelper,
  uLucidityEnums,
  Messages, eePluginSettings;

const
  kCompany = 'One Small Clue';
  kProduct = 'Lucidity';
  kProductVersion = '1.0';

const
  kKeyFileName = 'LucidityKey.dat';

var
  AppDataDir   : string; //As specified by windows.
  VstPluginDir : string; //Location of the VST plugin dll.
  PresetsDir   : string; //Location of preset files...


  function LucidMsgIDToStr(const ID : cardinal):string;

type
  TLucidMsgID = record
  const
    //----- Gui Messages---------------
    SampleFocusChanged               = 1; //sent when the region or sample group focus changes..
    SampleRegionChanged              = 2; //sent when something about a region has changed. ie. the region has moved.
    MouseOverSampleRegionChanged     = 3;
    MidiKeyChanged                   = 4;
    PreviewInfoChanged               = 5;
    SampleMarkersChanged             = 6;
    SampleOscTypeChanged             = 7;
    LoopTypeChanged                  = 8;
    SampleDirectoriesChanged         = 9;
    FilterChanged                    = 10;
    ModSlotChanged                   = 11;
    LfoChanged                       = 12;
    ModAmountChanged                 = 13;
    GUILayoutChanged                 = 14;
    VstParameterChanged              = 15;
    Command                          = VstParameterChanged + 1;
    Command_ShowSampleMapEdit        = Command + 1;  // Maybe this commands can be deleted?
    Command_HideSampleMapEdit        = Command + 2;  // Maybe this commands can be deleted?
    Command_ShowAboutDialog          = Command + 3;
    //Command_ShowLoopEditFrame        = Command + 4;
    Command_CloseCurrentDialog       = Command + 5;
    Command_UpdateControlVisibility  = Command + 6; //something has changed, check to see if any controls need to be visible/invisible.
    Command_UpdateModMatrix          = Command + 7;
    Command_ShowReplaceRegionMessage = Command + 8;
    Command_HideReplaceRegionMessage = Command + 9;
    Command_UpdateSampleDisplay      = Command + 10;
    Command_UpdateSampleInfo         = Command + 11;
    Command_UpdateScope              = Command + 12;
    Command_DisposeKeyGroup          = Command + 13;
    Command_UpdateGUI                = Command + 14; // All GUI elements should be updated.
    Command_BeginGuiUpdate           = Command + 15;
    Command_EndGuiUpdate             = Command + 16;
    Command_Sample_ZoomIn            = Command + 17;
    Command_Sample_ZoomOut           = Command + 18;
    Command_Sample_ZoomOutFull       = Command + 19;
    Command_ShowParChangeInfo        = Command + 20;
    Command_HideParChangeInfo        = Command + 21;
    Actions                          = Command_HideParChangeInfo + 1;
    ProgramSavedToDisk               = Actions + 1;
    ProgramLoaded                    = Actions + 2;
    RefreshRequest_StepSeqDisplay    = Actions + 3;
    Msg_XRegionsDuplicated           = Actions + 4;
    OnControlEnter                   = Actions + 5; //TODO: deprecated. replace by OnParControlEnter
    OnControlLeave                   = Actions + 6; //TODO: deprecated. replace by OnParControlLeave
    OnParControlEnter                = Actions + 7;
    OnParControlLeave                = Actions + 8;
    OnParControlChanged              = Actions + 9;
    OnActiveParameterChanged         = Actions + 10; // The active GUI parameter has changed. The active GUI parameter is the parameter that the mouse is currently over or otherwise focused on somehow.
    OnPostCreateFinished             = Actions + 11;
    OnShowMenu                       = Actions + 12;  //TMsgData_ShowMenu
    //----- Audio Messages---------------
    AudioMsg                          = OnShowMenu + 1;
    Audio_VoiceTriggered              = AudioMsg + 1;
    Audio_VoiceFinished               = AudioMsg + 2;
    Audio_KeyGroupInactive            = AudioMsg + 3;
    Audio_PolyNoteTrigger             = AudioMsg + 4;
    Audio_PolyNoteRelease             = AudioMsg + 5;
    Audio_MonoNoteTrigger             = AudioMsg + 6;
    Audio_MonoNoteRelease             = AudioMsg + 7;
    Audio_LegatoNoteTrigger           = AudioMsg + 8;
    Audio_LegatoNoteRelease           = AudioMsg + 9;
    Audio_LatchNoteTrigger            = AudioMsg + 10;
    Audio_LatchNoteRelease            = AudioMsg + 11;
    AudioCommand_QuickReleaseAllNotes = AudioMsg + 12;
  end;

  PMsgData_NoteEvent = ^TMsgData_NoteEvent;
  TMsgData_NoteEvent = record
    Data1 : byte; // normally a MIDI Note number.
    Data2 : byte; // normally a MIDI Velocity amount.
    NoteStackCount : integer;
  end;

  PMsgData_Audio_VoiceTriggered = ^TMsgData_Audio_VoiceTriggered;
  TMsgData_Audio_VoiceTriggered = record
    KeyGroupID : Pointer;
    Voice      : Pointer;
  end;

  PMsgData_ShowMenuEvent = ^TMsgData_ShowMenu;
  TMsgData_ShowMenu = record
  public
    MenuName : string;
    Menu     : Pointer;
  end;




const
  kMaxStepSequencerLength = 32;
  kMaxVoiceCount = 64; //max number of voices per group.
  kMaxActiveVoices = 12;

  kModulatedParameterCount = 35;
  kModSlotCount = 8;


type
  //==============================================================================
  // TSampleBounds is used by the oscillator type classes to store
  // a temporary copy of the sample start/end and loop start/end data.
  // I think it will be better to use a temporary copy of the start/end
  // markers. That way the oscillators can update the marker positions
  // independently of the region properties which may update at any time.
  PSampleOsc_SampleBounds = ^TSampleOsc_SampleBounds;
  TSampleOsc_SampleBounds = record
    AbsoluteSampleStart : integer; //Should be 0.
    AbsoluteSampleEnd   : integer; //should be equal to SampleFrames-1
    SampleStart         : integer;
    SampleEnd           : integer;
    LoopStart           : integer;
    LoopEnd             : integer;
    PlaybackSampleStart : integer;
    PlaybackLoopStart   : integer;
    PlaybackEnd         : integer; //Will loop here if loop is enabled.
  end;

  PGlobalModulationPoints = ^TGlobalModulationPoints;
  TGlobalModulationPoints = record
  public
    Source_TriggeredNoteCount : cardinal;
    Source_MonophonicMidiNote : single;
    Source_MidiPitchBendST    : single; //Range is in semitones. Should it be CV?
    Source_MidiPitchbend_Unipolar : single; //range 0..1
    Source_MidiPitchbend_Bipolar  : single; //range -1..1
    Source_MidiModWheel_Unipolar  : single; //range 0..1
    Source_MidiModWheel_Bipolar   : single; //range -1..1
    //Unipolar sources...
    Source_PadX1_Unipolar     : PSingle;
    Source_PadY1_Unipolar     : PSingle;
    Source_PadX2_Unipolar     : PSingle;
    Source_PadY2_Unipolar     : PSingle;
    Source_PadX3_Unipolar     : PSingle;
    Source_PadY3_Unipolar     : PSingle;
    Source_PadX4_Unipolar     : PSingle;
    Source_PadY4_Unipolar     : PSingle;

    procedure Init;
  end;

  // Per voice modulation points. All modulation values are in audio range.
  PVoiceModulationPoints = ^TVoiceModulationPoints;
  TVoiceModulationPoints = record
    // Modulation Input Points
    MidiNote_Unipolar       : single; //MIDI note. 0 = c2.
    MidiNote_Bipolar        : single; //MIDI note. 0 = c2.
    MidiVelocity_Unipolar   : single; // range 0..1
    MidiVelocity_Bipolar    : single;
    MidiToggle_Unipolar     : single; // range 0 or 1.
    MidiToggle_Bipolar      : single; // range -1 or 1.
    KeyFollowFreqMultiplier : single; // TODO:HIGH Maybe this isn't used anywhere.
    SampleStart             : single;
    SampleEnd               : single;
    LoopStart               : single;
    LoopEnd                 : single;

    // Modulation Output Points
    OutputAmplitudeLevel : single;
  end;

  PFilePreviewInfo = ^TFilePreviewInfo;
  TFilePreviewInfo = record
    FileName     : string;
    IsSupported  : boolean;
    IsValid      : boolean;
    SampleTime   : string; //Time in textual form.
    SampleRate   : string;
    BitDepth     : string;
    ChannelCount : string;
    procedure Clear;
  end;






//==============================================================================
//   GUI Skin Constants
//==============================================================================

const
  kColor_LcdDark1 = '$ff242B39';
  kColor_LcdDark2 = '$ff313B4F';
  kColor_LcdDark3 = '$ff5D7095';
  kColor_LcdDark4 = '$ff8792A9';
  kColor_LcdDark5 = '$ffA1BDED';
  kColor_LcdDark6 = '$ffD3E0F7';

  kColor_LcdInvert1 = '$ffC1D3F3';

  kColor_ButtonMouseOver = '$ff404A5E';

  kColor_ToggleButtonOn           = '$FFC1D3F3';
  kColor_ToggleButtonOnMouseOver  = '$FFD3E0F7';
  kColor_ToggleButtonOff          = '$FFF1CBB9';
  kColor_ToggleButtonOffMouseOver = '$FFFAECE6';
  kColor_ToggleButtonBorder       = '$FF242B39';



  kModLineColorOff = '$ffA5A5A5';

  //kModLineColorA = '$ffFAFAFA';
  //kModLineColorA = '$ffDEE9FD';
  kModLineColorA = '$ffffffff';

  //kModLineColorB = '$ffFFC7D6';
  //kModLineColorB = '$ffFFA989';
  //kModLineColorB = '$ffFF7C4C';
  kModLineColorB = '$ffFFB264';

  kArrowColor1 = '$33FFFFFF';
  kArrowColor2 = '$ccFFFFFF';







  //=== grays for normal ===
  kPanelVeryDark = '$ff3B3B3B';
  kPanelDark  = '$ff939393';
  kPanelMid   = '$ffAEAEAE';
  kPanelLight = '$ffCCCCCA';

  //=== Greens for testing ====
  //kPanelVeryDark = '$ff3B3B3B';
  //kPanelDark     = '$ff92B420';
  //kPanelMid      = '$ffBCDF49';
  //kPanelLight   = '$ffBCDF49';

const
  kColor_SampleDisplayLine = kColor_LcdDark4;
  kSampleImageWidth  = 592; //used for the pre-rendering of sample images.
  //kSampleImageHeight = 77;  //used for the pre-rendering of sample images.
  kSampleImageHeight = 167;  //used for the pre-rendering of sample images.

type
  TGuiConst = class
  public const
    KnobWidth            : integer = 40;
    KnobHeight           : integer = 40;
    KnobLabelHeight      : integer = 18;
    SectionLabelHeight   : integer = 18;
    SelectorButtonHeight : integer = 18;
  end;


const
  dcVstMenuButton = '1'; //Menu buttons linked to VST parameters use this display class.
  dcGUIMenuButton = '2'; //Menu buttons with all their functionality handed by the GUI use this display class.


const
  kLucidityProgramFileExtension = '.lpg';



//==============================================================================
type
  TLucidityException = Exception;




//==============================================================================

function PluginInfo:TeePluginSettings;

//==============================================================================

type
  TParName = record
  const
    VoiceMode              = 'VoiceMode';
    VoiceGlide             = 'VoiceGlide';
    PitchTracking          = 'PitchTracking';
    SamplePlaybackType     = 'SamplePlaybackType';
    SampleResetClockSource = 'SampleResetClockSource';
    SamplerLoopBounds      = 'SamplerLoopBounds';
    SamplerLoopMode        = 'SamplerLoopMode';
    GrainLoop              = 'GrainLoop';
    GrainLength            = 'GrainLength';
    GrainRate              = 'GrainRate';
    GrainPosition          = 'GrainPosition';
    OutputGain             = 'OutputGain';
    OutputPan              = 'OutputPan';
    VoicePitchOne          = 'VoicePitchOne';
    VoicePitchTwo          = 'VoicePitchTwo';
    AuxALevel              = 'AuxALevel';
    AuxBLevel              = 'AuxBLevel';
    OscShape               = 'OscShape';
    OscPulseWidth          = 'OscPulseWidth';
    NoiseLevel             = 'NoiseLevel';
    SampleStart            = 'SampleStart';
    SampleEnd              = 'SampleEnd';
    LoopStart              = 'LoopStart';
    LoopEnd                = 'LoopEnd';
    AmpAttack              = 'AmpAttack';
    AmpHold                = 'AmpHold';
    AmpDecay               = 'AmpDecay';
    AmpSustain             = 'AmpSustain';
    AmpRelease             = 'AmpRelease';
    AmpVelocity            = 'AmpVelocity';
    FilterAttack           = 'FilterAttack';
    FilterHold             = 'FilterHold';
    FilterDecay            = 'FilterDecay';
    FilterSustain          = 'FilterSustain';
    FilterRelease          = 'FilterRelease';
    FilterVelocity         = 'FilterVelocity';
    FilterRouting          = 'FilterRouting';
    FilterOutputBlend      = 'FilterOutputBlend';
    Filter1Type            = 'Filter1Type';
    Filter2Type            = 'Filter2Type';
    Filter1KeyFollow       = 'Filter1KeyFollow';
    Filter2KeyFollow       = 'Filter2KeyFollow';
    Filter1Par1            = 'Filter1Par1';
    Filter1Par2            = 'Filter1Par2';
    Filter1Par3            = 'Filter1Par3';
    Filter1Par4            = 'Filter1Par4';
    Filter2Par1            = 'Filter2Par1';
    Filter2Par2            = 'Filter2Par2';
    Filter2Par3            = 'Filter2Par3';
    Filter2Par4            = 'Filter2Par4';
    Lfo1Shape              = 'Lfo1Shape';
    Lfo1FreqMode           = 'Lfo1FreqMode';
    Lfo1Range              = 'Lfo1Range';
    Lfo1Par1               = 'Lfo1Par1';
    Lfo1Par2               = 'Lfo1Par2';
    Lfo1Par3               = 'Lfo1Par3';
    Lfo2Shape              = 'Lfo2Shape';
    Lfo2FreqMode           = 'Lfo2FreqMode';
    Lfo2Range              = 'Lfo2Range';
    Lfo2Par1               = 'Lfo2Par1';
    Lfo2Par2               = 'Lfo2Par2';
    Lfo2Par3               = 'Lfo2Par3';
    Seq1Clock              = 'Seq1Clock';
    Seq1Direction          = 'Seq1Direction';
    Seq1Length             = 'Seq1Length';
    Seq2Clock              = 'Seq2Clock';
    Seq2Direction          = 'Seq2Direction';
    Seq2Length             = 'Seq2Length';
    PreviewVolume          = 'PreviewVolume';
    Preview                = 'Preview';
  end;

const
  //TODO: is this still being used?
  LfoControlDisplayClass = 'LfoControl';





implementation

uses
  RTTI, TypInfo, Math, uDataFolderUtils, eeVstExtra;

var
  Global_Info:TeePluginSettings;

function PluginInfo:TeePluginSettings;
begin
  if not assigned(Global_Info) then
  begin
    Global_Info := TeePluginSettings.Create;

    //Set plugin information here.
    Global_Info.PluginVendor       := kCompany;
    Global_Info.PluginName         := kProduct;
    Global_Info.PluginVersion      := kProductVersion;
    Global_Info.VstUniqueId        := '52Tw';

    Global_Info.NumberOfPrograms   := 1;
    Global_Info.UseHostGui         := false;

    Global_Info.InitialGuiWidth    := 980;
    Global_Info.InitialGuiHeight   := 654;

    Global_Info.IsSynth            := true;
    Global_Info.PresetsAreChunks   := true;
    Global_Info.SoftBypass         := false;

    Global_Info.InitialInputCount  := 0;
    Global_Info.InitialOutputCount := 2;

    Global_Info.IsOverSamplingEnabled   := true;
    Global_Info.OverSampleFactor        := 2;
    Global_Info.FastControlRateDivision := 8;
    Global_Info.SlowControlRateDivision := 64;
  end;

  result := Global_Info;
end;



function LucidMsgIDToStr(const ID : cardinal):string;
begin
  if ID = TLucidMsgID.SampleFocusChanged                 then exit('SampleFocusChanged');
  if ID = TLucidMsgID.SampleRegionChanged                then exit('SampleRegionChanged');
  if ID = TLucidMsgID.MouseOverSampleRegionChanged       then exit('MouseOverSampleRegionChanged');
  if ID = TLucidMsgID.MidiKeyChanged                     then exit('MidiKeyChanged');
  if ID = TLucidMsgID.PreviewInfoChanged                 then exit('PreviewInfoChanged');
  if ID = TLucidMsgID.SampleMarkersChanged               then exit('SampleMarkersChanged');
  if ID = TLucidMsgID.SampleOscTypeChanged               then exit('SampleOscTypeChanged');
  if ID = TLucidMsgID.LoopTypeChanged                    then exit('LoopTypeChanged');
  if ID = TLucidMsgID.SampleDirectoriesChanged           then exit('SampleDirectoriesChanged');
  if ID = TLucidMsgID.FilterChanged                      then exit('FilterChanged');
  if ID = TLucidMsgID.ModSlotChanged                     then exit('ModSlotChanged');
  if ID = TLucidMsgID.LfoChanged                         then exit('LfoChanged');
  if ID = TLucidMsgID.ModAmountChanged                   then exit('ModAmountChanged');
  if ID = TLucidMsgID.GUILayoutChanged                   then exit('GUILayoutChanged');
  if ID = TLucidMsgID.Command                            then exit('Command');
  if ID = TLucidMsgID.Command_ShowSampleMapEdit          then exit('Command_ShowSampleMapEdit');
  if ID = TLucidMsgID.Command_HideSampleMapEdit          then exit('Command_HideSampleMapEdit');
  if ID = TLucidMsgID.Command_ShowAboutDialog            then exit('Command_ShowAboutDialog');
  if ID = TLucidMsgID.Command_CloseCurrentDialog         then exit('Command_CloseCurrentDialog');
  if ID = TLucidMsgID.Command_UpdateControlVisibility    then exit('Command_UpdateControlVisibility');
  if ID = TLucidMsgID.Command_UpdateModMatrix            then exit('Command_UpdateModMatrix');
  if ID = TLucidMsgID.Command_ShowReplaceRegionMessage   then exit('Command_ShowReplaceRegionMessage');
  if ID = TLucidMsgID.Command_HideReplaceRegionMessage   then exit('Command_HideReplaceRegionMessage');
  if ID = TLucidMsgID.Command_UpdateSampleDisplay        then exit('Command_UpdateSampleDisplay');
  if ID = TLucidMsgID.Command_UpdateSampleInfo           then exit('Command_UpdateSampleInfo');
  if ID = TLucidMsgID.Command_UpdateScope                then exit('Command_UpdateScope');
  if ID = TLucidMsgID.Command_DisposeKeyGroup            then exit('Command_DisposeKeyGroup');
  if ID = TLucidMsgID.Command_UpdateGUI                  then exit('Command_UpdateGUI');
  if ID = TLucidMsgID.Command_BeginGuiUpdate             then exit('Command_BeginGuiUpdate');
  if ID = TLucidMsgID.Command_EndGuiUpdate               then exit('Command_EndGuiUpdate');
  if ID = TLucidMsgID.Command_Sample_ZoomIn              then exit('Command_Sample_ZoomIn');
  if ID = TLucidMsgID.Command_Sample_ZoomOut             then exit('Command_Sample_ZoomOut');
  if ID = TLucidMsgID.Command_Sample_ZoomOutFull         then exit('Command_Sample_ZoomOutFull');
  if ID = TLucidMsgID.Actions                            then exit('Actions');
  if ID = TLucidMsgID.ProgramSavedToDisk                 then exit('ProgramSavedToDisk');
  if ID = TLucidMsgID.ProgramLoaded                      then exit('ProgramLoaded');
  if ID = TLucidMsgID.RefreshRequest_StepSeqDisplay      then exit('RefreshRequest_StepSeqDisplay');
  if ID = TLucidMsgID.Msg_XRegionsDuplicated             then exit('Msg_XRegionsDuplicated');
  if ID = TLucidMsgID.OnControlEnter                     then exit('OnControlEnter');
  if ID = TLucidMsgID.OnControlLeave                     then exit('OnControlLeave');
  if ID = TLucidMsgID.OnParControlEnter                  then exit('OnParControlEnter');
  if ID = TLucidMsgID.OnParControlLeave                  then exit('OnParControlLeave');
  if ID = TLucidMsgID.OnParControlChanged                then exit('OnParControlChanged');
  if ID = TLucidMsgID.OnActiveParameterChanged           then exit('OnActiveParameterChanged');
  if ID = TLucidMsgID.OnPostCreateFinished               then exit('OnPostCreateFinished');
  if ID = TLucidMsgID.OnShowMenu                         then exit('OnShowMenu');
  if ID = TLucidMsgID.AudioMsg                           then exit('AudioMsg');
  if ID = TLucidMsgID.Audio_VoiceTriggered               then exit('Audio_VoiceTriggered');
  if ID = TLucidMsgID.Audio_VoiceFinished                then exit('Audio_VoiceFinished');
  if ID = TLucidMsgID.Audio_KeyGroupInactive             then exit('Audio_KeyGroupInactive');
  if ID = TLucidMsgID.Audio_PolyNoteTrigger              then exit('Audio_PolyNoteTrigger');
  if ID = TLucidMsgID.Audio_PolyNoteRelease              then exit('Audio_PolyNoteRelease');
  if ID = TLucidMsgID.Audio_MonoNoteTrigger              then exit('Audio_MonoNoteTrigger');
  if ID = TLucidMsgID.Audio_MonoNoteRelease              then exit('Audio_MonoNoteRelease');
  if ID = TLucidMsgID.Audio_LegatoNoteTrigger            then exit('Audio_LegatoNoteTrigger');
  if ID = TLucidMsgID.Audio_LegatoNoteRelease            then exit('Audio_LegatoNoteRelease');
  if ID = TLucidMsgID.Audio_LatchNoteTrigger             then exit('Audio_LatchNoteTrigger');
  if ID = TLucidMsgID.Audio_LatchNoteRelease             then exit('Audio_LatchNoteRelease');
  if ID = TLucidMsgID.AudioCommand_QuickReleaseAllNotes  then exit('AudioCommand_QuickReleaseAllNotes');

  result := 'error: unknown ID';
end;








{ TFilePreviewInfo }

procedure TFilePreviewInfo.Clear;
begin
  self.FileName     := '';
  self.IsSupported  := false;
  self.IsValid      := false;
  self.SampleTime   := '';
  self.SampleRate   := '';
  self.BitDepth     := '';
  self.ChannelCount := '';
end;




{ TGlobalModulationPoints }

procedure TGlobalModulationPoints.Init;
begin
  {
  self.Source_PadX1_Unipolar := 0;
  self.Source_PadY1_Unipolar := 0;
  self.Source_PadX2_Unipolar := 0;
  self.Source_PadY2_Unipolar := 0;
  self.Source_PadX3_Unipolar := 0;
  self.Source_PadY3_Unipolar := 0;
  self.Source_PadX4_Unipolar := 0;
  self.Source_PadY4_Unipolar := 0;

  self.Source_PadX1_Bipolar := 0;
  self.Source_PadY1_Bipolar := 0;
  self.Source_PadX2_Bipolar := 0;
  self.Source_PadY2_Bipolar := 0;
  self.Source_PadX3_Bipolar := 0;
  self.Source_PadY3_Bipolar := 0;
  self.Source_PadX4_Bipolar := 0;
  self.Source_PadY4_Bipolar := 0;
  }
end;

initialization
  {
  if TModParHelper.GetEnumTypeCount <> kModulatedParameterCount
    then raise Exception.Create('Fatal Error: kModulatedParameterCount should be ' + IntToStr(TModParHelper.GetEnumTypeCount));
  }
finalization
  if assigned(Global_Info) then Global_Info.Free;
end.
