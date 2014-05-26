unit eePlugin;

interface

{$INCLUDE Defines.inc}

{$M+}

uses
  Lucidity.MidiInputProcessor,
  Lucidity.PluginParameters,
  Lucidity.VoiceController,
  eePublishedVstParameters,
  Lucidity.PluginParameterController,
  Lucidity.Types,
  VamLib.Utils,
  OtlCommon.Utils,
  RTTI,
  GpStuff,
  GpLists,
  Generics.Defaults,
  Generics.Collections,
  OtlSync,
  eeAudioBufferUtils,
  eeAudioFilePreviewPlayerVoice,
  eeMidiInputSmoother,
  uLucidityEnums, uLucidityData,
  Math, VamLib.MoreTypes, VamKeyStateTracker,
  uConstants, uLucidityXYPads,
  SyncObjs, eePatchObject,
  eeSampleInt, eeSampleFloat,
  Classes, eePluginBase , eeMidiEvents,
  eeMidiAutomationV2,
  Effect.MidiAutomation,
  Lucidity.SampleMap, Lucidity.KeyGroup, uGuiFeedBackData, Lucidity.Interfaces,
  B2.Filter.CriticallyDampedLowpass,
  uKeyGroupManager,
  eeAudioFilePreviewPlayer,
  FilterCore.SimperSVF,
  soLevelMeter,
  soLucidityWaveOsc,
  soLucidityVoice,
  uLucidityPanner,
  soLucidityFilter,
  uLucidityVCA,
  Lucidity.Osc.GrainStretch,
  soFivePointEnvelope,
  Lucidity.Osc.OneShotSampler,
  Lucidity.Osc.OneShotSampler.SubOsc,
  soADSR,
  soFreqAnalyzer,
  soSignalRecorder,
  soFilter.BlueFilter,
  soSawSquareOsc,
  soModMatrix,
  SampleOscUtils,
  soGateEnvelope,
  uLucidityClock,
  uLucidity.Lfo,
  uLucidityCustomSampleOsc,
  soDynamicWaveTableOsc,
  soGrainStretchSubOsc,
  soLucidityVoiceParameterWrapper,
  uLucidityStepSequencer,
  Lucidity.KeyGroupPlayer,
  eeSimpleGate;

type
  TeePlugin = class(TeePluginBase)
  private
    fMidiAutomation: TMidiAutomation;
    fXYPads: TLucidityXYPads;
    fKeyGroups: TKeyGroupManager;
    fSampleMap: TSampleMap;
    fKeyStateTracker: TKeyStateTracker;
    fAudioPreviewPlayer: TAudioFilePreviewPlayer;
    fFilePreviewInfo: TFilePreviewInfo;
    fSampleDirectories: TSampleDirectories;
    fFocusedKeyGroup: IKeyGroup;
    fIsPreviewEnabled: boolean;
    fSignalRecorder  : TSignalRecorder;
    fFreqAnalyzer: TFrequencyAnalyzer;
    function GetFocusedRegion: IRegion;
    function GetFilePreviewInfo: PFilePreviewInfo;
    function GetVoiceGlide: single;
    function GetVoiceMode: TVoiceMode;
    procedure SetVoiceGlide(const Value: single);
    procedure SetVoiceMode(const Value: TVoiceMode);
    function GetPreviewVolume: single;
    procedure SetPreviewVolume(const Value: single);
    function GetFocusedKeyGroup: IKeyGroup;
  protected
    DeltaOffset     : integer;
    GlobalModPoints : TGlobalModulationPoints;

    MidiInputProcessor : TMidiInputProcessor;
    VoiceController     : TVoiceController;
    KeyGroupPlayer  : TKeyGroupPlayer;

    Voices : TArrayOfLucidityVoice;

    EmptyKeyGroup : IKeyGroup;

    procedure GetVstParameter(const Par:TVstParameter);
    procedure VstParameterChanged(const Par:TVstParameter);

    property AudioPreviewPlayer : TAudioFilePreviewPlayer read fAudioPreviewPlayer write fAudioPreviewPlayer;

    procedure EventHandle_SampleRateChanged(Sender:TObject);

    procedure PreLoadProgram;
    procedure PostLoadProgram;

    procedure Clear;

    procedure Event_MidiAutomation_Message(Sender : TObject; const MidiData1, MidiData2 : integer; const Binding : ICustomMidiBinding);
    procedure Event_MidiAutomation_NewBinding(Sender : TObject; const MidiData1, MidiData2 : integer; const Binding : ICustomMidiBinding);

    procedure PublishPluginParameterAsVstParameter(const Par : TPluginParameter);
  public
    constructor Create; override;
	  destructor Destroy; override;

    function GetPluginParameter(const ParName : string):single; override;
    procedure SetPluginParameter(const ParName : string; const ParValue : single); overload; override;
    procedure SetPluginParameter(const Scope : TParChangeScope; const KeyGroupName : string; const ParName : string; const Value : single); reintroduce; overload;

    function GetPluginParameterVstInfo(const ParName : string):TVstParameterInfo; override;

    procedure SetPluginParameterModAmount(const Scope : TParChangeScope; const ParName : string; const ModSlot : integer; const ModAmount : single);
    function GetPluginParameterModAmount(const ParName : string; const ModSlot : integer):single;
    function GetPluginParameterInfo(const ParName : string) : TPluginParameterInfo;

    procedure GetModParModMinMax(const ParName : string; out ModMin, ModMax:single);

    procedure Suspend; override;
    procedure Resume; override;

    procedure InitializeState; override;

    procedure ImportProgram(const FileName : string; ProgramFormat : TProgramFormat);
    procedure LoadProgramFromFile(const FileName : string);
    procedure SaveProgramToFile(const FileName : string);

    procedure SetPreset(var ms:TMemoryStream); override;
    procedure GetPreset(var ms:TMemoryStream); override;

    procedure ProcessMidiEvent(Event:TeeMidiEvent); override;

    procedure AudioProcess(Sampleframes:integer); override; // processes audio.
    procedure FastControlProcess; override;                 // processes fast modulation here. (high bandwidth modulation)
    procedure SlowControlProcess; override;                 // processes slow modulation here. (low bandwidth modulation
    procedure ProcessEnd; // called once the current processing block as been finished.

    property PresetName;

    property MidiAutomation:TMidiAutomation read fMidiAutomation write fMidiAutomation;

    property XYPads : TLucidityXYPads read fXYPads;

    property SampleMap : TSampleMap       read fSampleMap;
    property KeyGroups : TKeyGroupManager read fKeyGroups;

    property KeyStateTracker : TKeyStateTracker read fKeyStateTracker write fKeyStateTracker;

    property PreviewInfo : PFilePreviewInfo read GetFilePreviewInfo;
    procedure UpdateFilePreivewInfo(const fn : string);
    procedure TriggerPreview(const fn : string);
    procedure StopPreview;
    procedure ClearPreviewInfo;
    property PreviewVolume    : single  read GetPreviewVolume  write SetPreviewVolume;
    property IsPreviewEnabled : boolean read fIsPreviewEnabled write fIsPreviewEnabled;

    //== The GUI should access the current Engine/Region using these properties ===
    property FocusedKeyGroup : IKeyGroup read GetFocusedKeyGroup;
    property FocusedRegion   : IRegion   read GetFocusedRegion;
    function ActiveKeyGroup : IKeyGroup;


    procedure FocusFirstKeyGroup;
    procedure FocusKeyGroup(const aKeyGroupName : string);
    procedure FocusRegion(aRegionID : TGUID);
    procedure ClearFocus;
    procedure ClearSelected;
    procedure SelectRegion(aRegionID : TGUID);

    procedure MoveSelectedRegionsToKeyGroup(const aKeyGroupName : string);
    procedure DuplicateSelectedRegions;
    procedure DeleteSelectedRegions;
    procedure DeleteKeyGroup(const aKeyGroupName : string);

    procedure MoveRootKey(const RootKeyOffset : integer);

    procedure TriggerNoteOn(const MidiNote, MidiVelocity : integer);  //intended for use by the GUI.
    procedure TriggerNoteOff(const MidiNote, MidiVelocity : integer); //intended for use by the GUI.


    function NewRegion(CreateInfo : TRegionCreateInfo):IRegion;

    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);
    function ActiveVoiceCount : integer;

    property SampleDirectories : TSampleDirectories read fSampleDirectories;
    property SignalRecorder    : TSignalRecorder read fSignalRecorder write fSignalRecorder;
    property FreqAnalyzer      : TFrequencyAnalyzer read fFreqAnalyzer;

  published
    // Global parameters. These properties are for the benefit of the statemanager.
    property VoiceMode  : TVoiceMode read GetVoiceMode    write SetVoiceMode;
    property VoiceGlide : single     read GetVoiceGlide   write SetVoiceGlide;
  end;




implementation

uses
  VamGuiControlInterfaces,
  VamLib.ZeroObject,
  MadExcept, Windows,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  VamLib.LoggingProxy,
  eeCustomGlobals,
  uLucidityExtra,
  LucidityParameterScaling,
  eeProfilerV2,
  eePluginDataDir, eePatchObject_XmlWrapper,
  SysUtils, NativeXML, uAutoFree, eeFunctions, eeDsp,
  Dialogs, uLucidityStateManager, Lucidity.Globals, Lucidity.StateHelpers,
  AudioIO,
  eeSaveLoadFunctions;

const
  kCurrentPatchFormat  = 'Lucidity';
  kCurrentPatchVersion = 1;


{ TeePlugin }

constructor TeePlugin.Create;
var
  //DefaultMidiMapFileName : string; //TODO:
  c1 : integer;
  DataFileName : string;
  DataDir : string;
  fn : string;
  fnA, fnB : string;
  IsDefaultPatchLoaded : boolean;
begin
  inherited;


  PublishPluginParameterAsVstParameter(TPluginParameter.VoiceMode);
  PublishPluginParameterAsVstParameter(TPluginParameter.VoiceGlide);
  PublishPluginParameterAsVstParameter(TPluginParameter.PitchTracking);
  //PublishPluginParameterAsVstParameter(TPluginParameter.SamplePlaybackType);
  PublishPluginParameterAsVstParameter(TPluginParameter.SampleResetClockSource);
  PublishPluginParameterAsVstParameter(TPluginParameter.SamplerLoopBounds);
  PublishPluginParameterAsVstParameter(TPluginParameter.SamplerLoopMode);
  PublishPluginParameterAsVstParameter(TPluginParameter.OutputGain);
  PublishPluginParameterAsVstParameter(TPluginParameter.OutputPan);
  PublishPluginParameterAsVstParameter(TPluginParameter.VoicePitchOne);
  PublishPluginParameterAsVstParameter(TPluginParameter.VoicePitchTwo);
  //PublishPluginParameterAsVstParameter(TPluginParameter.SampleStart);
  //PublishPluginParameterAsVstParameter(TPluginParameter.SampleEnd);
  //PublishPluginParameterAsVstParameter(TPluginParameter.LoopStart);
  //PublishPluginParameterAsVstParameter(TPluginParameter.LoopEnd);
  PublishPluginParameterAsVstParameter(TPluginParameter.AmpAttack);
  PublishPluginParameterAsVstParameter(TPluginParameter.AmpHold);
  PublishPluginParameterAsVstParameter(TPluginParameter.AmpDecay);
  PublishPluginParameterAsVstParameter(TPluginParameter.AmpSustain);
  PublishPluginParameterAsVstParameter(TPluginParameter.AmpRelease);
  PublishPluginParameterAsVstParameter(TPluginParameter.AmpVelocity);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterAttack);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterHold);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterDecay);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterSustain);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterRelease);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterVelocity);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterRouting);
  PublishPluginParameterAsVstParameter(TPluginParameter.FilterOutputBlend);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter1Type);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter2Type);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter1KeyFollow);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter2KeyFollow);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter1Par1);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter1Par2);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter1Par3);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter1Par4);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter2Par1);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter2Par2);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter2Par3);
  PublishPluginParameterAsVstParameter(TPluginParameter.Filter2Par4);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo1Shape);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo2Shape);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo1FreqMode);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo2FreqMode);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo1Range);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo2Range);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo1Par1);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo1Par2);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo1Par3);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo2Par1);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo2Par2);
  PublishPluginParameterAsVstParameter(TPluginParameter.Lfo2Par3);
  PublishPluginParameterAsVstParameter(TPluginParameter.Seq1Clock);
  PublishPluginParameterAsVstParameter(TPluginParameter.Seq1Direction);
  PublishPluginParameterAsVstParameter(TPluginParameter.Seq1Length);
  PublishPluginParameterAsVstParameter(TPluginParameter.Seq2Clock);
  PublishPluginParameterAsVstParameter(TPluginParameter.Seq2Direction);
  PublishPluginParameterAsVstParameter(TPluginParameter.Seq2Length);
  PublishPluginParameterAsVstParameter(TPluginParameter.PreviewVolume);
  PublishPluginParameterAsVstParameter(TPluginParameter.Preview);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadX1);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadY1);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadX2);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadY2);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadX3);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadY3);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadX4);
  PublishPluginParameterAsVstParameter(TPluginParameter.PadY4);



  GlobalModPoints.Source_TriggeredNoteCount := 0;

  fIsPreviewEnabled := true;

  Globals.AddEventListener(TPluginEvent.SampleRateChanged, EventHandle_SampleRateChanged);

  TProfiler.Open;



  {$IFDEF Logging}
  if (PluginDataDir^.Exists)
    then LogMain.LogText('Data Directory Found', PluginDataDir^.Path)
    else LogMain.LogText('Data Directory NOT Found!', '');
  {$ENDIF}


  //========= Create the Voices =================
  SetLength(Voices, kMaxVoiceCount);
  for c1 := 0 to kMaxVoiceCount-1 do
  begin
    Voices[c1] := TLucidityVoice.Create('VoiceClass', @GlobalModPoints, Globals);
    Voices[c1].VoiceID := c1;

    // TODO: OnFinish handling will need to be
    // replaced with a mother ship message.
    //Voices[c1].OnFinish := EventHandle_VoiceFinished;
  end;
  //=============================================



  fSignalRecorder  := TSignalRecorder.Create(Globals);
  Globals.MotherShip.RegisterZeroObject(fSignalRecorder, TZeroObjectRank.Audio);

  fFreqAnalyzer := TFrequencyAnalyzer.Create;
  Globals.MotherShip.RegisterZeroObject(fFreqAnalyzer, TZeroObjectRank.Audio);


  // TODO: Should do some data directory validation here.
  // - check if the data directory exists,
  // - if it does, ensure the User and Factory directories exist.
  fSampleDirectories := TSampleDirectories.Create;
  Globals.MotherShip.RegisterZeroObject(fSampleDirectories, TZeroObjectRank.Audio); // or this could be TZeroObjectRank.Main? dunno.


  if (PluginDataDir^.Exists) then
  begin
    //==== Load the sample directories info ====
    DataFileName := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Sample Directories.xml';
    SampleDirectories.DataFileName := DataFileName;

    if FileExists(DataFileName)
      then SampleDirectories.ReadDirectoryInfoFromfile(DataFileName);


    //===== set the last used directories variable to something useful ====
    if LastProgramLoadDir = '' then
    begin
      DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Patches';
      if DirectoryExists(DataDir) then LastProgramLoadDir := DataDir;
    end;

    if LastProgramSaveDir = '' then
    begin
      DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Patches';
      if DirectoryExists(DataDir) then LastProgramSaveDir := DataDir;
    end;
  end;

  PreviewInfo^.Clear;

  AudioPreviewPlayer := TAudioFilePreviewPlayer.Create;
  KeyStateTracker    := TKeyStateTracker.Create;

  DeltaOffset := 0;

  MidiAutomation := TMidiAutomation.Create;
  MidiAutomation.OnMidiMessage := Event_MidiAutomation_Message;
  MidiAutomation.OnNewBinding  := Event_MidiAutomation_NewBinding;
  // TODO: Load default MIDI map here!


  fXYPads := TLucidityXYPads.Create(@GlobalModPoints, Globals);
  fXYPads.PadX1 := -0.5;

  MidiInputProcessor := TMidiInputProcessor.Create(@GlobalModPoints, Globals);
  Globals.MotherShip.RegisterZeroObject(MidiInputProcessor, TZeroObjectRank.Audio);

  VoiceController := TVoiceController.Create(Globals, @Voices);
  Globals.MotherShip.RegisterZeroObject(VoiceController, TZeroObjectRank.Audio);

  KeyGroupPlayer  := TKeyGroupPlayer.Create(Globals);
  Globals.MotherShip.RegisterZeroObject(KeyGroupPlayer, TZeroObjectRank.Audio);

  fSampleMap := TSampleMap.Create;
  Globals.SampleMapReference := fSampleMap;
  Globals.MotherShip.RegisterZeroObject(fSampleMap, TZeroObjectRank.Audio);

  fKeyGroups := TKeyGroupManager.Create(@Voices, @GlobalModPoints, Globals);
  Globals.KeyGroupsReference := fKeyGroups;
  Globals.MotherShip.RegisterZeroObject(fKeyGroups, TZeroObjectRank.Audio);

  EmptyKeyGroup := TKeyGroup.Create(@Voices, @GlobalModPoints, Globals, 'Empty');
  Globals.KeyGroupLifeTimeManager.Add(EmptyKeyGroup);

  //==== Look for key file ===
  if Globals.UserDataDir <> '' then
  begin
    fn := IncludeTrailingPathDelimiter(Globals.UserDataDir) + kKeyFileName;
    if FileExists(fn) then
    begin
      Globals.LoadRegistrationKeyFile(fn);
    end;
  end;


  {
  //===== Finially - Init Plugin State ========
  KeyGroups.Clear;
  KeyGroups.NewKeyGroup;
  FocusFirstKeyGroup;

  // Set all parameters to default values.
  for c1 := 0 to GetPluginParameterCount-1 do
  begin
    Par := IndexToPluginParameter(c1);
    ParName  := PluginParToName(Par);
    ParValue := GetPluginParInfo(Par).DefaultValue;
    SetPluginParameter(TParChangeScope.psGlobal, '', ParName, ParValue);
  end;

  //Important: call SampleGroups.UpdateInitReference() after reseting all properties to default.
  KeyGroups.UpdateInitReference;
  KeyGroups.Clear;
  }

  //============================================================================
  // CODESMELL: HACK: WANRING: Update the "empty" keygroup with default values.
  // This seems a bit hacky as I don't like the way "EmptyKeyGroup" sits
  // in relation to rest of the plugin. It feels like a code smell but
  // i'm not yet sure if this will lead to problems now the track.
  // NOTE: This code copies the "default" key group parameter state to
  // the "Empty" keygroup.
  //kgObj := (KeyGroups.FindFirstKeyGroup.GetObject as TKeyGroup);
  //(EmptyKeyGroup.GetObject as TKeyGroup).AssignFrom(kgObj);
  //============================================================================



  //==== temporarily don't initialize a default patch ====
  //InitializeState;


  //==== temporarily don't load default patch. ====

  IsDefaultPatchLoaded := false;

  // Now load default patch if it exists.
  if (PluginDataDir^.Exists) then
  begin
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + IncludeTrailingPathDelimiter('Patches');
    fnA := DataDir + 'Default.lpg';

    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Factory') + IncludeTrailingPathDelimiter('Patches');
    fnB := DataDir + 'Default.lpg';

    if FileExists(fnA) then
    begin
      LoadProgramFromFile(fnA);
      IsDefaultPatchLoaded := true;
    end else
    if FileExists(fnB) then
    begin
      LoadProgramFromFile(fnB);
      IsDefaultPatchLoaded := true;
    end;
  end;

  if IsDefaultPatchLoaded = false
    then InitializeState;

end;

destructor TeePlugin.Destroy;
var
  c1 : integer;
  fn : string;
begin
  Log.LogMessage('TPlugin.Destroy - Begin');

  Clear;

  if (PluginDataDir^.Exists)
    then fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Lucidity Profiler Report.txt'
    else fn := '';

  fFocusedKeyGroup := nil;
  EmptyKeyGroup    := nil;

  MidiAutomation.Free;
  VoiceController.Free;
  MidiInputProcessor.Free;
  KeyGroupPlayer.Free;
  fKeyGroups.Free;
  fSampleMap.Free;
  fXYPads.Free;
  KeyStateTracker.Free;
  AudioPreviewPlayer.Free;
  fSampleDirectories.Free;
  fSignalRecorder.Free;
  fFreqAnalyzer.Free;
  TProfiler.Close;


  //===== free all the voices ===================
  for c1 := 0 to kMaxVoiceCount-1 do
  begin
    Voices[c1].Free;
  end;
  SetLength(Voices, 0);
  //=============================================


  Log.LogMessage('==== TPlugin.Destroy - End ====');

  inherited;
end;

procedure TeePlugin.Clear;
begin
  fFocusedKeyGroup := nil;
  KeyGroupPlayer.Clear;
  KeyGroups.Clear;
  SampleMap.Clear;
end;

procedure TeePlugin.PublishPluginParameterAsVstParameter(const Par: TPluginParameter);
var
  pn : string;
begin
  pn := PluginParToName(Par);
  PublishedVstParameters.AddParameter(pn);
end;


function TeePlugin.GetPluginParameter(const ParName: string): single;
begin
  result := TPluginParameterController.GetPluginParameter(self, ParName);
end;

procedure TeePlugin.SetPluginParameter(const ParName: string; const ParValue: single);
begin
  // Generally this form of SetPluginParameter() will only be called by published VST parameters.
  // those changes need to effect all key groups so use the 'Global' TParChange scope.
  TPluginParameterController.SetPluginParameter(self, TParChangeScope.psGlobal, '', ParName, ParValue);
end;

procedure TeePlugin.SetPluginParameter(const Scope: TParChangeScope; const KeyGroupName, ParName: string; const Value: single);
begin
  TPluginParameterController.SetPluginParameter(self, Scope, KeyGroupName, ParName, Value);
end;

function TeePlugin.GetPluginParameterModAmount(const ParName: string; const ModSlot: integer): single;
begin
  result := TPluginParameterController.GetParameterModAmount(self, ParName, ModSlot);
end;

function TeePlugin.GetPluginParameterVstInfo(const ParName: string): TVstParameterInfo;
begin
  result := TPluginParameterController.GetPluginParameterVstInfo(self, ParName);
end;

procedure TeePlugin.SetPluginParameterModAmount(const Scope: TParChangeScope; const ParName: string; const ModSlot: integer; const ModAmount: single);
begin
  TPluginParameterController.SetParameterModAmount(self, Scope, ParName, ModSlot, ModAmount);
end;

procedure TeePlugin.GetModParModMinMax(const ParName: string; out ModMin, ModMax: single);
begin
  TPluginParameterController.GetModParModMinMax(self, ParName, ModMin, ModMax);
end;

function TeePlugin.GetPluginParameterInfo(const ParName: string): TPluginParameterInfo;
begin
  result := TPluginParameterController.GetParameterInfo(self, ParName);
end;

procedure TeePlugin.EventHandle_SampleRateChanged(Sender: TObject);
begin
  MidiAutomation.SampleRate := Globals.FastControlRate;
end;

function TeePlugin.ActiveKeyGroup: IKeyGroup;
var
  kg : IKeyGroup;
begin
  kg := fFocusedKeyGroup;
  if assigned(kg)
    then result := kg
    else result := EmptyKeyGroup;
end;

procedure TeePlugin.Resume;
begin
  inherited;
  AudioPreviewPlayer.SampleRate := Globals.SampleRate;
  Globals.AudioActions.IsProcessingActive := true;
end;

procedure TeePlugin.Suspend;
begin
  inherited;
  AudioPreviewPlayer.Kill;
  Globals.AudioActions.IsProcessingActive := false;
  // run audio actions after stopping to catch any late additions.
  Globals.AudioActions.Run;
end;

procedure TeePlugin.InitializeState;
var
  c1 : integer;
  ParName : string;
  Par : TPluginParameter;
  ParValue : single;
begin
  inherited;

  VoiceMode := TVoiceMode.Poly;
  VoiceGlide := 0;

  KeyGroups.Clear;
  SampleMap.Clear;

  KeyGroups.NewKeyGroup;

  // Set all parameters to default values.
  for c1 := 0 to GetPluginParameterCount-1 do
  begin
    Par := IndexToPluginParameter(c1);
    ParName  := PluginParToName(Par);
    ParValue := GetPluginParInfo(Par).DefaultValue;

    SetPluginParameter(TParChangeScope.psGlobal, '', ParName, ParValue);
  end;

  // finally.
  FocusFirstKeyGroup;
end;

procedure TeePlugin.FocusFirstKeyGroup;
var
  aRegion : IRegion;
  KG : IKeyGroup;
  KeyGroupName : string;
begin
  KG := KeyGroups.FindFirstKeyGroup;

  if (fFocusedKeyGroup <> KG) then
  begin
    fFocusedKeyGroup := KG;

    if (assigned(KG))
      then KeyGroupName := KG.GetName
      else KeyGroupName := '';

    SampleMap.DeselectAllRegions;

    if KeyGroupName <> ''
      then aRegion := SampleMap.FindRegionByKeyGroup(KeyGroupName)
      else aRegion := nil;

    if assigned(aRegion)
      then SampleMap.FocusRegion(aRegion.GetProperties^.UniqueID);

    // signal to the GUI that the focus has changed.
    Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
  end;
end;

procedure TeePlugin.FocusKeyGroup(const aKeyGroupName: string);
var
  DoFocus : boolean;
  aRegion : IRegion;
  KG : IKeyGroup;
begin
  KG := KeyGroups.FindSampleGroup(aKeyGroupName);

  DoFocus := false;

  if (assigned(KG)) and (fFocusedKeyGroup <> KG)
    then DoFocus := true;

  if DoFocus then
  begin
    fFocusedKeyGroup := KG;

    SampleMap.DeselectAllRegions;
    aRegion := SampleMap.FindRegionByKeyGroup(aKeyGroupName);

    if assigned(aRegion) then
    begin
      SampleMap.FocusRegion(aRegion.GetProperties^.UniqueID);
    end;

    // signal to the GUI that the focus has changed.
    Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
  end;



end;

procedure TeePlugin.FocusRegion(aRegionID: TGUID);
var
  aRegion : IRegion;
begin
  aRegion := SampleMap.FindRegionByUniqueID(aRegionID);

  if assigned(aRegion) then
  begin
    if ((assigned(FocusedKeyGroup)) and (aRegion.GetKeyGroup.GetName <> FocusedKeyGroup.GetName)) or (not assigned(FocusedKeyGroup)) then
    begin
      FocusKeyGroup(aRegion.GetKeyGroup.GetName);
      SampleMap.DeselectAllRegions;
    end;

    SampleMap.FocusRegion(aRegionID);

    // signal to the GUI that the focus has changed.
    Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
  end;
end;

procedure TeePlugin.SelectRegion(aRegionID: TGUID);
var
  aRegion : IRegion;
begin
  aRegion := SampleMap.FindRegionByUniqueID(aRegionID);

  if assigned(aRegion) then
  begin
    aRegion.GetProperties^.IsSelected := true;
  end;
end;

procedure TeePlugin.ClearFocus;
begin
  SampleMap.ClearFocus;

  // signal to the GUI that the focus has changed.
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
end;

procedure TeePlugin.ClearSelected;
begin
  SampleMap.DeselectAllRegions;

  // signal to the GUI that the focus has changed.
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
end;

procedure TeePlugin.DeleteKeyGroup(const aKeyGroupName: string);
begin
  if (assigned(fFocusedKeyGroup)) and (fFocusedKeyGroup.GetName = aKeyGroupName)
    then fFocusedKeyGroup := nil;

  SampleMap.DeselectAllRegions;

  SampleMap.DeleteRegionsInKeyGroup(aKeyGroupName);
  KeyGroups.DeleteKeyGroup(aKeyGroupName);

  // signal to the GUI that the focus has changed.
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
end;

procedure TeePlugin.DeleteSelectedRegions;
begin
  SampleMap.DeleteSelectedRegions;

  // signal to the GUI that the focus has changed.
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
end;

procedure TeePlugin.DuplicateSelectedRegions;
begin
  SampleMap.DuplicateSelectedRegions;

  // signal to the GUI that the focus has changed.
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleFocusChanged);
end;



procedure TeePlugin.MoveRootKey(const RootKeyOffset: integer);
var
  c1: Integer;
  aRegion : IRegion;
  NewRoot : integer;
begin
  for c1 := 0 to SampleMap.RegionCount-1 do
  begin
    aRegion := SampleMap.Regions[c1];
    if aRegion.GetProperties^.IsSelected then
    begin
      NewRoot := aRegion.GetProperties^.RootNote + RootKeyOffset;
      NewRoot := Clamp(NewRoot, 0, 127);
      aRegion.GetProperties^.RootNote := NewRoot;
    end;
  end;


  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleRegionChanged);
end;

procedure TeePlugin.MoveSelectedRegionsToKeyGroup(const aKeyGroupName: string);
var
  KG : IKeyGroup;
begin
  KG := KeyGroups.FindSampleGroup(aKeyGroupName);
  if not assigned(KG) then raise Exception.Create('Key group (' + aKeyGroupName + ') doesn''t exist. ');
  SampleMap.MoveSelectedRegionsToKeyGoup(KG);
  SampleMap.DeselectAllRegions;

  // signal to the GUI that the focus has changed.
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleRegionChanged);
end;

function TeePlugin.NewRegion(CreateInfo: TRegionCreateInfo): IRegion;
var
  kg : IKeyGroup;
begin
  if KeyGroups.Count = 0 then
  begin
    kg := KeyGroups.NewKeyGroup;
    FocusKeyGroup(kg.GetName);
  end;

  if not assigned(CreateInfo.KeyGroup) then
  begin
    if not assigned(FocusedKeyGroup)
      then FocusFirstKeyGroup;

    CreateInfo.KeyGroup := FocusedKeyGroup;
  end;

  result := SampleMap.NewRegion(CreateInfo);
end;

function TeePlugin.GetFilePreviewInfo: PFilePreviewInfo;
begin
  result := @fFilePreviewInfo;
end;

function TeePlugin.GetFocusedKeyGroup: IKeyGroup;
begin
  if assigned(fFocusedKeyGroup) then
  begin
    result := fFocusedKeyGroup;
  end else
  begin
    fFocusedKeyGroup := KeyGroups.FindFirstKeyGroup;
    result := fFocusedKeyGroup;
  end;
end;

function TeePlugin.GetFocusedRegion: IRegion;
begin
  result := SampleMap.FindFocusedRegion;
end;

procedure TeePlugin.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
var
  SG : IKeyGroup;
  aVoice : TLucidityVoice;
begin
  // Reset the feedback data variables to negative defaults
  FeedbackData.IsVoiceActive := false;
  FeedbackData.SampleBounds.ShowPlaybackBounds  := false;
  FeedbackData.SampleBounds.ShowLoopBounds      := false;
  FeedbackData.SampleBounds.ShowHighlightBounds := false;

  FeedbackData.ActiveVoiceCount := VoiceController.GetActiveVoiceCount;

  // Get the current feedback data
  SG := ActiveKeyGroup;
  if assigned(SG) then
  begin
    aVoice := VoiceController.GetLastTriggeredVoice;
    if (assigned(aVoice)) and (aVoice.IsActive) and (aVoice.KeyGroupID = SG.GetID) then
    begin
      // There is an active voice and it matches the current sample group.
      FeedBackData.IsVoiceActive := true;

      aVoice.GetGuiFeedBack(FeedBackData);

      //Get the feedback data for this active voice.
      (SG.GetObject as TKeyGroup).GetGuiFeedBack(FeedbackData);
    end else
    begin
      FeedBackData.IsVoiceActive := false;
    end;
  end;
end;

procedure TeePlugin.GetPreset(var ms: TMemoryStream);
var
  StateManager : TLucidityStateManager;
begin
  inherited;

  StateManager := TLucidityStateManager.Create(self);
  try
    StateManager.GetPreset(ms);
  finally
    StateManager.Free;
  end;

end;

function TeePlugin.GetPreviewVolume: single;
begin
  result := AudioPreviewPlayer.Volume;
end;

procedure TeePlugin.SetPreset(var ms: TMemoryStream);
var
  StateManager : TLucidityStateManager;
begin
  inherited;

  InitializeState;

  StateManager := TLucidityStateManager.Create(self);
  try
    StateManager.SetPreset(ms);
  finally
    StateManager.Free;
  end;

  PostLoadProgram;

end;

procedure TeePlugin.SetPreviewVolume(const Value: single);
begin
  AudioPreviewPlayer.Volume := Value;
end;

procedure TeePlugin.SetVoiceGlide(const Value: single);
begin
  MidiInputProcessor.VoiceGlide := Value;
end;

procedure TeePlugin.SetVoiceMode(const Value: TVoiceMode);
begin
  MidiInputProcessor.VoiceMode := Value;
end;

procedure TeePlugin.LoadProgramFromFile(const FileName: string);
var
  StateManager : TLucidityStateManager;
begin
  {$IFNDEF Demo}
  Lucidity.Globals.LastProgramLoadDir := ExtractFileDir(FileName);

  PreLoadProgram;

  StateManager := TLucidityStateManager.Create(self);
  try
    StateManager.LoadPesetFromFile(FileName);
  finally
    StateManager.Free;
  end;

  PostLoadProgram;

  PresetName := RemoveFileExt(FileName);
  {$ENDIF}


end;

procedure TeePlugin.ImportProgram(const FileName: string; ProgramFormat : TProgramFormat);
var
  StateManager : TLucidityStateManager;
begin
  {$IFNDEF Demo}
  Lucidity.Globals.LastProgramLoadDir := ExtractFileDir(FileName);

  PreLoadProgram;

  StateManager := TLucidityStateManager.Create(self);
  try
    case ProgramFormat of
      TProgramFormat.Lucidity: StateManager.LoadPesetFromFile(FileName);
      TProgramFormat.Sfz:      StateManager.ImportProgram_Sfz(FileName);
    else
      raise Exception.Create('unexpected program format');
    end;
  finally
    StateManager.Free;
  end;

  PostLoadProgram;

  PresetName := RemoveFileExt(FileName);
  {$ENDIF}
end;

procedure TeePlugin.PreLoadProgram;
begin
  //Reset some parameters to default values...
  VoiceMode := TVoiceMode.Poly;
  VoiceGlide := 0;

  SampleMap.Clear;
  KeyGroups.Clear;
end;

procedure TeePlugin.PostLoadProgram;
var
  aRegionID : TGUID;
begin
  if KeyGroups.GetInfo.GetKeyGroupCount = 0 then
  begin
    KeyGroups.NewKeyGroup;
  end;

  FocusFirstKeyGroup;

  // select the first region after loading a program.
  if (FocusedRegion = nil) and (SampleMap.RegionCount > 0) then
  begin
    aRegionID := SampleMap.Regions[0].GetProperties^.UniqueID;
    SampleMap.FocusRegion(aRegionID);
  end;


  //TODO: These send message calls could be replaced by...
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleMarkersChanged);
  Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleOscTypeChanged);
  Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateControlVisibility);
  Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateModMatrix);
  Globals.MotherShip.MsgVclTS(TLucidMsgID.FilterChanged);
  // this one call here perhaps.
  Globals.MotherShip.MsgVclTS(TLucidMsgID.ProgramLoaded);
  Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateGUI);
end;



procedure TeePlugin.SaveProgramToFile(const FileName: string);
var
  StateManager : TLucidityStateManager;
begin
  {$IFNDEF Demo}

  Lucidity.Globals.LastProgramSaveDir := ExtractFileDir(FileName);

  SaveSamplesToDisk(FileName, SampleMap);

  StateManager := TLucidityStateManager.Create(self);
  try
    StateManager.SavePesetToFile(FileName);
  finally
    StateManager.Free;
  end;

  PresetName := RemoveFileExt(FileName);

  Globals.MotherShip.MsgVclTS(TLucidMsgID.ProgramSavedToDisk);
  {$ENDIF}
end;


procedure TeePlugin.VstParameterChanged(const Par: TVstParameter);
begin
end;

function TeePlugin.GetVoiceGlide: single;
begin
  result :=  MidiInputProcessor.VoiceGlide;
  //result := VoiceController.VoiceGlide;
end;

function TeePlugin.GetVoiceMode: TVoiceMode;
begin
  result :=  MidiInputProcessor.VoiceMode;
  //result := VoiceController.VoiceMode;
end;

procedure TeePlugin.GetVstParameter(const Par:TVstParameter);
begin
end;

procedure TeePlugin.TriggerPreview(const fn: string);
var
  errorMsg : string;
begin
  AudioPreviewPlayer.Stop; // Call to stop any existing previews..

  UpdateFilePreivewInfo(fn);

  if (IsPreviewEnabled) and (IsSupportedAudioFileFormat(fn, true)) then
  begin
    if IsSupportedAudioFileFormat(fn, errorMsg, false) then
    begin
      AudioPreviewPlayer.Trigger(fn,200);
    end;
  end;
end;

procedure TeePlugin.UpdateFilePreivewInfo(const fn: string);
var
  Info : TAudioFileInfo;
begin
  PreviewInfo^.Clear;

  if IsSupportedAudioFileFormat(fn, true) then
  begin
    GetAudioFileInfoEx(fn, Info);

    PreviewInfo^.FileName    := ExtractFileName(fn);
    PreviewInfo^.IsValid     := Info.IsValid;
    PreviewInfo^.IsSupported := Info.IsSupported;

    if Info.IsValid then
    begin
      PreviewInfo^.SampleTime   := SamplesToTime(Info.SampleFrames, Info.SampleRate);
      PreviewInfo^.SampleRate   := IntToStr(Info.SampleRate);
      PreviewInfo^.ChannelCount := IntToStr(Info.Channels);
      PreviewInfo^.BitDepth     := IntToStr(Info.BitDepth);
    end;
  end;

  Globals.MotherShip.MsgVclTS(TLucidMsgID.PreviewInfoChanged);
end;

procedure TeePlugin.StopPreview;
begin
  AudioPreviewPlayer.Stop;
end;

procedure TeePlugin.ClearPreviewInfo;
begin
  PreviewInfo^.Clear;
  Globals.MotherShip.MsgVclTS(TLucidMsgID.PreviewInfoChanged);
end;

procedure TeePlugin.TriggerNoteOn(const MidiNote, MidiVelocity: integer);
begin
  assert(InRange(MidiNote, 0,127));
  assert(InRange(MidiVelocity, 0,127));

  KeyStateTracker.NoteOn(MidiNote, MidiVelocity);
  MidiInputProcessor.NoteOn(MidiNote, MidiVelocity);
  Globals.MotherShip.MsgVclTS(TLucidMsgID.MidiKeyChanged);
end;

procedure TeePlugin.TriggerNoteOff(const MidiNote, MidiVelocity: integer);
begin
  assert(InRange(MidiNote, 0,127));
  assert(InRange(MidiVelocity, 0,127));

  KeyStateTracker.NoteOff(MidiNote, MidiVelocity);
  MidiInputProcessor.NoteOff(MidiNote, MidiVelocity);
  Globals.MotherShip.MsgVclTS(TLucidMsgID.MidiKeyChanged);
end;

procedure TeePlugin.Event_MidiAutomation_Message(Sender: TObject; const MidiData1, MidiData2: integer; const Binding: ICustomMidiBinding);
const
  OneOver127 = 1/127;
var
  mb : IMidiBinding;
begin
  mb := Binding as IMidiBinding;
  SetPluginParameter(mb.GetParName, MidiData2 * OneOver127);
end;

procedure TeePlugin.Event_MidiAutomation_NewBinding(Sender: TObject; const MidiData1, MidiData2: integer; const Binding: ICustomMidiBinding);
var
  mb : IMidiBinding;
begin
  mb := Binding as IMidiBinding;

  // remove any existing bindings with this parameter name.
  MidiAutomation.ClearBinding(mb.GetParName);
end;




procedure TeePlugin.ProcessMidiEvent(Event: TeeMidiEvent);
const
  OneOver127 = 1 / 127;
var
  pba : single;
begin
  inherited;

  try
    if IsNoteOn(Event) then
    begin
      KeyStateTracker.NoteOn(Event.Data1, Event.Data2);
      MidiInputProcessor.NoteOn(Event.Data1, Event.Data2);
      Globals.MotherShip.MsgVclTS(TLucidMsgID.MidiKeyChanged);

      inc(GlobalModPoints.Source_TriggeredNoteCount);
    end;
  except
    Log.LogMessage('NoteOn Exception.');
    raise;
  end;


  try
    if IsNoteOff(Event) then
    begin
      KeyStateTracker.NoteOff(Event.Data1, Event.Data2);
      MidiInputProcessor.NoteOff(Event.Data1, Event.Data2);
      Globals.MotherShip.MsgVclTS(TLucidMsgID.MidiKeyChanged);
    end;
  except
    Log.LogMessage('NoteOff Exception.');
    raise;
  end;

  if IsControlChange(Event) then
  begin
    MidiAutomation.ProcessMidiCC(Event.Data1, Event.Data2);
  end;

  if IsModWheel(Event) then
  begin
    MidiInputProcessor.Modwheel(Event.Data2 * OneOver127);
  end;


  if IsPitchBend(Event) then
  begin
    pba := GetPitchBendAmount(Event);
    assert(InRange(pba,-1,1));
    MidiInputProcessor.PitchBend(pba);
  end;




end;




procedure TeePlugin.FastControlProcess;
begin
  try
    XYPads.ControlRateProcess;
    MidiAutomation.FastControlProcess;
    MidiInputProcessor.FastControlProcess;
    KeyGroupPlayer.FastControlProcess;
  except
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;
end;

procedure TeePlugin.SlowControlProcess;
begin
  try
    KeyGroupPlayer.SlowControlProcess;
  except
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;

end;

function TeePlugin.ActiveVoiceCount: integer;
begin
  result := VoiceController.GetActiveVoiceCount;
end;

procedure TeePlugin.AudioProcess(Sampleframes: integer);
var
  c1: Integer;
begin
  ClearBuffer(Outputs[0], SampleFrames);
  ClearBuffer(Outputs[1], SampleFrames);
  //ClearBuffer(Outputs[2], SampleFrames);
  //ClearBuffer(Outputs[3], SampleFrames);
  //ClearBuffer(Outputs[4], SampleFrames);
  //ClearBuffer(Outputs[5], SampleFrames);

  try
    AudioPreviewPlayer.Process(Outputs[0], Outputs[1], SampleFrames);
    KeyGroupPlayer.AudioProcess(Outputs, SampleFrames);
    SignalRecorder.Process(Outputs[0], Outputs[1], SampleFrames);
    FreqAnalyzer.Process(Outputs[0], Outputs[1], SampleFrames);

    //Don't forget to increment inputs and outputs.
    for c1 := 0 to self.InputCount-1 do
    begin
      inc(Inputs[c1], SampleFrames);
    end;
    for c1 := 0 to self.OutputCount-1 do
    begin
      inc(Outputs[c1], SampleFrames);
    end;

    inc(DeltaOffset,SampleFrames); //Always increment DeltaOffset last.
  except
    Log.LogMessage('Audio Process Exception.');
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;

end;

procedure TeePlugin.ProcessEnd;
begin
  Globals.AudioActions.Run;
end;


end.
