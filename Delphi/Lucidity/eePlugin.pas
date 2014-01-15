unit eePlugin;

interface

{$INCLUDE Defines.inc}

uses
  OtlCommon.Utils,
  RTTI,
  GpStuff,
  GpLists,
  Generics.Defaults,
  Generics.Collections,
  OtlSync,
  eePluginParameterWizard,
  eeAudioBufferUtils,
  uLucidityEnums, uLucidityData,
  Math, VamLib.MoreTypes, VamKeyStateTracker,
  uConstants, uLucidityXYPads, uLucidityVoiceController,
  SyncObjs, eePatchObject, eeVstParameter, eeVstParameterList,
  eeSampleInt, eeSampleFloat, uGuiState,
  Classes, eePluginBase , eeMidiEvents, eeMidiAutomation,
  Lucidity.SampleMap, uLucidityKeyGroup, uGuiFeedBackData, uLucidityKeyGroupInterface,
  uKeyGroupManager,
  eeAudioFilePreviewPlayer,
  FilterCore.SimperSVF,
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
  soFilter.LowpassA,
  soFilter.BandPassA,
  soFilter.HighPassA,
  soSawSquareOsc,
  soModMatrix,
  SampleOscUtils,
  soGateEnvelope,
  uLucidityClock,
  uLucidityLfo,
  uLucidityCustomSampleOsc,
  soDynamicWaveTableOsc,
  soGrainStretchSubOsc,
  soLucidityVoiceParameterWrapper,
  uLucidityStepSequencer;

type
  TeePlugin = class(TeePluginBase)
  private
    FActiveKeyGroup : IKeyGroup; //HACK: Do not access variable directly. Use ActiveKeyGroup().

    fMidiAutomation: TMidiAutomation;
    fGuiState: TGuiState;
    fXYPads: TLucidityXYPads;
    fKeyGroups: TKeyGroupManager;
    fSampleMap: TSampleMap;
    fKeyStateTracker: TKeyStateTracker;
    fAudioPreviewPlayer: TAudioFilePreviewPlayer;
    fFilePreviewInfo: TFilePreviewInfo;
    fSampleDirectories: TSampleDirectories;
    fFocusedKeyGroup: IKeyGroup;
    fIsPreviewEnabled: boolean;
    function GetFocusedRegion: IRegion;
    function GetFilePreviewInfo: PFilePreviewInfo;
    function GetVoiceGlide: single;
    function GetVoiceMode: TVoiceMode;
    procedure SetVoiceGlide(const Value: single);
    procedure SetVoiceMode(const Value: TVoiceMode);
    function GetPreviewVolume: single;
    procedure SetPreviewVolume(const Value: single);
  protected
    DeltaOffset     : integer;
    ParameterWizard : TPluginParameterWizard;
    GlobalModPoints : TGlobalModulationPoints;
    VoiceController : TLucidityVoiceController;

    EmptyKeyGroup : IKeyGroup;

    procedure SetupVstParameters;

    procedure GetVstParameter(const Par:TVstParameter);
    procedure VstParameterChanged(const Par:TVstParameter);
    procedure VstParameterChangedViaMidiAutomation(Index: integer; Value: single);

    property AudioPreviewPlayer : TAudioFilePreviewPlayer read fAudioPreviewPlayer write fAudioPreviewPlayer;

    procedure EventHandle_SampleRateChanged(Sender:TObject);

    procedure PreLoadProgram;
    procedure PostLoadProgram;
  public
    constructor Create; override;
	  destructor Destroy; override;

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
    property PreviewVolume    : single  read GetPreviewVolume    write SetPreviewVolume;
    property IsPreviewEnabled : boolean read fIsPreviewEnabled write fIsPreviewEnabled;

    //== The GUI should access the current Engine/Region using these properties ===
    property FocusedKeyGroup : IKeyGroup read fFocusedKeyGroup;
    property FocusedRegion   : IRegion   read GetFocusedRegion;
    function ActiveKeyGroup : IKeyGroup;
    function ActiveVoicePar : TLucidityVoiceParameterWrapper;
    function ActiveVoiceModPar : PModulatedPars;

    procedure FocusFirstKeyGroup;
    procedure FocusKeyGroup(const aKeyGroupName : string);
    procedure FocusRegion(aRegionID : TGUID);
    procedure ClearFocus;
    procedure ClearSelected;
    procedure SelectRegion(aRegionID : TGUID);

    procedure MoveSelectedRegionsToKeyGroup(const aKeyGroupName : string);
    procedure DeleteSelectedRegions;
    procedure DeleteKeyGroup(const aKeyGroupName : string);

    procedure MoveRootKey(const RootKeyOffset : integer);

    procedure TriggerNoteOn(const MidiNote, MidiVelocity : integer);  //intended for use by the GUI.
    procedure TriggerNoteOff(const MidiNote, MidiVelocity : integer); //intended for use by the GUI.


    function NewRegion(CreateInfo : TRegionCreateInfo):IRegion;


    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);
    procedure GetFilterInfo(const Info : PFilterParameterInfo);

    function GetVstParameterValue(Index:integer):single;

    property GuiState : TGuiState read fGuiState;

    property SampleDirectories : TSampleDirectories read fSampleDirectories;




  published
    // Global parameters. These properties are for the benefit of the statemanager.
    property VoiceMode  : TVoiceMode read GetVoiceMode    write SetVoiceMode;
    property VoiceGlide : single     read GetVoiceGlide   write SetVoiceGlide;
  end;




implementation

uses
  MadExcept, Windows,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eeCustomGlobals,
  LucidityParameterScaling,
  eeProfilerV2,
  eePluginDataDir, eePatchObject_XmlWrapper,
  SysUtils, NativeXML, uAutoFree, eeFunctions, eeDsp,
  Dialogs, uLucidityStateManager, LucidityGlobals, Lucidity.StateHelpers,
  AudioIO,
  eeSaveLoadFunctions;

const
  kCurrentPatchFormat  = 'Lucidity';
  kCurrentPatchVersion = 1;


{ TeePlugin }

constructor TeePlugin.Create;
var
  kg : IKeyGroup;
  kgObj : TKeyGroup;
  DefaultMidiMapFileName : string;
  //aPar : TVstParameter;
  c1: Integer;
  DataFileName : string;
  DataDir : string;
  fn : string;
begin
  inherited;

  fIsPreviewEnabled := true;

  Globals.AddEventListener(TPluginEvent.SampleRateChanged, EventHandle_SampleRateChanged);

  OutputDebugString('SAMPLING ON');

  TProfiler.Open;

  {$IFDEF Logging}
  if (PluginDataDir^.Exists)
    then LogMain.LogText('Data Directory Found', PluginDataDir^.Path)
    else LogMain.LogText('Data Directory NOT Found!', '');
  {$ENDIF}

  // TODO: Should do some data directory validation here.
  // - check if the data directory exists,
  // - if it does, ensure the User and Factory directories exist.
  fSampleDirectories := TSampleDirectories.Create;

  if (PluginDataDir^.Exists) then
  begin
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'User';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Patches';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);
  end;

  if (PluginDataDir^.Exists) then
  begin
    DataFileName := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Sample Directories.xml';
    SampleDirectories.DataFileName := DataFileName;

    if FileExists(DataFileName)
      then SampleDirectories.ReadDirectoryInfoFromfile(DataFileName);
  end;


  if (PluginDataDir^.Exists) then
  begin
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

  KeyStateTracker := TKeyStateTracker.Create;

  fGuiState := TGuiState.Create;

  DeltaOffset := 0;

  if PluginDataDir^.Exists then
  begin
    DefaultMidiMapFileName := IncludeTrailingPathDelimiter(PluginDataDir^.MidiMapsDir) + 'Default Midi Map.xml';
    MidiAutomation := TMidiAutomation.Create(DefaultMidiMapFileName);
    MidiAutomation.AutoSaveMidiMapOnExit := true;
    MidiAutomation.OnVstParameterAutomation := VstParameterChangedViaMidiAutomation;
  end else
  begin
    MidiAutomation := TMidiAutomation.Create('');
    MidiAutomation.AutoSaveMidiMapOnExit := false;
    MidiAutomation.OnVstParameterAutomation := VstParameterChangedViaMidiAutomation;
  end;

  fXYPads := TLucidityXYPads.Create(@GlobalModPoints, Globals);
  fXYPads.PadX1 := -0.5;

  VoiceController := TLucidityVoiceController.Create(@GlobalModPoints, Globals);

  fSampleMap := TSampleMap.Create;

  fKeyGroups := TKeyGroupManager.Create(VoiceController.GetVoiceArray, @VoiceController, @GlobalModPoints, Globals);

  EmptyKeyGroup := TKeyGroup.Create(VoiceController.GetVoiceArray, @GlobalModPoints, Globals);

  ParameterWizard := TPluginParameterWizard.Create(self, VoiceController);


  //==== Look for key file ===
  if Globals.UserDataDir <> '' then
  begin
    fn := IncludeTrailingPathDelimiter(Globals.UserDataDir) + kKeyFileName;
    if FileExists(fn) then
    begin
      Globals.LoadRegistrationKeyFile(fn);
    end;
  end;


  //===== Finially - Init Plugin State ========
  KeyGroups.Clear;
  KeyGroups.NewKeyGroup;
  FocusFirstKeyGroup;

  //Reset all VST parameters to default values.
  for c1 := 0 to Globals.VstParameters.Count-1 do
  begin
    Globals.VstParameters[c1].ResetToDefault;
  end;

  Globals.VstParameters.FindParameter('StepSeq1Length').ValueVST := 1;
  Globals.VstParameters.FindParameter('StepSeq2Length').ValueVST := 1;

  //Important: call SampleGroups.UpdateInitReference() after reseting all properties to default.
  KeyGroups.UpdateInitReference;

  //============================================================================
  // CODESMELL: HACK: WANRING: Update the "empty" keygroup with default values.
  // This seems a bit hacky as I don't like the way "EmptyKeyGroup" sits
  // in relation to rest of the plugin. It feels like a code smell but
  // i'm not yet sure if this will lead to problems now the track.
  // NOTE: This code copies the "default" key group parameter state to
  // the "Empty" keygroup.
  kgObj := (KeyGroups.FindFirstKeyGroup.GetObject as TKeyGroup);
  (EmptyKeyGroup.GetObject as TKeyGroup).AssignFrom(kgObj);
  //============================================================================



  InitializeState;

  // Now load default patch if it exists.
  if (PluginDataDir^.Exists) then
  begin
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + IncludeTrailingPathDelimiter('Patches');
    fn := DataDir + 'Default.lpg';

    if FileExists(fn) then LoadProgramFromFile(fn);
  end;
end;

destructor TeePlugin.Destroy;
var
  fn : string;
begin
  if (PluginDataDir^.Exists)
    then fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Lucidity Profiler Report.txt'
    else fn := '';

  FActiveKeyGroup := nil;

  ParameterWizard.Free;
  MidiAutomation.Free;
  VoiceController.Free;  //here.
  fKeyGroups.Free;
  fSampleMap.Free;
  fGuiState.Free;
  fXYPads.Free;
  KeyStateTracker.Free;
  AudioPreviewPlayer.Free;
  fSampleDirectories.Free;

  TProfiler.Close;

  OutputDebugString('SAMPLING OFF');
  inherited;
end;

procedure TeePlugin.EventHandle_SampleRateChanged(Sender: TObject);
begin
  MidiAutomation.SampleRate := globals.FastControlRate;
end;

function TeePlugin.ActiveKeyGroup: IKeyGroup;
var
  kg : IKeyGroup;
begin
  kg := FocusedKeyGroup;
  if not assigned(kg) then kg := EmptyKeyGroup;

  //HACK: Assign kg to FActiveKeyGroup to ensure the key group doesn't get free'ed
  // while it's being used. This hack is required for ActiveVoicePar().
  FActiveKeyGroup := kg;
  result := FActiveKeyGroup;
end;

function TeePlugin.ActiveVoiceModPar: PModulatedPars;
begin
  result := @(ActiveKeyGroup.GetObject as TKeyGroup).ModulatedParameters;
end;

function TeePlugin.ActiveVoicePar: TLucidityVoiceParameterWrapper;
begin
  result := (ActiveKeyGroup.GetObject as TKeyGroup).VoiceParameters;
end;



procedure TeePlugin.SetupVstParameters;
var
  c1 : integer;
  InfoMethod : TStringFunction;
  Par : TVstParameter;
begin
  
end;



procedure TeePlugin.Suspend;
begin
  inherited;
  AudioPreviewPlayer.Kill;
end;

procedure TeePlugin.Resume;
begin
  inherited;
  AudioPreviewPlayer.SampleRate := Globals.SampleRate;
end;



procedure TeePlugin.InitializeState;
var
  c1: Integer;
  fn, DataDir :string;
  RegionCreateInfo : TRegionCreateInfo;
  kg : IKeyGroup;
  aRegion : IRegion;
  SG : IKeyGroup;
  x : single;
begin
  inherited;

  VoiceMode := TVoiceMode.Poly;
  VoiceGlide := 0;

  KeyGroups.Clear;
  SampleMap.Clear;

  //Reset all VST parameters to default values.
  for c1 := 0 to Globals.VstParameters.Count-1 do
  begin
    Globals.VstParameters[c1].ResetToDefault;
  end;

  Globals.VstParameters.FindParameter('StepSeq1Length').ValueVST := 1;
  Globals.VstParameters.FindParameter('StepSeq2Length').ValueVST := 1;


  fFocusedKeyGroup := nil;
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
  end;

  // signal to the GUI that the focus has changed.
  Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
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
  end;


  // signal to the GUI that the focus has changed.
  Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
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
  end;


  // signal to the GUI that the focus has changed.
  Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
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
  Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
end;

procedure TeePlugin.ClearSelected;
begin
  SampleMap.DeselectAllRegions;

  // signal to the GUI that the focus has changed.
  Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
end;






procedure TeePlugin.DeleteKeyGroup(const aKeyGroupName: string);
begin
  SampleMap.DeselectAllRegions;
  SampleMap.DeleteRegionsInKeyGroup(aKeyGroupName);
  KeyGroups.DeleteKeyGroup(aKeyGroupName);

  //TODO: re-set focus
  if assigned(FocusedKeyGroup) and (FocusedKeyGroup.GetName = aKeyGroupname)
    then self.FocusFirstKeyGroup;

  // signal to the GUI that the focus has changed.
  Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
end;

procedure TeePlugin.DeleteSelectedRegions;
begin
  SampleMap.DeleteSelectedRegions;

  // signal to the GUI that the focus has changed.
  Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
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
      Clamp(NewRoot, 0, 127);
      aRegion.GetProperties^.RootNote := NewRoot;
    end;
  end;

  Globals.SendWindowsMessage(UM_SAMPLE_REGION_CHANGED);
end;

procedure TeePlugin.MoveSelectedRegionsToKeyGroup(const aKeyGroupName: string);
var
  KG : IKeyGroup;
  c1: Integer;
begin
  KG := KeyGroups.FindSampleGroup(aKeyGroupName);
  if not assigned(KG) then raise Exception.Create('Key group (' + aKeyGroupName + ') doesn''t exist. ');
  SampleMap.MoveSelectedRegionsToKeyGoup(KG);
  SampleMap.DeselectAllRegions;

  // signal to the GUI that the focus has changed.
  Globals.SendWindowsMessage(UM_SAMPLE_REGION_CHANGED);
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
  SG := FocusedKeyGroup;
  if assigned(SG) then
  begin
    aVoice := VoiceController.GetLastTriggeredVoice;
    if (assigned(aVoice)) and (aVoice.IsActive) and (aVoice.SampleGroup = SG) then
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
  end else
  begin
    //TODO:
    //assert(false, 'TODO');
  end;
end;

procedure TeePlugin.GetFilterInfo(const Info: PFilterParameterInfo);
var
  SG : IKeyGroup;
begin
  SG := FocusedKeyGroup;
  if assigned(SG) then
  begin
    (SG.GetObject as TKeyGroup).GetFilterInfo(Info);
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

end;

procedure TeePlugin.SetPreviewVolume(const Value: single);
begin
  AudioPreviewPlayer.Volume := Value;
end;

procedure TeePlugin.SetVoiceGlide(const Value: single);
begin
  VoiceController.VoiceGlide := Value;
end;

procedure TeePlugin.SetVoiceMode(const Value: TVoiceMode);
begin
  VoiceController.VoiceMode := Value;
end;

procedure TeePlugin.LoadProgramFromFile(const FileName: string);
var
  StateManager : TLucidityStateManager;
begin
  {$IFNDEF Demo}
  LucidityGlobals.LastProgramLoadDir := ExtractFileDir(FileName);

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
  LucidityGlobals.LastProgramLoadDir := ExtractFileDir(FileName);

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

  self.Globals.SendWindowsMessage(UM_SAMPLE_FOCUS_CHANGED);
  self.Globals.SendWindowsMessage(UM_SAMPLE_MARKERS_CHANGED);
  self.Globals.SendWindowsMessage(UM_SAMPLE_OSC_TYPE_CHANGED);
  self.Globals.SendWindowsMessage(UM_Update_Control_Visibility);
  self.Globals.SendWindowsMessage(UM_Update_Mod_Matrix);
  self.Globals.SendWindowsMessage(UM_FILTER_CHANGED);
end;



procedure TeePlugin.SaveProgramToFile(const FileName: string);
var
  StateManager : TLucidityStateManager;
begin
  {$IFNDEF Demo}

  LucidityGlobals.LastProgramSaveDir := ExtractFileDir(FileName);

  SaveSamplesToDisk(FileName, SampleMap);

  StateManager := TLucidityStateManager.Create(self);
  try
    StateManager.SavePesetToFile(FileName);
  finally
    StateManager.Free;
  end;

  PresetName := RemoveFileExt(FileName);

  Globals.SendWindowsMessage(UM_PROGRAM_SAVED_TO_DISK);
  {$ENDIF}
end;







function TeePlugin.GetVstParameterValue(Index: integer): single;
begin
  result := Globals.VstParameters[Index].ValueVST;
end;

procedure TeePlugin.VstParameterChanged(const Par: TVstParameter);
begin
end;

function TeePlugin.GetVoiceGlide: single;
begin
  result := VoiceController.VoiceGlide;
end;

function TeePlugin.GetVoiceMode: TVoiceMode;
begin
  result := VoiceController.VoiceMode;
end;

procedure TeePlugin.GetVstParameter(const Par:TVstParameter);
begin
end;

procedure TeePlugin.VstParameterChangedViaMidiAutomation(Index: integer; Value: single);
begin
  Globals.VstParameters.Parameter[Index].ValueVST := Value;
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

  Globals.SendWindowsMessage(UM_PREVIEW_INFO_CHANGED);
end;

procedure TeePlugin.StopPreview;
begin
  AudioPreviewPlayer.Stop;
end;

procedure TeePlugin.ClearPreviewInfo;
begin
  PreviewInfo^.Clear;
  Globals.SendWindowsMessage(UM_PREVIEW_INFO_CHANGED);
end;

procedure TeePlugin.TriggerNoteOn(const MidiNote, MidiVelocity: integer);
begin
  assert(InRange(MidiNote, 0,127));
  assert(InRange(MidiVelocity, 0,127));

  KeyStateTracker.NoteOn(MidiNote, MidiVelocity);
  VoiceController.NoteOn(MidiNote, MidiVelocity, SampleMap);
  Globals.SendWindowsMessage(UM_MIDI_KEY_CHANGED);
end;

procedure TeePlugin.TriggerNoteOff(const MidiNote, MidiVelocity: integer);
begin
  assert(InRange(MidiNote, 0,127));
  assert(InRange(MidiVelocity, 0,127));

  KeyStateTracker.NoteOff(MidiNote, MidiVelocity);
  VoiceController.NoteOff(MidiNote, MidiVelocity, SampleMap);
  Globals.SendWindowsMessage(UM_MIDI_KEY_CHANGED);
end;



procedure TeePlugin.ProcessMidiEvent(Event: TeeMidiEvent);
const
  OneOver127 = 1 / 127;
var
  kg : IKeyGroup;
  pba : single;
begin
  inherited;

  if IsNoteOn(Event) then
  begin
    KeyStateTracker.NoteOn(Event.Data1, Event.Data2);
    VoiceController.NoteOn(Event.Data1, Event.Data2, SampleMap);
    Globals.SendWindowsMessage(UM_MIDI_KEY_CHANGED);
  end;

  if IsNoteOff(Event) then
  begin
    KeyStateTracker.NoteOff(Event.Data1, Event.Data2);
    VoiceController.NoteOff(Event.Data1, Event.Data2, SampleMap);

    // TODO: SendGuiMessage is blocking and way too slow.
    Globals.SendWindowsMessage(UM_MIDI_KEY_CHANGED);
  end;

  if IsControlChange(Event) then
  begin
    MidiAutomation.ProcessMidiCC(Event.Data1, Event.Data2);
  end;

  if IsModWheel(Event) then
  begin
    VoiceController.Modwheel(Event.Data2 * OneOver127);
  end;


  if IsPitchBend(Event) then
  begin
    pba := GetPitchBendAmount(Event);
    assert(InRange(pba,-1,1));
    VoiceController.PitchBend(pba);
  end;




end;



procedure TeePlugin.FastControlProcess;
begin
  try
    XYPads.ControlRateProcess;
    VoiceController.FastControlProcess;
    MidiAutomation.FastControlProcess;
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
    VoiceController.SlowControlProcess;

  except
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;

end;

procedure TeePlugin.AudioProcess(Sampleframes: integer);
var
  c1: Integer;
begin
  ClearBuffer(Outputs[0], SampleFrames);
  ClearBuffer(Outputs[1], SampleFrames);
  ClearBuffer(Outputs[2], SampleFrames);
  ClearBuffer(Outputs[3], SampleFrames);
  ClearBuffer(Outputs[4], SampleFrames);
  ClearBuffer(Outputs[5], SampleFrames);

  try
    AudioPreviewPlayer.Process(Outputs[0], Outputs[1], SampleFrames);
    VoiceController.AudioProcess(Outputs, SampleFrames);

    //Don't forget to increment inputs and outputs.
    for c1 := 0 to self.InputCount-1 do
    begin
      inc(Inputs[c1], SampleFrames);
    end;
    for c1 := 0 to self.OutputCount-1 do
    begin
      inc(Outputs[c1], SampleFrames);
    end;


    {
    //Don't forget to increment inputs and outputs.
    inc(Inputs[0],SampleFrames);
    inc(Inputs[1],SampleFrames);

    inc(Outputs[0],SampleFrames);
    inc(Outputs[1],SampleFrames);

    for c1 := 0 to SampleFrames-1 do
    begin
      Outputs[3]^ := Outputs[2]^;
      Outputs[5]^ := Outputs[4]^;
      inc(Outputs[2]);
      inc(Outputs[3]);
      inc(Outputs[4]);
      inc(Outputs[5]);
    end;
    }

    inc(DeltaOffset,SampleFrames); //Always increment DeltaOffset last.
  except
    {$IFDEF MadExcept}
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;
end;


end.
