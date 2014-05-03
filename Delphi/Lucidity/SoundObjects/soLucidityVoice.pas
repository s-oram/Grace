unit soLucidityVoice;

interface

{$INCLUDE Defines.inc}

uses
  Classes,
  Generics.Collections,
  Math,
  VamLib.MoreTypes,
  eeSampleFloat, eeDsp,
  eeTypes, eeVirtualCV,
  eeGlobals, eeVoiceLogic,
  eeCounter,
  uLucidityEnums,
  Lucidity.Interfaces,
  Lucidity.Types,
  LucidityModConnections,
  Lucidity.Env.ADSR,
  Lucidity.Env.ASR,
  Lucidity.Osc.OneShotSampler,
  Lucidity.Osc.OneShotSampler.SubOsc,
  Lucidity.Osc.LoopSampler,
  Lucidity.Osc.GrainStretch,
  Lucidity.SampleMap, soLucidityWaveOsc, soLucidityFilter,
  uGuiFeedbackData,
  soModMatrix, soFivePointEnvelope,
  uLucidity.Lfo,
  uLucidityStepSequencer,
  uLucidityVCA,
  uLucidityClock, uOutputMixer,
  soGateEnvelope,
  soGrainStretchSubOsc,
  soSawSquareOsc,
  soDynamicWaveTableOsc,
  soLevelMeter,
  FilterCore.SimperSVF,
  soFilter.BlueFilter,
  VamLib.Utils,
  SampleOscUtils,
  uConstants,
  B2.Filter.CriticallyDampedLowpass;

type
  TLucidityVoice = class;
  PLucidityVoice = ^TLucidityVoice;

  PArrayOfLucidityVoice = ^TArrayOfLucidityVoice;
  TArrayOfLucidityVoice = array of TLucidityVoice;

  TLucidityVoiceList = class(TObjectList<TLucidityVoice>);

  TOscModule = (oscNoteSampler, oscLoopSampler);

  // NOTE: TLucidityVoice descends from TPureInterfaced object
  // so that interfaces can be used without reference counting.
  // The voice logic class requires an interface to the Voice class
  // to be passed in.
  TLucidityVoice = class(TPureInterfacedObject, IVoiceStateInfo)
  private
    fAmpEnv : TLucidityADSR;
    fTriggerNote: byte;
    fFilterEnv: TLucidityADSR;
    fFilterOne: TLucidityFilter;
    fFilterTwo: TLucidityFilter;
    fModMatrix: TModMatrix;
    fStepSeqOne: TLucidyStepSequencer;
    fGrainStretchOsc: TLucidityGrainStretchOsc;
    fSamplePlaybackType: TSamplePlaybackType;
    fStepSeqTwo: TLucidyStepSequencer;
    fOscVCA: TLucidityVCA;
    fOutputMixer: TOutputMixer;
    fLoopSampleOsc: TLoopSampleOsc;
    fWaveOsc      : TLucidityWaveOsc;
    fSampleReset: TClockSource;
    fVoiceID: integer;
    fVoiceMode: TVoiceMode;
    fVoiceGlide: single;
    fOnFinish: TNotifyEvent;
    fPitchTracking: TPitchTracking;
    fOscModule: TOscModule;
    fLucidityLfoA: TLucidityLfo;
    fLucidityLfoB: TLucidityLfo;
    fFilterRouting: TFilterRouting;
    fLevelMonitor: TLevelMonitor;
    fKeyGroupID: TKeyGroupID;
    fKeyGroup : IKeyGroup;
    function GetObject:TObject;
    procedure SetSamplePlaybackType(const Value: TSamplePlaybackType);
    procedure SetSampleReset(const Value: TClockSource);
    procedure SetVoiceGlide(const Value: single);
    procedure SetPitchTracking(const Value: TPitchTracking);
    procedure SetFilterRouting(const Value: TFilterRouting);
  protected
    VoiceClockManager : TLucidityVoiceClockManager;

    LfoOut : Psingle;

    SampleGainCh1 : single;
    SampleGainCh2 : single;
    VoiceGainCh1 : single;
    VoiceGainCh2 : single;

    BufferA, BufferB : array of single;

    Globals : TGlobals;
    GlobalModPoints : PGlobalModulationPoints;

    ModPoints           : TVoiceModulationPoints;
    ModConnections      : PModConnections;    // Info about what the modulation sources are.
    ParValueData        : PModulatedPars;     // Raw parameter values. The values are identical for all voices in the voice group.
    ParModData          : TParModulationData; // stores the summed modulation input for each parameter. (Most parameters will be zero)

    fOneShotSampleOsc : TOneShotSampleOsc;
    OscPitchParameters : PSampleOscPitchPar;


    // TODO: fSampleREgion is only used as a reference point for other code outside
    // the TLucidityVoice. Rather than maintaining a reference to an interface
    // it might be better to use a SampleRegionID similar to the KeyGroupID.
    fSampleRegion : IRegion;

    fIsActive, HasBeenReleased, HasBeenQuickReleased : boolean;
    AmpLevel : single;

    FRInput     : single;
    FRFilterOne : single;

    FBOut1 : single;
    FBOut2 : single;

    procedure SampleRateChanged(Sender:TObject);
    procedure TempoChanged(Sender:TObject);

    // CleanUp() clears references to other resouces and zeros some internal values.
    // It should be called whenever the voice becomes in-active.
    procedure CleanUp;

    procedure SampleResetClockEvent(Sender : TObject; ClockID : cardinal);

    procedure SampleFinished(Sender : TObject);

    function CalcPitchTransitionTime : single;

    property OscModule : TOscModule read fOscModule write fOscModule;

    procedure UpdateOscPitch;
  public
    constructor Create(aObjectName : string; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals : TGlobals);
    destructor Destroy; override;

    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);

    procedure Trigger(const MidiNote, MidiVelocity:byte; const aSampleGroup : IKeyGroup; const aSampleRegion:IRegion);
    procedure Release;
    procedure QuickRelease;
    procedure Kill;

    procedure GetVoiceState(out aIsActive, aHasBeenReleased, aHasBeenQuickReleased: boolean;  out aAmpLevel: single);

    procedure AudioProcess(const OutA, OutB:PSingle; const SampleFrames : integer);{$IFDEF AudioInline}inline;{$ENDIF}
    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}

    property IsActive    : boolean read fIsActive;
    property TriggerNote : byte    read fTriggerNote;

    //==== Sound modules ====
    property GrainStretchOsc  : TLucidityGrainStretchOsc read fGrainStretchOsc  write fGrainStretchOsc;
    property OneShotSampleOsc : TOneShotSampleOsc        read fOneShotSampleOsc write fOneShotSampleOsc;
    property LoopSampleOsc    : TLoopSampleOsc           read fLoopSampleOsc    write fLoopSampleOsc;
    property WaveOsc          : TLucidityWaveOsc         read fWaveOsc          write fWaveOsc;
    property AmpEnv           : TLucidityADSR            read fAmpEnv           write fAmpEnv;
    property FilterEnv        : TLucidityADSR            read fFilterEnv        write fFilterEnv;
    property OscVCA           : TLucidityVCA             read fOscVCA           write fOscVCA;
    property OutputMixer      : TOutputMixer             read fOutputMixer      write fOutputMixer;
    property FilterOne        : TLucidityFilter          read fFilterOne        write fFilterOne;
    property FilterTwo        : TLucidityFilter          read fFilterTwo        write fFilterTwo;
    property LfoA             : TLucidityLfo             read fLucidityLfoA     write fLucidityLfoA;
    property LfoB             : TLucidityLfo             read fLucidityLfoB     write fLucidityLfoB;

    property StepSeqOne       : TLucidyStepSequencer     read fStepSeqOne       write fStepSeqOne;
    property StepSeqTwo       : TLucidyStepSequencer     read fStepSeqTwo       write fStepSeqTwo;
    property ModMatrix        : TModMatrix               read fModMatrix        write fModMatrix;

    property LevelMonitor     : TLevelMonitor            read fLevelMonitor     write fLevelMonitor;

    //===== Parameters ======
    property SamplePlaybackType : TSamplePlaybackType read fSamplePlaybackType write SetSamplePlaybackType;
    property PitchTracking      : TPitchTracking      read fPitchTracking      write SetPitchTracking;
    property VoiceMode          : TVoiceMode          read fVoiceMode          write fVoiceMode;
    property VoiceGlide         : single              read fVoiceGlide         write SetVoiceGlide; //range 0..1.
    property SampleReset        : TClockSource        read fSampleReset        write SetSampleReset;

    property FilterRouting      : TFilterRouting      read fFilterRouting      write SetFilterRouting;

    property LinkedSampleRegion : IRegion read fSampleRegion;


    property KeyGroupID   : TKeyGroupID read fKeyGroupID;
    property KeyGroup     : IKeyGroup   read fKeyGroup;
    property SampleRegion : IRegion   read fSampleRegion;

    property VoiceID : integer read fVoiceID write fVoiceID;

    //OnFinish is called when a Voice becomes inactive.
    property OnFinish : TNotifyEvent read fOnFinish write fOnFinish;
  end;

implementation


uses
  VamLib.ZeroObject,
  {$IFDEF Logging}
  SmartInspectLogging,
  VamLib.LoggingProxy,
  {$ENDIF}
  eeCustomGlobals,
  eeProfiler,
  SysUtils, eePitch,
  soADSR;

const
  k3dB : double = 1.4125;


{ TLucidityVoice }

constructor TLucidityVoice.Create(aObjectName: string; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
begin
  fVoiceID := -1;

  VoiceClockManager := TLucidityVoiceClockManager.Create;

  //TODO: This buffer size needs to be set correctly.
  SetLength(BufferA, 512);
  SetLength(BufferB, 512);

  Globals := aGlobals;
  GlobalModPoints := aGlobalModPoints;

  fSampleRegion := nil;

  fTriggerNote := 0;
  fIsActive := false;
  HasBeenReleased := false;
  HasBeenQuickReleased := false;
  AmpLevel := 0;

  ModPoints.MidiNote := 0;

  Globals.AddEventListener(TPluginEvent.SampleRateChanged, SampleRateChanged);
  Globals.AddEventListener(TPluginEvent.TempoChanged,      TempoChanged);


  ModMatrix := TModMatrix.Create;

  ModMatrix.SetModSourcePointer(TModSource.Midi_Note, @ModPoints.MidiNote);
  ModMatrix.SetModSourcePointer(TModSource.Midi_PitchBend, @GlobalModPoints^.Source_MidiPitchbend);
  ModMatrix.SetModSourcePointer(TModSource.Midi_Modwheel, @GlobalModPoints^.Source_MidiModwheel);

  ModMatrix.SetModDestPointer(TModDest.SampleStart, @ModPoints.SampleStart);
  ModMatrix.SetModDestPointer(TModDest.SampleEnd, @ModPoints.SampleEnd);
  ModMatrix.SetModDestPointer(TModDest.LoopStart, @ModPoints.LoopStart);
  ModMatrix.SetModDestPointer(TModDest.LoopEnd, @ModPoints.LoopEnd);

  GrainStretchOsc := TLucidityGrainStretchOsc.Create(@ModPoints, VoiceClockManager);

  OneShotSampleOsc := TOneShotSampleOsc.Create(@ModPoints, VoiceClockManager);
  OneShotSampleOsc.OnFinished := self.SampleFinished;
  OscPitchParameters := OneShotSampleOsc.GetPitchParameters;

  LoopSampleOsc := TLoopSampleOsc.Create(@ModPoints, VoiceClockManager);
  LoopSampleOsc.OnFinished := self.SampleFinished;

  fWaveOsc   := TLucidityWaveOsc.Create;

  AmpEnv := TLucidityADSR.Create;
  ModMatrix.SetModSourcePointer(TModSource.AmpEnv, AmpEnv.GetModPointer('EnvOut'));

  FilterEnv := TLucidityADSR.Create;
  ModMatrix.SetModSourcePointer(TModSource.FilterEnv, FilterEnv.GetModPointer('EnvOut'));

  FilterOne := TLucidityFilter.Create(@ModPoints);
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par1, FilterOne.GetModPointer('Par1Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par2, FilterOne.GetModPointer('Par2Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par3, FilterOne.GetModPointer('Par3Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par4, FilterOne.GetModPointer('Par4Mod'));

  FilterTwo := TLucidityFilter.Create(@ModPoints);
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par1, FilterTwo.GetModPointer('Par1Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par2, FilterTwo.GetModPointer('Par2Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par3, FilterTwo.GetModPointer('Par3Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par4, FilterTwo.GetModPointer('Par4Mod'));

  OscVCA := TLucidityVCA.Create;
  ModMatrix.SetModDestPointer(TModDest.VoiceAmplitude, OscVCA.GetModPointer('ModInput_Gain'));
  ModMatrix.SetModDestPointer(TModDest.VoicePan, OscVCA.GetModPointer('ModInput_Gain')); //HACK: Delete asap!

  LfoA := TLucidityLfo.Create(0, VoiceClockManager);
  ModMatrix.SetModSourcePointer(TModSource.Lfo1, LfoA.GetModPointer('LfoOutput'));
  LfoOut := LfoA.GetModPointer('LfoOutput');

  LfoB := TLucidityLfo.Create(1, VoiceClockManager);
  ModMatrix.SetModSourcePointer(TModSource.Lfo2, LfoB.GetModPointer('LfoOutput'));

  StepSeqOne := TLucidyStepSequencer.Create(VoiceClockManager);
  ModMatrix.SetModSourcePointer(TModSource.StepSeq1, StepSeqOne.GetModPointer('StepSeqOutput'));

  StepSeqTwo := TLucidyStepSequencer.Create(VoiceClockManager);
  ModMatrix.SetModSourcePointer(TModSource.StepSeq2, StepSeqTwo.GetModPointer('StepSeqOutput'));

  OutputMixer := TOutputMixer.Create;
  ModMatrix.SetModDestPointer(TModDest.ModOutA, OutputMixer.GetModPointer('ModOutA'));
  ModMatrix.SetModDestPointer(TModDest.ModOutB, OutputMixer.GetModPointer('ModOutB'));
  OutputMixer.VoiceMixMain := 1;

  LevelMonitor := TLevelMonitor.Create;
  Globals.MotherShip.RegisterZeroObject(LevelMonitor, TZeroObjectRank.Audio);

  // Finally,
  SampleRateChanged(self);
  TempoChanged(self);
end;

destructor TLucidityVoice.Destroy;
begin
  fKeyGroupID.Clear;
  fKeyGroup     := nil;
  fSampleRegion := nil;

  AmpEnv.Free;
  FilterEnv.Free;
  OneShotSampleOsc.Free;
  LoopSampleOsc.Free;
  GrainStretchOsc.Free;
  fWaveOsc.Free;
  FilterOne.Free;
  FilterTwo.Free;
  OscVCA.Free;
  LfoA.Free;
  LfoB.Free;
  StepSeqOne.Free;
  StepSeqTwo.Free;
  ModMatrix.Free;
  SetLength(BufferA, 0);
  SetLength(BufferB, 0);
  OutputMixer.Free;
  VoiceClockManager.Free;
  LevelMonitor.Free;
  inherited;
end;

function TLucidityVoice.GetObject: TObject;
begin
  result := self;
end;

procedure TLucidityVoice.SampleFinished(Sender: TObject);
begin
  QuickRelease;
end;

procedure TLucidityVoice.SampleRateChanged(Sender: TObject);
begin
  //==== Audio Rate Modules ====
  OneShotSampleOsc.SampleRate       := Globals.SampleRate;
  LoopSampleOsc.SampleRate          := Globals.SampleRate;
  GrainStretchOsc.SampleRate        := Globals.SampleRate;
  WaveOsc.SampleRate                := Globals.SampleRate;
  FilterOne.SampleRate              := Globals.SampleRate;
  FilterTwo.SampleRate              := Globals.SampleRate;
  LevelMonitor.SampleRate           := Globals.SampleRate;

  //==== Control Rate Modules ====
  AmpEnv.SampleRate     := Globals.ControlRate;
  FilterEnv.SampleRate  := Globals.ControlRate;
  LfoA.SampleRate       := Globals.ControlRate;
  LfoB.SampleRate       := Globals.ControlRate;
  StepSeqOne.SampleRate := Globals.ControlRate;
  StepSeqTwo.SampleRate := Globals.ControlRate;


end;

procedure TLucidityVoice.TempoChanged(Sender: TObject);
begin
  OneShotSampleOsc.Tempo := Globals.Tempo;
  LoopSampleOsc.Tempo    := Globals.Tempo;

  StepSeqOne.Bpm := Globals.Tempo;
  StepSeqTwo.Bpm := Globals.Tempo;

  LfoA.Bpm := Globals.Tempo;
  LfoB.Bpm := Globals.Tempo;
end;

procedure TLucidityVoice.SetFilterRouting(const Value: TFilterRouting);
begin
  fFilterRouting := Value;

  case Value of
    TFilterRouting.Serial:
    begin
      FRInput     := 0;
      FRFilterOne := 1;
    end;

    TFilterRouting.Parallel:
    begin
      FRInput     := 1;
      FRFilterOne := 0;
    end;


    TFilterRouting.FiftyFifty:
    begin
      FRInput     := 0.5;
      FRFilterOne := 0.5;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

procedure TLucidityVoice.SetPitchTracking(const Value: TPitchTracking);
begin
  fPitchTracking := Value;
  //NOTE: The voice class expects the SamplePlayback type to not change while Active.
  if IsActive then assert('ERROR: Changing Pitch Tracking type while Voice is active.');
end;

procedure TLucidityVoice.SetSamplePlaybackType(const Value: TSamplePlaybackType);
begin
  fSamplePlaybackType := Value;
  //NOTE: The voice class expects the SamplePlayback type to not change while Active.
  if IsActive then assert('ERROR: Changing Playback type while Voice is active.');
end;

procedure TLucidityVoice.SetSampleReset(const Value: TClockSource);
begin
  fSampleReset := Value;

  VoiceClockManager.RemoveListener('LucidityVoice', Self);

  case Value of
    TClockSource.None: ;
    TClockSource.Lfo1: VoiceClockManager.AddListener(ClockID_Lfo1, Self, SampleResetClockEvent, 'LucidityVoice');
    TClockSource.Lfo2: VoiceClockManager.AddListener(ClockID_Lfo2, Self, SampleResetClockEvent, 'LucidityVoice');
  end;
end;

procedure TLucidityVoice.SetVoiceGlide(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fVoiceGlide := Value;
end;

procedure TLucidityVoice.SampleResetClockEvent(Sender: TObject; ClockID: cardinal);
begin
  OneShotSampleOsc.ResetSamplePosition;
end;

function TLucidityVoice.CalcPitchTransitionTime: single;
begin
  result := 1 + VoiceGlide * 5000;
end;

procedure TLucidityVoice.UpdateOscPitch;
var
  PitchOne: single;
  PitchTwo: single;
begin
  PitchOne := ParValueData^[TModParIndex.VoicePitchOne].ParValue + ParModData[TModParIndex.VoicePitchOne];
  PitchOne := Clamp(PitchOne, 0, 1) * 2 - 1;

  PitchTwo := ParValueData^[TModParIndex.VoicePitchTwo].ParValue + ParModData[TModParIndex.VoicePitchTwo];
  PitchTwo := Clamp(PitchTwo, 0, 1) * 2 - 1;

  OscPitchParameters^.PitchTracking  := self.PitchTracking;

  OscPitchParameters^.RegionRootNote := SampleRegion.GetProperties^.RootNote;

  if VoiceMode = TVoiceMode.Poly
        then OscPitchParameters^.PlaybackNote := fTriggerNote
        else OscPitchParameters^.PlaybackNote := GlobalModPoints.Source_MonophonicMidiNote;

  OscPitchParameters^.SamplePitchAdjust := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
  OscPitchParameters^.VoicePitchAdjust  := round(PitchOne * 12) + PitchTwo;
  OscPitchParameters^.PitchBendAdjust   := GlobalModPoints^.Source_MidiPitchBendST;
end;

procedure TLucidityVoice.Trigger(const MidiNote, MidiVelocity: byte; const aSampleGroup : IKeyGroup; const aSampleRegion:IRegion);
var
  CV : TModularVoltage;
begin
  //assert(aSampleGroup <> nil, 'Sample region can not be nil.');
  assert(aSampleRegion <> nil, 'Sample region can not be nil.');
  //assert(MidiNote >= 0);
  assert(MidiNote <= 127);
  //assert(MidiVelocity >= 0);
  assert(MidiVelocity <= 127);

  fIsActive := true;
  fTriggerNote := MidiNote;
  HasBeenReleased := false;
  HasBeenQuickReleased := false;
  AmpLevel := 0;

  //=== Pre-trigger setup ======================================================
  fKeyGroupID   := aSampleGroup.GetID;
  fKeyGroup     := aSampleGroup;
  fSampleRegion := aSampleRegion;

  ParValueData   := aSampleGroup.GetModulatedParameters;
  ModConnections := aSampleGroup.GetModConnectionsPointer;

  //=== init all processing objects with links to voice parameters etc ===
  StepSeqOne.SequenceData := aSampleGroup.GetSequenceData(0);
  StepSeqTwo.SequenceData := aSampleGroup.GetSequenceData(1);

  LfoA.Par1 := @ParValueData^[TModParIndex.Lfo1Par1].ModulatedParValue;
  LfoA.Par2 := @ParValueData^[TModParIndex.Lfo1Par2].ModulatedParValue;
  LfoA.Par3 := @ParValueData^[TModParIndex.Lfo1Par3].ModulatedParValue;

  LfoB.Par1 := @ParValueData^[TModParIndex.Lfo2Par1].ModulatedParValue;
  LfoB.Par2 := @ParValueData^[TModParIndex.Lfo2Par2].ModulatedParValue;
  LfoB.Par3 := @ParValueData^[TModParIndex.Lfo2Par3].ModulatedParValue;

  //-- IMPORTANT: Do first. --
  ModMatrix.Init(ParValueData, @self.ParModData, ModConnections);
  ModMatrix.UpdateModConnections;
  ModMatrix.ZeroAllValues;
  //--------------------------

  OneShotSampleOsc.Init(ParValueData, @self.ParModData);
  FilterOne.Init(0, ParValueData, @self.ParModData);
  FilterTwo.Init(1, ParValueData, @self.ParModData);
  AmpEnv.Init(0, ParValueData, @self.ParModData);
  FilterEnv.Init(1, ParValueData, @self.ParModData);

  OscVCA.Config^.GainPar := @ParValueData^[TModParIndex.OutputGain].ParValue;
  OscVCA.Config^.GainMod := @ParModData[TModParIndex.OutputGain];
  OscVCA.Config^.PanPar  := @ParValueData^[TModParIndex.OutputPan].ParValue;
  OscVCA.Config^.PanMod  := @ParModData[TModParIndex.OutputPan];
  //=============================================================


  // set some modulation source values...
  if VoiceMode = TVoiceMode.Poly then
  begin
    // NOTE: The goal of MIDI Note as a mod source is something similar to 1 volt per octave
    // pitch scaling in modular synths. I want filters to be able to track the keyboard.
    CV := MidiNote / 12;
    ModPoints.MidiNote := ModularVoltageToAudioRange(cv);
    ModPoints.KeyFollowFreqMultiplier := PitchShiftToRate(MidiNote - 36);
  end else
  begin
    CV := GlobalModPoints.Source_MonophonicMidiNote / 12;
    ModPoints.MidiNote := ModularVoltageToAudioRange(cv);
    ModPoints.KeyFollowFreqMultiplier := PitchShiftToRate(GlobalModPoints.Source_MonophonicMidiNote - 36);
  end;

  // call StepReset on all modulation sources.
  LfoA.StepResetA;
  LfoB.StepResetA;
  AmpEnv.StepResetA;
  FilterEnv.StepResetA;
  StepSeqOne.StepResetA(aSampleGroup.GetTriggeredNoteCount);
  StepSeqTwo.StepResetA(aSampleGroup.GetTriggeredNoteCount);

  ModMatrix.FastControlProcess;
  ModMatrix.SlowControlProcess;

  LfoA.StepResetB;
  LfoB.StepResetB;
  StepSeqOne.StepResetB;
  StepSeqTwo.StepResetB;

  //TODO: The amp and filter envelopes need a StepResetB.

  // Call Trigger on all components that need it....
  AmpEnv.Trigger(MidiVelocity / 127);
  FilterEnv.Trigger(MidiVelocity / 127);
  LfoA.Trigger;
  LfoB.Trigger;

  UpdateOscPitch;

  OneShotSampleOsc.Trigger(MidiNote, aSampleRegion, aSampleRegion.GetSample^);

  FilterOne.Reset;
  FilterTwo.Reset;
end;

procedure TLucidityVoice.Release;
begin
  if HasBeenReleased = false then
  begin
    HasBeenReleased := true;

    OneShotSampleOsc.Release;
    AmpEnv.Release;
    FilterEnv.Release;
    LfoA.Release;
    LfoB.Release;
  end;
end;

procedure TLucidityVoice.QuickRelease;
begin
  if HasBeenQuickReleased = false then
  begin
    AmpEnv.QuickRelease(35);
    HasBeenReleased      := true;
    HasBeenQuickReleased := true;
  end;
end;

procedure TLucidityVoice.Kill;
begin
  AmpEnv.Kill;
  FilterEnv.Kill;

  fIsActive := false;
  HasBeenReleased := false;
  HasBeenQuickReleased := false;
  AmpLevel := 0;

  // Important: Clear voice resources...
  CleanUp;
end;

procedure TLucidityVoice.CleanUp;
var
  vp : PLucidityVoice;
begin
  // TODO: Need to board cast the voice being finished here.
  vp := @self;
  Globals.MotherShip.MsgAudio(TLucidMsgID.Audio_VoiceFinished, vp);


  try
    // CleanUp() clears references to other resouces and zeros some internal values.
    // It should be called whenever the voice becomes inactive.
    // NOTE: it's important to nil fSampleRegion interface references
    // here. Lucidity uses interface reference count as a garbage collection device.
    fKeyGroupID.Clear;
    fKeyGroup     := nil;
    fSampleRegion := nil;
    OneShotSampleOsc.Kill;
    LoopSampleOsc.Kill;
    GrainStretchOsc.Kill;
    LfoA.Kill;
    LfoB.Kill;

    if assigned(OnFinish) then OnFinish(self);
  except
    LogMain.LogException('TLucidityVoice.CleanUp');
    raise;
  end;
end;

procedure TLucidityVoice.GetVoiceState(out aIsActive, aHasBeenReleased, aHasBeenQuickReleased: boolean; out aAmpLevel: single);
begin
  aIsActive := fIsActive;
  aHasBeenReleased := HasBeenReleased;
  aHasBeenQuickReleased := HasBeenQuickReleased;
  aAmpLevel := AmpLevel;
end;

procedure TLucidityVoice.FastControlProcess;
var
  CV : TModularVoltage;
  Par1 : single;
  PanX, VolX : single;
begin
  VolX := ParValueData^[TModParIndex.OutputGain].ModulatedParValue;
  PanX := ParValueData^[TModParIndex.OutputPan].ModulatedParValue;

  assert(InRange(VolX, 0, 1));
  assert(InRange(PanX, 0, 1));

  Calculate3dbPan(PanX, VoiceGainCh1, VoiceGainCh2);

  Volx := Volx * Volx * 4 * k3dB;

  VoiceGainCh1 := VoiceGainCh1 * Volx;
  VoiceGainCh2 := VoiceGainCh2 * Volx;

  if VoiceMode = TVoiceMode.Poly then
  begin
    // NOTE: The goal of MIDI Note as a mod source is something similar to 1 volt per octave
    // pitch scaling in modular synths. I want filters to be able to track the keyboard.
  end else
  begin
    CV := GlobalModPoints.Source_MonophonicMidiNote / 12;
    ModPoints.MidiNote := ModularVoltageToAudioRange(cv);
    ModPoints.KeyFollowFreqMultiplier := PitchShiftToRate(GlobalModPoints.Source_MonophonicMidiNote - 36);
  end;

  //=== Control rate step for all control rate modules ===
  LfoA.FastControlProcess;
  LfoB.FastControlProcess;
  AmpEnv.FastControlProcess;
  FilterEnv.FastControlProcess;
  StepSeqOne.Step;
  StepSeqTwo.Step;

  //=== Mod Matrix ====
  ModMatrix.FastControlProcess;

  //=== Control rate step for all audio rate modules ===
  //OscVCA.FastControlProcess(AmpEnv.Value);

  FilterOne.FastControlProcess;
  FilterTwo.FastControlProcess;


  //==========
  Par1 := ParValueData^[TModParIndex.FilterOutputBlend].ModulatedParValue;
  assert(Par1 >= 0);
  assert(Par1 <= 1);
  FBOut1 := 1 - Par1;
  FBOut2 := Par1;
end;

procedure TLucidityVoice.SlowControlProcess;
var
  PanX, VolX : single;
begin
  UpdateOscPitch;

  LfoA.SlowControlProcess;
  LfoB.SlowControlProcess;
  AmpEnv.SlowControlProcess;
  FilterEnv.SlowControlProcess;

  ModMatrix.SlowControlProcess;
  OneShotSampleOsc.SlowControlProcess;


  // update sample pan / volume offsets..
  assert(SampleRegion.GetProperties^.SamplePan >= -100);
  assert(SampleRegion.GetProperties^.SamplePan <= 100);
  PanX := (SampleRegion.GetProperties^.SamplePan + 100) / 200;
  VolX := DecibelsToLinear(SampleRegion.GetProperties^.SampleVolume);

  Calculate3dbPan(PanX, SampleGainCh1, SampleGainCh2);
  SampleGainCh1 := SampleGainCh1 * VolX;
  SampleGainCh2 := SampleGainCh2 * VolX;
end;



procedure TLucidityVoice.AudioProcess(const OutA, OutB:PSingle; const SampleFrames: integer);
var
  c1: Integer;
  SampleOscX1, SampleOscX2 : single;
  MixX1, MixX2 : single;
  MixY1, MixY2 : single;
  pxA, pxB : PSingle;
  pOutA, pOutB : PSingle;
begin
  assert(IsActive);

  pxA := @BufferA[0];
  pxB := @BufferB[0];

  for c1 := 0 to SampleFrames-1 do
  begin
    OneShotSampleOsc.AudioRateStep(SampleOscX1, SampleOscX2);

    MixX1 := SampleOscX1 * SampleGainCh1;
    MixX2 := SampleOscX2 * SampleGainCh2;

    FilterOne.AudioRateStep(MixX1, MixX2);

    MixY1 := (SampleOscX1 * FRInput) + (MixX1 * FRFilterOne);
    MixY2 := (SampleOscX2 * FRInput) + (MixX2 * FRFilterOne);

    FilterTwo.AudioRateStep(MixY1, MixY2);

    MixX1 := (MixX1 * FBOut1) + (MixY1 * FBOut2);
    MixX2 := (MixX2 * FBOut1) + (MixY2 * FBOut2);


    MixX1 := MixX1 * AmpEnv.Value;
    MixX2 := MixX2 * AmpEnv.Value;

    //OscVCA.AudioRateStep(MixX1, MixX2);

    pxA^ := MixX1 * VoiceGainCh1;
    pxB^ := MixX2 * VoiceGainCh2;

    //pxA^ := LfoOut^;
    //pxB^ := LfoOut^;

    inc(pxA);
    inc(pxB);
  end;

  pxA := @BufferA[0];
  pxB := @BufferB[0];

  LevelMonitor.Process(pxA, pxB, SampleFrames);

  pOutA := OutA;
  pOutB := OutB;

  for c1 := 0 to SampleFrames-1 do
  begin
    pOutA^ := pOutA^ + pxA^;
    pOutB^ := pOutB^ + pxB^;

    inc(pOutA);
    inc(pOutB);
    inc(pxA);
    inc(pxB);
  end;


  //TODO: This output mixer isn't being utilised at this stage. It seem to make
  // sense when there were multiple outputs but perhaps it would be better to
  // remove it entirely for the time being.
  //OutputMixer.AudioRateProcess(pxA, pxB, Outputs, SampleFrames);

  AmpLevel := AmpEnv.Value;

  if (AmpEnv.Stage = TEnvelopeStage.esOff) then
  begin
    fIsActive := false;
    // Important: Clear voice resources...
    CleanUp;
  end;
end;

procedure TLucidityVoice.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
begin
  FeedBackData.StepSeq1CurStep := StepSeqOne.CurrentStep;
  FeedBackData.StepSeq2CurStep := StepSeqTwo.CurrentStep;

  if FeedbackData.FocusedRegion = self.SampleRegion then
  begin
    FeedbackData.SampleBounds.ShowRealTimeMarkers := true;
    OneShotSampleOsc.GetGuiFeedBack(FeedbackData);
  end else
  begin
    FeedbackData.SampleBounds.ShowRealTimeMarkers := false;
  end;
end;





end.

