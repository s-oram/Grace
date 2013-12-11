unit soLucidityVoice;

interface

{$INCLUDE Defines.inc}

uses
  Classes, LucidityModConnections,
  Generics.Collections,
  Math, uLucidityEnums, uGuiFeedbackData,
  eeSampleFloat, eeDsp,
  eeTypes, eeVirtualCV, uLucidityKeyGroupInterface,
  MoreTypes, eeGlobals, eeVoiceLogic,
  eeCounter,
  Lucidity.Env.ADSR,
  Lucidity.Env.ASR,
  Lucidity.Osc.OneShotSampler,
  Lucidity.Osc.OneShotSampler.SubOsc,
  Lucidity.Osc.LoopSampler,
  Lucidity.Osc.GrainStretch,
  Lucidity.SampleMap, soLucidityWaveOsc, soLucidityFilter,
  soModMatrix, soFivePointEnvelope,
  uLucidityLfo, uLucidityStepSequencer,
  uLucidityVCA, uLucidityPanner,
  uLucidityClock, uOutputMixer,
  soGateEnvelope,
  soGrainStretchSubOsc,
  soSawSquareOsc,
  soDynamicWaveTableOsc,
  FilterCore.SimperSVF,
  soFilter.LowpassA,
  soFilter.BandPassA,
  soFilter.HighPassA,
  soFilter.LowpassB,
  eeFunctions,
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
    fLFO: TLucidityLfo;
    fModMatrix: TModMatrix;
    fStepSeqOne: TLucidyStepSequencer;
    fGrainStretchOsc: TLucidityGrainStretchOsc;
    fSamplePlaybackType: TSamplePlaybackType;
    fStepSeqTwo: TLucidyStepSequencer;
    fOscVCA: TLucidityVCA;
    fOscPanner: TLucidityPanner;
    fOutputMixer: TOutputMixer;
    fLoopSampleOsc: TLoopSampleOsc;
    fWaveOsc      : TLucidityWaveOsc;
    fSampleReset: TClockSource;
    fModEnvB: TLucidityASR;
    fModEnvA: TLucidityASR;
    fVoiceID: integer;
    fVoiceMode: TVoiceMode;
    fVoiceGlide: single;
    fPitchOne: single;
    fPitchTwo: single;
    fOnFinish: TNotifyEvent;
    fPitchTracking: TPitchTracking;
    fOscModule: TOscModule;
    function GetObject:TObject;
    procedure SetSamplePlaybackType(const Value: TSamplePlaybackType);
    procedure SetSampleReset(const Value: TClockSource);
    procedure SetVoiceGlide(const Value: single);
    procedure SetPitchTracking(const Value: TPitchTracking);
  protected
    VoiceClockManager : TLucidityVoiceClockManager;

    SampleLevelOffsetA : single;
    SampleLevelOffsetB : single;

    BufferA, BufferB : array of single;

    Globals : TGlobals;
    GlobalModPoints : PGlobalModulationPoints;

    ModPoints : TVoiceModulationPoints;

    fOneShotSampleOsc : TOneShotSampleOsc;
    OscPitchParameters : PSampleOscPitchPar;

    fSampleGroup  : IKeyGroup;
    fSampleRegion : IRegion;

    fIsActive, HasBeenReleased, HasBeenQuickReleased : boolean;
    AmpLevel : single;

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

    procedure UpdateAllModLinks(const aModConnections : TModConnections);
    procedure UpdateModLink(const ModLinkData : PModLink);

    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);

    procedure Trigger(const MidiNote, MidiVelocity:byte; const aSampleGroup : IKeyGroup; const aSampleRegion:IRegion; const aModConnections: TModConnections);
    //procedure Trigger_Poly(const MidiNote, MidiVelocity:byte; const aSampleGroup : IKeyGroup; const aSampleRegion:IRegion);
    //procedure Trigger_Mono(const MidiNote, MidiVelocity:byte; const aSampleGroup : IKeyGroup; const aSampleRegion:IRegion); //used by Mono and legato modes.
    //procedure Trigger_Legato(const MidiNote, MidiVelocity:byte; const aSampleGroup : IKeyGroup; const aSampleRegion:IRegion; const WithGlide : boolean); //used by Mono and legato modes.
    procedure Release;
    procedure QuickRelease;
    procedure Kill;

    procedure GetVoiceState(out aIsActive, aHasBeenReleased, aHasBeenQuickReleased: boolean;  out aAmpLevel: single);

    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer);{$IFDEF AudioInline}inline;{$ENDIF}
    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}

    property IsActive : boolean read fIsActive;
    property TriggerNote : byte read fTriggerNote;

    //==== Sound modules ====
    property GrainStretchOsc  : TLucidityGrainStretchOsc read fGrainStretchOsc  write fGrainStretchOsc;
    property OneShotSampleOsc : TOneShotSampleOsc        read fOneShotSampleOsc write fOneShotSampleOsc;
    property LoopSampleOsc    : TLoopSampleOsc           read fLoopSampleOsc    write fLoopSampleOsc;
    property WaveOsc          : TLucidityWaveOsc         read fWaveOsc          write fWaveOsc;
    property AmpEnv           : TLucidityADSR            read fAmpEnv           write fAmpEnv;
    property FilterEnv        : TLucidityADSR            read fFilterEnv        write fFilterEnv;
    property ModEnvA          : TLucidityASR             read fModEnvA          write fModEnvA;
    property ModEnvB          : TLucidityASR             read fModEnvB          write fModEnvB;
    property OscVCA           : TLucidityVCA             read fOscVCA           write fOscVCA;
    property OscPanner        : TLucidityPanner          read fOscPanner        write fOscPanner;
    property OutputMixer      : TOutputMixer             read fOutputMixer      write fOutputMixer;
    property FilterOne        : TLucidityFilter          read fFilterOne        write fFilterOne;
    property FilterTwo        : TLucidityFilter          read fFilterTwo        write fFilterTwo;
    property LFO              : TLucidityLfo             read fLFO              write fLFO;
    property StepSeqOne       : TLucidyStepSequencer     read fStepSeqOne       write fStepSeqOne;
    property StepSeqTwo       : TLucidyStepSequencer     read fStepSeqTwo       write fStepSeqTwo;
    property ModMatrix        : TModMatrix               read fModMatrix        write fModMatrix;

    //===== Parameters ======
    property SamplePlaybackType : TSamplePlaybackType read fSamplePlaybackType write SetSamplePlaybackType;
    property PitchTracking      : TPitchTracking      read fPitchTracking      write SetPitchTracking;
    property VoiceMode          : TVoiceMode          read fVoiceMode          write fVoiceMode;
    property VoiceGlide         : single              read fVoiceGlide         write SetVoiceGlide; //range 0..1.
    property SampleReset        : TClockSource        read fSampleReset        write SetSampleReset;

    property PitchOne           : single              read fPitchOne           write fPitchOne; //range -1..1
    property PitchTwo           : single              read fPitchTwo           write fPitchTwo; //range -1..1

    property LinkedSampleRegion : IRegion read fSampleRegion;

    property SampleGroup  : IKeyGroup read fSampleGroup;
    property SampleRegion : IRegion   read fSampleRegion;

    property VoiceID : integer read fVoiceID write fVoiceID;

    //OnFinish is called when a Voice becomes inactive.
    property OnFinish : TNotifyEvent read fOnFinish write fOnFinish;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eeCustomGlobals,
  eeProfiler,
  SysUtils, eePitch,
  soADSR;

{ TLucidityVoice }

constructor TLucidityVoice.Create(aObjectName: string; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
begin
  fVoiceID := -1;

  fPitchOne := 0;
  fPitchTwo := 0;

  VoiceClockManager := TLucidityVoiceClockManager.Create;

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

  ModMatrix.SetModSourcePointer(TModSource.PadX1, @GlobalModPoints^.Source_PadX1);
  ModMatrix.SetModSourcePointer(TModSource.PadY1, @GlobalModPoints^.Source_PadY1);
  ModMatrix.SetModSourcePointer(TModSource.PadX2, @GlobalModPoints^.Source_PadX2);
  ModMatrix.SetModSourcePointer(TModSource.PadY2, @GlobalModPoints^.Source_PadY2);
  ModMatrix.SetModSourcePointer(TModSource.PadX3, @GlobalModPoints^.Source_PadX3);
  ModMatrix.SetModSourcePointer(TModSource.PadY3, @GlobalModPoints^.Source_PadY3);
  ModMatrix.SetModSourcePointer(TModSource.PadX4, @GlobalModPoints^.Source_PadX4);
  ModMatrix.SetModSourcePointer(TModSource.PadY4, @GlobalModPoints^.Source_PadY4);

  ModMatrix.SetModDestPointer(TModDest.SampleStart, @ModPoints.SampleStart);
  ModMatrix.SetModDestPointer(TModDest.SampleEnd, @ModPoints.SampleEnd);
  ModMatrix.SetModDestPointer(TModDest.LoopStart, @ModPoints.LoopStart);
  ModMatrix.SetModDestPointer(TModDest.LoopEnd, @ModPoints.LoopEnd);

  GrainStretchOsc := TLucidityGrainStretchOsc.Create(@ModPoints, VoiceClockManager);
  //ModMatrix.SetModDestPointer(TModDest.GrainStretch_GrainSize, GrainStretchOsc.GetModPointer('DestGrainSize'));
  //ModMatrix.SetModDestPointer(TModDest.GrainStretch_Rate,      GrainStretchOsc.GetModPointer('DestRate'));
  //ModMatrix.SetModDestPointer(TModDest.GrainStretch_Position,  GrainStretchOsc.GetModPointer('DestPosition'));

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

  ModEnvA := TLucidityASR.Create;
  ModMatrix.SetModSourcePointer(TModSource.ModEnv1, ModEnvA.GetModPointer('EnvOut'));

  ModEnvB := TLucidityASR.Create;
  ModMatrix.SetModSourcePointer(TModSource.ModEnv2, ModEnvB.GetModPointer('EnvOut'));

  FilterOne := TLucidityFilter.Create;
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par1, FilterOne.GetModPointer('Par1Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par2, FilterOne.GetModPointer('Par2Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par3, FilterOne.GetModPointer('Par3Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter1_Par4, FilterOne.GetModPointer('Par4Mod'));

  FilterTwo := TLucidityFilter.Create;
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par1, FilterTwo.GetModPointer('Par1Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par2, FilterTwo.GetModPointer('Par2Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par3, FilterTwo.GetModPointer('Par3Mod'));
  ModMatrix.SetModDestPointer(TModDest.Filter2_Par4, FilterTwo.GetModPointer('Par4Mod'));

  OscVCA := TLucidityVCA.Create;
  ModMatrix.SetModDestPointer(TModDest.VoiceAmplitude, OscVCA.GetModPointer('ModInput_Gain'));

  OscPanner := TLucidityPanner.Create;
  ModMatrix.SetModDestPointer(TModDest.VoicePan, OscPanner.GetModPointer('ModInput_Pan'));

  LFO := TLucidityLfo.Create(VoiceClockManager);
  ModMatrix.SetModSourcePointer(TModSource.Lfo1, LFO.GetModPointer('LfoOut1'));
  ModMatrix.SetModSourcePointer(TModSource.Lfo2, LFO.GetModPointer('LfoOut2'));
  ModMatrix.SetModDestPointer(TModDest.Lfo1_Rate, LFO.GetModPointer('LfoRateMod1'));
  ModMatrix.SetModDestPointer(TModDest.Lfo1_ParB, LFO.GetModPointer('LfoParBMod1'));
  ModMatrix.SetModDestPointer(TModDest.Lfo2_Rate, LFO.GetModPointer('LfoRateMod2'));
  ModMatrix.SetModDestPointer(TModDest.Lfo2_ParB, LFO.GetModPointer('LfoParBMod2'));

  StepSeqOne := TLucidyStepSequencer.Create(VoiceClockManager);
  ModMatrix.SetModSourcePointer(TModSource.StepSeq1, StepSeqOne.GetModPointer('StepSeqOutput'));

  StepSeqTwo := TLucidyStepSequencer.Create(VoiceClockManager);
  ModMatrix.SetModSourcePointer(TModSource.StepSeq2, StepSeqTwo.GetModPointer('StepSeqOutput'));

  OutputMixer := TOutputMixer.Create;
  ModMatrix.SetModDestPointer(TModDest.ModOutA, OutputMixer.GetModPointer('ModOutA'));
  ModMatrix.SetModDestPointer(TModDest.ModOutB, OutputMixer.GetModPointer('ModOutB'));
  OutputMixer.VoiceMixMain := 1;

  // Finally,
  SampleRateChanged(self);
  TempoChanged(self);
end;

destructor TLucidityVoice.Destroy;
begin
  AmpEnv.Free;
  FilterEnv.Free;
  ModEnvA.Free;
  ModEnvB.Free;
  OneShotSampleOsc.Free;
  LoopSampleOsc.Free;
  GrainStretchOsc.Free;
  fWaveOsc.Free;
  fSampleGroup  := nil;
  fSampleRegion := nil;
  FilterOne.Free;
  FilterTwo.Free;
  OscVCA.Free;
  OscPanner.Free;
  LFO.Free;
  StepSeqOne.Free;
  StepSeqTwo.Free;
  ModMatrix.Free;
  SetLength(BufferA, 0);
  SetLength(BufferB, 0);
  OutputMixer.Free;
  VoiceClockManager.Free;
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

  FilterOne.SampleRate := Globals.SampleRate;
  FilterTwo.SampleRate := Globals.SampleRate;

  //==== Control Rate Modules ====
  AmpEnv.SampleRate     := Globals.ControlRate;
  FilterEnv.SampleRate  := Globals.ControlRate;
  ModEnvA.SampleRate    := Globals.ControlRate;
  ModEnvB.SampleRate    := Globals.ControlRate;
  LFO.SampleRate     := Globals.ControlRate;
  StepSeqOne.SampleRate := Globals.ControlRate;
  StepSeqTwo.SampleRate := Globals.ControlRate;
end;

procedure TLucidityVoice.TempoChanged(Sender: TObject);
begin
  OneShotSampleOsc.Tempo := Globals.Tempo;
  LoopSampleOsc.Tempo    := Globals.Tempo;

  StepSeqOne.Bpm := Globals.Tempo;
  StepSeqTwo.Bpm := Globals.Tempo;
end;

procedure TLucidityVoice.SetPitchTracking(const Value: TPitchTracking);
begin
  fPitchTracking := Value;

  //NOTE: The voice class expects the SamplePlayback type to not change while Active.
  if IsActive then assert('ERROR: Changing Pitch Tracking type while Voice is active.');


  {
  if (PitchTracking = TPitchTracking.Note) or (PitchTracking = TPitchTracking.Off)
    then OscModule := TOscModule.oscNoteSampler
    else OscModule := TOscModule.oscLoopSampler;
  }
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

  {
  case OscModule of
    oscNoteSampler: OneShotSampleOsc.ResetSamplePosition;
    oscLoopSampler: LoopSampleOsc.ResetSamplePosition;
  else
    raise Exception.Create('type not handled.');
  end;
  }
end;

function TLucidityVoice.CalcPitchTransitionTime: single;
begin
  result := 1 + VoiceGlide * 5000;
end;

procedure TLucidityVoice.UpdateAllModLinks(const aModConnections: TModConnections);
begin
  ModMatrix.UpdateAllModLinks(aModConnections);
end;

procedure TLucidityVoice.UpdateModLink(const ModLinkData: PModLink);
begin
  ModMatrix.UpdateModLink(ModLinkData);
end;

procedure TLucidityVoice.UpdateOscPitch;
var
  PitchShift : single;
  SamplePitch : single;
begin
  OscPitchParameters^.PitchTracking  := self.PitchTracking;

  OscPitchParameters^.RegionRootNote := SampleRegion.GetProperties^.RootNote;

  if VoiceMode = TVoiceMode.Poly
        then OscPitchParameters^.PlaybackNote := fTriggerNote
        else OscPitchParameters^.PlaybackNote := GlobalModPoints.Source_MonophonicMidiNote;

  OscPitchParameters^.SamplePitchAdjust := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
  OscPitchParameters^.VoicePitchAdjust  := round(PitchOne * 12) + PitchTwo;
  OscPitchParameters^.PitchBendAdjust   := GlobalModPoints^.Source_MidiPitchBendST;



  {
  case PitchTracking of
    TPitchTracking.Note:
    begin
      if VoiceMode = TVoiceMode.Poly
        then PitchShift := fTriggerNote
        else PitchShift := GlobalModPoints.Source_MonophonicMidiNote;

      SamplePitch := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
      PitchShift := (PitchShift - SampleRegion.GetProperties^.RootNote) + round(PitchOne * 12) + PitchTwo + SamplePitch;
      OneShotSampleOsc.PitchShift := PitchShift;
    end;

    TPitchTracking.BPM:
    begin
      SamplePitch := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
      OneShotSampleOsc.PitchShift := CalcPitchShift(SampleRegion.GetProperties^.RootNote, SampleRegion.GetProperties^.RootNote, PitchOne, PitchTwo, SamplePitch);
    end;

    TPitchTracking.Off:
    begin
      SamplePitch := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
      OneShotSampleOsc.PitchShift := CalcPitchShift(SampleRegion.GetProperties^.RootNote, SampleRegion.GetProperties^.RootNote, PitchOne, PitchTwo, SamplePitch);
    end;
  else
    raise Exception.Create('type not handled.');
  end;
  }
end;

procedure TLucidityVoice.Trigger(const MidiNote, MidiVelocity: byte; const aSampleGroup : IKeyGroup; const aSampleRegion:IRegion; const aModConnections: TModConnections);
var
  CV : TModularVoltage;
  PitchShift : single;
  SamplePitch : single;
begin
  //assert(aSampleGroup <> nil, 'Sample region can not be nil.');
  assert(aSampleRegion <> nil, 'Sample region can not be nil.');
  assert(MidiNote >= 0);
  assert(MidiNote <= 127);
  assert(MidiVelocity >= 0);
  assert(MidiVelocity <= 127);

  fIsActive := true;
  fTriggerNote := MidiNote;
  HasBeenReleased := false;
  HasBeenQuickReleased := false;
  AmpLevel := 0;

  //=== Pre-trigger setup ======================================================
  fSampleGroup  := aSampleGroup;
  fSampleRegion := aSampleRegion;

  ModMatrix.ZeroAllValues; //IMPORTANT: Do first.

  // set some modulation source values...
  if VoiceMode = TVoiceMode.Poly then
  begin
    // NOTE: The goal of MIDI Note as a mod source is something similar to 1 volt per octave
    // pitch scaling in modular synths. I want filters to be able to track the keyboard.
    CV := MidiNote / 12;
    ModPoints.MidiNote := ModularVoltageToAudioRange(cv);
  end else
  begin
    CV := GlobalModPoints.Source_MonophonicMidiNote / 12;
    ModPoints.MidiNote := ModularVoltageToAudioRange(cv);
  end;

  // call StepReset on all modulation sources.
  LFO.StepResetA;
  AmpEnv.StepResetA;
  FilterEnv.StepResetA;
  ModEnvA.StepResetA;
  ModEnvB.StepResetA;
  StepSeqOne.StepResetA(aSampleGroup.GetTriggeredNoteCount);
  StepSeqTwo.StepResetA(aSampleGroup.GetTriggeredNoteCount);

  ModMatrix.UpdateAllModLinks(aModConnections);
  ModMatrix.FastControlProcess;
  ModMatrix.SlowControlProcess;

  Lfo.StepResetB;
  StepSeqOne.StepResetB;
  StepSeqTwo.StepResetB;

  //ModMatrix.

  // Call Trigger on all components that need it....
  AmpEnv.Trigger(MidiVelocity / 127);
  FilterEnv.Trigger(MidiVelocity / 127);
  ModEnvA.Trigger(1);
  ModEnvB.Trigger(1);

  UpdateOscPitch;

  OneShotSampleOsc.Trigger(MidiNote, aSampleRegion, aSampleRegion.GetSample^);



  {
  case OscModule of
    oscNoteSampler:
    begin
      if (PitchTracking = TPitchTracking.Note) then
      begin
        if VoiceMode = TVoiceMode.Poly
          then PitchShift := fTriggerNote
          else PitchShift := GlobalModPoints.Source_MonophonicMidiNote;

        SamplePitch := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
        PitchShift := (PitchShift - SampleRegion.GetProperties^.RootNote) + round(PitchOne * 12) + PitchTwo + SamplePitch;
        OneShotSampleOsc.PitchShift := PitchShift;
      end else
      begin
        SamplePitch := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
        OneShotSampleOsc.PitchShift := CalcPitchShift(aSampleRegion.GetProperties^.RootNote, aSampleRegion.GetProperties^.RootNote, PitchOne, PitchTwo, SamplePitch);
      end;

      OneShotSampleOsc.Trigger(MidiNote, aSampleRegion, aSampleRegion.GetSample^);
    end;

    oscLoopSampler:
    begin
      LoopSampleOsc.Trigger(MidiNote, aSampleRegion, aSampleRegion.GetSample^);
    end
  else
    raise Exception.Create('type not handled.');
  end;
  }


  FilterOne.Reset;
  FilterTwo.Reset;
end;

procedure TLucidityVoice.Release;
begin
  if HasBeenReleased = false then
  begin
    AmpEnv.Release;
    FilterEnv.Release;
    ModEnvA.Release;
    ModEnvB.Release;
    HasBeenReleased := true;


    OneShotSampleOsc.Release;
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
  ModEnvA.Kill;
  ModEnvB.Kill;

  fIsActive := false;
  HasBeenReleased := false;
  HasBeenQuickReleased := false;
  AmpLevel := 0;

  // Important: Clear voice resources...
  CleanUp;
end;

procedure TLucidityVoice.CleanUp;
begin
  // CleanUp() clears references to other resouces and zeros some internal values.
  // It should be called whenever the voice becomes in-active.
  // NOTE: it's important to nil the fSampleGroup and fSampleRegion interface references
  // here. Lucidity uses interface reference count as a garbage collection device.
  fSampleGroup  := nil;
  fSampleRegion := nil;
  OneShotSampleOsc.Kill;
  LoopSampleOsc.Kill;
  GrainStretchOsc.Kill;

  if assigned(OnFinish) then OnFinish(self);
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
  PanX, VolX : single;
  PitchShift : single;
  CV : TModularVoltage;
  SamplePitch : single;
begin
  //GlobalProfiler.StartTimer('Control Rate Process');

  // update sample pan / volume offsets..
  assert(SampleRegion.GetProperties^.SamplePan >= -100);
  assert(SampleRegion.GetProperties^.SamplePan <= 100);
  PanX := (SampleRegion.GetProperties^.SamplePan + 100) / 200;
  VolX := DecibelsToVoltage(SampleRegion.GetProperties^.SampleVolume);
  SampleLevelOffsetA := VolX * Sqrt(1 - PanX);
  SampleLevelOffsetB := VolX * Sqrt(PanX);


  if VoiceMode = TVoiceMode.Poly then
  begin
    // NOTE: The goal of MIDI Note as a mod source is something similar to 1 volt per octave
    // pitch scaling in modular synths. I want filters to be able to track the keyboard.
  end else
  begin
    CV := GlobalModPoints.Source_MonophonicMidiNote / 12;
    ModPoints.MidiNote := ModularVoltageToAudioRange(cv);
  end;

  //=== Control rate step for all control rate modules ===
  LFO.FastControlProcess;
  AmpEnv.Step;
  FilterEnv.Step;
  ModEnvA.Step;
  ModEnvB.Step;
  StepSeqOne.Step;
  StepSeqTwo.Step;

  //=== Mod Matrix ====
  ModMatrix.FastControlProcess;

  //=== Control rate step for all audio rate modules ===
  OscVCA.FastControlProcess(AmpEnv.Value);
  OscPanner.FastControlProcess;


  {
  case OscModule of
    oscNoteSampler:
    begin
      if (PitchTracking = TPitchTracking.Note) then
      begin
        if VoiceMode = TVoiceMode.Poly
          then PitchShift := fTriggerNote
          else PitchShift := GlobalModPoints.Source_MonophonicMidiNote;
        SamplePitch := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
        PitchShift := GlobalModPoints.Source_MidiPitchBendST + (PitchShift - SampleRegion.GetProperties^.RootNote) + round(PitchOne * 12) + PitchTwo + SamplePitch;
        OneShotSampleOsc.PitchShift := PitchShift;
      end else
      begin
        SamplePitch := SampleRegion.GetProperties^.SampleTune + (SampleRegion.GetProperties^.SampleFine * 0.01);
        OneShotSampleOsc.PitchShift := GlobalModPoints.Source_MidiPitchBendST + CalcPitchShift(SampleRegion.GetProperties^.RootNote, SampleRegion.GetProperties^.RootNote, PitchOne, PitchTwo, SamplePitch);
      end;

      OneShotSampleOsc.FastControlProcess;
    end;

    oscLoopSampler:
    begin
      LoopSampleOsc.FastControlProcess;
    end
  else
    raise Exception.Create('type not handled.');
  end;
  }

  FilterOne.FastControlProcess;
  FilterTwo.FastControlProcess;


  //GlobalProfiler.StopTimer;
end;

procedure TLucidityVoice.SlowControlProcess;
begin
  UpdateOscPitch;

  LFO.SlowControlProcess;
  ModMatrix.SlowControlProcess;
  OneShotSampleOsc.SlowControlProcess;

  {
  case OscModule of
    oscNoteSampler:
    oscLoopSampler: LoopSampleOsc.SlowControlProcess;
  else
    raise Exception.Create('type not handled.');
  end;
  }
end;



procedure TLucidityVoice.AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames: integer);
var
  c1: Integer;
  SampleOscX1, SampleOscX2 : single;
  MixX1, MixX2 : single;
  pxA, pxB : PSingle;
begin
  //GlobalProfiler.StartTimer('Audio Rate Process');

  assert(IsActive);

  pxA := @BufferA[0];
  pxB := @BufferB[0];

  for c1 := 0 to SampleFrames-1 do
  begin
    OneShotSampleOsc.AudioRateStep(SampleOscX1, SampleOscX2);
    {
    case OscModule of
      oscNoteSampler: OneShotSampleOsc.AudioRateStep(SampleOscX1, SampleOscX2);
      oscLoopSampler: LoopSampleOsc.AudioRateStep(SampleOscX1, SampleOscX2);
    else
      raise Exception.Create('type not handled.');
    end;
    }

    MixX1 := SampleOscX1;
    MixX2 := SampleOscX2;

    FilterOne.AudioRateStep(MixX1, MixX2);
    FilterTwo.AudioRateStep(MixX1, MixX2);
    //TODO: OscVCA and OscPanner could be combined into one module.
    OscVCA.AudioRateStep(MixX1, MixX2);
    OscPanner.AudioRateStep(MixX1, MixX2);

    pxA^ := MixX1 * SampleLevelOffsetA;
    pxB^ := MixX2 * SampleLevelOffsetB;

    inc(pxA);
    inc(pxB);
  end;

  pxA := @BufferA[0];
  pxB := @BufferB[0];

  OutputMixer.AudioRateProcess(pxA, pxB, Outputs, SampleFrames);

  AmpLevel := AmpEnv.Value;

  if (AmpEnv.Stage = TEnvelopeStage.esOff) then
  begin
    fIsActive := false;
    // Important: Clear voice resources...
    CleanUp;
  end;


  //GlobalProfiler_StopTimer('AudioRateProcess');
  //GlobalProfiler.StopTimer;

end;

procedure TLucidityVoice.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
begin
  FeedBackData.StepSeq1CurStep := StepSeqOne.CurrentStep;
  FeedBackData.StepSeq2CurStep := StepSeqTwo.CurrentStep;

  if FeedbackData.FocusedRegion = self.SampleRegion then
  begin
    FeedbackData.SampleBounds.ShowRealTimeMarkers := true;
    OneShotSampleOsc.GetGuiFeedBack(FeedbackData);

    {
    case OscModule of
      oscNoteSampler:
      oscLoopSampler: LoopSampleOsc.GetGuiFeedBack(FeedbackData);
    else
      raise Exception.Create('type not handled.');
    end;
    }
  end else
  begin
    FeedbackData.SampleBounds.ShowRealTimeMarkers := false;
  end;
end;





end.

