unit uLucidityEnums;

interface

uses
  eeEnumHelper;


//==== Filter Types ====
{$SCOPEDENUMS OFF}
type
  TFilterType = (
    ftNone,
    ftLofiA, ftRingModA, ftCombA,
    ft2PoleLowPass,
    ft2PoleBandPass,
    ft2PoleHighPass,
    ft4PoleLowPass,
    ft4PoleBandPass,
    ft4PoleHighPass
  );

  TFilterTypeHelper = class(TEnumHelper<TFilterType>)
  public
    class function ToFullGuiString(aEnum : TFilterType):string; override;
    class function ToShortGuiString(aEnum : TFilterType):string; override;
  end;
{$SCOPEDENUMS ON}

type
  TFilterRouting = (Serial, Parallel);
  TFilterRoutingHelper = class(TEnumHelper<TFilterRouting>)
  public
    class function ToFullGuiString(aEnum : TFilterRouting):string; override;
    class function ToShortGuiString(aEnum : TFilterRouting):string; override;
  end;


  TStepSequencerLength = (Two, Three, Four, Five, Six, Seven, Eight, Twelve, Sixteen);
  TStepSequencerLengthHelper = class(TEnumHelper<TStepSequencerLength>);

  function StepSequencerLengthToInteger(const x : TStepSequencerLength):integer;

type
  TStepSequencerDirection = (Forwards, Backwards, PingPong, Random);
  TStepSequencerDirectionHelper = class(TEnumHelper<TStepSequencerDirection>);

  TLfoShape = (SawUp, SawDown, Square, Triangle, Sine, RandomStepped, RandomSmooth, AttackRelease, AttackDecay, Cycle);
  TLfoShapeHelper = class(TEnumHelper<TLfoShape>)
  public
    class function ToFullGuiString(aEnum : TLfoShape):string; override;
    class function ToShortGuiString(aEnum : TLfoShape):string; override;
  end;

  // TODO: delete TLfoMode
  TLfoMode = (Free, Sync, Tempo);
  TLfoModeHelper = class(TEnumHelper<TLfoMode>);

  TLfoFreqMode = (Hertz, Sync4, Sync8, Sync16, Sync32, Sync64);
  TLfoFreqModeHelper = class(TEnumHelper<TLfoFreqMode>)
  public
    class function ToFullGuiString(aEnum : TLfoFreqMode):string; override;
    class function ToShortGuiString(aEnum : TLfoFreqMode):string; override;
  end;

  //TODO:HIGH delete this.
  TLfoRange = (x1, x2, x4, x8);
  TLfoRangeHelper = class(TEnumHelper<TLfoRange>)
  public
    class function ToFullGuiString(aEnum : TLfoRange):string; override;
    class function ToShortGuiString(aEnum : TLfoRange):string; override;
  end;

  TLfoPhaseReset = (ph0, ph90, ph180, ph270);
  TLfoPhaseResetHelper = class(TEnumHelper<TLfoPhaseReset>);

  TModEnvMode = (TriggerAR, TriggerASR, TriggerCycle);
  TModEnvModeHelper = class(TEnumHelper<TModEnvMode>)
  public
    class function ToFullGuiString(aEnum : TModEnvMode):string; override;
    class function ToShortGuiString(aEnum : TModEnvMode):string; override;
  end;

  TModSourcePolarity = (Unipolar, Bipolar);

  TModSource = (
    None,
    //Unipolar sources... //TODO:MED all sources are unipolar. These should probably be renamed.
    Midi_Note_Unipolar,
    Midi_Velocity_Unipolar,
    Midi_PitchBend_Unipolar,
    Midi_ModWheel_Unipolar,
    Midi_Toggle_Unipolar,
    AmpEnv_Unipolar,
    ModEnv_Unipolar,
    Lfo1_Unipolar,
    Lfo2_Unipolar,
    StepSeq1_Unipolar,
    StepSeq2_Unipolar,
    PadX1_Unipolar,
    PadY1_Unipolar,
    PadX2_Unipolar,
    PadY2_Unipolar,
    PadX3_Unipolar,
    PadY3_Unipolar,
    PadX4_Unipolar,
    PadY4_Unipolar
  );

  {
  MIDI sources
  - MIDI note toggle. first note 0, second note 1.
  - Midi CC
  }


  TModSourceHelper = class(TEnumHelper<TModSource>)
  public
    class function ToFullGuiString(aEnum : TModSource):string; override;
    class function ToShortGuiString(aEnum : TModSource):string; override;
  end;

  TModDest = (
    None,
    ModOutA,
    ModOutB,
    VoiceAmplitude,
    VoicePan,
    Lfo1_Rate,
    Lfo1_ParB,
    Lfo2_Rate,
    Lfo2_ParB,
    Filter1_Par1,
    Filter1_Par2,
    Filter1_Par3,
    Filter1_Par4,
    Filter2_Par1,
    Filter2_Par2,
    Filter2_Par3,
    Filter2_Par4,
    SampleStart,
    SampleEnd,
    LoopStart,
    LoopEnd
  );
  TModDestHelper = class(TEnumHelper<TModDest>)
  public
    class function ToFullGuiString(aEnum : TModDest):string; override;
    class function ToShortGuiString(aEnum : TModDest):string; override;
  end;

const
  ClockID_Lfo1 = 1;
  ClockID_Lfo2 = 2;
  ClockID_Lfo3 = 3;
  ClockID_Lfo4 = 4;
  ClockID_SampleLoop = 5;


type
  TClockSource = (
    None,
    Lfo1,
    Lfo2,
    SampleLoop
  );
  TClockSourceHelper = class(TEnumHelper<TClockSource>);

  TSequencerClock = (
    Div_1,
    Div_2,
    Div_4,
    Div_8,
    Div_12,
    Div_16,
    Div_32,
    Div_64,
    Div_128,
    Div_1T,
    Div_2T,
    Div_4T,
    Div_8T,
    Div_12T,
    Div_16T,
    Div_32T,
    Div_64T,
    Div_128T,
    Note,
    Lfo1,
    Lfo2,
    SampleLoop
  );
  TSequencerClockHelper = class(TEnumHelper<TSequencerClock>)
  public
    class function ToFullGuiString(aEnum : TSequencerClock):string; override;
    class function ToShortGuiString(aEnum : TSequencerClock):string; override;
  end;

  TVoiceMode = (Poly, Mono, Legato, Latch);
  TVoiceModeHelper = class(TEnumHelper<TVoiceMode>)
  public
  end;

  TSamplePlaybackType = (NoteSampler, LoopSampler, OneShotSampler, GrainStretch, WaveOsc);
  TSamplePlaybackTypeHelper = class(TEnumHelper<TSamplePlaybackType>)
  public
    class function ToFullGuiString(aEnum : TSamplePlaybackType):string; override;
    class function ToShortGuiString(aEnum : TSamplePlaybackType):string; override;
  end;

  TPitchTracking = (Note, BPM, Off);
  TPitchTrackingHelper = class(TEnumHelper<TPitchTracking>)
  public
    class function ToFullGuiString(aEnum : TPitchTracking):string; override;
    class function ToShortGuiString(aEnum : TPitchTracking):string; override;
  end;


  //  This needs to be "Sample Loop Bounds"
  TSamplerLoopBounds = (LoopSample, LoopPoints);
  TSamplerLoopBoundsHelper = class(TEnumHelper<TSamplerLoopBounds>)
  public
    class function ToFullGuiString(aEnum : TSamplerLoopBounds):string; override;
    class function ToShortGuiString(aEnum : TSamplerLoopBounds):string; override;
  end;

  // Trigger Mode.
  // no_loop: no looping will be performed. Sample will play straight from start to end, or until note off, whatever reaches first.
  // one_shot: sample will play from start to end, ignoring note off. Looping is disabled.
  //           This mode is engaged automatically if the count opcode isn't defined.
  // loop_continuous: once the player reaches sample loop point, the loop will play until note expiration.
  // loop_sustain: the player will play the loop while the note is held, by keeping it depressed or by using
  //               the sustain pedal (CC64). The rest of the sample will play after note release.
  TKeyGroupTriggerMode = (LoopContinuous, LoopSustain, LoopOff, OneShot);
  TKeyGroupTriggerModeHelper = class(TEnumHelper<TKeyGroupTriggerMode>)
  public
    class function ToFullGuiString(aEnum : TKeyGroupTriggerMode):string; override;
    class function ToShortGuiString(aEnum : TKeyGroupTriggerMode):string; override;
  end;




  TGrainStretchLoopMode = (LoopOff, LoopOn);
  TGrainStretchLoopModeHelper = class(TEnumHelper<TGrainStretchLoopMode>)
  public
    class function ToFullGuiString(aEnum : TGrainStretchLoopMode):string; override;
    class function ToShortGuiString(aEnum : TGrainStretchLoopMode):string; override;
  end;

  TEnvVelocityDepth = (Vel100, Vel80, Vel60, Vel40, Vel20, VelOff);
  TEnvVelocityDepthHelper = class(TEnumHelper<TEnvVelocityDepth>)
  public
    class function ToFullGuiString(aEnum : TEnvVelocityDepth):string; override;
    class function ToShortGuiString(aEnum : TEnvVelocityDepth):string; override;
  end;

  TLowerTabOptions = (TabMain, TabSeq1, TabSeq2, TabSetup);

  TKeyCommand = (
    ContextUp,
    ContextDown,
    ContextLeft,
    ContextRight,
    PageUp,
    PageDown,
    SelectUp,
    SelectDown,
    ReplaceLoad
  );
  TKeyCommandHelper = class(TEnumHelper<TKeyCommand>);

  TProgramFormat = (Lucidity, Sfz);


  TMainGuiLayout = (Default, SampleZoom, MapEdit);


implementation

uses
  SysUtils;

function StepSequencerLengthToInteger(const x : TStepSequencerLength):integer;
begin
  case x of
    TStepSequencerLength.Two:     result := 2;
    TStepSequencerLength.Three:   result := 3;
    TStepSequencerLength.Four:    result := 4;
    TStepSequencerLength.Five:    result := 5;
    TStepSequencerLength.Six:     result := 6;
    TStepSequencerLength.Seven:   result := 7;
    TStepSequencerLength.Eight:   result := 8;
    TStepSequencerLength.Twelve:  result := 12;
    TStepSequencerLength.Sixteen: result := 16;
  else
    raise Exception.Create('Unexpected StepSequencerLength value.');
  end;
end;


{ TModSourceHelper }

class function TModSourceHelper.ToFullGuiString(aEnum: TModSource): string;
begin
  case aEnum of
    TModSource.None:                    result := 'None';
    TModSource.Midi_Note_Unipolar:      result := 'MIDI Note';
    TModSource.Midi_Velocity_Unipolar:  result := 'MIDI Velocity';
    TModSource.Midi_PitchBend_Unipolar: result := 'MIDI Pitchbend';
    TModSource.Midi_ModWheel_Unipolar:  result := 'MIDI Modwheel';
    TModSource.Midi_Toggle_Unipolar:    result := 'MIDI Toggle';
    TModSource.AmpEnv_Unipolar:         result := 'Amp Envelope';
    TModSource.ModEnv_Unipolar:      result := 'Mod Envelope';
    TModSource.Lfo1_UniPolar:           result := 'LFO 1';
    TModSource.Lfo2_UniPolar:           result := 'LFO 2';
    TModSource.StepSeq1_Unipolar:       result := 'Step Sequencer 1';
    TModSource.StepSeq2_Unipolar:       result := 'Step Sequencer 2';
    // unipolar sources...
    TModSource.PadX1_Unipolar:          result := 'Pad X1';
    TModSource.PadY1_Unipolar:          result := 'Pad Y1';
    TModSource.PadX2_Unipolar:          result := 'Pad X2';
    TModSource.PadY2_Unipolar:          result := 'Pad Y2';
    TModSource.PadX3_Unipolar:          result := 'Pad X3';
    TModSource.PadY3_Unipolar:          result := 'Pad Y3';
    TModSource.PadX4_Unipolar:          result := 'Pad X4';
    TModSource.PadY4_Unipolar:          result := 'Pad Y4';
  else
    result := inherited;
  end;
end;

class function TModSourceHelper.ToShortGuiString(aEnum: TModSource): string;
begin
  case aEnum of
    TModSource.None:                    result := '-';
    TModSource.Midi_Note_Unipolar:      result := 'Note';
    TModSource.Midi_Velocity_Unipolar:  result := 'Vel';
    TModSource.Midi_PitchBend_Unipolar: result := 'P.Bend';
    TModSource.Midi_ModWheel_Unipolar:  result := 'Mod Whl';
    TModSource.Midi_Toggle_Unipolar:    result := 'Toggle';
    TModSource.AmpEnv_Unipolar:         result := 'Amp Env';
    TModSource.ModEnv_Unipolar:      result := 'Mod Env';
    TModSource.Lfo1_UniPolar:           result := 'LFO1';
    TModSource.Lfo2_UniPolar:           result := 'LFO2';
    TModSource.StepSeq1_Unipolar:       result := 'Seq1';
    TModSource.StepSeq2_Unipolar:       result := 'Seq2';
    // unipolar sources...
    TModSource.PadX1_Unipolar:          result := 'Pad X1';
    TModSource.PadY1_Unipolar:          result := 'Pad Y1';
    TModSource.PadX2_Unipolar:          result := 'Pad X2';
    TModSource.PadY2_Unipolar:          result := 'Pad Y2';
    TModSource.PadX3_Unipolar:          result := 'Pad X3';
    TModSource.PadY3_Unipolar:          result := 'Pad Y3';
    TModSource.PadX4_Unipolar:          result := 'Pad X4';
    TModSource.PadY4_Unipolar:          result := 'Pad Y4';
  else
    result := inherited;
  end;

end;

{ TLfoShapeHelper }



{ TLfoShapeHelper }

class function TLfoShapeHelper.ToFullGuiString(aEnum: TLfoShape): string;
begin
  case aEnum of
    TLfoShape.SawUp:         result := 'Saw (Up)';
    TLfoShape.SawDown:       result := 'Ramp (Down)';
    TLfoShape.Square:        result := 'Square';
    TLfoShape.Triangle:      result := 'Triangle';
    TLfoShape.Sine:          result := 'Sine';
    TLfoShape.RandomSmooth:  result := 'Random Smooth';
    TLfoShape.RandomStepped: result := 'Random Stepped';
    TLfoShape.AttackDecay:   result := 'Attack-Decay';
    TLfoShape.AttackRelease: result := 'Attack-Release';
    TLfoShape.Cycle:         result := 'Attack-Decay Cycle';
  else
    result := inherited;
  end;
end;

class function TLfoShapeHelper.ToShortGuiString(aEnum: TLfoShape): string;
begin
  case aEnum of
    TLfoShape.SawUp:         result := 'Saw';
    TLfoShape.SawDown:       result := 'Ramp';
    TLfoShape.Square:        result := 'Sqr';
    TLfoShape.Triangle:      result := 'Tri';
    TLfoShape.Sine:          result := 'Sine';
    TLfoShape.RandomSmooth:  result := 'R.Smth';
    TLfoShape.RandomStepped: result := 'R.Stp';
    TLfoShape.AttackDecay:   result := 'AD';
    TLfoShape.AttackRelease: result := 'AR';
    TLfoShape.Cycle:         result := 'AD Cycle';
  else
    result := inherited;
  end;
end;

{ TModDestHelper }

class function TModDestHelper.ToFullGuiString(aEnum: TModDest): string;
begin
  case aEnum of
    TModDest.None:                      result := 'None';
    TModDest.VoiceAmplitude:            result := 'Voice Amplitude';
    TModDest.VoicePan:                  result := 'Voice Pan';
    TModDest.Lfo1_Rate:                 result := 'Lfo 1 Rate';
    TModDest.Lfo1_ParB:                 result := 'Lfo 1 ParB';
    TModDest.Lfo2_Rate:                 result := 'Lfo 2 Rate';
    TModDest.Lfo2_ParB:                 result := 'Lfo 2 ParB';
    TModDest.Filter1_Par1:              result := 'Filter 1 Par 1';
    TModDest.Filter1_Par2:              result := 'Filter 1 Par 2';
    TModDest.Filter1_Par3:              result := 'Filter 1 Par 3';
    TModDest.Filter1_Par4:              result := 'Filter 1 Par 4';
    TModDest.Filter2_Par1:              result := 'Filter 2 Par 1';
    TModDest.Filter2_Par2:              result := 'Filter 2 Par 2';
    TModDest.Filter2_Par3:              result := 'Filter 2 Par 3';
    TModDest.Filter2_Par4:              result := 'Filter 2 Par 4';
    TModDest.SampleStart:               result := 'Sample Start';
    TModDest.SampleEnd:                 result := 'Sample End';
    TModDest.LoopStart:                 result := 'Loop Start';
    TModDest.LoopEnd:                   result := 'Loop End';
    TModDest.ModOutA:                   result := 'Mod Out A';
    TModDest.ModOutB:                   result := 'Mod Out B';
  else
    result := inherited;
  end;
end;

class function TModDestHelper.ToShortGuiString(aEnum: TModDest): string;
begin
  case aEnum of
    TModDest.None:                      result := '-';
    TModDest.VoiceAmplitude:            result := 'Voice Amplitude';
    TModDest.VoicePan:                  result := 'Voice Pan';
    TModDest.Lfo1_Rate:                 result := 'Lfo 1 Rate';
    TModDest.Lfo1_ParB:                 result := 'Lfo 1 ParB';
    TModDest.Lfo2_Rate:                 result := 'Lfo 2 Rate';
    TModDest.Lfo2_ParB:                 result := 'Lfo 2 ParB';
    TModDest.Filter1_Par1:              result := 'Filter 1 Par 1';
    TModDest.Filter1_Par2:              result := 'Filter 1 Par 2';
    TModDest.Filter1_Par3:              result := 'Filter 1 Par 3';
    TModDest.Filter1_Par4:              result := 'Filter 1 Par 4';
    TModDest.Filter2_Par1:              result := 'Filter 2 Par 1';
    TModDest.Filter2_Par2:              result := 'Filter 2 Par 2';
    TModDest.Filter2_Par3:              result := 'Filter 2 Par 3';
    TModDest.Filter2_Par4:              result := 'Filter 2 Par 4';
    TModDest.SampleStart:               result := 'Sample Start';
    TModDest.SampleEnd:                 result := 'Sample End';
    TModDest.LoopStart:                 result := 'Loop Start';
    TModDest.LoopEnd:                   result := 'Loop End';

    TModDest.ModOutA:                   result := 'Mod Out A';
    TModDest.ModOutB:                   result := 'Mod Out B';
  else
    result := inherited;
  end;
end;

{ TSequencerClockHelper }

class function TSequencerClockHelper.ToFullGuiString(aEnum: TSequencerClock): string;
begin
  case aEnum of
    TSequencerClock.Div_1:      result := '1/1';
    TSequencerClock.Div_2:      result := '1/2';
    TSequencerClock.Div_4:      result := '1/4';
    TSequencerClock.Div_8:      result := '1/8';
    TSequencerClock.Div_12:     result := '1/12';
    TSequencerClock.Div_16:     result := '1/16';
    TSequencerClock.Div_32:     result := '1/32';
    TSequencerClock.Div_64:     result := '1/64';
    TSequencerClock.Div_128:    result := '1/128';
    TSequencerClock.Div_1T:     result := '1/1 Triplet';
    TSequencerClock.Div_2T:     result := '1/2 Triplet';
    TSequencerClock.Div_4T:     result := '1/4 Triplet';
    TSequencerClock.Div_8T:     result := '1/8 Triplet';
    TSequencerClock.Div_12T:    result := '1/12 Triplet';
    TSequencerClock.Div_16T:    result := '1/16 Triplet';
    TSequencerClock.Div_32T:    result := '1/32 Triplet';
    TSequencerClock.Div_64T:    result := '1/64 Triplet';
    TSequencerClock.Div_128T:   result := '1/128 Triplet';
    TSequencerClock.Note:       result := 'MIDI Note';
    TSequencerClock.Lfo1:       result := 'LFO 1';
    TSequencerClock.Lfo2:       result := 'LFO 2';
    TSequencerClock.SampleLoop: result := 'Sample Loop';
  else
    raise Exception.Create('type not handled.');
  end;
end;

class function TSequencerClockHelper.ToShortGuiString(aEnum: TSequencerClock): string;
begin
  case aEnum of
    TSequencerClock.Div_1:     result := '1/1';
    TSequencerClock.Div_2:     result := '1/2';
    TSequencerClock.Div_4:     result := '1/4';
    TSequencerClock.Div_8:     result := '1/8';
    TSequencerClock.Div_12:    result := '1/12';
    TSequencerClock.Div_16:    result := '1/16';
    TSequencerClock.Div_32:    result := '1/32';
    TSequencerClock.Div_64:    result := '1/64';
    TSequencerClock.Div_128:   result := '1/128';
    TSequencerClock.Div_1T:    result := '1/1 T';
    TSequencerClock.Div_2T:    result := '1/2 T';
    TSequencerClock.Div_4T:    result := '1/4 T';
    TSequencerClock.Div_8T:    result := '1/8 T';
    TSequencerClock.Div_12T:   result := '1/12 T';
    TSequencerClock.Div_16T:   result := '1/16 T';
    TSequencerClock.Div_32T:   result := '1/32 T';
    TSequencerClock.Div_64T:   result := '1/64 T';
    TSequencerClock.Div_128T:  result := '1/128 T';
    TSequencerClock.Note:      result := 'Note';
    TSequencerClock.Lfo1:      result := 'LFO 1';
    TSequencerClock.Lfo2:      result := 'LFO 2';
    TSequencerClock.SampleLoop: result := 'Sample Loop';
  else
    raise Exception.Create('type not handled.');
  end;
end;

{ TFilterTypeHelper }

class function TFilterTypeHelper.ToFullGuiString(aEnum: TFilterType): string;
begin
  case aEnum of
    ftNone:          result := 'None';
    ftLofiA:         result := 'LoFi';
    ftRingModA:      result := 'Ring Mod';
    //ftDistA:         result := 'Distortion';
    ftCombA:         result := 'Comb';
    ft2PoleLowPass:  result := '2 Pole Low Pass';
    ft2PoleBandPass: result := '2 Pole Band Pass';
    ft2PoleHighPass: result := '2 Pole High Pass';
    ft4PoleLowPass:  result := '4 Pole Low Pass';
    ft4PoleBandPass: result := '4 Pole Band Pass';
    ft4PoleHighPass: result := '4 Pole High Pass';
  else
    raise Exception.Create('type not handled.');
  end;
end;

class function TFilterTypeHelper.ToShortGuiString(aEnum: TFilterType): string;
begin
  case aEnum of
    ftNone:             result := '-';
    ftLofiA:            result := 'LoFi';
    ftRingModA:         result := 'Ring Mod';
    //ftDistA:            result := 'Distortion';
    ftCombA:            result := 'Comb';
    ft2PoleLowPass:     result := '2xLP';
    ft2PoleBandPass:    result := '2xBP';
    ft2PoleHighPass:    result := '2xHP';
    ft4PoleLowPass:     result := '4xLP';
    ft4PoleBandPass:    result := '4xBP';
    ft4PoleHighPass:    result := '4xHP';
  else
    raise Exception.Create('type not handled.');
  end;
end;

{ TOneShotSamplerLoopModeHelper }

class function TSamplerLoopBoundsHelper.ToFullGuiString(aEnum: TSamplerLoopBounds): string;
begin
  case aEnum of
    TSamplerLoopBounds.LoopSample:  result := 'Sample';
    TSamplerLoopBounds.LoopPoints:  result := 'Loop';
  else
    raise Exception.Create('type not handled.');
  end;
end;

class function TSamplerLoopBoundsHelper.ToShortGuiString(aEnum: TSamplerLoopBounds): string;
begin
  case aEnum of
    TSamplerLoopBounds.LoopSample:  result := 'Sample';
    TSamplerLoopBounds.LoopPoints:  result := 'Loop';
  else
    raise Exception.Create('type not handled.');
  end;
end;


{ TGrainStretchLoopModeHelper }

class function TGrainStretchLoopModeHelper.ToFullGuiString(aEnum: TGrainStretchLoopMode): string;
begin
  case aEnum of
    TGrainStretchLoopMode.LoopOff: result := 'Loop Off';
    TGrainStretchLoopMode.LoopOn:  result := 'Loop On';
  else
    raise Exception.Create('type not handled.');
  end;
end;

class function TGrainStretchLoopModeHelper.ToShortGuiString(aEnum: TGrainStretchLoopMode): string;
begin
  case aEnum of
    TGrainStretchLoopMode.LoopOff: result := 'Off';
    TGrainStretchLoopMode.LoopOn:  result := 'On';
  else
    raise Exception.Create('type not handled.');
  end;
end;

{ TSamplePlaybackTypeHelper }

class function TSamplePlaybackTypeHelper.ToFullGuiString(aEnum: TSamplePlaybackType): string;
begin
  case aEnum of
    TSamplePlaybackType.NoteSampler:    result := 'Note';
    TSamplePlaybackType.LoopSampler:    result := 'Loop';
    TSamplePlaybackType.OneShotSampler: result := 'One Shot ';
    TSamplePlaybackType.GrainStretch:   result := 'Grain Stretch';
    TSamplePlaybackType.WaveOsc:        result := 'Wave Osc';
  else
    raise Exception.Create('type not handled.');
  end;
end;

class function TSamplePlaybackTypeHelper.ToShortGuiString(aEnum: TSamplePlaybackType): string;
begin
  case aEnum of
    TSamplePlaybackType.NoteSampler:    result := 'Note';
    TSamplePlaybackType.LoopSampler:    result := 'Loop';
    TSamplePlaybackType.OneShotSampler: result := 'One Shot ';
    TSamplePlaybackType.GrainStretch:   result := 'Grain Stretch';
    TSamplePlaybackType.WaveOsc:        result := 'Wave Osc';
  else
    raise Exception.Create('type not handled.');
  end;
end;

{ TModEnvModeHelper }

class function TModEnvModeHelper.ToFullGuiString(aEnum: TModEnvMode): string;
begin
  case aEnum of
    TModEnvMode.TriggerAR:    result := 'AR';
    TModEnvMode.TriggerASR:   result := 'ASR';
    TModEnvMode.TriggerCycle: result := 'Cycle';
  else
    raise Exception.Create('type not handled.');
  end;
end;

class function TModEnvModeHelper.ToShortGuiString(aEnum: TModEnvMode): string;
begin
  case aEnum of
    TModEnvMode.TriggerAR:    result := 'AR';
    TModEnvMode.TriggerASR:   result := 'ASR';
    TModEnvMode.TriggerCycle: result := 'Cycle';
  else
    raise Exception.Create('type not handled.');
  end;
end;

{ TEnvVelocityDepthHelper }

class function TEnvVelocityDepthHelper.ToFullGuiString(aEnum: TEnvVelocityDepth): string;
begin
  case aEnum of
    TEnvVelocityDepth.VelOff: result := 'Velocity Off';
    TEnvVelocityDepth.Vel20:  result := 'Velocity 20%';
    TEnvVelocityDepth.Vel40:  result := 'Velocity 40%';
    TEnvVelocityDepth.Vel60:  result := 'Velocity 60%';
    TEnvVelocityDepth.Vel80:  result := 'Velocity 80%';
    TEnvVelocityDepth.Vel100: result := 'Velocity 100%';
  else
    raise Exception.Create('Type not handled.');
  end;

end;

class function TEnvVelocityDepthHelper.ToShortGuiString(aEnum: TEnvVelocityDepth): string;
begin
  case aEnum of
    TEnvVelocityDepth.VelOff: result := 'V:Off';
    TEnvVelocityDepth.Vel20:  result := 'V:20%';
    TEnvVelocityDepth.Vel40:  result := 'V:40%';
    TEnvVelocityDepth.Vel60:  result := 'V:60%';
    TEnvVelocityDepth.Vel80:  result := 'V:80%';
    TEnvVelocityDepth.Vel100: result := 'V:100%';
  else
    raise Exception.Create('Type not handled.');
  end;
end;



{ TKeyGroupTriggerModeHelper }

class function TKeyGroupTriggerModeHelper.ToFullGuiString(aEnum: TKeyGroupTriggerMode): string;
begin
  case aEnum of
    TKeyGroupTriggerMode.LoopOff:         result := 'Note On (Loop Off)';
    TKeyGroupTriggerMode.LoopContinuous:  result := 'Loop Continuous';
    TKeyGroupTriggerMode.LoopSustain:     result := 'Loop Until Release';
    TKeyGroupTriggerMode.OneShot:         result := 'One Shot';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

class function TKeyGroupTriggerModeHelper.ToShortGuiString(aEnum: TKeyGroupTriggerMode): string;
begin
  case aEnum of
    TKeyGroupTriggerMode.LoopOff:         result := 'Note On';
    TKeyGroupTriggerMode.LoopContinuous:  result := 'Loop ';
    TKeyGroupTriggerMode.LoopSustain:     result := 'Loop Rls';
    TKeyGroupTriggerMode.OneShot:         result := 'One Shot';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

{ TPitchTrackingHelper }

class function TPitchTrackingHelper.ToFullGuiString(aEnum: TPitchTracking): string;
begin
  case aEnum of
    TPitchTracking.Note: result := 'Note';
    TPitchTracking.BPM:  result := 'BPM (For Tempo Synced Loops)';
    TPitchTracking.Off:  result := 'Off';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

class function TPitchTrackingHelper.ToShortGuiString(aEnum: TPitchTracking): string;
begin
  case aEnum of
    TPitchTracking.Note: result := 'Note';
    TPitchTracking.BPM:  result := 'BPM';
    TPitchTracking.Off:  result := 'Off';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

{ TLfoFreqModeHelper }

class function TLfoFreqModeHelper.ToFullGuiString(aEnum: TLfoFreqMode): string;
begin
  case aEnum of
    TLfoFreqMode.Hertz:   result := 'Hertz';
    TLfoFreqMode.Sync4:   result := 'Sync 1/4';
    TLfoFreqMode.Sync8:   result := 'Sync 1/8';
    TLfoFreqMode.Sync16:  result := 'Sync 1/16';
    TLfoFreqMode.Sync32:  result := 'Sync 1/32';
    TLfoFreqMode.Sync64:  result := 'Sync 1/64';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

class function TLfoFreqModeHelper.ToShortGuiString(aEnum: TLfoFreqMode): string;
begin
  case aEnum of
    TLfoFreqMode.Hertz:   result := 'Hz';
    TLfoFreqMode.Sync4:   result := '1/4';
    TLfoFreqMode.Sync8:   result := '1/8';
    TLfoFreqMode.Sync16:  result := '1/16';
    TLfoFreqMode.Sync32:  result := '1/32';
    TLfoFreqMode.Sync64:  result := '1/64';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

{ TFilterRoutingHelper }

class function TFilterRoutingHelper.ToFullGuiString(aEnum: TFilterRouting): string;
begin
  case aEnum of
    TFilterRouting.Serial:     result := 'Serial';
    TFilterRouting.Parallel:   result := 'Parallel';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

class function TFilterRoutingHelper.ToShortGuiString(aEnum: TFilterRouting): string;
begin
  case aEnum of
    TFilterRouting.Serial:     result := 'Serial';
    TFilterRouting.Parallel:   result := 'Paral';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

{ TLfoRangeHelper }

class function TLfoRangeHelper.ToFullGuiString(aEnum: TLfoRange): string;
begin
  case aEnum of
    TLfoRange.x1: result := 'Max Time x 1';
    TLfoRange.x2: result := 'Max Time x 2';
    TLfoRange.x4: result := 'Max Time x 4';
    TLfoRange.x8: result := 'Max Time x 8';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

class function TLfoRangeHelper.ToShortGuiString(aEnum: TLfoRange): string;
begin
  case aEnum of
    TLfoRange.x1: result := 'x1';
    TLfoRange.x2: result := 'x2';
    TLfoRange.x4: result := 'x4';
    TLfoRange.x8: result := 'x8';
  else
    raise Exception.Create('Type not handled.');
  end;
end;





end.



