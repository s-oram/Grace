unit soLucidityVoiceParameterWrapper;

interface

uses
  LucidityModConnections,
  uConstants, uLucidityEnums, soModMatrix,
  uLucidityKeyGroupInterface,
  soLucidityVoice;

type
  //  forward declarations
  TLucidityVoiceParameterWrapper = class;
  //============================================================================

  TUpdateActionProcedure = reference to procedure(v:PLucidityVoice);

  IVoicePar = interface
    ['{FCE3B1A1-F6C1-480F-A69F-5E4ABDAE3F78}']
    function GetObject:TLucidityVoiceParameterWrapper;
  end;

  TLucidityVoiceParameterWrapper = class
  private
    fOscShape: single;
    fOscPulseWidth: single;
    fSamplePlaybackType: TSamplePlaybackType;
    fVoiceGain: single;
    fVoicePan: single;
    fVoicePitchOne: single;
    fVoicePitchTwo: single;
    fFilter1Type: TFilterType;
    fFilter2Type: TFilterType;
    fFilter1Par1: single;
    fFilter1Par2: single;
    fFilter1Par3: single;
    fFilter2Par1: single;
    fFilter2Par2: single;
    fFilter2Par3: single;
    fAmpAttack: single;
    fAmpHold: single;
    fAmpDecay: single;
    fAmpSustain: single;
    fAmpRelease: single;
    fFilterAttack: single;
    fFilterHold: single;
    fFilterDecay: single;
    fFilterSustain: single;
    fFilterRelease: single;
    fLfoShape1: TLfoShape;
    fLfoShape2: TLfoShape;
    fLfoShape3: TLfoShape;
    fLfoShape4: TLfoShape;
    fLfoRate1: single;
    fLfoRate2: single;
    fLfoRate3: single;
    fLfoRate4: single;
    fLfoResetPhase1: TLfoPhaseReset;
    fLfoResetPhase2: TLfoPhaseReset;
    fLfoResetPhase3: TLfoPhaseReset;
    fLfoResetPhase4: TLfoPhaseReset;
    fFilter1ModDepth1: single;
    fFilter1ModDepth2: single;
    fFilter2ModDepth1: single;
    fFilter2ModDepth2: single;
    fSeq1Clock: TSequencerClock;
    fSeq1Direction: TStepSequencerDirection;
    fStepSeq1Length: TStepSequencerLength;
    fSeq2Clock: TSequencerClock;
    fSeq2Direction: TStepSequencerDirection;
    fStepSeq2Length: TStepSequencerLength;
    fGrainLoop: TGrainStretchLoopMode;
    fGrainLength: single;
    fGrainRate: single;
    fGrainPosition: single;
    fMixAuxB: single;
    fMixAuxA: single;
    fSamplerLoopBounds: TSamplerLoopBounds;
    fSampleReset: TClockSource;
    fLfoAPar2: single;
    fLfoBPar2: single;
    fAmpVelocityDepth: TEnvVelocityDepth;
    fFilterVelocityDepth: TEnvVelocityDepth;
    fVoiceMode: TVoiceMode;
    fVoiceGlide: single;
    fFilter1Par4: single;
    fFilter2Par4: single;
    fSamplerLoopMode: TSamplerLoopMode;
    fPitchTracking: TPitchTracking;
    procedure SetAmpAttack(const Value: single);
    procedure SetAmpDecay(const Value: single);
    procedure SetAmpHold(const Value: single);
    procedure SetAmpRelease(const Value: single);
    procedure SetAmpSustain(const Value: single);
    procedure SetFilter1Par1(const Value: single);
    procedure SetFilter1Par2(const Value: single);
    procedure SetFilter1Par3(const Value: single);
    procedure SetFilter1Type(const Value: TFilterType);
    procedure SetFilter2Par1(const Value: single);
    procedure SetFilter2Par2(const Value: single);
    procedure SetFilter2Par3(const Value: single);
    procedure SetFilter2Type(const Value: TFilterType);
    procedure SetFilterAttack(const Value: single);
    procedure SetFilterDecay(const Value: single);
    procedure SetFilterHold(const Value: single);
    procedure SetFilterRelease(const Value: single);
    procedure SetFilterSustain(const Value: single);
    procedure SetGrainLength(const Value: single);
    procedure SetGrainLoop(const Value: TGrainStretchLoopMode);
    procedure SetGrainPosition(const Value: single);
    procedure SetGrainRate(const Value: single);
    procedure SetLfoRate1(const Value: single);
    procedure SetLfoRate2(const Value: single);
    procedure SetLfoShape1(const Value: TLfoShape);
    procedure SetLfoShape2(const Value: TLfoShape);
    procedure SetOscPulseWidth(const Value: single);
    procedure SetOscShape(const Value: single);
    procedure SetSamplePlaybackType(const Value: TSamplePlaybackType);
    procedure SetSeq1Clock(const Value: TSequencerClock);
    procedure SetSeq1Direction(const Value: TStepSequencerDirection);
    procedure SetSeq2Clock(const Value: TSequencerClock);
    procedure SetSeq2Direction(const Value: TStepSequencerDirection);
    procedure SetStepSeq1Length(const Value: TStepSequencerLength);
    procedure SetStepSeq2Length(const Value: TStepSequencerLength);
    procedure SetVoiceGain(const Value: single);
    procedure SetVoicePan(const Value: single);
    function GetSeq1StepValue(Index: integer): single;
    procedure SetSeq1StepValue(Index: integer; const Value: single);
    function GetSeq2StepValue(Index: integer): single;
    procedure SetSeq2StepValue(Index: integer; const Value: single);
    procedure SetVoicePitchOne(const Value: single);
    procedure SetVoicePitchTwo(const Value: single);
    procedure SetMixAuxA(const Value: single);
    procedure SetMixAuxB(const Value: single);
    procedure SetSampleLoopBounds(const Value: TSamplerLoopBounds);
    procedure SetSampleReset(Value: TClockSource);
    procedure SetLfoAPar2(const Value: single);
    procedure SetLfoBPar2(const Value: single);
    procedure SetAmpVelocityDepth(const Value: TEnvVelocityDepth);
    procedure SetFilterVelocityDepth(const Value: TEnvVelocityDepth);
    procedure SetVoiceMode(const Value: TVoiceMode);
    procedure SetVoiceGlide(const Value: single);
    procedure SetFilter1Par4(const Value: single);
    procedure SetFilter2Par4(const Value: single);
    procedure SetSamplerLoopMode(const Value: TSamplerLoopMode);
    procedure SetPitchTracking(const Value: TPitchTracking);
  protected
    OwningSampleGroup : IKeyGroup;
    Voices : PArrayOfLucidityVoice;
    VoiceCount : integer;
    fSeq1StepValues : array[0..kMaxStepSequencerLength-1] of single;
    fSeq2StepValues : array[0..kMaxStepSequencerLength-1] of single;
    procedure UpdateActiveVoices(UpdateAction:TUpdateActionProcedure);

    // VoiceMode and VoiceGlide are unused at this point. They might be used in future. Currently using a global VoiceMode and VoiceGlide.
    // IMPORTANT: If re-implementing, check that the settings are applied to the voice classes, currently everything is commentted out.
    property VoiceMode                : TVoiceMode                         read fVoiceMode               write SetVoiceMode;
    property VoiceGlide               : single                             read fVoiceGlide              write SetVoiceGlide;
  public
    constructor Create(const aVoices:PArrayOfLucidityVoice; const aOwningSampleGroup : IKeyGroup);
    destructor Destroy; override;

    procedure UpdateAllModLinks(const Source : TModConnections_OLD);
    procedure UpdateModLink(const ModLinkData : PModLink_OLD);

    procedure AssignFrom(const Source : TLucidityVoiceParameterWrapper);

    procedure ApplyParametersToVoice(aVoice : TLucidityVoice);

    property Seq1StepValue[Index : integer]:single read GetSeq1StepValue write SetSeq1StepValue;
    property Seq2StepValue[Index : integer]:single read GetSeq2StepValue write SetSeq2StepValue;
  published
    property PitchTracking            : TPitchTracking                     read fPitchTracking           write SetPitchTracking;
    property SamplePlaybackType       : TSamplePlaybackType                read fSamplePlaybackType      write SetSamplePlaybackType;
    property SampleReset              : TClockSource                       read fSampleReset             write SetSampleReset;
    property VoiceGain                : single                             read fVoiceGain               write SetVoiceGain;
    property VoicePan                 : single                             read fVoicePan                write SetVoicePan;
    property VoicePitchOne            : single                             read fVoicePitchOne           write SetVoicePitchOne; //range -1..1
    property VoicePitchTwo            : single                             read fVoicePitchTwo           write SetVoicePitchTwo; //range -1..1

    property GrainLoop                : TGrainStretchLoopMode              read fGrainLoop               write SetGrainLoop;
    property GrainLength              : single                             read fGrainLength             write SetGrainLength;
    property GrainRate                : single                             read fGrainRate               write SetGrainRate;
    property GrainPosition            : single                             read fGrainPosition           write SetGrainPosition;

    property SamplerLoopBounds        : TSamplerLoopBounds                 read fSamplerLoopBounds       write SetSampleLoopBounds;
    property SamplerLoopMode          : TSamplerLoopMode                   read fSamplerLoopMode         write SetSamplerLoopMode;

    property MixAuxA                  : single                             read fMixAuxA                 write SetMixAuxA;
    property MixAuxB                  : single                             read fMixAuxB                 write SetMixAuxB;

    property OscShape                 : single                             read fOscShape                write SetOscShape;
    property OscPulseWidth            : single                             read fOscPulseWidth           write SetOscPulseWidth;
    property Filter1Type              : TFilterType                        read fFilter1Type             write SetFilter1Type;
    property Filter2Type              : TFilterType                        read fFilter2Type             write SetFilter2Type;
    property Filter1Par1              : single                             read fFilter1Par1             write SetFilter1Par1;
    property Filter1Par2              : single                             read fFilter1Par2             write SetFilter1Par2;
    property Filter1Par3              : single                             read fFilter1Par3             write SetFilter1Par3;
    property Filter1Par4              : single                             read fFilter1Par4             write SetFilter1Par4;
    property Filter2Par1              : single                             read fFilter2Par1             write SetFilter2Par1;
    property Filter2Par2              : single                             read fFilter2Par2             write SetFilter2Par2;
    property Filter2Par3              : single                             read fFilter2Par3             write SetFilter2Par3;
    property Filter2Par4              : single                             read fFilter2Par4             write SetFilter2Par4;
    property AmpAttack                : single                             read fAmpAttack               write SetAmpAttack;
    property AmpHold                  : single                             read fAmpHold                 write SetAmpHold;
    property AmpDecay                 : single                             read fAmpDecay                write SetAmpDecay;
    property AmpSustain               : single                             read fAmpSustain              write SetAmpSustain;
    property AmpRelease               : single                             read fAmpRelease              write SetAmpRelease;
    property AmpVelocityDepth         : TEnvVelocityDepth                  read fAmpVelocityDepth        write SetAmpVelocityDepth;
    property FilterAttack             : single                             read fFilterAttack            write SetFilterAttack;
    property FilterHold               : single                             read fFilterHold              write SetFilterHold;
    property FilterDecay              : single                             read fFilterDecay             write SetFilterDecay;
    property FilterSustain            : single                             read fFilterSustain           write SetFilterSustain;
    property FilterRelease            : single                             read fFilterRelease           write SetFilterRelease;
    property FilterVelocityDepth      : TEnvVelocityDepth                  read fFilterVelocityDepth     write SetFilterVelocityDepth;
    property LfoShape1                : TLfoShape                          read fLfoShape1               write SetLfoShape1;
    property LfoShape2                : TLfoShape                          read fLfoShape2               write SetLfoShape2;
    property LfoRate1                 : single                             read fLfoRate1                write SetLfoRate1;
    property LfoRate2                 : single                             read fLfoRate2                write SetLfoRate2;
    property LfoAPar2                 : single                             read fLfoAPar2                write SetLfoAPar2;
    property LfoBPar2                 : single                             read fLfoBPar2                write SetLfoBPar2;
    property Seq1Clock                : TSequencerClock                    read fSeq1Clock               write SetSeq1Clock;
    property Seq1Direction            : TStepSequencerDirection            read fSeq1Direction           write SetSeq1Direction;
    property StepSeq1Length           : TStepSequencerLength               read fStepSeq1Length          write SetStepSeq1Length;
    property Seq2Clock                : TSequencerClock                    read fSeq2Clock               write SetSeq2Clock;
    property Seq2Direction            : TStepSequencerDirection            read fSeq2Direction           write SetSeq2Direction;
    property StepSeq2Length           : TStepSequencerLength               read fStepSeq2Length          write SetStepSeq2Length;


  end;

implementation

uses
  SysUtils, System.TypInfo;


{ TLucidityEngineParameterWrapper }

constructor TLucidityVoiceParameterWrapper.Create(const aVoices:PArrayOfLucidityVoice; const aOwningSampleGroup : IKeyGroup);
begin
  OwningSampleGroup := aOwningSampleGroup;
  VoiceCount := uConstants.kMaxVoiceCount;
  Voices := aVoices;

  self.fVoiceGain := 1;
  self.fVoicePan  := 0.5;
end;

destructor TLucidityVoiceParameterWrapper.Destroy;
begin
  OwningSampleGroup := nil;
  inherited;
end;




procedure TLucidityVoiceParameterWrapper.UpdateActiveVoices(UpdateAction: TUpdateActionProcedure);
var
  c1 : integer;
begin
  // UpdateActiveVoices() takes uses an anonymous method.
  // The anonymous method specifies how the voice needs to be updated.
  // The UpdateActiveVoices() "applies the action" to individual voices.
  // This is another layer of complication, but it makes it possible
  // to change which voices actions are applied to by changing one
  // place in the code.
  for c1 := 0 to kMaxVoiceCount-1 do
  begin
    if (Voices^[c1].IsActive) and (Voices^[c1].SampleGroup = self.OwningSampleGroup) then
    begin
      UpdateAction(@Voices^[c1]);
    end;
  end;
end;




procedure TLucidityVoiceParameterWrapper.UpdateAllModLinks(const Source: TModConnections_OLD);
begin
  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.UpdateAllModLinks(Source);
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.UpdateModLink(const ModLinkData: PModLink_OLD);
begin
  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.UpdateModLink(ModLinkData);
    end
  );
end;

function TLucidityVoiceParameterWrapper.GetSeq1StepValue(Index: integer): single;
begin
  result := fSeq1StepValues[Index];
end;

function TLucidityVoiceParameterWrapper.GetSeq2StepValue(Index: integer): single;
begin
  result := fSeq2StepValues[Index];
end;

procedure TLucidityVoiceParameterWrapper.SetAmpAttack(const Value: single);
begin
  fAmpAttack := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetAmpDecay(const Value: single);
begin
  fAmpDecay := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetAmpHold(const Value: single);
begin
  fAmpHold := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetAmpRelease(const Value: single);
begin
  fAmpRelease := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetAmpSustain(const Value: single);
begin
  fAmpSustain := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetAmpVelocityDepth(const Value: TEnvVelocityDepth);
begin
  fAmpVelocityDepth := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.AmpEnv.VelocityDepth := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetFilter1Par1(const Value: single);
begin
  fFilter1Par1 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter1Par2(const Value: single);
begin
  fFilter1Par2 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter1Par3(const Value: single);
begin
  fFilter1Par3 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter1Par4(const Value: single);
begin
  fFilter1Par4 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter1Type(const Value: TFilterType);
begin
  fFilter1Type := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.FilterOne.FilterType := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetFilter2Par1(const Value: single);
begin
  fFilter2Par1 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter2Par2(const Value: single);
begin
  fFilter2Par2 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter2Par3(const Value: single);
begin
  fFilter2Par3 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter2Par4(const Value: single);
begin
  fFilter2Par4 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilter2Type(const Value: TFilterType);
begin
  fFilter2Type := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.FilterTwo.FilterType := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetFilterAttack(const Value: single);
begin
  fFilterAttack := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilterDecay(const Value: single);
begin
  fFilterDecay := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilterHold(const Value: single);
begin
  fFilterHold := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilterRelease(const Value: single);
begin
  fFilterRelease := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilterSustain(const Value: single);
begin
  fFilterSustain := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetFilterVelocityDepth(const Value: TEnvVelocityDepth);
begin
  fFilterVelocityDepth := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.FilterEnv.VelocityDepth := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetGrainLength(const Value: single);
begin
  fGrainLength := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.GrainStretchOsc.GrainLength := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetGrainLoop(const Value: TGrainStretchLoopMode);
begin
  fGrainLoop := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.GrainStretchOsc.LoopMode := Value;
    end
  );
end;


procedure TLucidityVoiceParameterWrapper.SetGrainPosition(const Value: single);
begin
  fGrainPosition := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.GrainStretchOsc.GrainPosition := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetGrainRate(const Value: single);
begin
  fGrainRate := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.GrainStretchOsc.GrainRate := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetLfoAPar2(const Value: single);
begin
  fLfoAPar2 := Value;

end;

procedure TLucidityVoiceParameterWrapper.SetLfoBPar2(const Value: single);
begin
  fLfoBPar2 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetLfoRate1(const Value: single);
begin
  fLfoRate1 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetLfoRate2(const Value: single);
begin
  fLfoRate2 := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetLfoShape1(const Value: TLfoShape);
begin
  fLfoShape1 := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.LfoA.Shape := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetLfoShape2(const Value: TLfoShape);
begin
  fLfoShape2 := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.LfoB.Shape := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSampleLoopBounds(const Value: TSamplerLoopBounds);
begin
  fSamplerLoopBounds := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.OneShotSampleOsc.LoopBounds := Value;
      v^.LoopSampleOsc.LoopBounds := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetOscPulseWidth(const Value: single);
begin
  fOscPulseWidth := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.WaveOsc.PulseWidth := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetOscShape(const Value: single);
begin
  fOscShape := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.WaveOsc.Shape := Value;
    end
  );
end;




procedure TLucidityVoiceParameterWrapper.SetPitchTracking(const Value: TPitchTracking);
begin
  fPitchTracking := Value;


  // NOTE: Don't update the PitchTracking for active voices.
  // UpdateActiveVoices(
  //   procedure(v:PLucidityVoice)
  //   begin
  //     v^.PitchTracking := Value;
  //   end
  // );
end;

procedure TLucidityVoiceParameterWrapper.SetSamplePlaybackType(const Value: TSamplePlaybackType);
begin
  case Value of
    TSamplePlaybackType.NoteSampler:    fSamplePlaybackType := Value;
    TSamplePlaybackType.LoopSampler:    fSamplePlaybackType := Value;
    TSamplePlaybackType.OneShotSampler: fSamplePlaybackType := Value;
    TSamplePlaybackType.GrainStretch:   fSamplePlaybackType := TSamplePlaybackType.NoteSampler;
    TSamplePlaybackType.WaveOsc:        fSamplePlaybackType := TSamplePlaybackType.NoteSampler;
  else
    raise Exception.Create('Unexpected Sample Playback type.');
  end;


  // NOTE: Don't update the SamplePlaybackType for active voices.
  //UpdateActiveVoices(
  //  procedure(v:PLucidityVoice)
  //  begin
  //    v^.SampleOscType := Value;
  //  end
  //);
end;

procedure TLucidityVoiceParameterWrapper.SetSampleReset(Value: TClockSource);
begin
  if Value = TClockSource.SampleLoop
    then Value := TClockSource.None;

  fSampleReset := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.SampleReset := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSamplerLoopMode(const Value: TSamplerLoopMode);
begin
  fSamplerLoopMode := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.OneShotSampleOsc.LoopMode := Value;
      v^.LoopSampleOsc.LoopMode    := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSeq1Clock(const Value: TSequencerClock);
begin
  fSeq1Clock := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqOne.Clock := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSeq1Direction(const Value: TStepSequencerDirection);
begin
  fSeq1Direction := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqOne.Direction := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSeq1StepValue(Index: integer; const Value: single);
begin
  fSeq1StepValues[Index] := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqOne.StepValue[Index] := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSeq2Clock(const Value: TSequencerClock);
begin
  fSeq2Clock := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqTwo.Clock := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSeq2Direction(const Value: TStepSequencerDirection);
begin
  fSeq2Direction := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqTwo.Direction := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetSeq2StepValue(Index: integer; const Value: single);
begin
  fSeq2StepValues[Index] := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqTwo.StepValue[Index] := Value;
    end
  );
end;


procedure TLucidityVoiceParameterWrapper.SetStepSeq1Length(const Value: TStepSequencerLength);
begin
  fStepSeq1Length := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqOne.StepCount := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetStepSeq2Length(const Value: TStepSequencerLength);
begin
  fStepSeq2Length := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.StepSeqTwo.StepCount := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetVoiceGain(const Value: single);
begin
  fVoiceGain := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetVoiceGlide(const Value: single);
begin
  fVoiceGlide := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      //v^.VoiceGlide := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetVoiceMode(const Value: TVoiceMode);
begin
  fVoiceMode := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      //v^.VoiceMode := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetVoicePan(const Value: single);
begin
  fVoicePan := Value;
end;

procedure TLucidityVoiceParameterWrapper.SetVoicePitchOne(const Value: single);
begin
  fVoicePitchOne := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.PitchOne := Value;
    end
  );

end;

procedure TLucidityVoiceParameterWrapper.SetVoicePitchTwo(const Value: single);
begin
  fVoicePitchTwo := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.PitchTwo := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetMixAuxA(const Value: single);
begin
  fMixAuxA := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.OutputMixer.VoiceMixAuxA := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetMixAuxB(const Value: single);
begin
  fMixAuxB := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.OutputMixer.VoiceMixAuxB := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.AssignFrom(const Source: TLucidityVoiceParameterWrapper);
var
  c1: Integer;
begin
  for c1 := 0 to kMaxStepSequencerLength-1 do
  begin
    Self.Seq1StepValue[c1] := Source.Seq1StepValue[c1];
    Self.Seq2StepValue[c1] := Source.Seq2StepValue[c1];
  end;

  //self.VoiceMode                := Source.VoiceMode;
  //self.VoiceGlide               := Source.VoiceGlide;

  self.PitchTracking            := Source.PitchTracking;
  self.SamplePlaybackType       := Source.SamplePlaybackType;
  self.SampleReset              := Source.SampleReset;

  Self.GrainLoop                := Source.GrainLoop;
  Self.GrainLength              := Source.GrainLength;
  Self.GrainRate                := Source.GrainRate;
  Self.GrainPosition            := Source.GrainPosition;

  self.SamplerLoopBounds        := Source.SamplerLoopBounds;
  self.SamplerLoopMode          := Source.SamplerLoopMode;

  self.MixAuxA                  := Source.MixAuxA;
  self.MixAuxB                  := Source.MixAuxB;

  Self.OscShape                 := Source.OscShape;
  Self.OscPulseWidth            := Source.OscPulseWidth;
  Self.VoiceGain                := Source.VoiceGain;
  Self.VoicePan                 := Source.VoicePan;
  Self.VoicePitchOne            := Source.VoicePitchOne;
  Self.VoicePitchTwo            := Source.VoicePitchTwo;
  Self.Filter1Type              := Source.Filter1Type;
  Self.Filter2Type              := Source.Filter2Type;
  Self.Filter1Par1              := Source.Filter1Par1;
  Self.Filter1Par2              := Source.Filter1Par2;
  Self.Filter1Par3              := Source.Filter1Par3;
  Self.Filter1Par4              := Source.Filter1Par4;
  Self.Filter2Par1              := Source.Filter2Par1;
  Self.Filter2Par2              := Source.Filter2Par2;
  Self.Filter2Par3              := Source.Filter2Par3;
  Self.Filter2Par4              := Source.Filter2Par4;
  Self.AmpAttack                := Source.AmpAttack;
  Self.AmpHold                  := Source.AmpHold;
  Self.AmpDecay                 := Source.AmpDecay;
  Self.AmpSustain               := Source.AmpSustain;
  Self.AmpRelease               := Source.AmpRelease;
  Self.AmpVelocityDepth         := Source.AmpVelocityDepth;
  Self.FilterAttack             := Source.FilterAttack;
  Self.FilterHold               := Source.FilterHold;
  Self.FilterDecay              := Source.FilterDecay;
  Self.FilterSustain            := Source.FilterSustain;
  Self.FilterRelease            := Source.FilterRelease;
  Self.FilterVelocityDepth      := Source.FilterVelocityDepth;
  Self.LfoShape1                := Source.LfoShape1;
  Self.LfoShape2                := Source.LfoShape2;
  Self.LfoRate1                 := Source.LfoRate1;
  Self.LfoRate2                 := Source.LfoRate2;
  self.LfoAPar2                 := Source.LfoAPar2;
  self.LfoBPar2                 := Source.LfoBPar2;
  Self.Seq1Clock                := Source.Seq1Clock;
  Self.Seq1Direction            := Source.Seq1Direction;
  Self.StepSeq1Length           := Source.StepSeq1Length;
  Self.Seq2Clock                := Source.Seq2Clock;
  Self.Seq2Direction            := Source.Seq2Direction;
  Self.StepSeq2Length           := Source.StepSeq2Length;
end;

procedure TLucidityVoiceParameterWrapper.ApplyParametersToVoice(aVoice: TLucidityVoice);
var
  c1 : integer;
begin
  //==== set step values...====
  for c1 := 0 to kMaxStepSequencerLength-1 do
  begin
    aVoice.StepSeqOne.StepValue[c1] := Seq1StepValue[c1];
    aVoice.StepSeqTwo.StepValue[c1] := Seq2StepValue[c1];
  end;

  //=== set all one-to-one properties.====
  //aVoice.VoiceMode                       := VoiceMode;
  //aVoice.VoiceGlide                      := VoiceGlide;

  aVoice.PitchTracking                   := PitchTracking;
  aVoice.SamplePlaybackType              := SamplePlayBackType;
  aVoice.SampleReset                     := SampleReset;

  aVoice.GrainStretchOsc.LoopMode        := GrainLoop;
  aVoice.GrainStretchOsc.GrainLength     := GrainLength;
  aVoice.GrainStretchOsc.GrainRate       := GrainRate;
  aVoice.GrainStretchOsc.GrainPosition   := GrainPosition;

  aVoice.OneShotSampleOsc.LoopBounds       := SamplerLoopBounds;
  aVoice.LoopSampleOsc.LoopBounds          := SamplerLoopBounds;

  aVoice.OneShotSampleOsc.LoopMode       := SamplerLoopMode;
  aVoice.LoopSampleOsc.LoopMode          := SamplerLoopMode;

  aVoice.OutputMixer.VoiceMixAuxA        := MixAuxA;
  aVoice.OutputMixer.VoiceMixAuxB        := MixAuxB;
  aVoice.WaveOsc.Shape                   := OscShape;
  aVoice.WaveOsc.PulseWidth              := OscPulseWidth;
  //aVoice.SampleOsc.FmAmount := OscFm;  //!! what's going on here. duplicated parameters again?
  //aVoice.SampleOsc.AmAmount := OscAm;  //!! what's going on here. duplicated parameters again?
  //aVoice.StepSeq1Clock := ClockDivisionA; //I don't think this is needed.
  //aVoice.StepSeq2Clock := ClockDivisionB; //I don't think this is needed.
  aVoice.PitchOne                        := VoicePitchOne;
  aVoice.PitchTwo                        := VoicePitchTwo;
  aVoice.FilterOne.FilterType            := Filter1Type;
  aVoice.FilterTwo.FilterType            := Filter2Type;
  aVoice.AmpEnv.VelocityDepth            := AmpVelocityDepth;
  aVoice.FilterEnv.VelocityDepth         := FilterVelocityDepth;
  aVoice.LfoA.Shape                      := LfoShape1;
  aVoice.LfoB.Shape                      := LfoShape2;
  aVoice.StepSeqOne.Clock                := Seq1Clock;
  aVoice.StepSeqOne.Direction            := Seq1Direction;
  aVoice.StepSeqOne.StepCount            := StepSeq1Length;
  aVoice.StepSeqTwo.Clock                := Seq2Clock;
  aVoice.StepSeqTwo.Direction            := Seq2Direction;
  aVoice.StepSeqTwo.StepCount            := StepSeq2Length;
end;



end.
