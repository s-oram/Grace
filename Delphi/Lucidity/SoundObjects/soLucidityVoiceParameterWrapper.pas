unit soLucidityVoiceParameterWrapper;

interface

uses
  VamLib.ZeroObject,
  LucidityGui.VectorSequence,
  LucidityModConnections,
  uConstants, Lucidity.Enums, soModMatrix,
  Lucidity.Interfaces,
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
    fSamplePlaybackType: TSamplePlaybackType;
    fFilter1Type: TFilterType;
    fFilter2Type: TFilterType;
    fSeq1Clock: TSequencerClock;
    fSeq1Direction: TStepSequencerDirection;
    fStepSeq1Length: TStepSequencerLength;
    fSeq2Clock: TSequencerClock;
    fSeq2Direction: TStepSequencerDirection;
    fStepSeq2Length: TStepSequencerLength;
    fSamplerLoopBounds: TSamplerLoopBounds;
    fSampleReset: TClockSource;
    fAmpVelocityDepth: TEnvVelocityDepth;
    fModVelocityDepth: TEnvVelocityDepth;
    fVoiceMode: TVoiceMode;
    fVoiceGlide: single;
    fSamplerTriggerMode: TKeyGroupTriggerMode;
    fPitchTracking: TPitchTracking;
    fFilter2KeyFollow: single;
    fFilter1KeyFollow: single;
    fLfoFreqMode2: TLfoFreqMode;
    fLfoFreqMode1: TLfoFreqMode;
    fFilterRouting: TFilterRouting;
    fLfoShape2: TLfoShape;
    fLfoShape1: TLfoShape;
    fModEnvSnap: TEnvSnap;
    fAmpEnvSnap: TEnvSnap;
    procedure SetFilter1Type(const Value: TFilterType);
    procedure SetFilter2Type(const Value: TFilterType);
    procedure SetLfoShape1(const Value: TLfoShape);
    procedure SetLfoShape2(const Value: TLfoShape);
    procedure SetSamplePlaybackType(const Value: TSamplePlaybackType);
    procedure SetSeq1Clock(const Value: TSequencerClock);
    procedure SetSeq1Direction(const Value: TStepSequencerDirection);
    procedure SetSeq2Clock(const Value: TSequencerClock);
    procedure SetSeq2Direction(const Value: TStepSequencerDirection);
    procedure SetStepSeq1Length(const Value: TStepSequencerLength);
    procedure SetStepSeq2Length(const Value: TStepSequencerLength);
    procedure SetSampleLoopBounds(const Value: TSamplerLoopBounds);
    procedure SetSampleReset(Value: TClockSource);
    procedure SetAmpVelocityDepth(const Value: TEnvVelocityDepth);
    procedure SetModVelocityDepth(const Value: TEnvVelocityDepth);
    procedure SetVoiceMode(const Value: TVoiceMode);
    procedure SetVoiceGlide(const Value: single);
    procedure SetSamplerTriggerMode(const Value: TKeyGroupTriggerMode);
    procedure SetPitchTracking(const Value: TPitchTracking);
    procedure SetFilter1KeyFollow(const Value: single);
    procedure SetFilter2KeyFollow(const Value: single);
    procedure SetLfoFreqMode1(const Value: TLfoFreqMode);
    procedure SetLfoFreqMode2(const Value: TLfoFreqMode);
    procedure SetFilterRouting(const Value: TFilterRouting);
    procedure SetAmpEnvSnap(const Value: TEnvSnap);
    procedure SetModEnvSnap(const Value: TEnvSnap);
  protected
    OwningSampleGroup : Pointer; //weak reference to IKeyGroup
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

    procedure UpdateModConnections;

    procedure AssignFrom(const Source : TLucidityVoiceParameterWrapper);

    procedure ApplyParametersToVoice(aVoice : TLucidityVoice);
  published //TODO:HIGH try removing the published requirement here.
    // Not currently used.
    property SamplePlaybackType       : TSamplePlaybackType                read fSamplePlaybackType      write SetSamplePlaybackType;

    property PitchTracking            : TPitchTracking                     read fPitchTracking           write SetPitchTracking;
    property SampleReset              : TClockSource                       read fSampleReset             write SetSampleReset;

    property SamplerLoopBounds        : TSamplerLoopBounds                 read fSamplerLoopBounds       write SetSampleLoopBounds;
    property SamplerTriggerMode       : TKeyGroupTriggerMode               read fSamplerTriggerMode      write SetSamplerTriggerMode;

    property FilterRouting            : TFilterRouting                     read fFilterRouting           write SetFilterRouting;
    property Filter1Type              : TFilterType                        read fFilter1Type             write SetFilter1Type;
    property Filter2Type              : TFilterType                        read fFilter2Type             write SetFilter2Type;
    property Filter1KeyFollow         : single                             read fFilter1KeyFollow        write SetFilter1KeyFollow; //range 0..1
    property Filter2KeyFollow         : single                             read fFilter2KeyFollow        write SetFilter2KeyFollow; //range 0..1
    property AmpVelocityDepth         : TEnvVelocityDepth                  read fAmpVelocityDepth        write SetAmpVelocityDepth;
    property ModVelocityDepth         : TEnvVelocityDepth                  read fModVelocityDepth        write SetModVelocityDepth;
    property AmpEnvSnap               : TEnvSnap                           read fAmpEnvSnap              write SetAmpEnvSnap;
    property ModEnvSnap               : TEnvSnap                           read fModEnvSnap              write SetModEnvSnap;
    property LfoShape1                : TLfoShape                          read fLfoShape1               write SetLfoShape1;
    property LfoShape2                : TLfoShape                          read fLfoShape2               write SetLfoShape2;
    property LfoFreqMode1             : TLfoFreqMode                       read fLfoFreqMode1            write SetLfoFreqMode1;
    property LfoFreqMode2             : TLfoFreqMode                       read fLfoFreqMode2            write SetLfoFreqMode2;

    property Seq1Clock                : TSequencerClock                    read fSeq1Clock               write SetSeq1Clock;
    property Seq1Direction            : TStepSequencerDirection            read fSeq1Direction           write SetSeq1Direction;
    property StepSeq1Length           : TStepSequencerLength               read fStepSeq1Length          write SetStepSeq1Length;
    property Seq2Clock                : TSequencerClock                    read fSeq2Clock               write SetSeq2Clock;
    property Seq2Direction            : TStepSequencerDirection            read fSeq2Direction           write SetSeq2Direction;
    property StepSeq2Length           : TStepSequencerLength               read fStepSeq2Length          write SetStepSeq2Length;
  end;

implementation

uses
  SysUtils, System.TypInfo,
  Lucidity.Types;


{ TLucidityEngineParameterWrapper }

constructor TLucidityVoiceParameterWrapper.Create(const aVoices:PArrayOfLucidityVoice; const aOwningSampleGroup : IKeyGroup);
begin
  OwningSampleGroup := Pointer(aOwningSampleGroup);
  VoiceCount := uConstants.kMaxVoiceCount;
  Voices := aVoices;
end;

destructor TLucidityVoiceParameterWrapper.Destroy;
begin
  OwningSampleGroup := nil;
  inherited;
end;




procedure TLucidityVoiceParameterWrapper.UpdateActiveVoices(UpdateAction: TUpdateActionProcedure);
var
  c1 : integer;
  OwningID : TKeyGroupID;
begin
  // UpdateActiveVoices() takes uses an anonymous method.
  // The anonymous method specifies how the voice needs to be updated.
  // The UpdateActiveVoices() "applies the action" to individual voices.
  // This is another layer of complication, but it makes it possible
  // to change which voices actions are applied to by changing one
  // place in the code.
  for c1 := 0 to kMaxVoiceCount-1 do
  begin
    OwningID := IKeyGroup(OwningSampleGroup).GetID;
    if (Voices^[c1].IsActive) and (Voices^[c1].KeyGroupID = OwningID) then
    begin
      UpdateAction(@Voices^[c1]);
    end;
  end;
end;

procedure TLucidityVoiceParameterWrapper.UpdateModConnections;
begin
  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.ModMatrix.UpdateModConnections;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetAmpEnvSnap(const Value: TEnvSnap);
begin
  fAmpEnvSnap := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.AmpEnv.EnvSnap := Value;
    end
  );
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

procedure TLucidityVoiceParameterWrapper.SetFilter1KeyFollow(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fFilter1KeyFollow := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.FilterOne.KeyFollow := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetFilter2KeyFollow(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fFilter2KeyFollow := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.FilterTwo.KeyFollow := Value;
    end
  );
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

procedure TLucidityVoiceParameterWrapper.SetFilterRouting(const Value: TFilterRouting);
begin
  fFilterRouting := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.FilterRouting := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetModEnvSnap(const Value: TEnvSnap);
begin
  fModEnvSnap := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.ModEnv.EnvSnap := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetModVelocityDepth(const Value: TEnvVelocityDepth);
begin
  fModVelocityDepth := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.ModEnv.VelocityDepth := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetLfoFreqMode1(const Value: TLfoFreqMode);
begin
  fLfoFreqMode1 := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.LfoA.FreqMode := Value;
    end
  );
end;

procedure TLucidityVoiceParameterWrapper.SetLfoFreqMode2(const Value: TLfoFreqMode);
begin
  fLfoFreqMode2 := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.LfoB.FreqMode := Value;
    end
  );
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

procedure TLucidityVoiceParameterWrapper.SetSamplerTriggerMode(const Value: TKeyGroupTriggerMode);
begin
  fSamplerTriggerMode := Value;

  UpdateActiveVoices(
    procedure(v:PLucidityVoice)
    begin
      v^.TriggerMode := Value;
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


procedure TLucidityVoiceParameterWrapper.AssignFrom(const Source: TLucidityVoiceParameterWrapper);
begin
  //self.VoiceMode                := Source.VoiceMode;
  //self.VoiceGlide               := Source.VoiceGlide;

  self.PitchTracking            := Source.PitchTracking;
  self.SamplePlaybackType       := Source.SamplePlaybackType;
  self.SampleReset              := Source.SampleReset;

  self.SamplerLoopBounds        := Source.SamplerLoopBounds;
  self.SamplerTriggerMode       := Source.SamplerTriggerMode;

  Self.FilterRouting            := Source.FilterRouting;
  Self.Filter1Type              := Source.Filter1Type;
  Self.Filter2Type              := Source.Filter2Type;
  Self.Filter1KeyFollow         := Source.Filter1KeyFollow;
  Self.Filter2KeyFollow         := Source.Filter2KeyFollow;
  Self.AmpVelocityDepth         := Source.AmpVelocityDepth;
  Self.ModVelocityDepth         := Source.ModVelocityDepth;
  Self.AmpEnvSnap               := Source.AmpEnvSnap;
  Self.ModEnvSnap               := Source.ModEnvSnap;
  Self.LfoShape1                := Source.LfoShape1;
  Self.LfoShape2                := Source.LfoShape2;
  Self.LfoFreqMode1             := Source.LfoFreqMode1;
  Self.LfoFreqMode2             := Source.LfoFreqMode2;
  Self.Seq1Clock                := Source.Seq1Clock;
  Self.Seq1Direction            := Source.Seq1Direction;
  Self.StepSeq1Length           := Source.StepSeq1Length;
  Self.Seq2Clock                := Source.Seq2Clock;
  Self.Seq2Direction            := Source.Seq2Direction;
  Self.StepSeq2Length           := Source.StepSeq2Length;
end;

procedure TLucidityVoiceParameterWrapper.ApplyParametersToVoice(aVoice: TLucidityVoice);
begin
  //=== set all one-to-one properties.====
  //aVoice.VoiceMode                       := VoiceMode;
  //aVoice.VoiceGlide                      := VoiceGlide;

  aVoice.PitchTracking                   := PitchTracking;
  aVoice.SamplePlaybackType              := SamplePlayBackType;
  aVoice.SampleReset                     := SampleReset;

  aVoice.OneShotSampleOsc.LoopBounds     := SamplerLoopBounds;
  aVoice.LoopSampleOsc.LoopBounds        := SamplerLoopBounds;

  aVoice.TriggerMode                     := SamplerTriggerMode;

  aVoice.FilterRouting                   := FilterRouting;
  aVoice.FilterOne.FilterType            := Filter1Type;
  aVoice.FilterTwo.FilterType            := Filter2Type;
  aVoice.FilterOne.KeyFollow             := Filter1KeyFollow;
  aVoice.FilterTwo.KeyFollow             := Filter2KeyFollow;
  aVoice.AmpEnv.VelocityDepth            := AmpVelocityDepth;
  aVoice.ModEnv.VelocityDepth            := ModVelocityDepth;
  aVoice.AmpEnv.EnvSnap                  := AmpEnvSnap;
  aVoice.ModEnv.EnvSnap                  := ModEnvSnap;
  aVoice.LfoA.Shape                      := LfoShape1;
  aVoice.LfoB.Shape                      := LfoShape2;
  aVoice.LfoA.FreqMode                   := LfoFreqMode1;
  aVoice.LfoB.FreqMode                   := LfoFreqMode2;
  aVoice.StepSeqOne.Clock                := Seq1Clock;
  aVoice.StepSeqOne.Direction            := Seq1Direction;
  aVoice.StepSeqOne.StepCount            := StepSeq1Length;
  aVoice.StepSeqTwo.Clock                := Seq2Clock;
  aVoice.StepSeqTwo.Direction            := Seq2Direction;
  aVoice.StepSeqTwo.StepCount            := StepSeq2Length;
end;






end.

