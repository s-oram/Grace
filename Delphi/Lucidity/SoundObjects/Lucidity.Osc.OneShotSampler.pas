unit Lucidity.Osc.OneShotSampler;

interface

{$INCLUDE Defines.inc}

uses
  Lucidity.Interfaces,
  Classes,
  VamLib.MoreTypes, Lucidity.SampleMap, uLucidityCustomSampleOsc, soGateEnvelope,
  uConstants, uLucidityEnums, eeSampleFloat, uLucidityClock,
  eeCounter,
  soStepInFilter,
  Lucidity.Osc.FadeOutSampler,
  uGuiFeedbackData;

type
  TLuciditySampleOscModulationPoints = record
    SampleResetInput : single;
  end;

  PSampleOscPitchPar = ^TSampleOscPitchPar;
  TSampleOscPitchPar = record
    PitchTracking  : TPitchTracking;
    RegionRootNote : single; //0..127 corrosponding to MIDI note numbers...
    PlaybackNote   : single; //0..127 corrosponding to MIDI note numbers...
    SamplePitchAdjust : single; //in semitones. 0 = no shift.
    VoicePitchAdjust  : single; //in semitones. 0 = no shift.
    PitchBendAdjust   : single; //in semitones. 0 = no shift.
  end;

  TOneShotSampleOsc = class(TCustomSampleOsc)
  private
    fLoopBounds: TSamplerLoopBounds;
    fOnFinished: TNotifyEvent;
    fPitchShift: single;
    fLoopMode: TKeyGroupTriggerMode;
    fIsSampleResetActive: boolean;
    procedure SetLoopBounds(const Value: TSamplerLoopBounds);
    procedure SetLoopMode(const Value: TKeyGroupTriggerMode);
  protected
    IsLoopTooSmall : boolean;
    LoopTooSmallCount : integer;
    LoopTooSmallResetPoint : integer;

    PitchParameters : TSampleOscPitchPar;
    ModPoints : TLuciditySampleOscModulationPoints;
    CurRegion : IRegion;
    CurSample : TSampleFloat;

    PhaseCounter : TCounter;
    CurrentSampleBounds : TSampleOsc_SampleBounds;


    SourceTempoFactor : single;

    //TODO: There are two fade out oscillators here.
    // Currently only one is being used most of the time. It could
    // be possible to use both fade out oscillators in a round-robin fashion
    // to reduce clicks when retriggering *very* quickly. Is this needed?
    // Alternatively look at how the second retrigger fade out oscillator
    // is being used. Maybe it can be removed?...
    StepInFilter        : TStepInFilter;
    LoopingFadeOutOsc   : TFadeOutSampler;
    RetriggerFadeOutOsc : TFadeOutSampler;

    IsFinishCalledNeeded : boolean;
    HasBeenReleased : boolean;

    procedure SetSampleRate(const Value: single); override;

    procedure UpdateSampleBounds;

    function CalcPhaseCounterStepSize:double; {$IFDEF AudioInline}inline;{$ENDIF}
  public
    constructor Create(const aVoiceModPoints : PVoiceModulationPoints; const aVoiceClockManager : TLucidityVoiceClockManager); override;
    destructor Destroy; override;

    function GetPitchParameters : PSampleOscPitchPar;

    // call to reset sample playback to the beginning of the sample.
    procedure ResetSamplePosition;

    function GetModPointer(const Name:string):PSingle;

    procedure Trigger(const MidiNote : byte; const aSampleRegion:IRegion; const aSample:TSampleFloat);
    procedure Release;
    procedure Kill;

    procedure AudioRateStep(out Out1, Out2 : Single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}

    //== Parameters ===
    property PitchShift : single read fPitchShift write fPitchShift; //SemiTones.

    property LoopBounds    : TSamplerLoopBounds  read fLoopBounds         write SetLoopBounds;
    property LoopMode      : TKeyGroupTriggerMode    read fLoopMode           write SetLoopMode;
    property IsSampleResetActive : boolean  read fIsSampleResetActive write fIsSampleResetActive;


    //== For GUI Feedback ==
    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);

    property OnFinished : TNotifyEvent read fOnFinished write fOnFinished;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils, SampleOscUtils,
  Math, eeDsp, eePitch, eeFunctions;

{ TLuciditySampleOsc }

constructor TOneShotSampleOsc.Create(const aVoiceModPoints : PVoiceModulationPoints; const aVoiceClockManager : TLucidityVoiceClockManager);
begin
  inherited;

  StepInFilter := TStepInFilter.Create;
  StepInFilter.DecayTime := 35;

  LoopingFadeOutOsc   := TFadeOutSampler.Create;
  LoopingFadeOutOsc.DecayTime := 35;

  RetriggerFadeOutOsc := TFadeOutSampler.Create;
  RetriggerFadeOutOsc.DecayTime := 35;

  fPitchShift := 0;
end;

destructor TOneShotSampleOsc.Destroy;
begin
  StepInFilter.Free;
  LoopingFadeOutOsc.Free;
  RetriggerFadeOutOsc.Free;

  inherited;
end;

function TOneShotSampleOsc.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'SampleResetInput' then exit(@ModPoints.SampleResetInput);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

function TOneShotSampleOsc.GetPitchParameters: PSampleOscPitchPar;
begin
  result := @PitchParameters;
end;

procedure TOneShotSampleOsc.SetLoopBounds(const Value: TSamplerLoopBounds);
begin
  fLoopBounds := Value;
end;

procedure TOneShotSampleOsc.SetLoopMode(const Value: TKeyGroupTriggerMode);
begin
  fLoopMode := Value;
end;

procedure TOneShotSampleOsc.SetSampleRate(const Value: single);
begin
  inherited;
  StepInFilter.SampleRate := round(Value);
  LoopingFadeOutOsc.SampleRate := Value;

  LoopTooSmallResetPoint := round(Value / 1000);
end;

function TOneShotSampleOsc.CalcPhaseCounterStepSize: double;
var
  x : double;
  TempoOfSource : single;
begin
  case PitchParameters.PitchTracking of
    TPitchTracking.Note:
    begin
      x := (PitchParameters.PlaybackNote - PitchParameters.RegionRootNote) + PitchParameters.SamplePitchAdjust + PitchParameters.VoicePitchAdjust + PitchParameters.PitchBendAdjust;
      x := SemiToneShiftToStepSize(x, 1);
      x := x * (CurSample.Properties.SampleRate / SampleRate);
      result := x;
    end;

    TPitchTracking.BPM:
    begin
      TempoOfSource   := CurRegion.GetProperties.SampleBeats * SourceTempoFactor;
      x := (CurSample.Properties.SampleRate * OneOverSampleRate) * (Tempo / TempoOfSource);
      result := x;
    end;

    TPitchTracking.Off:
    begin
      x := PitchParameters.SamplePitchAdjust + PitchParameters.VoicePitchAdjust + PitchParameters.PitchBendAdjust;
      x := SemiToneShiftToStepSize(x, 1);
      x := x * (CurSample.Properties.SampleRate / SampleRate);
      result := x;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;


procedure TOneShotSampleOsc.Trigger(const MidiNote : byte; const aSampleRegion:IRegion; const aSample: TSampleFloat);
begin
  IsFinishCalledNeeded := true;
  HasBeenReleased := false;

  if not assigned(CurSample) then
  begin
    CurRegion         := aSampleRegion;
    CurSample         := aSample;
  end else
  begin
    // TODO: We still have the potential for clicks with fast re-triggering here. Need to fix.
    StepInFilter.Trigger;
    RetriggerFadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);

    CurRegion         := aSampleRegion;
    CurSample         := aSample;
  end;






  //============================================================================
  //   Important: Do first!
  //============================================================================
  UpdateSampleBounds;

  //============================================================================
  //   Important: Do second!
  //============================================================================
  if assigned(CurSample) then
  begin
    PhaseCounter.ResetTo(CurrentSampleBounds.PlaybackSampleStart);
    PhaseCounter.StepSize := CalcPhaseCounterStepSize;
  end;

  // UpdateSampleBounds() needs to be called before CalcPhaseCounterStepSize()
  // because the StepSize depends on the sample start/end points for
  // tempo synced playback.
  //
  //============================================================================
  //============================================================================

end;

procedure TOneShotSampleOsc.Release;
begin
  HasBeenReleased := true;

  if (LoopMode = TKeyGroupTriggerMode.LoopRelease) then
  begin
    CurrentSampleBounds.PlaybackEnd := CurrentSampleBounds.SampleEnd;
  end;

end;



procedure TOneShotSampleOsc.UpdateSampleBounds;
begin
  assert(CurRegion <> nil);
  //Important: Update the sample bounds before any of the post processing...
  Event_UpdateSampleBounds(self, CurRegion, @CurrentSampleBounds);



  //==== Sample bounds post-processing ========================================
  case PitchParameters.PitchTracking of
    TPitchTracking.Note,
    TPitchTracking.Off:
    begin
      CurrentSampleBounds.PlaybackSampleStart := CurrentSampleBounds.SampleStart;

      if  (LoopBounds = TSamplerLoopBounds.LoopSample)
       or (LoopMode = TKeyGroupTriggerMode.LoopOff)
       or (LoopMode = TKeyGroupTriggerMode.OneShot)
       or ((HasBeenReleased) and (LoopMode = TKeyGroupTriggerMode.LoopRelease))
       then
      begin
        CurrentSampleBounds.PlaybackLoopStart := CurrentSampleBounds.SampleStart;
        CurrentSampleBounds.PlaybackEnd       := CurrentSampleBounds.SampleEnd;
      end else
      begin
        CurrentSampleBounds.PlaybackLoopStart := CurrentSampleBounds.LoopStart;
        CurrentSampleBounds.PlaybackEnd       := CurrentSampleBounds.LoopEnd;
      end;

      SourceTempoFactor := 1;
    end;

    TPitchTracking.BPM:
    begin
      case LoopBounds of
        TSamplerLoopBounds.LoopSample:
        begin
          CurrentSampleBounds.PlaybackSampleStart := CurrentSampleBounds.SampleStart;
          CurrentSampleBounds.PlaybackLoopStart   := CurrentSampleBounds.SampleStart;
          CurrentSampleBounds.PlaybackEnd         := CurrentSampleBounds.SampleEnd;
        end;

        TSamplerLoopBounds.LoopPoints:
        begin
          CurrentSampleBounds.PlaybackSampleStart := CurrentSampleBounds.LoopStart;
          CurrentSampleBounds.PlaybackLoopStart   := CurrentSampleBounds.LoopStart;

          if ((HasBeenReleased) and (LoopMode = TKeyGroupTriggerMode.LoopRelease))
            then CurrentSampleBounds.PlaybackEnd         := CurrentSampleBounds.SampleEnd
            else CurrentSampleBounds.PlaybackEnd         := CurrentSampleBounds.LoopEnd;
        end;
      else
        raise Exception.Create('Type not handled.');
      end;

      SourceTempoFactor := 1 / (CurrentSampleBounds.SampleEnd - CurrentSampleBounds.SampleStart) * CurSample.Properties.SampleRate * 60;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

  if (CurrentSampleBounds.PlaybackEnd < CurrentSampleBounds.PlaybackSampleStart) and (CurrentSampleBounds.PlaybackEnd < CurrentSampleBounds.PlaybackLoopStart) then
  begin
    IsLoopTooSmall := true;
    LoopTooSmallCount := 0;

    // TODO:MED it might be useful to have a gain variable for loop size so the
    // small loops will become quieter before stopping entirely.
  end else
  begin
    IsLoopTooSmall := false;
  end;




  // NOTE: The sample bounds post-processing above seems a bit complicated and
  // perhaps there are unnecessary calculations being made. It might
  // be worthwhile to take a closer look and fine tune the way the above stuff
  // works.

end;

procedure TOneShotSampleOsc.Kill;
begin
  CurRegion := nil;
  CurSample := nil;
  LoopingFadeOutOsc.Kill;
  RetriggerFadeOutOsc.Kill;
end;


procedure TOneShotSampleOsc.ResetSamplePosition;
begin
  StepInFilter.Trigger;
  LoopingFadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
  UpdateSampleBounds;
  PhaseCounter.ResetTo(CurrentSampleBounds.PlaybackSampleStart);
end;

procedure TOneShotSampleOsc.FastControlProcess;
begin

end;

procedure TOneShotSampleOsc.SlowControlProcess;
begin
  if assigned(CurSample) then
  begin
    PhaseCounter.StepSize := CalcPhaseCounterStepSize;
  end;
end;

procedure TOneShotSampleOsc.AudioRateStep(out Out1, Out2: Single);
var
  ax         : integer;
  frac       : single;
  xOut1, xOut2 : single;
begin
  if IsLoopTooSmall then
  begin
    if LoopTooSmallCount >= LoopTooSmallResetPoint
      then UpdateSampleBounds
      else Inc(LoopTooSmallCount);
    Out1 := 0;
    Out2 := 0;
  end else
  if (assigned(CurSample))  then
  begin
    ax   := PhaseCounter.IntegerPart;
    frac := PhaseCounter.FractionalPart;
    if ax >= CurSample.Properties.SampleFrames-4 then ax := CurSample.Properties.SampleFrames-5; //TODO: Can this be removed?
    assert(ax <= CurSample.Properties.SampleFrames-4);
    assert(ax >= 0);

    ReadValuesFromSample_4x3_Optimal(CurSample, ax, Frac, Out1, Out2);

    if StepInFilter.IsActive then StepInFilter.Process(Out1, Out2);
  end else
  begin
    Out1 := 0;
    Out2 := 0;
  end;


  if LoopingFadeOutOsc.IsActive then
  begin
    LoopingFadeOutOsc.AudioRateStep(xOut1, xOut2);
    Out1 := Out1 + xOut1;
    Out2 := Out2 + xOut2;
  end;

  if RetriggerFadeOutOsc.IsActive then
  begin
    RetriggerFadeOutOsc.AudioRateStep(xOut1, xOut2);
    Out1 := Out1 + xOut1;
    Out2 := Out2 + xOut2;
  end;


  if IsLoopTooSmall then
  begin
    // do nothing...
  end else
  if (PhaseCounter >= CurrentSampleBounds.PlaybackEnd) then
  begin
    case LoopMode of
      TKeyGroupTriggerMode.LoopOff:
      begin
        if (IsFinishCalledNeeded) and (not IsSampleResetActive) then
        begin
          IsFinishCalledNeeded := false;
          OnFinished(self);
        end;
      end;

      TKeyGroupTriggerMode.LoopSustain:
      begin
        StepInFilter.Trigger;
        LoopingFadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
        UpdateSampleBounds;
        PhaseCounter.ResetTo(CurrentSampleBounds.PlaybackLoopStart);
        VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
      end;

      TKeyGroupTriggerMode.LoopRelease:
      begin
        if HasBeenReleased = false then
        begin
          StepInFilter.Trigger;
          LoopingFadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
          UpdateSampleBounds;
          PhaseCounter.ResetTo(CurrentSampleBounds.PlaybackLoopStart);
          VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
        end else
        if (IsFinishCalledNeeded) and (not IsSampleResetActive) then
        begin
          IsFinishCalledNeeded := false;
          OnFinished(self);
        end;
      end;

      TKeyGroupTriggerMode.OneShot:
      begin
        // IMPORTANT: Igore the IsSampleResetActive value when in one shot mode.
        //if (IsFinishCalledNeeded) and (not IsSampleResetActive) then
        if (IsFinishCalledNeeded) then
        begin
          IsFinishCalledNeeded := false;
          OnFinished(self);
        end;
      end;
    else
      raise Exception.Create('type not handled.');
    end;
  end else
  begin
    PhaseCounter.Step;
  end;

end;

//================================================================================
//================================================================================

procedure TOneShotSampleOsc.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
var
  SampleBounds : TSampleOsc_SampleBounds;
begin
  // Get current playback position.

  FeedBackData.SampleBounds.ShowPlaybackBounds := true;

  case LoopBounds of
    //TSamplerLoopBounds.LoopOff:    FeedbackData.SampleBounds.ShowLoopBounds := false;
    TSamplerLoopBounds.LoopSample: FeedbackData.SampleBounds.ShowLoopBounds := false;
    TSamplerLoopBounds.LoopPoints: FeedbackData.SampleBounds.ShowLoopBounds := true;
  else
    raise Exception.Create('Loop type not handled.');
  end;

  case LoopBounds of
    //TSamplerLoopBounds.LoopOff:    FeedbackData.SampleBounds.ShowHighlightBounds := false;
    TSamplerLoopBounds.LoopSample: FeedbackData.SampleBounds.ShowHighlightBounds := false;
    TSamplerLoopBounds.LoopPoints: FeedbackData.SampleBounds.ShowHighlightBounds := true;
  else
    raise Exception.Create('Loop type not handled.');
  end;


  FeedBackData.SampleBounds.LoopStart      := CurrentSampleBounds.LoopStart;
  FeedBackData.SampleBounds.LoopEnd        := CurrentSampleBounds.LoopEnd;

  FeedbackData.SampleBounds.HighlightStart := CurrentSampleBounds.LoopStart;
  FeedBackData.SampleBounds.HighlightEnd   := CurrentSampleBounds.LoopEnd;

  FeedbackData.SampleBounds.PlaybackPos := PhaseCounter.IntegerPart;


  // get real time sample bounds...
  self.Event_UpdateSampleBounds(self, self.CurRegion, @SampleBounds);


  FeedBackData.SampleBounds.PlaybackStart  := CurrentSampleBounds.SampleStart;
  FeedBackData.SampleBounds.PlaybackEnd    := CurrentSampleBounds.SampleEnd;
  FeedBackData.SampleBounds.RealTime_ModSampleStart := SampleBounds.SampleStart;
  FeedBackData.SampleBounds.RealTime_ModSampleEnd   := SampleBounds.SampleEnd;
  FeedBackData.SampleBounds.RealTime_ModLoopStart   := SampleBounds.LoopStart;
  FeedBackData.SampleBounds.RealTime_ModLoopEnd     := SampleBounds.LoopEnd;

  case LoopBounds of
    TSamplerLoopBounds.LoopSample:
    begin
      FeedBackData.SampleBounds.PlaybackStart  := CurrentSampleBounds.SampleStart;
      FeedBackData.SampleBounds.PlaybackEnd    := CurrentSampleBounds.SampleEnd;
    end;

    TSamplerLoopBounds.LoopPoints:
    begin
      FeedBackData.SampleBounds.PlaybackStart  := CurrentSampleBounds.LoopStart;
      FeedBackData.SampleBounds.PlaybackEnd    := CurrentSampleBounds.LoopEnd;
    end
  else
    raise Exception.Create('Loop type not handled.');
  end;

end;



end.
