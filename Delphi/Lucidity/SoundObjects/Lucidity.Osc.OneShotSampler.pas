unit Lucidity.Osc.OneShotSampler;

interface

{$INCLUDE Defines.inc}

uses
  Classes,
  MoreTypes, Lucidity.SampleMap, uLucidityCustomSampleOsc, soGateEnvelope,
  uConstants, uLucidityEnums, eeSampleFloat, uLucidityClock,
  eeCounter,
  soStepInFilter,
  Lucidity.Osc.FadeOutSampler,
  uGuiFeedbackData;

type
  TLuciditySampleOscModulationPoints = record
    SampleResetInput : single;
  end;

  TOneShotSampleOsc = class(TCustomSampleOsc)
  private
    fLoopBounds: TSamplerLoopBounds;
    fOnFinished: TNotifyEvent;
    fPitchShift: single;
    fLoopMode: TSamplerLoopMode;
    procedure SetLoopBounds(const Value: TSamplerLoopBounds);
    procedure SetLoopMode(const Value: TSamplerLoopMode);
  protected
    ModPoints : TLuciditySampleOscModulationPoints;
    CurRegion : IRegion;
    CurSample : TSampleFloat;

    PhaseCounter : TCounter;
    CurrentSampleBounds : TSampleOsc_SampleBounds;

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
  public
    constructor Create(const aVoiceModPoints : PVoiceModulationPoints; const aVoiceClockManager : TLucidityVoiceClockManager); override;
    destructor Destroy; override;

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

    property LoopBounds : TSamplerLoopBounds read fLoopBounds write SetLoopBounds;
    property LoopMode   : TSamplerLoopMode   read fLoopMode   write SetLoopMode;


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
  result := nil;
end;

procedure TOneShotSampleOsc.SetLoopBounds(const Value: TSamplerLoopBounds);
begin
  fLoopBounds := Value;
end;

procedure TOneShotSampleOsc.SetLoopMode(const Value: TSamplerLoopMode);
begin
  fLoopMode := Value;
end;

procedure TOneShotSampleOsc.SetSampleRate(const Value: single);
begin
  inherited;
  StepInFilter.SampleRate := round(Value);
  LoopingFadeOutOsc.SampleRate := Value;
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

  UpdateSampleBounds;

  if assigned(CurSample) then
  begin
    PhaseCounter.ResetTo(CurrentSampleBounds.SampleStart);
    PhaseCounter.StepSize := SemiToneShiftToStepSize(PitchShift, 1) * (CurSample.Properties.SampleRate / SampleRate);
  end;
end;

procedure TOneShotSampleOsc.Release;
begin
  HasBeenReleased := true;

  if (LoopMode = TSamplerLoopMode.LoopRelease) then
  begin
    CurrentSampleBounds.LoopEnd   := CurrentSampleBounds.SampleEnd;
  end;

end;



procedure TOneShotSampleOsc.UpdateSampleBounds;
begin
  assert(CurRegion <> nil);
  Event_UpdateSampleBounds(self, CurRegion, @CurrentSampleBounds);

  if (LoopBounds = TSamplerLoopBounds.LoopSample) then
  begin
    CurrentSampleBounds.LoopStart := CurrentSampleBounds.SampleStart;
    CurrentSampleBounds.LoopEnd   := CurrentSampleBounds.SampleEnd;
  end;

  if (LoopMode = TSamplerLoopMode.LoopOff) then
  begin
    CurrentSampleBounds.LoopStart := CurrentSampleBounds.SampleStart;
    CurrentSampleBounds.LoopEnd   := CurrentSampleBounds.SampleEnd;
  end;

  if (LoopMode = TSamplerLoopMode.OneShot) then
  begin
    CurrentSampleBounds.LoopStart := CurrentSampleBounds.SampleStart;
    CurrentSampleBounds.LoopEnd   := CurrentSampleBounds.SampleEnd;
  end;

  if (HasBeenReleased) and (LoopMode = TSamplerLoopMode.LoopRelease) then
  begin
    CurrentSampleBounds.LoopEnd   := CurrentSampleBounds.SampleEnd;
  end;
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
  PhaseCounter.ResetTo(CurrentSampleBounds.SampleStart);
end;

procedure TOneShotSampleOsc.FastControlProcess;
begin

end;

procedure TOneShotSampleOsc.SlowControlProcess;
begin
  if assigned(CurSample) then
  begin
    PhaseCounter.StepSize := SemiToneShiftToStepSize(PitchShift, 1) * (CurSample.Properties.SampleRate / SampleRate);
  end;
end;

procedure TOneShotSampleOsc.AudioRateStep(out Out1, Out2: Single);
var
  ax         : integer;
  frac       : single;
  xOut1, xOut2 : single;
  StepSize : single;
begin
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



  if (PhaseCounter >= CurrentSampleBounds.LoopEnd) then
  begin
    case LoopMode of
      TSamplerLoopMode.LoopOff:
      begin
        if (IsFinishCalledNeeded) then
        begin
          IsFinishCalledNeeded := false;
          OnFinished(self);
        end;
      end;

      TSamplerLoopMode.LoopSustain:
      begin
        StepInFilter.Trigger;
        LoopingFadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
        UpdateSampleBounds;
        PhaseCounter.ResetTo(CurrentSampleBounds.LoopStart);
        VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
      end;

      TSamplerLoopMode.LoopRelease:
      begin
        if HasBeenReleased = false then
        begin
          StepInFilter.Trigger;
          LoopingFadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
          UpdateSampleBounds;
          PhaseCounter.ResetTo(CurrentSampleBounds.LoopStart);
          VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
        end else
        if (IsFinishCalledNeeded) then
        begin
          IsFinishCalledNeeded := false;
          OnFinished(self);
        end;
      end;

      TSamplerLoopMode.OneShot:
      begin
        if (IsFinishCalledNeeded) then
        begin
          IsFinishCalledNeeded := false;
          OnFinished(self);
        end;
      end;
    else
      raise Exception.Create('type not handled.');
    end;


    {
    case LoopBounds of
      //TODO: LoopMode isn't loop mode anymore.

      //TSamplerLoopBounds.LoopOff:
      //begin
      //  if (IsFinishCalledNeeded) then
      //  begin
      //    IsFinishCalledNeeded := false;
      //    OnFinished(self);
      //  end;
      //end;

      TSamplerLoopBounds.LoopSample,
      TSamplerLoopBounds.LoopPoints:
      begin
        StepInFilter.Trigger;
        LoopingFadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
        UpdateSampleBounds;
        PhaseCounter.ResetTo(CurrentSampleBounds.LoopStart);
        VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
      end;
    else
      raise Exception.Create('Loop type not handled.');
    end;
    }
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
  xPos : integer;
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

  FeedBackData.SampleBounds.PlaybackStart  := CurrentSampleBounds.SampleStart;
  FeedBackData.SampleBounds.PlaybackEnd    := CurrentSampleBounds.SampleEnd;

  FeedBackData.SampleBounds.LoopStart      := CurrentSampleBounds.LoopStart;
  FeedBackData.SampleBounds.LoopEnd        := CurrentSampleBounds.LoopEnd;

  FeedbackData.SampleBounds.HighlightStart := CurrentSampleBounds.LoopStart;
  FeedBackData.SampleBounds.HighlightEnd   := CurrentSampleBounds.LoopEnd;

  FeedbackData.SampleBounds.PlaybackPos := PhaseCounter.IntegerPart;


  // get real time sample bounds...
  self.Event_UpdateSampleBounds(self, self.CurRegion, @SampleBounds);

  FeedBackData.SampleBounds.ModLoopStart := SampleBounds.LoopStart;
  FeedBackData.SampleBounds.ModLoopEnd   := SampleBounds.LoopEnd;

end;



end.
