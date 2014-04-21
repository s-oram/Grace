unit Lucidity.Osc.LoopSampler;

interface

{$INCLUDE Defines.inc}

uses
  Classes, Lucidity.Interfaces,
  VamLib.MoreTypes, Lucidity.SampleMap, uLucidityCustomSampleOsc,
  soGateEnvelope, soStepInFilter,
  eeCounter,
  Lucidity.Osc.FadeOutSampler,
  uConstants, uLucidityEnums,
  eeSampleFloat,
  uGuiFeedbackData,
  uLucidityClock;


type
  TLoopSampleOsc = class(TCustomSampleOsc)
  private
    fLoopBounds: TSamplerLoopBounds;
    fOnFinished: TNotifyEvent;
    fLoopMode: TSamplerLoopMode;
    procedure SetLoopBounds(const Value: TSamplerLoopBounds);
    procedure SetLoopMode(const Value: TSamplerLoopMode);
  protected
    PhaseCounter : TCounter;
    TriggerNote : byte;
    CurRegion : IRegion;
    CurSample : TSampleFloat;
    CurrentSampleBounds : TSampleOsc_SampleBounds;

    StepInFilter : TStepInFilter;
    FadeOutOsc   : TFadeOutSampler;

    SourceTempoFactor : single;
    TempoOfSource : single;
    OneOverTempoOfSource : double; //for optimisation purposes.

    IsFinishCalledNeeded : boolean;

    procedure SetSampleRate(const Value: single); override;

    procedure UpdateSampleBounds;
  public
    constructor Create(const aVoiceModPoints : PVoiceModulationPoints; const aVoiceClockManager : TLucidityVoiceClockManager); override;
    destructor Destroy; override;

    // call to reset sample playback to the beginning of the sample.
    procedure ResetSamplePosition;

    function GetModPointer(const Name:string):PSingle;

    procedure Trigger(const MidiNote : byte; const aSampleRegion:IRegion; const aSample:TSampleFloat);
    procedure Kill;


    procedure AudioRateStep(out Out1, Out2 : Single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}


    //== Parameters ===
    property LoopBounds : TSamplerLoopBounds read fLoopBounds write SetLoopBounds;
    property LoopMode   : TSamplerLoopMode   read fLoopMode   write SetLoopMode;

    //== For GUI Feedback ==
    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);

    property OnFinished : TNotifyEvent read fOnFinished write fOnFinished;

  end;


implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils,
  Math, SampleOscUtils;

{ TLoopSampleOsc }

constructor TLoopSampleOsc.Create(const aVoiceModPoints: PVoiceModulationPoints; const aVoiceClockManager : TLucidityVoiceClockManager);
begin
  inherited;
  SourceTempoFactor := 0;

  StepInFilter := TStepInFilter.Create;
  StepInFilter.DecayTime := 20;

  FadeOutOsc   := TFadeOutSampler.Create;
  FadeOutOsc.DecayTime := 20;
end;

destructor TLoopSampleOsc.Destroy;
begin
  StepInFilter.Free;
  FadeOutOsc.Free;
  inherited;
end;

function TLoopSampleOsc.GetModPointer(const Name: string): PSingle;
begin
  assert(false, 'TODO');
  result := nil;
end;

procedure TLoopSampleOsc.ResetSamplePosition;
begin
  FadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
  UpdateSampleBounds;
  PhaseCounter.ResetTo(CurrentSampleBounds.SampleStart);
  StepInFilter.Trigger;
end;

procedure TLoopSampleOsc.SetLoopBounds(const Value: TSamplerLoopBounds);
begin
  fLoopBounds := Value;
end;

procedure TLoopSampleOsc.SetLoopMode(const Value: TSamplerLoopMode);
begin
  fLoopMode := Value;
end;

procedure TLoopSampleOsc.SetSampleRate(const Value: single);
begin
  inherited;
  StepInFilter.SampleRate := round(Value);
  FadeOutOsc.SampleRate := Value;
end;

procedure TLoopSampleOsc.UpdateSampleBounds;
begin
  assert(CurRegion <> nil);
  assert(CurSample <> nil);
  Event_UpdateSampleBounds(self, CurRegion, @CurrentSampleBounds);

  //SourceTempoFactor is calculated here because it only changes when the sample bounds change.
  SourceTempoFactor := 1 / (CurrentSampleBounds.SampleEnd - CurrentSampleBounds.SampleStart) * CurSample.Properties.SampleRate * 60;

end;

procedure TLoopSampleOsc.Trigger(const MidiNote: byte; const aSampleRegion: IRegion; const aSample: TSampleFloat);
begin
  IsFinishCalledNeeded := true;
  FadeOutOsc.Reset;

  TriggerNote := MidiNote;
  CurRegion   := aSampleRegion;
  CurSample   := aSample;

  UpdateSampleBounds;

  TempoOfSource   := CurRegion.GetProperties.SampleBeats * SourceTempoFactor;

  PhaseCounter.ResetTo(CurrentSampleBounds.LoopStart);
  PhaseCounter.StepSize := (CurSample.Properties.SampleRate * OneOverSampleRate) * (Tempo / TempoOfSource);
end;

procedure TLoopSampleOsc.Kill;
begin
  FadeOutOsc.Kill;
  CurRegion := nil;
  CurSample := nil;
  SourceTempoFactor := 0;
end;

procedure TLoopSampleOsc.FastControlProcess;
begin
  //SourceTempoFactor should never be zero by the time ControlRateStep() is called.
  assert(SourceTempoFactor <> 0);

  TempoOfSource         := CurRegion.GetProperties.SampleBeats * SourceTempoFactor;
  PhaseCounter.StepSize := (CurSample.Properties.SampleRate * OneOverSampleRate) * (Tempo / TempoOfSource);
end;

procedure TLoopSampleOsc.SlowControlProcess;
begin

end;

procedure TLoopSampleOsc.AudioRateStep(out Out1, Out2: Single);
var
  ax         : integer;
  frac       : single;
  xOut1, xOut2 : single;
begin
  ax   := PhaseCounter.IntegerPart;
  frac := PhaseCounter.FractionalPart;

  if ax >= CurSample.Properties.SampleFrames-4 then ax := CurSample.Properties.SampleFrames-4;

  ReadValuesFromSample_4x3_Optimal(CurSample, ax, Frac, Out1, Out2);

  StepInFilter.Process(Out1, Out2);
  if FadeOutOsc.IsActive then
  begin
    FadeOutOsc.AudioRateStep(xOut1, xOut2);
    Out1 := Out1 + xOut1;
    Out2 := Out2 + xOut2;
  end;


  //TODO: LoopMode isn't loopMode anymore.
  case LoopBounds of
    {
    TSamplerLoopBounds.LoopOff:
    begin
      if (PhaseCounter < CurrentSampleBounds.SampleEnd) then
      begin
        PhaseCounter.Step;
      end else
      begin
        if IsFinishCalledNeeded then
        begin
          IsFinishCalledNeeded := false;
          OnFinished(self);
        end;
      end;
    end;
    }
    TSamplerLoopBounds.LoopSample:
    begin
      PhaseCounter.Step;
      if (PhaseCounter >= CurrentSampleBounds.SampleEnd) then
      begin
        FadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
        UpdateSampleBounds;
        PhaseCounter.ResetTo(CurrentSampleBounds.SampleStart);
        StepInFilter.Trigger;
        VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
      end;
    end;

    TSamplerLoopBounds.LoopPoints:
    begin
      PhaseCounter.Step;
      if (PhaseCounter >= CurrentSampleBounds.LoopEnd) then
      begin
        FadeOutOsc.Trigger(CurRegion, PhaseCounter.AsFloat, PhaseCounter.StepSize);
        UpdateSampleBounds;
        PhaseCounter.ResetTo(CurrentSampleBounds.LoopStart);
        StepInFilter.Trigger;
        VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
      end;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

end;

procedure TLoopSampleOsc.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
begin
  FeedbackData.SampleBounds.PlaybackPos := PhaseCounter.IntegerPart;

  FeedbackData.SampleBounds.ShowHighlightBounds := true;
  FeedbackData.SampleBounds.HighlightStart := CurrentSampleBounds.LoopStart;
  FeedbackData.SampleBounds.HighlightEnd   := CurrentSampleBounds.LoopEnd;
end;

end.
