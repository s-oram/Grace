unit Lucidity.Osc.GrainStretch;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.MoreTypes, Lucidity.SampleMap, uLucidityCustomSampleOsc,
  soGrainStretchSubOsc, eeDsp, eeFunctions, uGuiFeedbackData,
  uConstants, uLucidityEnums, eeSampleFloat, eeVirtualCV, Math,
  soGateEnvelope,
  uLucidityClock;

const
  kMaxGrainCount = 16;

type
  TGrainStretchModPoints = record
    DestGrainSize : single;
    DestRate      : single;
    DestPosition  : single;
  end;

  TLucidityGrainStretchOsc = class(TCustomSampleOsc)
  private
    fGrainLength: single;
    fGrainRate: single;
    fPitchOne: single;
    fPitchTwo: single;
    fGrainPosition: single;
    fLoopMode: TGrainStretchLoopMode;
  protected
    TriggerNote : byte;
    RootNote    : byte;

    SampleBounds : TSampleOsc_SampleBounds;

    ModPoints : TGrainStretchModPoints;

    Sample : TSampleFloat;
    SampleRegion : IRegion;

    MasterReadIndex : single;
    MasterReadRate  : single;

    GrainOscs : array[0..kMaxGrainCount-1] of TGrainStretchSubOsc;

    LastGrain    : TGrainProperties;
    LastGrainOsc : TGrainStretchSubOsc;

    SamplesToNextGrain : integer;
    TriggerMoreGrains : boolean;



    procedure SetSampleRate(const Value: single); override;

    procedure UpdateSampleBounds; inline;

    procedure TriggerGrain; inline;

    function FindInactiveGrain:TGrainStretchSubOsc; inline;
  public
    constructor Create(const aVoiceModPoints : PVoiceModulationPoints; const aVoiceClockManager : TLucidityVoiceClockManager); override;
    destructor Destroy; override;

    // call to reset sample playback to the beginning of the sample.
    procedure ResetSamplePosition;

    function GetModPointer(const Name:string):PSingle;

    procedure Trigger(const MidiNote : byte; const aSampleRegion:IRegion; const aSample:TSampleFloat);
    procedure Kill;

    procedure FastControlRateProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateStep(out Out1, Out2 : Single);  {$IFDEF AudioInline}inline;{$ENDIF}


    //== Parameters ===
    property PitchOne : single read fPitchOne write fPitchOne; //range -1..1
    property PitchTwo : single read fPitchTwo write fPitchTwo; //range -1..1
    property LoopMode : TGrainStretchLoopMode read fLoopMode write fLoopMode;
    property GrainLength   : single read fGrainLength   write fGrainLength; //range 0..1
    property GrainRate     : single read fGrainRate     write fGrainRate;   //range -1..1
    property GrainPosition : single read fGrainPosition write fGrainPosition; //range 0..1

    //== For GUI Feedback ==
    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);
  end;


implementation

uses
  SysUtils,
  SampleOscUtils;

{ TLucidityGrainStretchOsc }

constructor TLucidityGrainStretchOsc.Create(const aVoiceModPoints : PVoiceModulationPoints; const aVoiceClockManager : TLucidityVoiceClockManager);
var
  c1: Integer;
begin
  inherited;

  for c1 := 0 to kMaxGrainCount-1 do
  begin
    GrainOscs[c1] := TGrainStretchSubOsc.Create;
    GrainOscs[c1].OnUpdateSampleBounds := Event_UpdateSampleBounds;
  end;
end;

destructor TLucidityGrainStretchOsc.Destroy;
var
  c1 : integer;
begin
  for c1 := 0 to kMaxGrainCount-1 do
  begin
    GrainOscs[c1].Free;
  end;
  inherited;
end;

function TLucidityGrainStretchOsc.FindInactiveGrain: TGrainStretchSubOsc;
var
  c1: Integer;
begin
  for c1 := 0 to kMaxGrainCount-1 do
  begin
    if GrainOscs[c1].IsActive = false then
    begin
      result := GrainOscs[c1];
      exit; //============================>>
    end;
  end;

  result := nil;
end;

function TLucidityGrainStretchOsc.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'DestGrainSize' then exit(@ModPoints.DestGrainSize);
  if Name = 'DestRate' then exit(@ModPoints.DestRate);
  if Name = 'DestPosition' then exit(@ModPoints.DestPosition);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

procedure TLucidityGrainStretchOsc.SetSampleRate(const Value: single);
var
  c1 : integer;
begin
  inherited;
  for c1 := 0 to kMaxGrainCount-1 do
  begin
    GrainOscs[c1].SampleRate := value;
  end;
end;

procedure TLucidityGrainStretchOsc.Trigger(const MidiNote: byte; const aSampleRegion: IRegion; const aSample: TSampleFloat);
var
  c1 : integer;
begin
  TriggerMoreGrains := true;

  Sample       := aSample;
  SampleRegion := aSampleRegion;

  UpdateSampleBounds;

  TriggerNote := MidiNote;
  RootNote    := aSampleRegion.GetProperties^.RootNote;

  for c1 := 0 to kMaxGrainCount-1 do
  begin
    GrainOscs[c1].SetupOsc(aSampleRegion, aSample);
  end;

  MasterReadIndex := SampleBounds.SampleStart;
  MasterReadRate  := GrainRate * (Sample.Properties.SampleRate / SampleRate);

  TriggerGrain;
end;

procedure TLucidityGrainStretchOsc.UpdateSampleBounds;
begin
  self.Event_UpdateSampleBounds(self, SampleRegion, @SampleBounds);
end;

procedure TLucidityGrainStretchOsc.Kill;
var
  c1 : integer;
begin
  for c1 := 0 to kMaxGrainCount-1 do
  begin
    GrainOscs[c1].Kill;
  end;


  Sample       := nil;
  SampleRegion := nil;

  TriggerMoreGrains := false;

  LastGrainOsc := nil;
end;

procedure TLucidityGrainStretchOsc.ResetSamplePosition;
begin

end;

procedure TLucidityGrainStretchOsc.TriggerGrain;
const
  MinimumGrainTime = 15;
var
  gp : TGrainProperties;
  NextGrainOsc : TGrainStretchSubOsc;

  GrainSampleFrames : single;
  LimitedSampleFrames : single;

  GrainLengthMS : single;

  GrainFadeSamples : single;

  xgLengthMod     : single;
  xgGrainStepSize : single;

  GrainStartOffset : single;

  GrainRateRatio : single;
begin
  //TODO: Need a minimum grain time.
  UpdateSampleBounds;

  //Sample bounds have been updated so we need to check MasterReadIndex is still in bounds.
  if LoopMode = TGrainStretchLoopMode.LoopOff then
  begin
    if MasterReadIndex >= SampleBounds.SampleEnd   then MasterReadIndex := SampleBounds.SampleEnd-1;
    if MasterReadIndex <  SampleBounds.SampleStart then MasterReadIndex :=  SampleBounds.SampleStart;
  end else
  begin
    if MasterReadIndex <  SampleBounds.SampleStart then MasterReadIndex := SampleBounds.SampleEnd-1;
    if MasterReadIndex >= SampleBounds.SampleEnd   then MasterReadIndex := SampleBounds.SampleStart;
  end;

  //Calculate grain step size to play at desired pitch.
  xgGrainStepSize := CalcSampleStepSize(TriggerNote, RootNote, PitchOne, PitchTwo) * (Sample.Properties.SampleRate / SampleRate);


  //Calculate grain length
  xgLengthMod := (self.GrainLength * 5) + AudioRangeToModularVoltage(ModPoints.DestGrainSize);
  GrainLengthMS := ExponentialShift(MinimumGrainTime, xgLengthMod);
  GrainSampleFrames := MillisecondsToSamples(GrainLengthMS, SampleRate);

  // Check if grain length exceeds entire sample area between sample start and sample end.
  LimitedSampleFrames := SampleBounds.SampleEnd - SampleBounds.SampleStart;
  if GrainSampleFrames > LimitedSampleFrames then GrainSampleFrames := LimitedSampleFrames;

  // Calculate the grain fade time. Grain fade is the same as the default overlap time
  GrainFadeSamples := round(GrainSampleFrames / 10);

  //== calculate when in the sample the grain will start ==
  // First step is to calculate the grain offset as a 0 to 1 ranged value..
  // The grain start position is relative to MasterReadIndex.

  if SampleBounds.SampleStart <> SampleBounds.SampleEnd
    then GrainStartOffset := (MasterReadIndex - SampleBounds.SampleStart) / (SampleBounds.SampleEnd - SampleBounds.SampleStart)
    else GrainStartOffset := 0;
  assert(GrainStartOffset >= 0);
  assert(GrainStartOffset <= 1);

  // modifiy the GrainStartOffset to take "GrainPosition" control into account, still in the 0 to 1 range..
  GrainStartOffset := GrainStartOffset + GrainPosition + ModPoints.DestPosition;
  while GrainStartOffset > 1 do GrainStartOffset := GrainStartOffset - 1;
  while GrainStartOffset < 0 do GrainStartOffset := GrainStartOffset + 1;

  assert(GrainStartOffset >= 0);
  assert(GrainStartOffset <= 1);

  // convert the 0 to 1 range GrainStartOffset value to an actual sample value. The grain.SampleStart value is
  // calculated so that the end of the grain will never exceed SampleBounds.SampleEnd;
  gp.SampleStart  := round(SampleBounds.SampleStart + ((LimitedSampleFrames - GrainSampleFrames) * GrainStartOffset));

  //====

  // write some other grain values...
  gp.SampleFrames := round(GrainSampleFrames);
  gp.GrainFadeSamples := round(GrainFadeSamples);
  gp.GrainStepSize := xgGrainStepSize;

  LastGrain.AssignFrom(gp);

  NextGrainOsc := FindInactiveGrain;
  NextGrainOsc.Trigger(gp);

  LastGrainOsc := NextGrainOsc;


  //=== Find the number of samples before the next grain starts ===
  // This calucation needs to take a couple of things into account.
  // - the grain size (in sampleframes)
  // - the rate the grain is being played back. (StepSize)\
  // - how fast the MasterReadIndex is being advanced through the sample.
  GrainRateRatio := abs(MasterReadRate) / abs(xgGrainStepSize);

  if GrainRateRatio > 1
    then SamplesToNextGrain := round((GrainSampleFrames / xgGrainStepSize - GrainFadeSamples) / GrainRateRatio)
    else SamplesToNextGrain := round((GrainSampleFrames / xgGrainStepSize - GrainFadeSamples));


  VoiceClockManager.SendClockEvent(ClockID_SampleLoop);
end;

procedure TLucidityGrainStretchOsc.FastControlRateProcess;
begin
  MasterReadRate := (GrainRate + ModPoints.DestRate) * (Sample.Properties.SampleRate / SampleRate);
end;

procedure TLucidityGrainStretchOsc.AudioRateStep(out Out1, Out2: Single);
var
  c1 : integer;
  xOut1, xOut2 : Single;
begin
  if TriggerMoreGrains then
  begin
    MasterReadIndex := MasterReadIndex + MasterReadRate;

    if LoopMode = TGrainStretchLoopMode.LoopOff then
    begin
      if MasterReadIndex <  SampleBounds.SampleStart then TriggerMoreGrains := false;
      if MasterReadIndex >= SampleBounds.SampleEnd   then TriggerMoreGrains := false;
    end else
    begin
      if MasterReadIndex <  SampleBounds.SampleStart then MasterReadIndex := SampleBounds.SampleEnd-1;
      if MasterReadIndex >= SampleBounds.SampleEnd   then MasterReadIndex := SampleBounds.SampleStart;
    end;

    dec(SamplesToNextGrain);
    if SamplesToNextGrain <= 0 then TriggerGrain;
  end;

  //=== Audio processing ===
  Out1 := 0;
  Out2 := 0;

  for c1 := 0 to kMaxGrainCount-1 do
  begin
    if GrainOscs[c1].IsActive then
    begin
      GrainOscs[c1].AudioRateStep(xOut1, xOut2);
      Out1 := Out1 + xOut1;
      Out2 := Out2 + xOut2;
    end;
  end;
end;

procedure TLucidityGrainStretchOsc.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
begin
  if assigned(LastGrainOsc)
    then FeedbackData.SampleBounds.PlaybackPos := round(LastGrainOsc.GetReadIndex)
    else FeedbackData.SampleBounds.PlaybackPos := round(MasterReadIndex);
  FeedbackData.SampleBounds.ShowHighlightBounds := true;
  FeedbackData.SampleBounds.HighlightStart      := LastGrain.SampleStart;
  FeedbackData.SampleBounds.HighlightEnd        := LastGrain.SampleStart + LastGrain.SampleFrames;
end;




end.

