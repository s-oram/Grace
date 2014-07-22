unit uLucidityStepSequencer;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.ZeroObject,
  VamGuiControlInterfaces,
  B2.Filter.CriticallyDampedLowpass, eeBiquadFilterCore,
  VamLib.MoreTypes,
  uLucidityEnums,
  uLucidityClock,
  uConstants;

type
  // TODO: Step Sequencer needs to use step sequence values from global Step seq data object
  // instead of the internal step values.
  TLucidyStepSequencer = class(TZeroObject)
  private
    fStepCountAsInt: integer;
    fCurrentStep: integer;
    fDirection: TStepSequencerDirection;
    fStepCount: TStepSequencerLength;
    fStepSeqClock: TSequencerClock;
    fSampleRate: single;
    fBpm: single;
    FSequenceData: IStepSequenceDataObject;
    procedure SetStepCount(const Value: TStepSequencerLength);
    procedure SetStepSeqClock(const Value: TSequencerClock);
    procedure SetBpm(const Value: single);
    procedure SetSampleRate(const Value: single);
    procedure SetSequenceData(const Value: IStepSequenceDataObject);
    function GetCurrentStepValue(Index: integer): single;
  protected
    VoiceClockManager : TLucidityVoiceClockManager;
    UseInternalCounter : boolean;
    Counter            : integer;
    CounterTarget      : single;

    IsSteppingForwards : boolean;

    // Mod Data...
    ModClockInput : single;
    ModSeqOutput_Unipolar : single;

    TargetOutput : single;

    SmoothingFilter : TCriticallyDampedLowpass;

    procedure ClockEvent(Sender : TObject; ClockID : cardinal);
    property CurrentStepValues [Index : integer] : single read GetCurrentStepValue;
  public
    constructor Create(const aVoiceClockManager : TLucidityVoiceClockManager);
    destructor Destroy; override;

    property SequenceData : IStepSequenceDataObject read FSequenceData write SetSequenceData;

    property SampleRate : single read fSampleRate write SetSampleRate;
    property Bpm        : single read fBpm        write SetBpm;

    function GetModPointer(const Name:string):PSingle;

    procedure ZeroOutput;
    procedure StepResetA(TriggeredNote : cardinal);
    procedure StepResetB;
    procedure Step;

    property Clock : TSequencerClock read fStepSeqClock write SetStepSeqClock;

    property StepCountAsInt : integer read fStepCountAsInt;
    property StepCount : TStepSequencerLength read fStepCount write SetStepCount;

    property CurrentStep : integer read fCurrentStep;

    property Direction : TStepSequencerDirection read fDirection write fDirection;
  end;

implementation

uses
  SysUtils,
  eeDsp;

{ TLucidyStepSequencer }

constructor TLucidyStepSequencer.Create(const aVoiceClockManager : TLucidityVoiceClockManager);
begin
  VoiceClockManager := aVoiceClockManager;

  fCurrentStep := 0;

  fStepSeqClock := TSequencerClock.Div_1;

  ModSeqOutput_Unipolar := 0;
  TargetOutput := 0;

  SmoothingFilter := TCriticallyDampedLowpass.Create;
end;

destructor TLucidyStepSequencer.Destroy;
begin
  SmoothingFilter.Free;
  inherited;
end;

function TLucidyStepSequencer.GetCurrentStepValue(Index: integer): single;
begin
  if assigned(SequenceData) then
  begin
    result := SequenceData.GetStepValue(Index);
  end else
  begin
    result := 0;
  end;
end;

function TLucidyStepSequencer.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'StepSeqOutput_Uni' then Exit(@ModSeqOutput_Unipolar);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

procedure TLucidyStepSequencer.SetBpm(const Value: single);
begin
  if fBpm <> Value then
  begin
    fBpm := Value;

    // reset the clock value to force the counter target to be
    // updated.
    SetStepSeqClock(Clock);
  end;
end;

procedure TLucidyStepSequencer.SetSampleRate(const Value: single);
begin
  if (Value <> fSampleRate) and (Value > 0) then
  begin
    fSampleRate := Value;

    //SmoothingFilter.SetTransitionTime(100,fSampleRate);
    SmoothingFilter.SetTransitionTime(25,fSampleRate);

    // reset the clock value to force the counter target to be
    // updated.
    SetStepSeqClock(Clock);
  end;
end;

procedure TLucidyStepSequencer.SetSequenceData(const Value: IStepSequenceDataObject);
begin
  FSequenceData := Value;
end;

procedure TLucidyStepSequencer.SetStepCount(const Value: TStepSequencerLength);
begin
  fStepCount := Value;

  case fStepCount of
    TStepSequencerLength.Two:      fStepCountAsInt := 2;
    TStepSequencerLength.Three:    fStepCountAsInt := 3;
    TStepSequencerLength.Four:     fStepCountAsInt := 4;
    TStepSequencerLength.Five:     fStepCountAsInt := 5;
    TStepSequencerLength.Six:      fStepCountAsInt := 6;
    TStepSequencerLength.Seven:    fStepCountAsInt := 7;
    TStepSequencerLength.Eight:    fStepCountAsInt := 8;
    TStepSequencerLength.Twelve:   fStepCountAsInt := 12;
    TStepSequencerLength.Sixteen:  fStepCountAsInt := 16;
  else
    raise Exception.Create('Unexpected StepCount value');
  end;
end;

procedure TLucidyStepSequencer.StepResetA(TriggeredNote: cardinal);
var
  x : integer;
begin
  if fStepSeqClock <> TSequencerClock.Note then
  begin
    fCurrentStep := 0;
    IsSteppingForwards := true;

    TargetOutput := CurrentStepValues[fCurrentStep];
    SmoothingFilter.Reset(TargetOutput);
    ModSeqOutput_Unipolar := TargetOutput;
  end else
  begin
    x := StepSequencerLengthToInteger(StepCount);
    fCurrentStep := Integer(TriggeredNote) mod x;
    IsSteppingForwards := true;

    TargetOutput  := CurrentStepValues[fCurrentStep];
    SmoothingFilter.Reset(TargetOutput);
    ModSeqOutput_Unipolar := TargetOutput;
  end;

  //TODO: add a filter for the step sequencer and reset the filter output here.
end;

procedure TLucidyStepSequencer.StepResetB;
begin

end;

procedure TLucidyStepSequencer.ZeroOutput;
begin
  ModSeqOutput_Unipolar := 0;
end;

procedure TLucidyStepSequencer.SetStepSeqClock(const Value: TSequencerClock);
var
  Beats : single;
begin
  fStepSeqClock := Value;

  case fStepSeqClock of
    TSequencerClock.Div_1:    Beats := 1;
    TSequencerClock.Div_2:    Beats := 1/2;
    TSequencerClock.Div_4:    Beats := 1/4;
    TSequencerClock.Div_8:    Beats := 1/8;
    TSequencerClock.Div_12:   Beats := 1/12;
    TSequencerClock.Div_16:   Beats := 1/16;
    TSequencerClock.Div_32:   Beats := 1/32;
    TSequencerClock.Div_64:   Beats := 1/64;
    TSequencerClock.Div_128:  Beats := 1/128;
    TSequencerClock.Div_1T:   Beats := 1*2/3;
    TSequencerClock.Div_2T:   Beats := 1/2*2/3;
    TSequencerClock.Div_4T:   Beats := 1/4*2/3;
    TSequencerClock.Div_8T:   Beats := 1/8*2/3;
    TSequencerClock.Div_12T:  Beats := 1/12*2/3;
    TSequencerClock.Div_16T:  Beats := 1/16*2/3;
    TSequencerClock.Div_32T:  Beats := 1/32*2/3;
    TSequencerClock.Div_64T:  Beats := 1/64*2/3;
    TSequencerClock.Div_128T: Beats := 1/128*2/3;
    TSequencerClock.Note: Beats := -1;
    TSequencerClock.Lfo1: Beats := -1;
    TSequencerClock.Lfo2: Beats := -1;
    TSequencerClock.SampleLoop: Beats := -1;
  else
    raise Exception.Create('Unexpected TSequencerClock Value.');
  end;

  if Beats > 0 then
  begin
    UseInternalCounter := true;
    CounterTarget := BeatsToSamples(Beats, BPM, SampleRate);
  end else
  begin
    UseInternalCounter := false;
  end;



  VoiceClockManager.RemoveListener('StepSeq', self);

  case fStepSeqClock of
    TSequencerClock.Lfo1: VoiceClockManager.AddListener(ClockID_Lfo1, self, ClockEvent, 'StepSeq');
    TSequencerClock.Lfo2: VoiceClockManager.AddListener(ClockID_Lfo2, self, ClockEvent, 'StepSeq');
    TSequencerClock.SampleLoop: VoiceClockManager.AddListener(ClockID_SampleLoop, self, ClockEvent, 'StepSeq');
  end;


end;

procedure TLucidyStepSequencer.Step;
var
  x : single;
begin
  if (UseInternalCounter) then
  begin
    Inc(Counter);
    if Counter > CounterTarget then
    begin
      Counter := 0;
      ClockEvent(self, 0);
    end;
  end;


  x := SmoothingFilter.Step(TargetOutput);

  ModSeqOutput_Unipolar := x;
end;

procedure TLucidyStepSequencer.ClockEvent(Sender: TObject; ClockID: cardinal);
begin
  case Direction of
    TStepSequencerDirection.Forwards:
    begin
      inc(fCurrentStep);
      if fCurrentStep >= StepCountAsInt then fCurrentStep := 0;
    end;

    TStepSequencerDirection.Backwards:
    begin
      dec(fCurrentStep);
      if fCurrentStep < 0 then fCurrentStep := fStepCountAsInt-1;
    end;

    TStepSequencerDirection.PingPong:
    begin
      if IsSteppingForwards
        then inc(fCurrentStep)
        else dec(fCurrentStep);

      if fCurrentStep >= StepCountAsInt then
      begin
        fCurrentStep := fStepCountAsInt-2;
        IsSteppingForwards := false;
      end else
      if fCurrentStep < 0 then
      begin
        fCurrentStep := 1;
        IsSteppingForwards := true;
      end;
    end;

    TStepSequencerDirection.Random:
    begin
      fCurrentStep := Random(fStepCountAsInt);
    end;
  end;

  //if Mutate <> TStepSequencerMutate.Off then MutateStep(fCurrentStep);

  TargetOutput := CurrentStepValues[fCurrentStep];
end;




end.


