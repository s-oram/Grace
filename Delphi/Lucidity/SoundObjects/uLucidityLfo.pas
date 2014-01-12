unit uLucidityLfo;

interface

{$INCLUDE Defines.inc}

uses
  B2.Filter.CriticallyDampedLowpass,
  VamLib.MoreTypes, eeBiquadFilterCore, eeBiquadFilters,
  uLucidityEnums, B2.MovingAverageFilter,
  eeVirtualCV, Math, eeFunctions,  uLucidityClock, eeDsp,
  uConstants;

const
  MaxLFO = High(Cardinal);
  OneOverMaxLfo = 1 / MaxLFO;
  LfoScaler = 1 / MaxLFO;
  InvertedLfoScaler = (1 / MaxLFO) * -1;
  HalfLfo : cardinal = MaxLfo div 2;
  ThreeQuarterLFO : cardinal = 3221225471;


type
  //LucidityLFO is a quad LFO based on the Vermona Fourmulator eurorack module.

  TLfoModulationPoints = record
    // Modulation output values...
    LfoOut1Raw : single;
    LfoOut2Raw : single;

    LfoOut1 : single;
    LfoOut2 : single;

    LfoClockOut1 : single;
    LfoClockOut2 : single;
  end;

  TLfo = class
  strict private
    fParB: single;
    fShape: TLfoShape;
    fSpeed: single;
    fSampleRate: single;
    fBpm: single;
  strict
  private
    function GetLfoOutputPointer: PSingle; protected
    SmoothingFilter : TCriticallyDampedLowpass;

    ModPoint_LfoOutput : single; //range 0..1. LFO output is uni-polar.
    ModPoint_ParAInput : single;
    ModPoint_ParBInput : single;

    LfoOutputRaw    : single; //range 0..1. LFO output is uni-polar.
    RandomLevelA    : single;
    RandomLevelB    : single;

    Phase       : cardinal;
    StepSize    : cardinal;
    PhaseOffset : cardinal;

    procedure SetBpm(const Value: single);
    procedure SetSampleRate(const Value: single); protected
    procedure _Step; inline;

    procedure UpdateLfoStepSizes; inline;
    procedure UpdatePhaseOffset; inline;

    function CalcLfoOut_UniPolar(const aPhase, aPhaseOffset : cardinal; const StepSize:cardinal; const aShape : TLfoShape; const RandomA, RandomB : single):single; inline;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure StepResetA;
    procedure StepResetB;
    function FastControlProcess:boolean;
    procedure SlowControlProcess;

    property Shape : TLfoShape read fShape write fShape;
    property Speed : single    read fSpeed write fSpeed; //range 0..1
    property ParB  : single    read fParB  write fParB;  //range 0..1

    property Bpm        : single read fBpm        write SetBpm;
    property SampleRate : single read fSampleRate write SetSampleRate;
  end;


  TLucidityLfo = class
  private
    fSampleRate: single;
    fBpm: single;
    procedure SetSampleRate(const Value: single);
    procedure SetBpm(const Value: single);
  protected
    VoiceClockManager : TLucidityVoiceClockManager;
    fLfoA, fLfoB : TLfo;
  public
    constructor Create(const aVoiceClockManager : TLucidityVoiceClockManager);
    destructor Destroy; override;

    procedure ResetLfoPhase;

    function GetModPointer(const Name:string):PSingle;

    property LfoA : TLfo read fLfoA;
    property LfoB : TLfo read fLfoB;

    property Bpm        : single read fBpm        write SetBpm;
    property SampleRate : single read fSampleRate write SetSampleRate;

    procedure StepResetA;
    procedure StepResetB;

    procedure FastControlProcess;
    procedure SlowControlProcess;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  LucidityParameterScaling,
  SysUtils;

{ TLucidityLfo }

constructor TLucidityLfo.Create(const aVoiceClockManager : TLucidityVoiceClockManager);
begin
  VoiceClockManager := aVoiceClockManager;

  fLfoA := TLfo.Create;
  fLfoB := TLfo.Create;
end;

destructor TLucidityLfo.Destroy;
begin
  fLfoA.Free;
  fLfoB.Free;
  inherited;
end;

function TLucidityLfo.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'LfoOut1' then Exit(LfoA.GetModPointer('LfoOutput'));
  if Name = 'LfoOut2' then Exit(LfoB.GetModPointer('LfoOutput'));

  if Name = 'LfoRateMod1' then Exit(LfoA.GetModPointer('ParAInput'));
  if Name = 'LfoRateMod2' then Exit(LfoB.GetModPointer('ParAInput'));

  if Name = 'LfoParBMod1' then Exit(LfoA.GetModPointer('ParBInput'));
  if Name = 'LfoParBMod2' then Exit(LfoB.GetModPointer('ParBInput'));

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

procedure TLucidityLfo.ResetLfoPhase;
begin
  assert(false, 'todo');
end;

procedure TLucidityLfo.SetBpm(const Value: single);
begin
  fBpm := Value;
  LfoA.Bpm := Value;
  LfoB.Bpm := Value;
end;

procedure TLucidityLfo.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  LfoA.SampleRate := Value;
  LfoB.SampleRate := Value;
end;

procedure TLucidityLfo.FastControlProcess;
begin
  if LfoA.FastControlProcess then
  begin
    VoiceClockManager.SendClockEvent(ClockID_Lfo1);
  end;

  if LfoB.FastControlProcess then
  begin
    VoiceClockManager.SendClockEvent(ClockID_Lfo2);
  end;
end;

procedure TLucidityLfo.SlowControlProcess;
begin
  LfoA.SlowControlProcess;
  LfoB.SlowControlProcess;
end;

procedure TLucidityLfo.StepResetA;
begin
  LfoA.StepResetA;
  LfoB.StepResetA;
end;

procedure TLucidityLfo.StepResetB;
begin
  LfoA.StepResetB;
  LfoB.StepResetB;
end;

{ TLfo }



constructor TLfo.Create;
begin
  SmoothingFilter := TCriticallyDampedLowpass.Create;
end;

destructor TLfo.Destroy;
begin
  SmoothingFilter.Free;
  inherited;
end;

function TLfo.GetLfoOutputPointer: PSingle;
begin
  result := @self.ModPoint_LfoOutput;
end;

function TLfo.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'LfoOutput' then exit(@ModPoint_LfoOutput);
  if Name = 'ParAInput' then exit(@ModPoint_ParAInput);
  if Name = 'ParBInput' then exit(@ModPoint_ParBInput);

  //if we've made it this far, nothing has been found.
  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

procedure TLfo.SetBpm(const Value: single);
begin
  fBpm := Value;
end;

procedure TLfo.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  SmoothingFilter.SetTransitionTime(25, fSampleRate);
end;

function TLfo.FastControlProcess:boolean;
var
  OldPhase : cardinal;
  LfoHasReset : boolean;
begin
  _Step;

  OldPhase := Phase;
  Phase := Phase + StepSize;
  if Phase < OldPhase
    then LfoHasReset := true
    else LfoHasReset := false;

  result := LfoHasReset;
end;

procedure TLfo.SlowControlProcess;
begin
  UpdatePhaseOffset;
  UpdateLfoStepSizes;
end;



procedure TLfo.StepResetA;
begin
  // TODO: currnetly reseting phase to 0. It might be
  // nicer to have some sort of global LFO free-running value that the LFO
  // can be reset to.
  Phase := 0;

  RandomLevelA := random;
  RandomLevelB := random;

  UpdatePhaseOffset;
  UpdateLfoStepSizes;
  _Step;
  SmoothingFilter.Reset(LfoOutputRaw);
  ModPoint_LfoOutput := LfoOutputRaw;
end;

procedure TLfo.StepResetB;
begin
  UpdatePhaseOffset;
  UpdateLfoStepSizes;
  _Step;
  SmoothingFilter.Reset(LfoOutputRaw);
  ModPoint_LfoOutput := LfoOutputRaw;
  //ModPoint_LfoOutput := LfoOutputRaw;
end;

procedure TLfo.UpdateLfoStepSizes;
const
  kMinFreq = 0.001;
  kMaxFreq = 5000;
var
  Freq : single;
  CV : TModularVoltage;
begin
  CV := (Speed * 12) + AudioRangeToModularVoltage(ModPoint_ParAInput);
  Freq := VoltsToFreq(0.05, CV);
  Clamp(Freq, kMinFreq, kMaxFreq);
  StepSize := round(High(cardinal) / SampleRate * Freq);

end;

procedure TLfo.UpdatePhaseOffset;
var
  x : single;
begin
  assert(InRange(ParB, 0,1));

  x := ParB + ModPoint_ParBInput;

  // NOTE: Because phase is a continous 360 degree type parameter, we
  // 'wrap' the out of range values back into the allowable range.
  //Wrap(x, 0, 1);
  // The alternative is to Clamp() the value. That will however
  // cause the phase to not fold around and will not sound natural.
  Clamp(x, 0, 1);

  case self.Shape of
    TLfoShape.SawUp,
    TLfoShape.SawDown,
    TLfoShape.Square,
    TLfoShape.Triangle,
    TLfoShape.Sine:
    begin
      PhaseOffset := round(high(Cardinal) * x);
    end;

    TLfoShape.Random:
    begin
      PhaseOffset := 0;
    end;
  end;
end;

procedure TLfo._Step;
const
  kMinFreq = 0.001;
  kMaxFreq = 5000;
var
  Freq : single;
  CV : TModularVoltage;
  x : cardinal;
  RFactor1, RFactor2 : single;
  ParBMod : single;
begin
  //== Lfo1 reset pos check ==
  if (Phase + PhaseOffset + StepSize) < (Phase + PhaseOffset) then
  begin
    ParBMod := ParB + ModPoint_ParBInput;
    Clamp(ParBMod, 0, 1);

    RFactor1 := (Sqr(ParBMod) - Random);
    RFactor2 := (Sqr(ParBMod) - Random);

    if RFactor1 >= 0
      then RandomLevelA := random
      else RandomLevelA := RandomLevelB;

    if RFactor2 >= 0
      then RandomLevelB := random
      else RandomLevelB := RandomLevelA;
  end;

  LfoOutputRaw := CalcLfoOut_UniPolar(Phase, PhaseOffset, StepSize, Shape, RandomLevelA, RandomLevelB);
  ModPoint_LfoOutput := SmoothingFilter.Step(LfoOutputRaw);
end;

function TLfo.CalcLfoOut_UniPolar(const aPhase, aPhaseOffset, StepSize: cardinal; const aShape: TLfoShape; const RandomA, RandomB: single): single;
var
  ModPhase : cardinal;
  TriShape : single;
begin
  ModPhase := aPhase + aPhaseOffset;

  case aShape of
    TLfoShape.SawUp:    result := ModPhase * LfoScaler;
    TLfoShape.SawDown:  result := ModPhase * InvertedLfoScaler + 1;
    TLfoShape.Square:   result := Integer(ModPhase > HalfLFO);
    TLfoShape.Triangle: result := abs((ModPhase + ThreeQuarterLFO) * LfoScaler - 0.5) * 2;
    TLfoShape.Sine:
    begin
      // Calc Triangle shape.
      TriShape := abs((ModPhase + ThreeQuarterLFO) * LfoScaler - 0.5) * 4 - 1;
      // Shape to a sine'ish wave shape.
      result := TriShape * (2 - abs(TriShape)) * 0.5 + 0.5;

      // NOTE: The sine shaping code is based on a function someone
      // posted on KVR Audio. From memory I think it was "Aciddose".

      //TODO: The above code is calling abs() twice.
      // It might be possible to refactor the functions so it is only called one.
    end;

    TLfoShape.Random:
    begin
      if ModPhase < HalfLFO
        then result := RandomA
        else result := RandomB;
    end;
  else
    raise Exception.Create('Lfo shape not handled.');
  end;

  assert(result >= 0, 'LFO is smaller than 0');
  assert(result <= 1, 'LFO is bigger than 0');
end;


end.
