unit Lucidity.Env.ASR;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.MoreTypes, uLucidityEnums, eeFunctions;

{$SCOPEDENUMS ON}

// TODO:HIGH I don't believe this asr class is being used any more. It probably can
// be deleted.

type
  TEnvStage = (Off, Attack, Hold, Sustain, Release);

  TLucidityASR = class
  private
    fSampleRate: integer;
    fAttackTime: single;
    fDecayTime: single;
    fMode: TModEnvMode;
    function GetEnvValue: double;
    procedure SetAttackTime(const Value: single);
    procedure SetDecayTime(const Value: single);
    procedure SetSampleRate(const Value: integer);
    procedure SetMode(const Value: TModEnvMode);
  protected
    fEnvStage         : TEnvStage;
    HasBeenReleased  : boolean;
    AttackRate : single;
    DecayRate  : single;
    EnvLevel   : single;
    HoldCount    : integer;
    HoldMaxCount : integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure StepResetA; inline;
    procedure Step; inline; //Process one sample frame.

    procedure Trigger(aVelocity:single);
    procedure Release;
    procedure Kill;

    property Value           : double         read GetEnvValue; //range 0..1?
    property SampleRate      : integer        read fSampleRate write SetSampleRate;

    property AttackTime   : single read fAttackTime   write SetAttackTime;    // range is 0..1
    property DecayTime    : single read fDecayTime    write SetDecayTime;     // range is 0..1

    property Mode : TModEnvMode read fMode write SetMode;

    property EnvelopeStage : TEnvStage read fEnvStage;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  LucidityParameterScaling,
  SysUtils;

{ TLucidityASR }

constructor TLucidityASR.Create;
begin
  EnvLevel := 0;
end;

destructor TLucidityASR.Destroy;
begin

  inherited;
end;

function TLucidityASR.GetEnvValue: double;
begin
  result := EnvLevel;
end;

function TLucidityASR.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'EnvOut' then Exit(@EnvLevel);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

procedure TLucidityASR.SetAttackTime(const Value: single);
const
  kStageTime = 8000; //milliseconds.
var
  TimeMS : single;
  x : single;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fAttackTime := Value;


  if fAttackTime <= 0 then
  begin
    AttackRate := 1;
  end else
  begin
    assert(SampleRate > 0);

    TimeMS := TParScaler.ModEnv_StageTimeToMS(Value);
    x := 1 / (TimeMS * 0.001 * SampleRate);

    if x >= 1
      then AttackRate := 1
      else AttackRate := x;
  end;

end;

procedure TLucidityASR.SetDecayTime(const Value: single);
const
  kStageTime = 8000; //milliseconds.
var
  TimeMS : single;
  x : single;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fDecayTime := Value;

  if fDecayTime <= 0 then
  begin
    DecayRate := 1;
  end else
  begin
    assert(SampleRate > 0);

    TimeMS := TParScaler.ModEnv_StageTimeToMS(Value);
    x := 1 / (TimeMS * 0.001 * SampleRate);

    if x >= 1
      then DecayRate := 1
      else DecayRate := x;
  end;

end;

procedure TLucidityASR.SetMode(const Value: TModEnvMode);
begin
  fMode := Value;
end;

procedure TLucidityASR.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;

  HoldMaxCount := round(0.01 * fSampleRate); //Hold for 10 milliseconds
end;

procedure TLucidityASR.Trigger(aVelocity: single);
begin
  HoldCount := 0;
  HasBeenReleased := false;

  if ((EnvLevel + AttackRate) < 1) then
  begin
    EnvLevel := 0;
    fEnvStage := TEnvStage.Attack;
  end else
  begin
    EnvLevel := 1;
    fEnvStage := TEnvStage.Hold;
  end;

end;

procedure TLucidityASR.Kill;
begin
  EnvLevel := 0;
end;

procedure TLucidityASR.Release;
begin
  HasBeenReleased := true;
end;

procedure TLucidityASR.Step;
begin
  case fEnvStage of
    TEnvStage.Off:
    begin
      //Do nothing.
    end;

    TEnvStage.Attack:
    begin
      EnvLevel := EnvLevel + AttackRate;
      if EnvLevel >= 1 then
      begin
        EnvLevel := 1;
        fEnvStage := TEnvStage.Hold;
        HoldCount := 0;
      end;
    end;

    TEnvStage.Hold:
    begin
      inc(HoldCount);
      if (HoldCount >= HoldMaxCount) then
      begin
        if (HasBeenReleased = false) and (Mode = TModEnvMode.TriggerASR)
          then fEnvStage := TEnvStage.Sustain
          else fEnvStage := TEnvStage.Release;
      end;
    end;

    TEnvStage.Sustain:
    begin
      if HasBeenReleased then fEnvStage := TEnvStage.Release;
    end;

    TEnvStage.Release:
    begin
      EnvLevel := EnvLevel - DecayRate;
      if (EnvLevel <= 0) then
      begin
        EnvLevel := 0;
        if (Mode = TModEnvMode.TriggerCycle)
          then fEnvStage := TEnvStage.Attack
          else fEnvStage := TEnvStage.Off;
      end;
    end;
  end;

end;

procedure TLucidityASR.StepResetA;
begin
  EnvLevel := 0;
end;



end.
