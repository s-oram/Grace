{
  TADSR is a basic RC-filter type ADSR envelope.

  ==== History =====
  12 Feb 2013
  - renamed unit to uADSR. Copied from EasyEffect base classes.

}

unit uADSR;

interface

uses
  eeTypes, eeDSP;

type
  TEnvelopeStage = (esOff, esAttack, esDecay, esSustain, esRelease);

  TADSR = class
  private
    fStage: TEnvelopeStage;
    fSampleRate: integer;
    fAttackTime: single;
    fDecayTime: single;
    fReleaseTime: single;
    fSustainLevel: single;
    procedure SetAttackTime(const Value: single);
    procedure SetDecayTime(const Value: single);
    procedure SetReleaseTime(const Value: single);
    procedure SetSustainLevel(const Value: single);
    procedure SetSampleRate(const Value: integer);
  protected
    EnvValue           :double;
    TargetValue        :double;
    VelocityScaler    :single;

    CurrentCoefficient :double;

    AttackCoefficient  :double;
    DecayCoefficient   :double;
    ReleaseCoefficient :double;

    function CalcCoefficient(const Time_ms, aSampleRate:single):double; inline;

  public
    constructor Create;
	  destructor Destroy; override;

    function Step:single; inline; //Process one sample frame.

    procedure Trigger(aVelocity:single);
    procedure Release;
    procedure QuickRelease(Time_ms:single);
    procedure Kill;

    property Value           :double         read EnvValue; //range 0..trigger-velocity.
    property Stage           :TEnvelopeStage read fStage;

    property SampleRate      :integer        read fSampleRate write SetSampleRate;

    property AttackTime   :single read fAttackTime   write SetAttackTime;    //Time is in milliseconds.
    property DecayTime    :single read fDecayTime    write SetDecayTime;     //Time is in milliseconds.
    property SustainLevel :single read fSustainLevel write SetSustainLevel;  //Time is in milliseconds.
    property ReleaseTime  :single read fReleaseTime  write SetReleaseTime;   //Time is in milliseconds.
  end;

implementation




{ TADSR }

constructor TADSR.Create;
begin

  fSampleRate := 44100;

  EnvValue           := 0;
  TargetValue        := 0;
  CurrentCoefficient := 0;
  fStage             := esOff;

  AttackTime   := 500;
  DecayTime    := 500;
  SustainLevel := 0.5;
  ReleaseTime  := 1000;
end;

destructor TADSR.Destroy;
begin

  inherited;
end;

function TADSR.CalcCoefficient(const Time_ms, aSampleRate: single): double;
var
  fc:double;
  cx:double;
begin
  // NOTE: The coefficient calculation function was source from this post at KVR.
  // http://www.kvraudio.com/forum/viewtopic.php?t=300689
  // TODO: It may be possible to optimise the equation used here...
  fc := 1 / (Time_ms / 1000);
  cx := 1-exp(-2 * pi * fc/aSampleRate);
  if cx > 1
    then result := 1
    else result := cx;
end;



procedure TADSR.SetAttackTime(const Value: single);
begin
  assert(Value >= 1);
  fAttackTime := Value;
  AttackCoefficient := CalcCoefficient(Value, SampleRate);
end;

procedure TADSR.SetDecayTime(const Value: single);
begin
  assert(Value >= 1);
  fDecayTime := Value;
  DecayCoefficient := CalcCoefficient(Value, SampleRate);
end;

procedure TADSR.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;

  AttackCoefficient  := CalcCoefficient(fAttackTime, SampleRate);
  DecayCoefficient   := CalcCoefficient(fDecayTime, SampleRate);
  ReleaseCoefficient := CalcCoefficient(fReleaseTime, SampleRate);
end;

procedure TADSR.SetSustainLevel(const Value: single);
begin
  fSustainLevel := Value;
end;

procedure TADSR.SetReleaseTime(const Value: single);
begin
  assert(Value >= 1);
  fReleaseTime := Value;
  ReleaseCoefficient := CalcCoefficient(Value, SampleRate);
end;

procedure TADSR.Trigger(aVelocity: single);
begin
  //normal range for velocity is 0..1.
  CurrentCoefficient := AttackCoefficient;
  fStage             := esAttack;
  VelocityScaler     := aVelocity;
  TargetValue        := 1 * VelocityScaler;
end;

procedure TADSR.Release;
begin
  TargetValue := 0;
  CurrentCoefficient := ReleaseCoefficient;
  fStage := esRelease;
end;

procedure TADSR.QuickRelease(Time_ms: single);
begin
  TargetValue := 0;
  CurrentCoefficient := CalcCoefficient(Time_ms, SampleRate);
  fStage := esRelease;
end;

procedure TADSR.Kill;
begin
  EnvValue           := 0;
  TargetValue        := 0;
  CurrentCoefficient := 0;
  fStage             := esOff;
end;

function TADSR.Step: single;
begin
  // TODO: the envelope processing code uses the SameValue_single() function. It calls Max()
  // This Max() call needs to be eliminated somehow. Much too expensive to be used in
  // an envelope stage.

  EnvValue := EnvValue + (TargetValue - EnvValue) * CurrentCoefficient;

  case Stage of
    esAttack:
    begin
      if SameValue_Single(EnvValue, TargetValue, 0.01) then
      begin
        fStage := esDecay;
        CurrentCoefficient := DecayCoefficient;
        TargetValue := SustainLevel  * VelocityScaler;
      end;
    end;

    esDecay:
    begin
      if SameValue_Single(EnvValue, TargetValue, 0.01) then
      begin
        fStage := esSustain;
        CurrentCoefficient := 0;
        TargetValue := SustainLevel * VelocityScaler;
      end;
    end;

    //esSustain: ;
    esRelease:
    begin
      if SameValue_Single(EnvValue, TargetValue, 0.01) then
      begin
        fStage := esOff;
        CurrentCoefficient := 0;
        TargetValue := 0;
        EnvValue := 0;
      end;
    end;
  end;

  result := EnvValue;
end;


end.
