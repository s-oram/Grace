{
  TADSR is a basic RC-filter type ADSR envelope.

  ==== History =====
  12 Feb 2013
  - renamed unit to uADSR. Copied from EasyEffect base classes.

}

unit soADSR;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.MoreTypes,
  eeTypes, eeDSP;

const
  //kThreshold65db  = 0.0005623413252;
  kAttackThreshold  = 0.005623413252;
  kThreshold90db    = 0.0000316227766;
  kThreshold120db   = 0.000001;

type
  TEnvelopeStage = (esOff, esAttack, esHold, esDecay, esSustain, esRelease, esQuickRelease);

  TADSR = class
  private
    fStage: TEnvelopeStage;
    fSampleRate: integer;
    fAttackTime: single;
    fDecayTime: single;
    fReleaseTime: single;
    fSustainLevel: single;
    fHoldTime: single;
    procedure SetAttackTime(const Value: single);
    procedure SetDecayTime(const Value: single);
    procedure SetReleaseTime(const Value: single);
    procedure SetSustainLevel(const Value: single);
    procedure SetSampleRate(const Value: integer);
    procedure SetHoldTime(const Value: single);
  protected
    InternalEnvValue  : double;
    ScaledEnvValue    : single;
    VelocityScaler     : single;
    CurrentHoldCount   : integer;

    AttackCoefficient  :double;
    DecayCoefficient   :double;
    ReleaseCoefficient :double;
    QuickReleaseCoefficient : double;

    HoldTimeInSamples    : single;
    DecayTimeInSamples   : single;
    CurrentStageTime     : integer;
  public
    constructor Create;
	  destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure StepReset; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure Step; {$IFDEF AudioInline}inline;{$ENDIF} //Process one sample frame.

    procedure Trigger(aVelocity:single);
    procedure Release;
    procedure QuickRelease(Time_ms:single);
    procedure Kill;

    property Value           :single         read ScaledEnvValue; //range 0..trigger-velocity.
    property Stage           :TEnvelopeStage read fStage;

    property SampleRate      :integer        read fSampleRate write SetSampleRate;

    property AttackTime   :single read fAttackTime   write SetAttackTime;    //Time is in milliseconds.
    property HoldTime     :single read fHoldTime     write SetHoldTime;      //Time is in milliseconds.
    property DecayTime    :single read fDecayTime    write SetDecayTime;     //Time is in milliseconds.
    property SustainLevel :single read fSustainLevel write SetSustainLevel;  //Time is in milliseconds.
    property ReleaseTime  :single read fReleaseTime  write SetReleaseTime;   //Time is in milliseconds.
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils;

{ TADSR }

constructor TADSR.Create;
begin
  fSampleRate      := 44100;
  InternalEnvValue := 0;
  fStage           := esOff;

  AttackTime   := 500;
  DecayTime    := 500;
  SustainLevel := 0.5;
  ReleaseTime  := 1000;
end;

destructor TADSR.Destroy;
begin

  inherited;
end;

function TADSR.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'EnvOut' then Exit(@ScaledEnvValue);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

procedure TADSR.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;

  AttackCoefficient  := CalcRcEnvelopeCoefficient(fAttackTime, SampleRate);
  DecayCoefficient   := CalcRcEnvelopeCoefficient(fDecayTime, SampleRate);
  ReleaseCoefficient := CalcRcEnvelopeCoefficient(fReleaseTime, SampleRate);

  HoldTimeInSamples    := MillisecondsToSamples(fHoldTime, SampleRate);
  DecayTimeInSamples := MillisecondsToSamples(Value, SampleRate);
end;

procedure TADSR.SetAttackTime(const Value: single);
begin
  fAttackTime := Value;
  AttackCoefficient := CalcRcEnvelopeCoefficient(Value, fSampleRate);
end;

procedure TADSR.SetHoldTime(const Value: single);
begin
  fHoldTime := Value;
  HoldTimeInSamples := MillisecondsToSamples(Value, SampleRate);
end;

procedure TADSR.SetDecayTime(const Value: single);
begin
  fDecayTime := Value;
  DecayCoefficient := CalcRcEnvelopeCoefficient(Value, SampleRate);
  DecayTimeInSamples := MillisecondsToSamples(Value, SampleRate);
end;

procedure TADSR.SetSustainLevel(const Value: single);
begin
  fSustainLevel := Value;
end;

procedure TADSR.SetReleaseTime(const Value: single);
begin
  fReleaseTime := Value;
  ReleaseCoefficient := CalcRcEnvelopeCoefficient(Value, SampleRate);
end;

procedure TADSR.Trigger(aVelocity: single);
begin
  //normal range for velocity is 0..1.
  fStage             := esAttack;
  VelocityScaler     := aVelocity;
  CurrentHoldCount   := 0;
  CurrentStageTime   := 0;
end;

procedure TADSR.Release;
begin
  fStage := esRelease;
end;

procedure TADSR.QuickRelease(Time_ms: single);
begin
  assert(fStage <> esOff);

  QuickReleaseCoefficient := CalcRcEnvelopeCoefficient(Time_ms, SampleRate);
  fStage := esQuickRelease;
end;

procedure TADSR.Kill;
begin
  InternalEnvValue   := 0;
  fStage             := esOff;
end;

procedure TADSR.Step;
var
  TargetValue     : single;
begin

  case Stage of
    esOff:
    begin
      InternalEnvValue := 0;
    end;

    esAttack:
    begin
      TargetValue := 1  * VelocityScaler;
      InternalEnvValue := RcEnvFilter(InternalEnvValue, TargetValue, AttackCoefficient);
      if IsNear(InternalEnvValue, TargetValue, kAttackThreshold) then
      begin
        fStage := esHold;
      end;
    end;

    esHold:
    begin
      inc(CurrentStageTime);
      if CurrentStageTime >= HoldTimeInSamples then
      begin
        fStage := esDecay;
        CurrentStageTime := 0;
      end;
    end;

    esDecay:
    begin
      TargetValue := fSustainLevel * VelocityScaler;
      InternalEnvValue := RcEnvFilter(InternalEnvValue, TargetValue, DecayCoefficient);

      inc(CurrentStageTime);
      if (CurrentStageTime >= DecayTimeInSamples) and (IsNear(InternalEnvValue, TargetValue, kAttackThreshold)) then
      begin
        fStage := esSustain;
        CurrentStageTime := 0;
      end;
    end;

    esSustain:
    begin
      TargetValue := fSustainLevel  * VelocityScaler;
      InternalEnvValue := RcEnvFilter(InternalEnvValue, TargetValue, DecayCoefficient);
    end;

    esRelease:
    begin
      TargetValue := 0;
      InternalEnvValue := RcEnvFilter(InternalEnvValue, TargetValue, ReleaseCoefficient);
      if IsNear(InternalEnvValue, TargetValue, kAttackThreshold) then
      begin
        fStage := esOff;
      end;
    end;

    esQuickRelease:
    begin
      //TODO: need quick release coefficient
      TargetValue := 0;
      InternalEnvValue := RcEnvFilter(InternalEnvValue, TargetValue, QuickReleaseCoefficient);
      if IsNear(InternalEnvValue, TargetValue, kAttackThreshold) then
      begin
        fStage := esOff;
      end;
    end;
  end;


  ScaledEnvValue := InternalEnvValue;
end;


procedure TADSR.StepReset;
begin

end;

end.
