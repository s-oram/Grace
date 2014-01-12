unit soGateEnvelope;

interface

uses
  VamLib.MoreTypes;

type
  TGateEnvelope = class
  private
    fSampleRate:single;

    AttackStepSize:single;
    ReleaseStepSize:single;
    CurrentEnvLevel:single;
    TargetValue:single;

    fEnvStage:integer;
    procedure SetAttackTime_MS(const Value: single);
    procedure SetReleaseTime_MS(const Value: single);
    procedure SetSampleRate(const Value: single);
    procedure SetAttackTime_Samples(const Value: single);
    procedure SetReleaseTime_Samples(const Value: single);

    property EnvStage:integer read fEnvStage;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Trigger;
    procedure Release;
    procedure Kill;

    function Step:single; inline;

    property AttackTime_MS  : single write SetAttackTime_MS;  //set in milliseconds.
    property ReleaseTime_MS : single write SetReleaseTime_MS; //set in milliseconds.

    property AttackTime_Samples  : single write SetAttackTime_Samples;  //set in samples.
    property ReleaseTime_Samples : single write SetReleaseTime_Samples; //set in samples.

    // WARNING: The AttackTime and ReleaseTime properties will need to be
    // set again after setting SampleRate. The AttackTime and ReleaseTime
    // properties set values relative to the SampleRate value.
    property SampleRate : single read fSampleRate write SetSampleRate;
  end;

implementation

uses
  SysUtils,
  eeDsp;

const
  //es = Envelope Stage.
  esOff     = 0;
  esAttack  = 1;
  esSustain = 2;
  esRelease = 3;


constructor TGateEnvelope.Create;
begin
  fSampleRate := 44100;

  AttackTime_MS := 0;
  ReleaseTime_MS := 0;

  CurrentEnvLevel := 0;
  fEnvStage := esOff;
end;

destructor TGateEnvelope.Destroy;
begin

  inherited;
end;

procedure TGateEnvelope.SetAttackTime_Samples(const Value: single);
begin
  if Value > 1
    then AttackStepSize := 1 / Value
    else AttackStepSize := 1;
end;

procedure TGateEnvelope.SetReleaseTime_Samples(const Value: single);
begin
  if Value > 1
    then ReleaseStepSize := 1 / Value
    else ReleaseStepSize := 1;
end;

procedure TGateEnvelope.SetAttackTime_MS(const Value: single);
var
  smps:single;
begin
  smps := MillisecondsToSamples(Value, fSampleRate);
  if smps <= 0 then smps := 1;
  AttackStepSize := 1 / smps;
end;

procedure TGateEnvelope.SetReleaseTime_MS(const Value: single);
var
  smps:single;
begin
  smps := MillisecondsToSamples(Value, fSampleRate);
  if smps <= 0 then smps := 1;
  ReleaseStepSize := 1 / smps;
end;

procedure TGateEnvelope.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
end;

procedure TGateEnvelope.Trigger;
begin
  TargetValue := 1;
  fEnvStage := esAttack;
end;

procedure TGateEnvelope.Release;
begin
  fEnvStage := esRelease;
  TargetValue := 0;
end;

procedure TGateEnvelope.Kill;
begin
  fEnvStage := esOff;
  TargetValue := 0;
  CurrentEnvLevel := 0;
end;

function TGateEnvelope.Step: single;
begin
  case EnvStage of
    esSustain:
    begin
      result := 1;
    end;

    esAttack:
    begin
      CurrentEnvLevel := CurrentEnvLevel + AttackStepSize;
      if CurrentEnvLevel >= TargetValue then
      begin
        fEnvStage := esSustain;
        CurrentEnvLevel := TargetValue;
      end;
      result := sqrt(CurrentEnvLevel);
    end;

    esRelease:
    begin
      CurrentEnvLevel := CurrentEnvLevel - ReleaseStepSize;
      if CurrentEnvLevel <= TargetValue then
      begin
        fEnvStage := esOff;
        CurrentEnvLevel := TargetValue;
      end;
      result := sqrt(CurrentEnvLevel);
    end;

    esOff:
    begin
      result := 0;
    end;
  else
    raise Exception.Create('Unexpected EnvStage value.');
  end;
end;


end.
