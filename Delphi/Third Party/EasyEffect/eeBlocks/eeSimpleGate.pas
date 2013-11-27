{
  A very simple gate class with attack & release times for smoothing the gate on/off signal. 

}

unit eeSimpleGate;

interface

type
  TGateStage = (gsOff, gsAttack, gsSustain, gsRelease);

  TSimpleGate = class
  private
    fAttackSamples: integer;
    fReleaseSamples: integer;
    fValue: single;
    fStage: TGateStage;
    procedure SetAttackSamples(const Value: integer);
    procedure SetReleaseSamples(const Value: integer);
  protected
    AttackStepSize  :single;
    ReleaseStepSize :single;
    TargetValue     :single;
  public
    constructor Create;

    function Step:single; inline; //Process one sample frame.

    procedure Trigger(aVelocity:single);
    procedure Release;
    procedure Kill;

    property AttackSamples   :integer read fAttackSamples  write SetAttackSamples;   //In samples.
    property ReleaseSamples  :integer read fReleaseSamples write SetReleaseSamples;  //In samples.

    property Value           :single     read fValue; //range 0..trigger-velocity.
    property Stage           :TGateStage read fStage;
  end;

implementation

{ TSimpleGate }

constructor TSimpleGate.Create;
begin
  fValue := 0;

  AttackSamples  := 5;
  ReleaseSamples := 5;

  fStage := gsOff;
end;

procedure TSimpleGate.SetAttackSamples(const Value: integer);
begin
  fAttackSamples := Value;

  if Value > 0
    then AttackStepSize := 1 / Value
    else AttackStepSize := 1;

  if AttackStepSize > 1 then AttackStepSize := 1;
end;

procedure TSimpleGate.SetReleaseSamples(const Value: integer);
begin
  fReleaseSamples := Value;

  if Value > 0
    then ReleaseStepSize := 1 / Value
    else ReleaseStepSize := 1;

  if ReleaseStepSize > 1 then ReleaseStepSize := 1;
end;

procedure TSimpleGate.Trigger(aVelocity: single);
begin
  assert(aVelocity > 0);
  assert(aVelocity <= 1);

  fStage      := gsAttack;
  TargetValue := aVelocity;
end;

procedure TSimpleGate.Release;
begin
  fStage := gsRelease;
end;

procedure TSimpleGate.Kill;
begin
  fStage := gsOff;
  fValue := 0;
end;



function TSimpleGate.Step: single;
begin
  case Stage of
    gsAttack:
    begin
      fValue := fValue + AttackStepSize * TargetValue;
      if fValue >= TargetValue then
      begin
        fValue := TargetValue;
        fStage  := gsSustain;
      end;
    end;

    gsRelease:
    begin
      fValue := fValue - ReleaseStepSize * TargetValue;
      if fValue <= 0 then
      begin
        fValue := 0;
        fStage  := gsOff;
      end;
    end;

  end;

  result := fValue;
end;





end.
