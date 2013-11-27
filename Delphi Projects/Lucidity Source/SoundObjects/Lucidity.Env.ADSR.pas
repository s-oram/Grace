unit Lucidity.Env.ADSR;

interface

uses
  MoreTypes, soADSR, eeFunctions, uLucidityEnums, eeDsp;

type
  TEnvelopeStage = soADSR.TEnvelopeStage;

  TLucidityADSR = class
  private
    fSampleRate: integer;
    fAttackTime: single;
    fHoldTime: single;
    fDecayTime: single;
    fReleaseTime: single;
    fSustainLevel: single;
    fVelocityDepth: TEnvVelocityDepth;
    function GetEnvStage: TEnvelopeStage;
    function GetEnvValue: single;
    procedure SetAttackTime(const Value: single);
    procedure SetDecayTime(const Value: single);
    procedure SetHoldTime(const Value: single);
    procedure SetReleaseTime(const Value: single);
    procedure SetSampleRate(const Value: integer);
    procedure SetSustainLevel(const Value: single);
  protected
    fADSR : TADSR;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure StepResetA; inline;
    procedure Step; inline; //Process one sample frame.

    procedure Trigger(aVelocity:single);
    procedure Release;
    procedure QuickRelease(Time_ms:single);
    procedure Kill;

    property Value           :single         read GetEnvValue; //range 0..trigger-velocity.
    property Stage           :TEnvelopeStage read GetEnvStage;

    property SampleRate      :integer        read fSampleRate write SetSampleRate;

    property AttackTime   :single read fAttackTime   write SetAttackTime;    // range is 0..1
    property HoldTime     :single read fHoldTime     write SetHoldTime;      // range is 0..1
    property DecayTime    :single read fDecayTime    write SetDecayTime;     // range is 0..1
    property SustainLevel :single read fSustainLevel write SetSustainLevel;  // range is 0..1
    property ReleaseTime  :single read fReleaseTime  write SetReleaseTime;   // range is 0..1

    property VelocityDepth : TEnvVelocityDepth read fVelocityDepth write fVelocityDepth;
  end;

implementation

uses
  SysUtils, LucidityParameterScaling;

{ TLucidityADSR }

constructor TLucidityADSR.Create;
begin
  fADSR := TADSR.Create;
end;

destructor TLucidityADSR.Destroy;
begin
  fADSR.Free;
  inherited;
end;

function TLucidityADSR.GetEnvStage: TEnvelopeStage;
begin
  result := fADSR.Stage;
end;

function TLucidityADSR.GetEnvValue: single;
begin
  result := fADSR.Value;
end;

function TLucidityADSR.GetModPointer(const Name: string): PSingle;
begin
  result := fADSR.GetModPointer(Name);
end;

procedure TLucidityADSR.SetAttackTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fAttackTime := Value;
  fADSR.AttackTime := TParScaler.ADSR_AttackTimeToMS(Value);
end;

procedure TLucidityADSR.SetHoldTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fHoldTime := Value;
  fADSR.HoldTime := TParScaler.ADSR_HoldTimeToMS(Value);
end;

procedure TLucidityADSR.SetDecayTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fDecayTime := Value;
  fADSR.DecayTime := TParScaler.ADSR_DecayTimeToMS(Value);
end;

procedure TLucidityADSR.SetSustainLevel(const Value: single);
var
  db : single;
begin
  assert((Value >= 0) and (Value <= 1));
  fSustainLevel := Value;
  fADSR.SustainLevel := Value * value;
end;

procedure TLucidityADSR.SetReleaseTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fReleaseTime := Value;
  fADSR.ReleaseTime := TParScaler.ADSR_ReleaseTimeToMS(Value);
end;

procedure TLucidityADSR.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  fADSR.SampleRate := Value;
end;

procedure TLucidityADSR.Trigger(aVelocity: single);
var
  ModVel : single;
begin
  assert(aVelocity >= 0);
  assert(aVelocity <= 1);

  //aVelocity := 0.2;

  case VelocityDepth of
    TEnvVelocityDepth.Vel100: ModVel := aVelocity;
    TEnvVelocityDepth.Vel80:  ModVel := aVelocity * 0.8 + 0.2;
    TEnvVelocityDepth.Vel60:  ModVel := aVelocity * 0.6 + 0.4;
    TEnvVelocityDepth.Vel40:  ModVel := aVelocity * 0.4 + 0.6;
    TEnvVelocityDepth.Vel20:  ModVel := aVelocity * 0.2 + 0.8;
    TEnvVelocityDepth.VelOff: ModVel := 1
  else
    raise Exception.Create('Type not handled.');
  end;

  fADSR.Trigger(ModVel);


end;

procedure TLucidityADSR.Release;
begin
  fADSR.Release;
end;

procedure TLucidityADSR.QuickRelease(Time_ms: single);
begin
  fADSR.QuickRelease(Time_ms);
end;

procedure TLucidityADSR.Kill;
begin
  fADSR.Kill;
end;

procedure TLucidityADSR.StepResetA;
begin
  fADSR.StepReset;
end;

procedure TLucidityADSR.Step;
begin
  fADSR.Step;
end;



end.
