unit Lucidity.Env.ADSR;

interface

{$INCLUDE Defines.inc}

uses
  uConstants,
  Lucidity.Types,
  VamLib.MoreTypes, soADSR, {eeFunctions,} uLucidityEnums, eeDsp;

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
    fPar2: PSynthPar;
    fPar3: PSynthPar;
    fPar1: PSynthPar;
    fPar4: PSynthPar;
    fPar5: PSynthPar;
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

    ModOutput_Unipolar : single;

    procedure UpdateParameters;

    property AttackTime   :single read fAttackTime   write SetAttackTime;    // range is 0..1
    property HoldTime     :single read fHoldTime     write SetHoldTime;      // range is 0..1
    property DecayTime    :single read fDecayTime    write SetDecayTime;     // range is 0..1
    property SustainLevel :single read fSustainLevel write SetSustainLevel;  // range is 0..1
    property ReleaseTime  :single read fReleaseTime  write SetReleaseTime;   // range is 0..1
  public
    constructor Create;
    destructor Destroy; override;

    //TODO: delete this old Mod Pointer stuff.
    function GetModPointer(const Name:string):PSingle;

    property Par1 : PSynthPar read fPar1 write fPar1;
    property Par2 : PSynthPar read fPar2 write fPar2;
    property Par3 : PSynthPar read fPar3 write fPar3;
    property Par4 : PSynthPar read fPar4 write fPar4;
    property Par5 : PSynthPar read fPar5 write fPar5;

    procedure ZeroOutput;
    procedure StepResetA; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}

    procedure Trigger(aVelocity:single);
    procedure Release;
    procedure QuickRelease(Time_ms:single);
    procedure Kill;

    property Value           :single         read GetEnvValue; //range 0..trigger-velocity.
    property Stage           :TEnvelopeStage read GetEnvStage;

    property SampleRate      :integer        read fSampleRate write SetSampleRate;

    property VelocityDepth : TEnvVelocityDepth read fVelocityDepth write fVelocityDepth;
  end;

implementation

uses
  VamLib.Utils,
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
  if Name = 'EnvOut_Uni' then exit(@ModOutput_Unipolar);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

procedure TLucidityADSR.SetAttackTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fAttackTime := Value;
  fADSR.AttackTime := Value * Value * Value * 8000 + 3;
end;

procedure TLucidityADSR.SetHoldTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fHoldTime := Value;
  fADSR.HoldTime := Value * Value * 500;
end;

procedure TLucidityADSR.SetDecayTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fDecayTime := Value;
  fADSR.DecayTime := Value * Value * Value * 8000 + 6;
end;

procedure TLucidityADSR.SetSustainLevel(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fSustainLevel := Value;
  fADSR.SustainLevel := Value * Value;
end;

procedure TLucidityADSR.SetReleaseTime(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  fReleaseTime := Value;
  fADSR.ReleaseTime := Value * Value * Value * 8000 + 6;
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

  UpdateParameters;

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

procedure TLucidityADSR.UpdateParameters;
begin
  self.AttackTime   := Par1^;
  self.HoldTime     := Par2^;
  self.DecayTime    := Par3^;
  self.SustainLevel := Par4^;
  self.ReleaseTime  := Par5^;
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

procedure TLucidityADSR.ZeroOutput;
begin
  ModOutput_Unipolar := 0;
end;

procedure TLucidityADSR.StepResetA;
begin
  UpdateParameters;
  fADSR.StepReset;

  ModOutput_Unipolar := 0;
end;

procedure TLucidityADSR.FastControlProcess;
begin
  fADSR.Step;

  ModOutput_Unipolar := fAdsr.Value;
end;

procedure TLucidityADSR.SlowControlProcess;
begin
  UpdateParameters;
end;



end.
