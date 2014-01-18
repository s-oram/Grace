unit Lucidity.Env.ADSR;

interface

{$INCLUDE Defines.inc}

uses
  uConstants,
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

    ModuleIndex    : integer;
    ParValueData : PModulatedPars;     // Raw parameter values. The values are identical for all voices in the voice group.
    ParModData   : PParModulationData; // stores the summed modulation input for each parameter. (Most parameters will be zero)

    procedure UpdateParameters;

    property AttackTime   :single read fAttackTime   write SetAttackTime;    // range is 0..1
    property HoldTime     :single read fHoldTime     write SetHoldTime;      // range is 0..1
    property DecayTime    :single read fDecayTime    write SetDecayTime;     // range is 0..1
    property SustainLevel :single read fSustainLevel write SetSustainLevel;  // range is 0..1
    property ReleaseTime  :single read fReleaseTime  write SetReleaseTime;   // range is 0..1
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(const aModuleIndex : integer; const aPars : PModulatedPars; const aModData : PParModulationData);

    function GetModPointer(const Name:string):PSingle;

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
  result := fADSR.GetModPointer(Name);
end;

procedure TLucidityADSR.Init(const aModuleIndex: integer; const aPars: PModulatedPars; const aModData: PParModulationData);
begin
  assert(ModuleIndex >= 0);
  assert(ModuleIndex <= 1);

  ModuleIndex  := aModuleIndex;
  ParValueData := aPars;
  ParModData   := aModData;
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
var
  Par1 : single;
  Par2 : single;
  Par3 : single;
  Par4 : single;
  Par5 : single;

  Par1Mod: single;
  Par2Mod: single;
  Par3Mod: single;
  Par4Mod: single;
  Par5Mod: single;
begin
  if ModuleIndex = 0 then
  begin
    Par1 := ParValueData^[TModParIndex.AmpAttack].ParValue;
    Par2 := ParValueData^[TModParIndex.AmpHold].ParValue;
    Par3 := ParValueData^[TModParIndex.AmpDecay].ParValue;
    Par4 := ParValueData^[TModParIndex.AmpSustain].ParValue;
    Par5 := ParValueData^[TModParIndex.AmpRelease].ParValue;

    Par1Mod := ParModData^[TModParIndex.AmpAttack];
    Par2Mod := ParModData^[TModParIndex.AmpHold];
    Par3Mod := ParModData^[TModParIndex.AmpDecay];
    Par4Mod := ParModData^[TModParIndex.AmpSustain];
    Par5Mod := ParModData^[TModParIndex.AmpRelease];
  end else
  begin
    Par1 := ParValueData^[TModParIndex.FilterAttack].ParValue;
    Par2 := ParValueData^[TModParIndex.FilterHold].ParValue;
    Par3 := ParValueData^[TModParIndex.FilterDecay].ParValue;
    Par4 := ParValueData^[TModParIndex.FilterSustain].ParValue;
    Par5 := ParValueData^[TModParIndex.FilterRelease].ParValue;

    Par1Mod := ParModData^[TModParIndex.FilterAttack];
    Par2Mod := ParModData^[TModParIndex.FilterHold];
    Par3Mod := ParModData^[TModParIndex.FilterDecay];
    Par4Mod := ParModData^[TModParIndex.FilterSustain];
    Par5Mod := ParModData^[TModParIndex.FilterRelease];
  end;

  Par1 := Clamp(Par1 + Par1Mod, 0, 1);
  Par2 := Clamp(Par2 + Par2Mod, 0, 1);
  Par3 := Clamp(Par3 + Par3Mod, 0, 1);
  Par4 := Clamp(Par4 + Par4Mod, 0, 1);
  Par5 := Clamp(Par5 + Par5Mod, 0, 1);

  self.AttackTime   := Par1;
  self.HoldTime     := Par2;
  self.DecayTime    := Par3;
  self.SustainLevel := Par4;
  self.ReleaseTime  := Par5;
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
  UpdateParameters;
  fADSR.StepReset;
end;

procedure TLucidityADSR.FastControlProcess;
begin
  fADSR.Step;
end;

procedure TLucidityADSR.SlowControlProcess;
begin
  UpdateParameters;
end;



end.
