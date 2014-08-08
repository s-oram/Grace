unit soLfo.SlopeGen;

interface

{$INCLUDE Defines.inc}
{$EXCESSPRECISION OFF}
{$SCOPEDENUMS ON}

//TODO:
// The slope gen needs to know when the note-off event occurs!

uses
  Math,
  VamLib.Utils,
  B2.Filter.CriticallyDampedLowpass,
  eeOscPhaseCounter,
  eeDsp;

type
  TSlopeMode = (AR, AD, Cycle);

  TSlopeGen = class
  private
    fSampleRate: single;
    fBpm: single;
    fDecayTime: single;
    fCurve: single;
    fSlopeMode: TSlopeMode;
    fAttackTime: single;
    fMinTotalTime: single;
    fEnvRate: single;
    procedure SetSampleRate(const Value: single);
    procedure SetMinTotalTime(const Value: single);
    procedure SetEnvRate(const Value: single);

  protected type
    TEnvStage = (Attack, Sustrain, Release, Off);
  protected
    EnvStage : TEnvStage;

    CurrentValue   : single;
    AttackStepSize : single;
    DecayStepSize  : single;


    procedure UpdateEnvStepSizes; //inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Trigger;
    procedure Release;
    procedure Kill;

    function Step(out CycleEnd : boolean): single; overload; // generate the next LFO output sample.
    function Step: single; overload;

    property Bpm          : single read fBpm           write fBpm;
    property SampleRate   : single read fSampleRate    write SetSampleRate;

    // MinTotalTime is used as a reference when calculating how fast any RATE value is.
    property MinTotalTime : single read fMinTotalTime  write SetMinTotalTime;  //milliseconds.
    property EnvRate      : single read fEnvRate       write SetEnvRate; // range 0..1



    property Curve        : single read fCurve         write fCurve;         // Envelope curve. 0..1 range. 0.5 is a straight line.


    property SlopeMode : TSlopeMode read fSlopeMode    write fSlopeMode;

  end;

implementation

uses
  SysUtils;

{ TSlopeGen }

constructor TSlopeGen.Create;
begin
  CurrentValue   := 0;
  AttackStepSize := 0;
  DecayStepSize  := 0;
end;

destructor TSlopeGen.Destroy;
begin

  inherited;
end;

procedure TSlopeGen.Kill;
begin
  CurrentValue := 0;
end;

procedure TSlopeGen.UpdateEnvStepSizes;
var
  TotalStepSize : single;
begin
  //TODO:MED optimisation opportuniity here! factor out the 1/samplerate value.
  TotalStepSize := 1 / (fSampleRate * fMinTotalTime * 0.001);
  TotalStepSize := TotalStepSize * fEnvRate * 2;
  AttackStepSize := TotalStepSize;
  DecayStepSize  := TotalStepSize;
end;

procedure TSlopeGen.SetEnvRate(const Value: single);
begin
  fEnvRate := Value;
  UpdateEnvStepSizes;
end;

procedure TSlopeGen.SetMinTotalTime(const Value: single);
begin
  fMinTotalTime := Value;
  UpdateEnvStepSizes;
end;

procedure TSlopeGen.SetSampleRate(const Value: single);
begin
  fSampleRate := value;
end;

procedure TSlopeGen.Trigger;
begin
  EnvStage := TEnvStage.Attack;
end;

procedure TSlopeGen.Release;
begin
  if SlopeMode = TSlopeMode.AR
    then EnvStage := TEnvStage.Release;
end;



function TSlopeGen.Step: single;
var
  x : boolean;
begin
  result := Step(x);
end;

function TSlopeGen.Step(out CycleEnd: boolean): single;
begin
  CycleEnd := false;

  case EnvStage of
    TSlopeGen.TEnvStage.Attack:
    begin
      CurrentValue := CurrentValue + AttackStepSize;
      if CurrentValue >= 1 then
      begin
        CurrentValue := 1;
        if SlopeMode = TSlopeMode.AR
          then EnvStage := TEnvStage.Sustrain
          else EnvStage := TEnvStage.Release;
      end;
    end;

    TSlopeGen.TEnvStage.Sustrain:
    begin

    end;

    TSlopeGen.TEnvStage.Release:
    begin
      CurrentValue := CurrentValue - DecayStepSize;
      if CurrentValue <= 0 then
      begin
        CurrentValue := 0;
        if SlopeMode = TSlopeMode.Cycle
          then EnvStage := TEnvStage.Attack
          else EnvStage := TEnvStage.Off;
      end;
    end;

    TSlopeGen.TEnvStage.Off:
    begin

    end;
  else
    raise Exception.Create('Type not handled.');
  end;


  result := CurrentValue;
end;





end.
