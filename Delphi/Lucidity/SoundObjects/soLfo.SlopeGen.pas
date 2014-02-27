unit soLfo.SlopeGen;

interface

{$INCLUDE Defines.inc}
{$EXCESSPRECISION OFF}
{$SCOPEDENUMS ON}

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
    procedure SetSampleRate(const Value: single);
    procedure SetAttackTime(const Value: single);
    procedure SetDecayTime(const Value: single);

  protected type
    TEnvStage = (Attack, Release, Off);
  protected
    EnvStage : TEnvStage;

    CurrentValue   : single;
    AttackStepSize : single;
    DecayStepSize  : single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Trigger;

    function Step(out CycleEnd : boolean): single; overload; // generate the next LFO output sample.
    function Step: single; overload;

    property Bpm          : single read fBpm           write fBpm;
    property SampleRate   : single read fSampleRate    write SetSampleRate;

    property Curve        : single read fCurve         write fCurve;         // Envelope curve. 0..1 range. 0.5 is a straight line.
    property AttackTime   : single read fAttackTime    write SetAttackTime;  // Time is in milliseconds. Probably shouldn't be 0.
    property DecayTime    : single read fDecayTime     write SetDecayTime;   // Time is in milliseconds. Probably shouldn't be 0.

    property SlopeMode : TSlopeMode read fSlopeMode    write fSlopeMode;

  end;

implementation

{ TSlopeGen }

constructor TSlopeGen.Create;
begin

end;

destructor TSlopeGen.Destroy;
begin

  inherited;
end;

procedure TSlopeGen.SetAttackTime(const Value: single);
begin
  fAttackTime := Value;
end;

procedure TSlopeGen.SetDecayTime(const Value: single);
begin
  fDecayTime := Value;
end;

procedure TSlopeGen.SetSampleRate(const Value: single);
begin

end;

procedure TSlopeGen.Trigger;
begin

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
  result := 0;
end;





end.
