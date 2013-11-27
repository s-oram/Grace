unit soStepInFilter;

interface

uses
  B2.Filter.CriticallyDampedLowpass;

type
  TStepInFilter = class
  private
    fSampleRate: integer;
    fDecayTime: double;
    fIsActive: boolean;
    procedure SetDecayTime(const Value: double);
    procedure SetSampleRate(const Value: integer);
  protected
    AmpFactor : double;
    Gain : single;
    GainFilter : TCriticallyDampedLowpass;
  public
    constructor Create;
    destructor Destroy;

    procedure Trigger;

    procedure Process(var x1, x2 : single);

    property DecayTime  : double read fDecayTime write SetDecayTime; //in milliseconds.
    property SampleRate : integer read fSampleRate write SetSampleRate;

    property IsActive : boolean read fIsActive;
  end;

implementation

uses
  eeDsp;

{ TStepInFilter }

constructor TStepInFilter.Create;
begin
  AmpFactor := 2;
  Gain := 1;

  fDecayTime := 5;
  fSampleRate := 44100;

  GainFilter := TCriticallyDampedLowpass.Create;

  fIsActive := false;

end;

destructor TStepInFilter.Destroy;
begin
  GainFilter.Free;
end;

procedure TStepInFilter.SetDecayTime(const Value: double);
begin
  fDecayTime := Value;

  if (fDecayTime > 0) and (fSampleRate > 0)
    then GainFilter.SetTransitionTime(fDecayTime, fSampleRate);

end;

procedure TStepInFilter.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;

  if (fDecayTime > 0) and (fSampleRate > 0)
    then GainFilter.SetTransitionTime(fDecayTime, fSampleRate);
end;

procedure TStepInFilter.Process(var x1, x2: single);
begin
  x1 := x1 * Gain;
  x2 := x2 * Gain;
  Gain := GainFilter.Step(1.01);
  if Gain > 1 then
  begin
    Gain := 1;
    fIsActive := false;
  end;
end;


procedure TStepInFilter.Trigger;
begin
  Gain := 0;
  GainFilter.Reset;
  fIsActive := true;
end;

end.
