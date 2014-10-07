unit soFilter.RingModA;

interface

uses
  soSineOsc;

type
  TRingModA = class
  private
    fSampleRate: single;
    fOscFreq: single;
    fMix: single;
    procedure SetOscFreq(const Value: single);
    procedure SetSampleRate(const Value: single);
    procedure SetMix(const Value: single);
  protected
    Osc : TSineOsc;
    MixDry, MixWet : single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure ControlRateStep; inline;
    procedure AudioRateStep(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write SetSampleRate;

    property OscFreq : single read fOscFreq write SetOscFreq; // Freq set in Hz.
    property Mix     : single read fMix     write SetMix; //range 0..1.
  end;


implementation

{ TRingModA }

constructor TRingModA.Create;
begin
  Osc := TSineOsc.Create;

  Osc.SampleRate := 44100;
  Osc.Freq := 50;
end;

destructor TRingModA.Destroy;
begin
  Osc.Free;
  inherited;
end;

procedure TRingModA.Reset;
begin

end;

procedure TRingModA.SetMix(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fMix := Value;
  MixDry := (1 - Value);
  MixWet := Value;
end;

procedure TRingModA.SetOscFreq(const Value: single);
begin
  fOscFreq := Value;
  Osc.Freq := Value;
end;

procedure TRingModA.SetSampleRate(const Value: single);
begin
  if Value <> fSampleRate then
  begin
    fSampleRate := Value;
    Osc.SampleRate := Value;
  end;
end;

procedure TRingModA.ControlRateStep;
begin

end;

procedure TRingModA.AudioRateStep(var x1, x2: single);
var
  rm : single;
begin
  rm := Osc.AudioRateStep;
  x1 := (x1 * MixDry) + (x1 * rm * MixWet);
  x2 := (x2 * MixDry) + (x2 * rm * MixWet);
end;

end.
