{
  This unit contains methods to calculate coefficients for biquad filters.
}

unit eeBiquadFilters;

interface

procedure CalcBiquad_CriticallyDamped_LowPass(const Freq, SampleRate : double; out B0, B1, B2, A1, A2 : double);

procedure CalcBiquad_Cookbook_LowPass(const Freq, SampleRate : double; out B0, B1, B2, A1, A2 : double);

procedure CalcBiquad_Bessel(const Freq, SampleRate : double; out B0, B1, B2, A1, A2 : double);

implementation

uses
  Math;


// Implementation reference:
// http://unicorn.us.com/alex/2polefilters.html
procedure CalcBiquad_CriticallyDamped_LowPass(const Freq, SampleRate : double; out B0, B1, B2, A1, A2 : double);
const
  CutoffCorrection : double = 1.5537739740300373073441589530631469481645834994103078;
  g = 1;
  p = 2;
var
  CorrectedFreq : double;
  W0 : double;
  K1, K2 : double;
begin
  //TODO: It might be possible to find some optimisations in here somewhere...

  CorrectedFreq := CutoffCorrection * Freq / SampleRate;
  W0 := tan(pi * CorrectedFreq);

  K1 := p * W0;
  k2 := g * w0 * w0;

  B0 := K2 / (1 + k1 + k2);
  B1 := 2 * B0;
  B2 := B0;
  A1 := 2 * B0 * (1 / k2 - 1);
  A2 := 1 - (B0 + B1 + B2 + A1);

  //TODO: see if we can work out a formula for 0hz cutoff.
end;

procedure CalcBiquad_Cookbook_LowPass(const Freq, SampleRate : double; out B0, B1, B2, A1, A2 : double);
const
  Q = 0.01;
var
  w0 : double;
  Alpha : double;
  a0 : double;
begin
  w0 := 2*pi*  Freq / SampleRate;
  Alpha := sin(w0)/(2*Q);

  b0 :=  (1 - cos(w0))/2;
  b1 :=   1 - cos(w0);
  b2 :=  (1 - cos(w0))/2;
  a0 :=   1 + alpha;
  a1 :=  -2*cos(w0);
  a2 :=   1 - alpha;


  b0 := b0 / a0;
  b1 := b1 / a0;
  b2 := b2 / a0;
  a1 := a1 / a0;
  a2 := a2 / a0;
end;


// implemented from these notes:
// http://unicorn.us.com/trading/2polefilters.html
procedure CalcBiquad_Bessel(const Freq, SampleRate : double; out B0, B1, B2, A1, A2 : double);
const
  c : double = 1.63299316185546;
  g = 3;
  p = 3;
var
  CorrectedFreq : double;
  w0 : double; //warped cutoff.
  k1, k2 : double;
begin
  CorrectedFreq := c * Freq / SampleRate;
  w0 := tan(pi * CorrectedFreq);

  k1 := p * w0;
  k2 := g * Power(w0, 2);
  b0 := k2/(1 + k1 + k2);
  b1 := 2 * b0;
  b2 := b0;
  a1 := 2 * b0 * (1 / k2 - 1);
  a2 := 1 - (b0 + b1 + b2 + a1);
end;

end.
