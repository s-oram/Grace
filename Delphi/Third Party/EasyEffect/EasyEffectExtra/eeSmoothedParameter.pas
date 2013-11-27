unit eeSmoothedParameter;

interface

type
  // NOTE: TSmoothParameter uses a lowpass RC filter / leaky integer to smooth value changes.
  TSmoothedParameter = record
    CurrentValue         : double;
    TargetValue          : double;
    SmoothingCoefficient : double;

    procedure Step; inline;
    procedure Reset(Value : double = 0);
    procedure SetCoefficientAsDecayTime(const DecayTimeMilliseconds, SampleRate : single);
  end;

implementation

uses
  SysUtils;

{ TSmoothedParameter }

procedure TSmoothedParameter.Reset(Value: double);
begin
  CurrentValue := Value;
  TargetValue  := Value;
end;

procedure TSmoothedParameter.SetCoefficientAsDecayTime(const DecayTimeMilliseconds, SampleRate: single);
begin
  // RC Filter coefficients.
  // http://www.kvraudio.com/forum/viewtopic.php?t=300689&highlight=filter+envelope
  SmoothingCoefficient := exp(-1 / (DecayTimeMilliseconds * 0.001 * SampleRate));
  if SmoothingCoefficient > 1 then raise Exception.Create('SmoothingCoefficient is bigger then 1.');
end;

procedure TSmoothedParameter.Step;
begin
  CurrentValue := TargetValue + (CurrentValue - TargetValue) * SmoothingCoefficient;
end;

end.
