unit eeParSmoothingUtils;

interface


// CalculateSmoothingCoefficient()
// - SmoothingTime is in milliseconds.
function CalculateSmoothingCoefficient(const SmoothingTime, SampleRate : single):single;

function SmoothParameterChange(const CurrentParameterValue, TargetParameterValue, SmoothingCoefficient:single):single; inline;

implementation

uses
  eeDSP;

function CalculateSmoothingCoefficient(const SmoothingTime, SampleRate : single):single;
begin
  if SmoothingTime = 0
    then result := 1
    else result := exp(-1 / (SmoothingTime * 0.001 * SampleRate));


  assert(Result >= 0, 'Smoothing coefficient is out of range.');
  assert(Result <= 1, 'Smoothing coefficient is out of range.');
end;

function SmoothParameterChange(const CurrentParameterValue, TargetParameterValue, SmoothingCoefficient:single):single; inline;
begin
  result := TargetParameterValue + (CurrentParameterValue - TargetParameterValue + kDenormal) * SmoothingCoefficient ;
end;

end.
