unit LucidityParameterScaling;

interface

uses
  Math;

type
  // TParScaler is a class to centralise converting
  // from 0..1 VST parameter range to the target parameter range.
  TParScaler = class
  private
  public
    class function ADSR_AttackTimeVstParToMS(const Value:single):single;
    class function ADSR_HoldTimeVstParToMS(const Value:single):single;
    class function ADSR_DecayTimeVstParToMS(const Value:single):single;
    class function ADSR_ReleaseTimeVstParToMS(const Value:single):single;

    //TODO:MED this can probably deleted at some stage.
    class function ModEnv_StageTimeToMS(const Value:single):single;
  end;



// VstFloat <-> FilterFrequency
// These methods convert between a 0-1 parameter range
// and an exponentially scalled filter frequency. It is
// the same scaling as used in Uhe Zebra and Ace. The
// scaling feels natural when adjust knobs.
function VstFloatToFilterFrequency(const Value : single):single;
function FilterFrequencyToVstFloat(Value : single):single;


implementation

uses
  VamLib.Utils,
  eeFunctions, eeVirtualCV;

{ TParScaler }

class function TParScaler.ADSR_AttackTimeVstParToMS(const Value: single): single;
begin
  assert((Value >= 0) and (Value <= 1));
  result := StaggeredExpand((Value * Value), 0,500,3000,8000);
end;

class function TParScaler.ADSR_HoldTimeVstParToMS(const Value: single): single;
begin
  assert((Value >= 0) and (Value <= 1));
  result := Value * Value * 500;
end;

class function TParScaler.ADSR_DecayTimeVstParToMS(const Value: single): single;
begin
  assert((Value >= 0) and (Value <= 1));
  result := StaggeredExpand((Value * Value), 6,500,3000,8000);
end;



class function TParScaler.ADSR_ReleaseTimeVstParToMS(const Value: single): single;
begin
  assert((Value >= 0) and (Value <= 1));
  result := StaggeredExpand((Value * Value), 6,500,3000,8000);
end;

class function TParScaler.ModEnv_StageTimeToMS(const Value: single): single;
begin
  assert((Value >= 0) and (Value <= 1));
  result := StaggeredExpand((Value * Value), 1,500,3000,8000);
end;

function VstFloatToFilterFrequency(const Value : single):single;
const
  a = 6.678023;
  b = 8.004718;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  result := a * exp(b * Value);

  assert(result >= 6);
  assert(result <= 20100);

  // Optimisation Note: This function was found with CurveExpert
  // by matching Zebra's filter frequency curve.
  // CurveExpert reported this expontential function and a
  // MMF type function to be good fits. The MMF function
  // has a different struction (it uses powers) and might
  // be more CPU efficient.


end;

function FilterFrequencyToVstFloat(Value : single):single;
const
  a = 6.678023;
  b = 8.004718;
begin
  // Use a slighting smaller range to ensure the result
  // remains in range for the VstFloatToFrequency() function.
  Value := Clamp(Value, 7, 20000);
  result  := Ln(Value / a) / b;

  // NOTE: For my future mathematically illiterate self,
  //    Ln(exp(x)) = x
  // IE: Ln() is the inverse of exp().

  // Optimisation Note: Ln() can be approximated with the taylor series.\
  // http://en.wikipedia.org/wiki/Natural_logarithm
end;

end.
