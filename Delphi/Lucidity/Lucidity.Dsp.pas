unit Lucidity.Dsp;

interface

{$INCLUDE Defines.inc}

procedure ComputeMixBalance(const MixBalance : single; var MixDry, MixWet : single);

implementation

procedure ComputeMixBalance(const MixBalance : single; var MixDry, MixWet : single);
begin
  assert(MixBalance >= 0);
  assert(MixBalance <= 1);

  MixDry := Sqrt(1 - MixBalance);
  MixWet := Sqrt(MixBalance);

  // TODO:MED I wonder if there is a faster funciton than Sqrt() for calculating the mix balance.
end;



end.
