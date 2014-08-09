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

implementation

uses
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

end.
