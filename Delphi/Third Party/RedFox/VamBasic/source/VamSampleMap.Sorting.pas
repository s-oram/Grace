{
  This unit provides extension methods for VamSampleMap.pas.
  The methods provided are for sorting the sample regions on a sample map.
  It is intended this unit will only be used by VamSampleMap.
  The sorting methods were moved into this unit to avoid cluttering the
  main unit.
}

unit VamSampleMap.Sorting;

interface

uses
  VamSampleMap;


procedure SortToAvoidUnclickableElements(const Regions : TVamSampleRegionList);

implementation



function IsAvoidablyConcealed(const Current, Target : TVamSampleRegion):boolean;
var
  IsConcealed : boolean;
begin


  if (Current.LowKey <= Target.LowKey)
    and (Current.HighKey >= Target.HighKey)
    and (Current.LowVelocity <= Target.LowVelocity)
    and (Current.HighVelocity >= Target.HighVelocity)
  then IsConcealed := true
  else IsConcealed := false;



  // Target is not concealed. exit and report false!!
  if IsConcealed = false then exit(false); //exit===>>



  // Check for the special case where the target occupies exactly the
  // same space as the current region. It is concealed, but unavoidaly so.
  if (Current.LowKey = Target.LowKey)
    and (Current.HighKey = Target.HighKey)
    and (Current.LowVelocity = Target.LowVelocity)
    and (Current.HighVelocity = Target.HighVelocity)
  then
  begin
    exit(false); //exit===>>
  end;



  // If we've made it this far, the target is avoidable concealed.
  assert(IsConcealed);
  result := true;
end;

procedure SortToAvoidUnclickableElements(const Regions : TVamSampleRegionList);
var
  Index : integer;
  CurrentRegion : TVamSampleRegion;
  TestRegion : TVamSampleRegion;
  ResetSort : boolean;
  c1: Integer;
begin
  Index := 0;

  ResetSort := false;

  while Index < Regions.Count-1 do
  begin
    CurrentRegion := Regions[Index];

    for c1 := Index to Regions.Count-1 do
    begin
      TestRegion := Regions[c1];
      if IsAvoidablyConcealed(CurrentRegion, TestRegion) then
      begin
        Regions.Move(c1, Index);
        ResetSort := true;
        break;
      end;
    end;

    if ResetSort then
    begin
      ResetSort := false;
      Index := 0;
    end else
    begin
      inc(Index);
    end;
  end;



end;

end.
