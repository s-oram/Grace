unit VamSampleMap.Movement;

interface

uses
  VamSampleMap;

procedure MoveSelectedRegions(Regions : TVamSampleRegionList; KeyOffset, VelocityOffset : integer; const Snapping : boolean);

implementation

procedure MoveSelectedRegions(Regions : TVamSampleRegionList; KeyOffset, VelocityOffset : integer; const Snapping : boolean);
var
  c1: Integer;
  LimitedKeyOffset : integer;
  LimitedVelocityOffset : integer;
  rw, rh : integer;
  rwShift, rhShift : integer;
begin
  for c1 := 0 to Regions.Count-1 do
  begin

    //==== Limit Key Shfits =====
    LimitedKeyOffset := KeyOffset;


    if Snapping then
    begin
      // Region width...
      rw := Regions[c1].HighKey - Regions[c1].LowKey + 1;

      if rw >= 12 then
      begin
        // snap key shifts to octave boundaries.
        rwShift := round((Regions[c1].LowKey + KeyOffset) / 12) * 12 - Regions[c1].LowKey;
        LimitedKeyOffset := rwShift;
      end else
      begin
        // Limit key shift to quantised shifts equal to the region width.
        rwShift := round(LimitedKeyOffset / rw);
        LimitedKeyOffset := rw * rwShift;
      end;
    end;

    if Regions[c1].LowKey  + LimitedKeyOffset < 0   then LimitedKeyOffset := -Regions[c1].LowKey;
    if Regions[c1].HighKey + LimitedKeyOffset > 127 then LimitedKeyOffset := 127 - Regions[c1].HighKey;




    //==== Limit Velocity Shifts ====

    LimitedVelocityOffset := VelocityOffset;

    if Snapping then
    begin
      // Limit velocity shifts to quantised shifts equal to the region hight.
      rh := Regions[c1].HighVelocity - Regions[c1].LowVelocity + 1;
      if rh > 16 then rh := 16;

      rhShift := round(LimitedVelocityOffset / rh);
      LimitedVelocityOffset := rh * rhShift;
    end;

    if Regions[c1].LowVelocity  + LimitedVelocityOffset < 0   then LimitedVelocityOffset := -Regions[c1].LowVelocity;
    if Regions[c1].HighVelocity + LimitedVelocityOffset > 127 then LimitedVelocityOffset := 127 - Regions[c1].HighVelocity;


    if Regions[c1].IsSelected then
    begin
      Regions[c1].IsMoving          := true;
      Regions[c1].MovedLowKey       := Regions[c1].LowKey       + LimitedKeyOffset;
      Regions[c1].MovedHighKey      := Regions[c1].HighKey      + LimitedKeyOffset;
      Regions[c1].MovedLowVelocity  := Regions[c1].LowVelocity  + LimitedVelocityOffset;
      Regions[c1].MovedHighVelocity := Regions[c1].HighVelocity + LimitedVelocityOffset;
      Regions[c1].MovedRootNote     := Regions[c1].RootNote     + LimitedKeyOffset;
    end;
  end;
end;

end.
