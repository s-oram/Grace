unit VamSampleMap.Movement;

interface

uses
  VamSampleMap;

procedure MoveSelectedRegions(const FocusedRegion:TVamSampleRegion; Regions:TVamSampleRegionList; KeyOffset, VelocityOffset:integer; const Snapping:boolean);

procedure ResizeSelectedRegions(
    const FocusedRegion:TVamSampleRegion;
    const Regions:TVamSampleRegionList;
    const KeyOffset, VelocityOffset : integer;
    const Handle : TRegionHandleID;
    const Snapping : boolean );

implementation

uses
  SysUtils,
  VamLib.Utils,
  VamLib.Collections.Lists,
  Spring,
  Types;


function FindClosestValue(const Value : integer; const ValueList : TIntegerList):integer;
var
  Dist : integer;
  cv : integer;
  c1: Integer;
  TestDist : integer;
begin
  assert(ValueList.Count > 0);

  cv := ValueList[0];
  Dist := abs(cv - Value);

  for c1 := 1 to ValueList.Count-1 do
  begin
    TestDist := abs(ValueList[c1] - Value);
    if TestDist < Dist then
    begin
      Dist := TestDist;
      cv := ValueList[c1];
    end;
  end;

  result := cv;
end;




procedure MoveRegion(const Region : TVamSampleRegion; KeyOffset, VelocityOffset : integer);
var
  MaxKeyOffset : integer;
  MinKeyOffset : integer;
  MaxVelocityOffset : integer;
  MinVelocityOffset : integer;
begin
  MaxKeyOffset := 127 - Region.HighKey;
  MinKeyOffset := -Region.LowKey;
  MaxVelocityOffset := 127 - Region.HighVelocity;
  MinVelocityOffset := -Region.LowVelocity;

  KeyOffset := Clamp(KeyOffset, MinKeyOffset, MaxKeyOffset);
  VelocityOffset := Clamp(VelocityOffset, MinVelocityOffset, MaxVelocityOffset);

  Region.IsMoving          := true;
  Region.MovedLowKey       := Region.LowKey       + KeyOffset;
  Region.MovedHighKey      := Region.HighKey      + KeyOffset;
  Region.MovedLowVelocity  := Region.LowVelocity  + VelocityOffset;
  Region.MovedHighVelocity := Region.HighVelocity + VelocityOffset;
  Region.MovedRootNote     := Region.RootNote     + KeyOffset;
end;



procedure FindMinMaxOffsets(const Regions : TVamSampleRegionList; out MinKeyOffset, MaxKeyOffset, MinVelocityOffset, MaxVelocityOffset : integer);
var
  LowKey  : integer;
  HighKey : integer;
  LowVelocity   : integer;
  HighVelocity  : integer;
  c1: Integer;
begin
  assert(Regions.Count > 0);

  LowKey  := 127;
  HighKey := 0;
  LowVelocity  := 127;
  HighVelocity := 0;

  for c1 := 0 to Regions.Count-1 do
  begin
    if Regions[c1].IsSelected then
    begin
      if LowKey > Regions[c1].LowKey   then LowKey := Regions[c1].LowKey;
      if HighKey < Regions[c1].HighKey then HighKey := Regions[c1].HighKey;

      if LowVelocity > Regions[c1].LowVelocity   then LowVelocity := Regions[c1].LowVelocity;
      if HighVelocity < Regions[c1].HighVelocity then HighVelocity := Regions[c1].HighVelocity;
    end;
  end;

  //== return the results ===
  MaxKeyOffset := 127 - HighKey;
  MinKeyOffset := -LowKey;
  MaxVelocityOffset := 127 - HighVelocity;
  MinVelocityOffset := -LowVelocity;
end;

procedure SetupSnapPoints(
      const FocusedRegion:TVamSampleRegion;
      const Regions:TVamSampleRegionList;
      var VertSnapPoints : TIntegerList;
      var HorzSnapPoints : TIntegerList);
var
  c1 : integer;
  Dist, Pos : integer;
begin
  //Add default snapping points...
  VertSnapPoints.Add(0);
  VertSnapPoints.Add(12);
  VertSnapPoints.Add(24);
  VertSnapPoints.Add(36);
  VertSnapPoints.Add(48);
  VertSnapPoints.Add(60);
  VertSnapPoints.Add(72);
  VertSnapPoints.Add(84);
  VertSnapPoints.Add(96);
  VertSnapPoints.Add(108);
  VertSnapPoints.Add(120);
  VertSnapPoints.Add(128);

  HorzSnapPoints.Add(0);
  HorzSnapPoints.Add(8);
  HorzSnapPoints.Add(16);
  HorzSnapPoints.Add(24);
  HorzSnapPoints.Add(32);
  HorzSnapPoints.Add(40);
  HorzSnapPoints.Add(48);
  HorzSnapPoints.Add(56);
  HorzSnapPoints.Add(64);
  HorzSnapPoints.Add(72);
  HorzSnapPoints.Add(80);
  HorzSnapPoints.Add(88);
  HorzSnapPoints.Add(96);
  HorzSnapPoints.Add(104);
  HorzSnapPoints.Add(112);
  HorzSnapPoints.Add(120);
  HorzSnapPoints.Add(128);



  // add snap points for multiplies of the current location + region width...
  VertSnapPoints.Add(FocusedRegion.LowKey);

  Dist := FocusedRegion.HighKey - FocusedRegion.LowKey + 1;
  Pos  := FocusedRegion.LowKey;
  while Pos < 128 do
  begin
    inc(Pos, Dist);
    VertSnapPoints.Add(Pos);
  end;

  Pos  := FocusedRegion.LowKey;
  while Pos > 128 do
  begin
    dec(Pos, Dist);
    VertSnapPoints.Add(Pos);
  end;


  // add snap points for multiplies of the current location + region height...
  HorzSnapPoints.Add(FocusedRegion.LowVelocity);

  Dist := FocusedRegion.HighVelocity - FocusedRegion.LowVelocity + 1;
  Pos  := FocusedRegion.LowVelocity;
  while Pos < 128 do
  begin
    inc(Pos, Dist);
    HorzSnapPoints.Add(Pos);
  end;

  Pos  := FocusedRegion.LowVelocity;
  while Pos > 128 do
  begin
    dec(Pos, Dist);
    HorzSnapPoints.Add(Pos);
  end;


  //Add snap points to align with existing regions...
  for c1 := 0 to Regions.Count-1 do
  begin
    if Regions[c1].IsSelected = false then
    begin
      VertSnapPoints.Add(Regions[c1].LowKey);
      VertSnapPoints.Add(Regions[c1].HighKey + 1);

      HorzSnapPoints.Add(Regions[c1].LowVelocity);
      HorzSnapPoints.Add(Regions[c1].HighVelocity + 1);
    end;
  end;

end;


procedure MoveSelectedRegions(const FocusedRegion:TVamSampleRegion; Regions:TVamSampleRegionList; KeyOffset, VelocityOffset:integer; const Snapping:boolean);
var
  c1: Integer;
  //LimitedKeyOffset : integer;
  //LimitedVelocityOffset : integer;
  //rw, rh : integer;
  //rwShift, rhShift : integer;
  NewBounds : TRect;
  VertSnapPoints : TIntegerList;
  HorzSnapPoints : TIntegerList;
  SnapOffsetX, SnapOffsetY : integer;
  cvVertA, cvHorzA : integer;
  cvVertB, cvHorzB : integer;
  DistA, DistB : integer;
  //pos, dist : integer;
  MaxKeyOffset : integer;
  MinKeyOffset : integer;
  MaxVelocityOffset : integer;
  MinVelocityOffset : integer;
  ModifiedOffsetX, ModifiedOffsetY : integer;
begin
  if not Snapping then
  begin
    FindMinMaxOffsets(Regions, MinKeyOffset, MaxKeyOffset, MinVelocityOffset, MaxVelocityOffset);

    ModifiedOffsetX := Clamp(KeyOffset,      MinKeyOffset,      MaxKeyOffset);
    ModifiedOffsetY := Clamp(VelocityOffset, MinVelocityOffset, MaxVelocityOffset);

    for c1 := 0 to Regions.Count-1 do
    begin
      if Regions[c1].IsSelected then
      begin
        MoveRegion(Regions[c1], ModifiedOffsetX, ModifiedOffsetY);
      end;
    end;
  end else
  begin
    VertSnapPoints := TIntegerList.Create;
    VertSnapPoints.AllowDuplicates := false;
    AutoFree(@VertSnapPoints);

    HorzSnapPoints := TIntegerList.Create;
    HorzSnapPoints.AllowDuplicates := false;
    AutoFree(@HorzSnapPoints);

    SetupSnapPoints(FocusedRegion, Regions, VertSnapPoints, HorzSnapPoints);

    NewBounds.Left   := FocusedRegion.LowKey  + KeyOffset;
    NewBounds.Right  := FocusedRegion.HighKey + KeyOffset;
    NewBounds.Top    := FocusedRegion.HighVelocity + VelocityOffset;
    NewBounds.Bottom := FocusedRegion.LowVelocity  + VelocityOffset;

    cvVertA := FindClosestValue(NewBounds.Left,  VertSnapPoints);
    cvVertB := FindClosestValue(NewBounds.Right+1, VertSnapPoints)-1;

    cvHorzA := FindClosestValue(NewBounds.Bottom, HorzSnapPoints);
    cvHorzB := FindClosestValue(NewBounds.Top+1,    HorzSnapPoints)-1;


    DistA := abs(cvVertA - NewBounds.Left);
    DistB := abs(cvVertB - NewBounds.Right);

    if DistA < DistB
      then SnapOffsetX := cvVertA - NewBounds.Left
      else SnapOffsetX := cvVertB - NewBounds.Right;

    DistA := abs(cvHorzA - NewBounds.Bottom);
    DistB := abs(cvHorzB - NewBounds.Top);

    if DistA < DistB
      then SnapOffsetY := cvHorzA - NewBounds.Bottom
      else SnapOffsetY := cvHorzB - NewBounds.Top;



    FindMinMaxOffsets(Regions, MinKeyOffset, MaxKeyOffset, MinVelocityOffset, MaxVelocityOffset);

    ModifiedOffsetX := KeyOffset + SnapOffsetX;
    ModifiedOffsetY := VelocityOffset + SnapOffsetY;

    ModifiedOffsetX := Clamp(ModifiedOffsetX, MinKeyOffset,      MaxKeyOffset);
    ModifiedOffsetY := Clamp(ModifiedOffsetY, MinVelocityOffset, MaxVelocityOffset);

    for c1 := 0 to Regions.Count-1 do
    begin
      if Regions[c1].IsSelected then
      begin
        MoveRegion(Regions[c1], ModifiedOffsetX, ModifiedOffsetY);
      end;
    end;
  end;


end;





procedure CorrectMovingRegionBounds(const aRegion : TVamSampleRegion);
begin
  if aRegion.MovedLowKey < 0   then aRegion.MovedLowKey := 0;
  if aRegion.MovedLowKey > 127 then aRegion.MovedLowKey := 127;

  if aRegion.MovedHighKey < 0   then aRegion.MovedHighKey := 0;
  if aRegion.MovedHighKey > 127 then aRegion.MovedHighKey := 127;

  if aRegion.MovedRootNote < 0   then aRegion.MovedRootNote := 0;
  if aRegion.MovedRootNote > 127 then aRegion.MovedRootNote := 127;

  if aRegion.MovedLowVelocity < 0   then aRegion.MovedLowVelocity := 0;
  if aRegion.MovedLowVelocity > 127 then aRegion.MovedLowVelocity := 127;

  if aRegion.MovedHighVelocity < 0   then aRegion.MovedHighVelocity := 0;
  if aRegion.MovedHighVelocity > 127 then aRegion.MovedHighVelocity := 127;
end;


procedure ApplyRegionResize(
    const Regions:TVamSampleRegionList;
    const Handle : TRegionHandleID;
    const KeyOffset, VelocityOffset:integer );
var
  c1 : integer;
  aRegion : TVamSampleRegion;
  SampleRegions : TVamSampleRegionList absolute Regions;
  tx : integer;
begin
  for c1 := 0 to SampleRegions.Count-1 do
  begin
    if SampleRegions[c1].IsSelected then
    begin
      aRegion := SampleRegions[c1];

      // Set default un-move values...
      aRegion.IsMoving          := true;
      aRegion.MovedLowKey       := aRegion.LowKey;
      aRegion.MovedHighKey      := aRegion.HighKey;
      aRegion.MovedLowVelocity  := aRegion.LowVelocity;
      aRegion.MovedHighVelocity := aRegion.HighVelocity;
      aRegion.MovedRootNote     := aRegion.RootNote;

      // compute moved edges
      if (Handle = rhTopLeft) or (Handle = rhLeft) or (Handle = rhBottomLeft) then
      begin
        aRegion.MovedLowKey := aRegion.LowKey + KeyOffset;

        if aRegion.MovedLowKey > aRegion.MovedHighKey then
        begin
          tx := aRegion.MovedLowKey;
          aRegion.MovedLowKey  := aRegion.MovedHighKey + 1;
          aRegion.MovedHighKey := tx-1;
        end;

        if aRegion.RootNote < aRegion.MovedLowKey
          then aRegion.MovedRootNote := aRegion.MovedLowKey;
      end;

      if (Handle = rhTopRight) or (Handle = rhRight) or (Handle = rhBottomRight) then
      begin
        aRegion.MovedHighKey := aRegion.HighKey + KeyOffset;

        if aRegion.MovedHighKey < aRegion.MovedLowKey then
        begin
          tx := aRegion.MovedLowKey;
          aRegion.MovedLowKey  := aRegion.MovedHighKey + 1;
          aRegion.MovedHighKey := tx - 1;
        end;

        if aRegion.RootNote > aRegion.MovedHighKey
          then aRegion.MovedRootNote := aRegion.MovedHighKey;
      end;

      if (Handle = rhTopLeft) or (Handle = rhTop) or (Handle = rhTopRight) then
      begin
        aRegion.MovedHighVelocity := aRegion.HighVelocity + VelocityOffset;

        if aRegion.MovedHighVelocity < aRegion.MovedLowVelocity then
        begin
          tx := aRegion.MovedLowVelocity;
          aRegion.MovedLowVelocity  := aRegion.MovedHighVelocity + 1;
          aRegion.MovedHighVelocity := tx - 1;
        end;
      end;

      if (Handle = rhBottomLeft) or (Handle = rhBottom) or (Handle = rhBottomRight) then
      begin
        aRegion.MovedLowVelocity  := aRegion.LowVelocity + VelocityOffset;

        if aRegion.MovedLowVelocity > aRegion.MovedHighVelocity then
        begin
          tx := aRegion.MovedLowVelocity;
          aRegion.MovedLowVelocity  := aRegion.MovedHighVelocity + 1;
          aRegion.MovedHighVelocity := tx - 1;
        end;
      end;

      CorrectMovingRegionBounds(aRegion);
    end;
  end;
end;


procedure CheckSmartSnappingState(
    const FocusedRegion:TVamSampleRegion;
    const KeyOffset, VelocityOffset : integer;
    const Handle : TRegionHandleID;
    out VertSmartSnapDisable : boolean;
    out HorzSmartSnapDisable : boolean );
var
  rw : integer;
begin
  rw := FocusedRegion.HighKey - FocusedRegion.LowKey;
  //rh := FocusedRegion.HighVelocity - FocusedRegion.LowVelocity;

  case Handle of
    rhBottomLeft,
    rhTopLeft,
    rhLeft:
    begin
      rw := rw - KeyOffset;
    end;

    rhBottomRight,
    rhTopRight,
    rhRight:
    begin
      rw := rw + KeyOffset;
    end;

    rhTop,
    rhBottom:
    begin
      rw := rw;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;


  if abs(rw) >= 12
    then VertSmartSnapDisable := false
    else VertSmartSnapDisable := true;

  HorzSmartSnapDisable := false;
end;



procedure ResizeSelectedRegions(
    const FocusedRegion:TVamSampleRegion;
    const Regions:TVamSampleRegionList;
    const KeyOffset, VelocityOffset : integer;
    const Handle : TRegionHandleID;
    const Snapping : boolean );
var
  VertSnapPoints : TIntegerList;
  HorzSnapPoints : TIntegerList;
  TargetKey, TargetVelocity : integer;
  ModifiedVelocityOffset : integer;
  ModifiedKeyOffset : integer;
  VertSmartSnapDisable : boolean;
  HorzSmartSnapDisable : boolean;
begin
  if not Snapping then
  begin
    ApplyRegionResize(Regions, Handle, KeyOffset, VelocityOffset);
  end else
  begin
    VertSnapPoints := TIntegerList.Create;
    VertSnapPoints.AllowDuplicates := false;
    AutoFree(@VertSnapPoints);

    HorzSnapPoints := TIntegerList.Create;
    HorzSnapPoints.AllowDuplicates := false;
    AutoFree(@HorzSnapPoints);

    SetupSnapPoints(FocusedRegion, Regions, VertSnapPoints, HorzSnapPoints);

    case Handle of
      rhTopLeft,
      rhTopRight,
      rhTop:
      begin
        TargetVelocity := FocusedRegion.HighVelocity + VelocityOffset;
        TargetVelocity := FindClosestValue(TargetVelocity+1, HorzSnapPoints)-1;
        ModifiedVelocityOffset := TargetVelocity - FocusedRegion.HighVelocity;
      end;

      rhBottomRight,
      rhBottomLeft,
      rhBottom:
      begin
        TargetVelocity := FocusedRegion.LowVelocity + VelocityOffset;
        TargetVelocity := FindClosestValue(TargetVelocity, HorzSnapPoints);
        ModifiedVelocityOffset := TargetVelocity - FocusedRegion.LowVelocity;
      end;

      rhRight,
      rhLeft:
      begin
        ModifiedVelocityOffset := VelocityOffset;
      end
    else
      raise Exception.Create('Type not handled.');
    end;


    case Handle of
      rhBottomLeft,
      rhTopLeft,
      rhLeft:
      begin
        TargetKey := FocusedRegion.LowKey + KeyOffset;
        TargetKey := FindClosestValue(TargetKey, VertSnapPoints);
        ModifiedKeyOffset := TargetKey - FocusedRegion.LowKey;
      end;

      rhBottomRight,
      rhTopRight,
      rhRight:
      begin
        TargetKey := FocusedRegion.HighKey + KeyOffset;
        TargetKey := FindClosestValue(TargetKey+1, VertSnapPoints)-1;
        ModifiedKeyOffset := TargetKey - FocusedRegion.HighKey;
      end;

      rhTop,
      rhBottom:
      begin
        ModifiedKeyOffset := KeyOffset;
      end;
    else
      raise Exception.Create('Type not handled.');
    end;

    CheckSmartSnappingState(FocusedRegion, KeyOffset, VelocityOffset, Handle, VertSmartSnapDisable, HorzSmartSnapDisable);
    if VertSmartSnapDisable
      then ModifiedKeyOffset := KeyOffset;

    ApplyRegionResize(Regions, Handle, ModifiedKeyOffset, ModifiedVelocityOffset);
  end;
end;


end.
