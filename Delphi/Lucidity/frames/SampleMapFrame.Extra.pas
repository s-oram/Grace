unit SampleMapFrame.Extra;

interface

uses
  eePlugin,
  Lucidity.Types;

type
  TAdjustRegions = record
    class procedure IncLowNote(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure DecLowNote(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure IncHighNote(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure DecHighNote(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure IncLowVel(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure DecLowVel(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure IncHighVel(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure DecHighVel(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure IncRootNote(const Plugin : TeePlugin; const RegionList : TGuidList); static;
    class procedure DecRootNote(const Plugin : TeePlugin; const RegionList : TGuidList); static;
  end;

implementation

uses
  Lucidity.SampleMap;

{ TAdjustRegions }

class procedure TAdjustRegions.IncLowNote(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
  Proceed : boolean;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.LowNote < MapRegion.GetProperties^.HighNote then
    begin
      MapRegion.GetProperties^.LowNote := MapRegion.GetProperties^.LowNote + 1;
    end;
  end;
end;

class procedure TAdjustRegions.DecLowNote(const Plugin : TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
  Proceed : boolean;
begin
  Proceed := true;
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);
    if MapRegion.GetProperties^.LowNote <= 0 then Proceed := false;
  end;

  if Proceed then
  begin
    for c1 := 0 to RegionList.Count-1 do
    begin
      MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);
      MapRegion.GetProperties^.LowNote := MapRegion.GetProperties^.LowNote - 1;
    end;
  end;
end;

class procedure TAdjustRegions.IncHighNote(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
  Proceed : boolean;
begin
  Proceed := true;
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);
    if MapRegion.GetProperties^.HighNote >= 127 then Proceed := false;
  end;

  if Proceed then
  begin
    for c1 := 0 to RegionList.Count-1 do
    begin
      MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);
      MapRegion.GetProperties^.HighNote := MapRegion.GetProperties^.HighNote + 1;
    end;
  end;
end;

class procedure TAdjustRegions.DecHighNote(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.HighNote > MapRegion.GetProperties^.LowNote then
    begin
      MapRegion.GetProperties^.HighNote := MapRegion.GetProperties^.HighNote - 1;
    end;
  end;
end;

class procedure TAdjustRegions.IncLowVel(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.LowVelocity < MapRegion.GetProperties^.HighVelocity then
    begin
      MapRegion.GetProperties^.LowVelocity := MapRegion.GetProperties^.LowVelocity + 1;
    end;
  end;
end;

class procedure TAdjustRegions.DecLowVel(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.LowVelocity > 0 then
    begin
      MapRegion.GetProperties^.LowVelocity := MapRegion.GetProperties^.LowVelocity - 1;
    end;
  end;
end;

class procedure TAdjustRegions.IncHighVel(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.HighVelocity < 127 then
    begin
      MapRegion.GetProperties^.HighVelocity := MapRegion.GetProperties^.HighVelocity + 1;
    end;
  end;
end;



class procedure TAdjustRegions.DecHighVel(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.HighVelocity > MapRegion.GetProperties^.LowVelocity then
    begin
      MapRegion.GetProperties^.HighVelocity := MapRegion.GetProperties^.HighVelocity - 1;
    end;
  end;
end;

class procedure TAdjustRegions.IncRootNote(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.RootNote < 127 then
    begin
      MapRegion.GetProperties^.RootNote := MapRegion.GetProperties^.RootNote + 1;
    end;
  end;
end;


class procedure TAdjustRegions.DecRootNote(const Plugin: TeePlugin; const RegionList: TGuidList);
var
  c1 : integer;
  MapRegion : IRegion;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    MapRegion := Plugin.SampleMap.FindRegionByUniqueID(RegionList[c1]);

    if MapRegion.GetProperties^.RootNote > 0 then
    begin
      MapRegion.GetProperties^.RootNote := MapRegion.GetProperties^.RootNote - 1;
    end;
  end;
end;








end.
