unit LucidityUtils;

interface

uses
  uConstants;

type
  TParInfoEx = record
    Name : string;
    ModLinkIndex : integer;
  end;

var
  ParInfoEx : array[0..kParameterCount-1] of TParInfoEx;

function ModLinkIndexToParName(Index : integer):string;
function ParNameToModLinkIndex(Name : string):integer;




implementation

function ModLinkIndexToParName(Index : integer):string;
var
  c1: Integer;
begin
  for c1 := 0 to kParameterCount-1 do
  begin
    if Index = ParInfoEx[c1].ModLinkIndex
      then exit(ParInfoEx[c1].Name);
  end;

  result := '';
end;

function ParNameToModLinkIndex(Name : string):integer;
var
  c1: Integer;
begin
  for c1 := 0 to kParameterCount-1 do
  begin
    if Name = ParInfoEx[c1].Name
      then exit(ParInfoEx[c1].ModLinkIndex);
  end;

  result := -1;
end;

end.
