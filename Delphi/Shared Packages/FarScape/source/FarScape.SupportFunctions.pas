unit FarScape.SupportFunctions;

interface

uses
  FarScape.CustomControl;

function StrToAlign(const Value : string):TControlAlignment;
function StrToHitTest(const Value : string):THitTest;




type
  TBitAccess = record
  private
    Data : integer;
  public
    function GetBit(const Index : integer):boolean;
    procedure SetBit(const Index : integer; const Value : boolean);
  end;


// ====== Section Position ========

// When creating GUI controls, control elements often need
// to be positioned at regular locations. This code helps
// calculate the exact position of a section.

type
  TSectionPos = record
    PointA : integer; // First pixel index in the section.
    PointB : integer; // Last pixel index in the section.
    Size   : integer; // Total pixel size in section.
  end;

// GetSectionPos()
// - TotalSize: (pixels) Total size of the section.
// - SectionIndex: Which section are we interested in. Range 0..SectionCount-1
// - SectionCount: How many sections are there? It's assumed each section will be equal in size.
// - SectionMargin: (pixels) Spacing between sections. This function doesn't create spacing between the sections
//     and the edges.
function GetSectionPos(const TotalSize : integer; const SectionIndex, SectionCount, SectionMargin : integer):TSectionPos;

implementation

uses
  Math,
  SysUtils;

function StrToAlign(const Value : string):TControlAlignment;
begin
  if SameText(Value, 'None')   then exit(caNone);
  if SameText(Value, 'Top')    then exit(caTop);
  if SameText(Value, 'Bottom') then exit(caBottom);
  if SameText(Value, 'Left')   then exit(caLeft);
  if SameText(Value, 'Right')  then exit(caRight);
  if SameText(Value, 'Client') then exit(caClient);
  if SameText(Value, 'Center') then exit(caCenter);
  if SameText(Value, 'Grid')   then exit(caGrid);

  raise Exception.Create('Unexpected value.');
end;


function StrToHitTest(const Value : string):THitTest;
begin
  if SameText(Value, 'None')    then exit(htNone);
  if SameText(Value, 'Partial') then exit(htPartial);
  if SameText(Value, 'Always')  then exit(htAlways);

  raise Exception.Create('Unexpected value.');
end;


{ TBitAccess }

function TBitAccess.GetBit(const Index: integer): boolean;
begin
  assert(Index >= 0);
  assert(Index <= 31);
  Result := (Data and (1 shl Index)) <> 0;
end;

procedure TBitAccess.SetBit(const Index: integer; const Value: boolean);
begin
  assert(Index >= 0);
  assert(Index <= 31);
  if Value
    then Data := Data or (1 shl Index)
    else Data := Data and not (1 shl Index);
end;

function GetSectionPos(const TotalSize : integer; const SectionIndex, SectionCount, SectionMargin : integer):TSectionPos;
var
  ptA, ptB : integer;
begin
  assert(TotalSize > 0);
  assert(SectionCount > 0);
  assert(SectionIndex >= 0);
  assert(SectionIndex < SectionCount);
  assert(SectionMargin * (SectionCount-1) < TotalSize);

  //GutterPixels := SectionMargin * (SectionCount-1);
  //SectSize := floor((TotalSize - GutterPixels) / SectionCount);


  ptA := floor(    ( SectionIndex    / SectionCount) * ((TotalSize) + SectionMargin)  );
  ptB := floor(    ((SectionIndex+1) / SectionCount) * ((TotalSize) + SectionMargin) - SectionMargin );

  assert(ptA >= 0);
  assert(ptA < TotalSize);
  assert(ptB >= 0);

  result.PointA := ptA;
  result.PointB := ptB;
  result.Size := ptB - ptA + 1;
end;

end.
