unit LucidityModConnections;

interface

uses
  VamLib.MoreTypes,
  uConstants, uLucidityEnums;

type
  // NOTE: TModConnections stores all mod connections for the sampler.

  PModLink = ^TModLink;
  TModLink = record
    UniqueID : string; //NOTE: I'm not entirely sure this UniqueID is a good idea.
    Source   : TModSource;
    Dest     : TModDest;
    Via      : TModSource;
    Amount   : single; //range -1..1
    Offset   : single; //range -0.5..0.5
    procedure AssignFrom(const aSource:TModLink);
  end;

  TModConnections = class
  private
    function GetModLinkCount: integer;
    function GetModLink(Index: integer): PModLink;
    procedure SetModLink(Index: integer; const Value: PModLink);
  protected
    fModLinkCount : integer;
    fModLinks : array of TModLink;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function FindModLinkByID(const UniqueID : string):PModLink;
    function FindModLink(const ModDest : TModDest; const Offset : integer):PModLink;

    procedure UpdateModLink(const ModDest : TModDest; const Offset : integer; const NewLinkData:PModLink);
    procedure UpdateModLinkByID(const UniqueID : string; const NewLinkData : PModLink);

    property ModLinkCount : integer read GetModLinkCount;
    property ModLinks[Index:integer]:PModLink read GetModLink write SetModLink;
  end;



implementation

uses
  SysUtils;

{ TModLink }

procedure TModLink.AssignFrom(const aSource: TModLink);
begin
  self.Source   := aSource.Source;
  self.Dest     := aSource.Dest;
  self.Via      := aSource.Via;
  self.Amount   := aSource.Amount;
  self.Offset   := aSource.Offset;
  self.UniqueID := aSource.UniqueID;
end;



{ TModConnections }

constructor TModConnections.Create;
begin
  fModLinkCount := TModDestHelper.GetEnumTypeCount * 4;

  SetLength(fModLinks, fModLinkCount);

  Clear;
end;

destructor TModConnections.Destroy;
begin
  SetLength(fModLinks, 0);
  inherited;
end;

procedure TModConnections.Clear;
var
  c1: Integer;
begin
  for c1 := 0 to ModLinkCount-1 do
  begin
    fModLinks[c1].UniqueID := TModDestHelper.ToString(c1 div 4) + '_Offset' + IntToStr(c1 mod 4);
    fModLinks[c1].Dest     := TModDestHelper.ToEnum(c1 div 4);
    fModLinks[c1].Source   := TModSource.None;
    fModLinks[c1].Via      := TModSource.None;
    fModLinks[c1].Amount   := 0;
    fModLinks[c1].Offset   := 0;
  end;
end;



function TModConnections.GetModLink(Index: integer): PModLink;
begin
  result := @fModLinks[Index];
end;

function TModConnections.GetModLinkCount: integer;
begin
  result := fModLinkCount;
end;

procedure TModConnections.SetModLink(Index: integer; const Value: PModLink);
begin
  if fModLinks[Index].UniqueID <> Value^.UniqueID
    then raise Exception.Create('ModLink unique IDs don''t match.');

  fModLinks[Index].AssignFrom(Value^);
end;

function TModConnections.FindModLink(const ModDest: TModDest; const Offset: integer): PModLink;
var
  Index : integer;
begin
  assert(Offset >= 0);
  assert(Offset <= 3);

  Index := TModDestHelper.ToInteger(ModDest) * 4;
  inc(Index, Offset);

  result := @fModLinks[Index];
end;

function TModConnections.FindModLinkByID(const UniqueID: string): PModLink;
var
  c1: Integer;
begin
  for c1 := 0 to ModLinkCount-1 do
  begin
    if fModLinks[c1].UniqueID = UniqueID
      then exit(@fModLinks[c1]);
  end;

  // if we've made it this far no match has been found
  result := nil;
end;

procedure TModConnections.UpdateModLink(const ModDest: TModDest; const Offset: integer; const NewLinkData: PModLink);
var
  Index : integer;
begin
  assert(Offset >= 0);
  assert(Offset <= 3);

  Index := TModDestHelper.ToInteger(ModDest) * 4;
  inc(Index, Offset);

  if fModLinks[Index].UniqueID <> NewLinkData^.UniqueID
    then raise Exception.Create('ModLink unique IDs don''t match.');

  fModLinks[Index].AssignFrom(NewLinkData^);
end;

procedure TModConnections.UpdateModLinkByID(const UniqueID: string; const NewLinkData: PModLink);
var
  c1: Integer;
  Dest : TModDest;
begin
  if NewLinkData^.UniqueID <> UniqueID
    then raise Exception.Create('ModLink unique IDs don''t match.');

  for c1 := 0 to ModLinkCount-1 do
  begin
    if fModLinks[c1].UniqueID = UniqueID then
    begin
      fModLinks[c1].AssignFrom(NewLinkData^);
      exit; //====================================>> exit >>=========>>
    end;
  end;
end;

end.
