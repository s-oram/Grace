unit LucidityModConnections;

interface

uses
  VamLib.Collections.Lists,
  VamLib.MoreTypes,
  uConstants, uLucidityEnums;

type
  // NOTE: TModConnections stores all mod connections for the sampler.
  TModLink = record
    ModAmount : array[0..kModSlots-1] of single;
  end;

  TModLinkArray = TArray<TModLink>;


  PModConnections = ^TModConnections;
  TModConnections = class
  private
    fModLinks: TModLinkArray;
  public
    ModSource : array[0..kModSlots-1] of TModSource;
    ModVia    : array[0..kModSlots-1] of TModSource;

    constructor Create;
    destructor Destroy; override;

    property ModLinks  : TModLinkArray   read fModLinks  write fModLinks; //One for each modulated parameter
  end;



  // NOTE: TModConnections stores all mod connections for the sampler.
  PModLink_OLD = ^TModLink_OLD;
  TModLink_OLD = record
    UniqueID : string; //NOTE: I'm not entirely sure this UniqueID is a good idea.
    Source   : TModSource;
    Dest     : TModDest;
    Via      : TModSource;
    Amount   : single; //range -1..1
    Offset   : single; //range -0.5..0.5
    procedure AssignFrom(const aSource:TModLink_OLD);
  end;

  TModConnections_OLD = class
  private
    function GetModLinkCount: integer;
    function GetModLink(Index: integer): PModLink_OLD;
    procedure SetModLink(Index: integer; const Value: PModLink_OLD);
  protected
    fModLinkCount : integer;
    fModLinks : array of TModLink_OLD;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function FindModLinkByID(const UniqueID : string):PModLink_OLD;
    function FindModLink(const ModDest : TModDest; const Offset : integer):PModLink_OLD;

    procedure UpdateModLink(const ModDest : TModDest; const Offset : integer; const NewLinkData:PModLink_OLD);
    procedure UpdateModLinkByID(const UniqueID : string; const NewLinkData : PModLink_OLD);

    property ModLinkCount : integer read GetModLinkCount;
    property ModLinks[Index:integer]:PModLink_OLD read GetModLink write SetModLink;
  end;



implementation

uses
  SysUtils;




{ TModConnections }

constructor TModConnections.Create;
begin
  SetLength(fModLinks, kModulatedParameterCount);

  self.ModSource[0] := TModSource.AmpEnv;
  self.ModSource[1] := TModSource.FilterEnv;
  self.ModSource[2] := TModSource.Lfo1;
  self.ModSource[3] := TModSource.Lfo2;
  self.ModSource[4] := TModSource.StepSeq1;
  self.ModSource[5] := TModSource.StepSeq2;
  self.ModSource[6] := TModSource.Midi_ModWheel;
  //self.ModSource[7] := TModSource.Midi_Velocity; //TODO Add velocity as a mod source.
end;

destructor TModConnections.Destroy;
begin
  SetLength(fModLinks, 0);
  inherited;
end;


{ TModLink_OLD }

procedure TModLink_OLD.AssignFrom(const aSource: TModLink_OLD);
begin
  self.Source   := aSource.Source;
  self.Dest     := aSource.Dest;
  self.Via      := aSource.Via;
  self.Amount   := aSource.Amount;
  self.Offset   := aSource.Offset;
  self.UniqueID := aSource.UniqueID;
end;



{ TModConnections_OLD }

constructor TModConnections_OLD.Create;
begin
  fModLinkCount := TModDestHelper.GetEnumTypeCount * 4;

  SetLength(fModLinks, fModLinkCount);

  Clear;
end;

destructor TModConnections_OLD.Destroy;
begin
  SetLength(fModLinks, 0);
  inherited;
end;

procedure TModConnections_OLD.Clear;
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



function TModConnections_OLD.GetModLink(Index: integer): PModLink_OLD;
begin
  result := @fModLinks[Index];
end;

function TModConnections_OLD.GetModLinkCount: integer;
begin
  result := fModLinkCount;
end;

procedure TModConnections_OLD.SetModLink(Index: integer; const Value: PModLink_OLD);
begin
  if fModLinks[Index].UniqueID <> Value^.UniqueID
    then raise Exception.Create('ModLink unique IDs don''t match.');

  fModLinks[Index].AssignFrom(Value^);
end;

function TModConnections_OLD.FindModLink(const ModDest: TModDest; const Offset: integer): PModLink_OLD;
var
  Index : integer;
begin
  assert(Offset >= 0);
  assert(Offset <= 3);

  Index := TModDestHelper.ToInteger(ModDest) * 4;
  inc(Index, Offset);

  result := @fModLinks[Index];
end;

function TModConnections_OLD.FindModLinkByID(const UniqueID: string): PModLink_OLD;
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

procedure TModConnections_OLD.UpdateModLink(const ModDest: TModDest; const Offset: integer; const NewLinkData: PModLink_OLD);
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

procedure TModConnections_OLD.UpdateModLinkByID(const UniqueID: string; const NewLinkData: PModLink_OLD);
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
