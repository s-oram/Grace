unit SfzParser;

interface

uses
  Classes, Contnrs,
  SfzParser.SfzOpcodes;

{$SCOPEDENUMS ON}

type
  TSfzTokenType = (Unknown, Comment, Group, Region, MultipleOpcodes, Opcode);

  // Events
  TOpcodeEvent = procedure(Sender : TObject; Opcode : TSfzOpcode; OpcodeValue : string) of object;

  TSfzParser = class
  private
    OpcodeList : TObjectList;
    fOnRegionStart: TNotifyEvent;
    fOnRegionEnd: TNotifyEvent;
    fOnRegionOpcode: TOpcodeEvent;
    fOnGroupEnd: TNotifyEvent;
    fOnGroupStart: TNotifyEvent;
    fOnGroupOpcode: TOpcodeEvent;
  protected
    IsRegionOpen : boolean;
    IsGroupOpen  : boolean;

    procedure ParseLine(const Text : string);
    function FindTokenType(s : string):TSfzTokenType;

    procedure ProcessUnknown(s : string);
    procedure ProcessComment(s : string);
    procedure ProcessRegion(s : string);
    procedure ProcessGroup(s : string);
    procedure ProcessMultipleOpcodes(s : string);
    procedure ProcessOpcode(s : string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseFile(Filename : string);
    procedure ParseText(Text : TStringList);

    property OnGroupStart  : TNotifyEvent read fOnGroupStart  write fOnGroupStart;
    property OnGroupEnd    : TNotifyEvent read fOnGroupEnd    write fOnGroupEnd;
    property OnGroupOpcode : TOpcodeEvent read fOnGroupOpcode write fOnGroupOpcode;

    property OnRegionStart  : TNotifyEvent read fOnRegionStart  write fOnRegionStart;
    property OnRegionEnd    : TNotifyEvent read fOnRegionEnd    write fOnRegionEnd;
    property OnRegionOpcode : TOpcodeEvent read fOnRegionOpcode write fOnRegionOpcode;
  end;

procedure ExplodeSfzString(s : string; var Results : TStringList);
function TrimTrailingComment(const s : string):string; // removes a double slash "//" and trailing text from a string.

implementation

uses
  Types,
  VamLib.Utils,
  SysUtils,
  StrUtils;

const
  kMaxInt = High(Integer);

procedure ExplodeSfzString(s : string; var Results : TStringList);
var
  c1: Integer;
  TestString : string;
  OpcodeTestResult : integer;
begin
  s := Trim(s);
  ExplodeString(' ', s, Results);
  for c1 := Results.Count-1 downto 1 do
  begin
    TestString := Lowercase(Results[c1]);
    OpcodeTestResult := Pos('=', TestString);
    if (TestString <> '<group>') and (TestString <> '<region>') and (OpcodeTestResult = 0) then
    begin
      TestString := Results[c1-1] + ' ' + TestString;
      Results[c1-1] := TestString;
      Results.Delete(c1);
    end;
  end;
end;

function TrimTrailingComment(const s : string):string; // removes a double slash "//" and trailing text from a string.
var
  Index : integer;
begin
  Index := Pos('//', s);

  if Index > 0 then
  begin
    result := Trim(LeftStr(s, Index-1));
  end else
  begin
    result := s;
  end;
end;

{ TSfzParser }

constructor TSfzParser.Create;
begin
  OpcodeList := TObjectList.Create;
  OpcodeList.OwnsObjects := true;
end;

destructor TSfzParser.Destroy;
begin
  OpcodeList.Free;
  inherited;
end;


procedure TSfzParser.ParseFile(Filename: string);
var
  Text : TStringList;
begin
  Text := TStringList.Create;
  try
    Text.LoadFromFile(FileName);
    ParseText(Text);
  finally
    Text.Free;
  end;
end;

procedure TSfzParser.ParseText(Text: TStringList);
var
  c1: Integer;
begin
  IsRegionOpen := false;
  IsGroupOpen  := false;

  for c1 := 0 to Text.Count-1 do
  begin
    ParseLine(Text[c1]);
  end;

  if IsRegionOpen then
  begin
    if assigned(OnRegionEnd) then OnRegionEnd(self);
    IsRegionOpen := false;
  end;

  if IsGroupOpen then
  begin
    if assigned(OnGroupEnd) then OnGroupEnd(self);
    IsGroupOpen := false;
  end;
end;

procedure TSfzParser.ParseLine(const Text: string);
var
  TokenType : TSfzTokenType;
  StringList : TStringList;
  s : string;
  c1 : integer;
begin
  if Text = '' then exit;

  StringList := TStringList.Create;
  try
    ExplodeSfzString(Text, StringList);

    for c1 := 0 to StringList.Count-1 do
    begin
      s := StringList[c1];
      s := TrimTrailingComment(s);

      TokenType := FindTokenType(s);

      case TokenType of
        TSfzTokenType.Unknown:         ProcessUnknown(s);
        TSfzTokenType.Comment:         ProcessComment(s);
        TSfzTokenType.Group:           ProcessGroup(s);
        TSfzTokenType.Region:          ProcessRegion(s);
        TSfzTokenType.MultipleOpcodes: ProcessMultipleOpcodes(s);
        TSfzTokenType.Opcode:          ProcessOpcode(s);
      else
        raise Exception.Create('Unexpected token type.');
      end;
    end;
  finally
    StringList.Free;
  end;
end;

function TSfzParser.FindTokenType(s: string): TSfzTokenType;
var
  x : integer;
begin
  //== Check for comment ==
  if StartsText('//', s) then exit(TSfzTokenType.Comment);

  //== Check for group ==
  if SameText('<group>', s) then exit(TSfzTokenType.Group);

  //== Check for region ==
  if SameText('<region>', s) then exit(TSfzTokenType.Region);

  //== Check for opcodes ==
  x := Occurrences('=', s);
  if x = 1 then exit(TSfzTokenType.Opcode);
  if x > 1 then exit(TSfzTokenType.MultipleOpcodes);

  //== token type is unknown if we've made it this far ==
  result := TSfzTokenType.Unknown;
end;


procedure TSfzParser.ProcessComment(s: string);
begin
  //It's a comment. Don't do anything.
end;

procedure TSfzParser.ProcessMultipleOpcodes(s: string);
var
  Lines : TStringDynArray;
  c1: Integer;
begin
  Lines := SplitString(s, ' ');

  for c1 := 0 to Length(Lines)-1 do
  begin
    if FindTokenType(Lines[c1]) = TSfzTokenType.MultipleOpcodes then
    begin
      ProcessUnknown('**ERROR**' + Lines[c1] + '**ERROR**');
    end else
    begin
      ParseLine(Lines[c1]);
    end;
  end;

end;

procedure TSfzParser.ProcessOpcode(s: string);
var
  OpcodeName, OpcodeValue : string;
  Opcode : TSfzOpcode;
  Lines : TStringDynArray;
begin
  Lines := SplitString(s, '=');

  if Length(Lines) = 2 then
  begin
    OpcodeName  := Lines[0];
    OpcodeValue := Lines[1];

    Opcode := StrToSfzOpcode(OpcodeName);

    if IsRegionOpen then
    begin
      if assigned(OnRegionOpcode) then OnRegionOpcode(self, Opcode, OpcodeValue);
    end else
    if IsGroupOpen then
    begin
      if assigned(OnGroupOpcode) then OnGroupOpcode(self, Opcode, OpcodeValue);
    end;
  end;
end;

procedure TSfzParser.ProcessGroup(s: string);
begin
  if IsGroupOpen then
  begin
    if IsRegionOpen then
    begin
      if assigned(OnRegionEnd)   then OnRegionEnd(self);
      IsRegionOpen := false;
    end;

    if assigned(OnGroupEnd)   then OnGroupEnd(self);
    if assigned(OnGroupStart) then OnGroupStart(self);
  end else
  begin
    if assigned(OnGroupStart) then OnGroupStart(self);
    IsGroupOpen := true;
  end;
end;

procedure TSfzParser.ProcessRegion(s: string);
begin
  // NOTE: SFZ files don't require a <group> tag to start a region.
  // To ease importing into other formats, lets open a group here
  // if one isn't already open.
  if not IsGroupOpen then
  begin
    if assigned(OnGroupStart) then OnGroupStart(self);
    IsGroupOpen := true;
  end;
  //================================================================

  if IsRegionOpen then
  begin
    if assigned(OnRegionEnd)   then OnRegionEnd(self);
    if assigned(OnRegionStart) then OnRegionStart(self);
  end else
  begin
    if assigned(OnRegionStart) then OnRegionStart(self);
    IsRegionOpen := true;
  end;
end;

procedure TSfzParser.ProcessUnknown(s: string);
begin

end;

end.
