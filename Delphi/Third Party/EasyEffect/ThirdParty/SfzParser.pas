unit SfzParser;

interface


//   SFZ Opcode documentation
//   http://www.cakewalk.com/DevXchange/article.aspx?aid=108

uses
  Classes, Contnrs;

type
  TSFZTokenType = (ttUnknown, ttComment, ttRegion, ttMultipleOpcodes, ttOpcode);

  // Events
  TOpcodeEvent = procedure(Sender : TObject; OpcodeName, OpcodeValue : string) of object;





  TSfzParser = class
  private
    OpcodeList : TObjectList;
    fOnRegionStart: TNotifyEvent;
    fOnRegionEnd: TNotifyEvent;
    fOnOpcode: TOpcodeEvent;
  protected
    IsRegionOpen : boolean;
    IsGroupOpen  : boolean;

    procedure ParseLine(s : string);
    function FindTokenType(s : string):TSfzTokenType;

    procedure ProcessUnknown(s : string);
    procedure ProcessComment(s : string);
    procedure ProcessRegion(s : string);
    procedure ProcessMultipleOpcodes(s : string);
    procedure ProcessOpcode(s : string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseFile(Filename : string);
    procedure ParseText(Text : TStringList);

    property OnRegionStart : TNotifyEvent read fOnRegionStart write fOnRegionStart;
    property OnRegionEnd   : TNotifyEvent read fOnRegionEnd   write fOnRegionEnd;

    property OnOpcode : TOpcodeEvent read fOnOpcode write fOnOpcode;
  end;




implementation

uses
  Types,
  eeFunctions,
  SysUtils,
  StrUtils;

const
  kMaxInt = High(Integer);


function Occurrences(const Substring, Text: string): integer;
var
  offset: integer;
begin
  result := 0;
  offset := PosEx(Substring, Text, 1);
  while offset <> 0 do
  begin
    inc(result);
    offset := PosEx(Substring, Text, offset + length(Substring));
  end;
end;




{ TSfzParser }

constructor TSfzParser.Create;
begin
  OpcodeList := TObjectList.Create;
  OpcodeList.OwnsObjects := true;


  {
  AddTextOpcode('sample');
  AddIntegerOpcode('pitch_keycenter', 0, 127); //actual range is -127..127
  AddIntegerOpcode('lokey', 0, 127);
  AddIntegerOpcode('hikey', 0, 127);
  AddIntegerOpcode('key', 0, 127);
  AddIntegerOpcode('lovel', 0, 127);
  AddIntegerOpcode('hivel', 0, 127);
  AddTextOpcode('trigger'); //attack, release, first, legato,
  AddIntegerOpcode('group', 0, kMaxInt);
  AddTextOpcode('loop_mode');
  AddIntegerOpcode('loop_start', 0, kMaxInt); //should be unsigned.
  AddIntegerOpcode('loop_end', 0, kMaxInt);   //should be unsigned.

  AddFloatOpcode('volume', -144, 6); //db.
  AddFloatOpcode('pan', -100, 100); //%


  // Amplitude envelope
  AddFloatOpcode('ampeg_attack', 0, 100); //seconds
  AddFloatOpcode('ampeg_hold', 0, 100); //seconds
  AddFloatOpcode('ampeg_decay', 0, 100); //seconds
  AddFloatOpcode('ampeg_sustain', 0, 100); //seconds
  AddFloatOpcode('ampeg_release', 0, 100); //seconds

  // filter envelope
  AddFloatOpcode('fileg_attack', 0, 100); //seconds
  AddFloatOpcode('fileg_hold', 0, 100); //seconds
  AddFloatOpcode('fileg_decay', 0, 100); //seconds
  AddFloatOpcode('fileg_sustain', 0, 100); //seconds
  AddFloatOpcode('fileg_release', 0, 100); //seconds
  }
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
end;

procedure TSfzParser.ParseLine(s: string);
var
  TokenType : TSfzTokenType;
begin
  s := Trim(s);

  if s <> '' then
  begin
    TokenType := FindTokenType(s);

    case TokenType of
      ttUnknown:         ProcessUnknown(s);
      ttComment:         ProcessComment(s);
      ttRegion:          ProcessRegion(s);
      ttMultipleOpcodes: ProcessMultipleOpcodes(s);
      ttOpcode:          ProcessOpcode(s);
    else
      raise Exception.Create('Unexpected token type.');
    end;
  end;
end;

function TSfzParser.FindTokenType(s: string): TSfzTokenType;
var
  x : integer;
begin
  if StartsText('//', s) then exit(ttComment);

  if SameText('<region>', s) then exit(ttRegion);

  x := Occurrences('=', s);
  if x = 1 then exit(ttOpcode);
  if x > 1 then exit(ttMultipleOpcodes);

  //if we've made it this far we haven't found what token the string contains. :(
  result := ttUnknown;
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
    if FindTokenType(Lines[c1]) = ttMultipleOpcodes then
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
  Lines : TStringDynArray;
begin
  Lines := SplitString(s, '=');

  if Length(Lines) = 2 then
  begin
    OpcodeName  := Lines[0];
    OpcodeValue := Lines[1];
    if assigned(OnOpcode) then OnOpcode(self, OpcodeName, OpcodeValue);
  end;
end;

procedure TSfzParser.ProcessRegion(s: string);
begin
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
