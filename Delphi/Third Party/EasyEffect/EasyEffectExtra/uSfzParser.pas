unit uSfzParser;

interface

uses
  {$IFDEF VER230}
  RegularExpressionsCore,
  {$ELSE}
  PerlRegEx,
  {$ENDIF}
  Classes;

type
  TOpcodeEvent = procedure(Sender:TObject; const Opcode, Value:string) of object;

  TSfzToken = (stNil, stUnknown, stRegion, stGroup, stOpcode, stComment);

  TSfzParser = class
  private
    fOnStart: TNotifyEvent;
    fOnEnd: TNotifyEvent;
    fOnRegionStart: TNotifyEvent;
    fOnRegionEnd: TNotifyEvent;
    fOnOpcode: TOpcodeEvent;
    fOnGroupStart: TNotifyEvent;
    fOnGroupEnd: TNotifyEvent;
  protected
    RegEx    : TPerlRegEx;
    FileData : TStringList;
    LinePos, StringPos : integer;

    procedure LoadFileData(FileName:string);

    //Read next token and advance file read locators...
    procedure ReadNextToken(out TokenType:TSfzToken; out TokenText:string);

    //== Methods to call event handlers =================
    procedure FileStart;
    procedure FileEnd;
    procedure RegionStart;
    procedure RegionEnd;
    procedure GroupStart;
    procedure GroupEnd;
    procedure Opcode(const aOpcode, aValue:string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(Filename:string);

    property OnFileStart   : TNotifyEvent read fOnStart write fOnStart;              // Called when parsing of SFZ file begins.
    property OnFileEnd     : TNotifyEvent read fOnEnd   write fOnEnd;                // Called when parsing ends.

    property OnRegionStart : TNotifyEvent read fOnRegionStart write fOnRegionStart;  // called before parsing a new region
    property OnRegionEnd   : TNotifyEvent read fOnRegionEnd   write fOnRegionEnd;    // called after at the end of a region.

    property OnGroupStart  : TNotifyEvent read fOnGroupStart  write fOnGroupStart;   // called at the start and end of a 'group'
    property OnGroupEnd    : TNotifyEvent read fOnGroupEnd    write fOnGroupEnd;

    property OnOpcode      : TOpcodeEvent read fOnOpcode      write fOnOpcode;       // called for each region or group opcode. 
  end;

implementation

uses
  SysUtils;


function GetKeyName(KeyValuePair:string):string;
var
  SplitPos : integer;
begin
  SplitPos := Pos('=', KeyValuePair);

  if SplitPos > 1 then
  begin
    result := Copy(KeyValuePair, 1, SplitPos-1);
  end else
  begin
    result := '';
  end;

end;

function GetKeyValue(KeyValuePair:string):string;
var
  StrLen   : integer;
  SplitPos : integer;
begin
  StrLen   := Length(KeyValuePair);
  SplitPos := Pos('=', KeyValuePair);

  if SplitPos < StrLen then
  begin
    result := Copy(KeyValuePair, SplitPos+1, StrLen-SplitPos);
  end else
  begin
    result := '';
  end;           
end;



{ TSfzParser }

constructor TSfzParser.Create;
begin
  FileData := TStringList.Create;
  RegEx    := TPerlRegEx.Create;
end;

destructor TSfzParser.Destroy;
begin
  RegEx.Free;
  FileData.Free;
  inherited;
end;

procedure TSfzParser.FileEnd;
begin
  if assigned(OnFileEnd) then OnFileEnd(self);
end;

procedure TSfzParser.FileStart;
begin
  if assigned(OnFileStart) then OnFileStart(self);
end;

procedure TSfzParser.GroupEnd;
begin
  if assigned(OnGroupEnd) then OnGroupEnd(self);
end;

procedure TSfzParser.GroupStart;
begin
 if assigned(OnGroupStart) then OnGroupStart(self);
end;

procedure TSfzParser.Opcode(const aOpcode, aValue: string);
begin
  if assigned(OnOpcode) then OnOpcode(self, aOpcode, aValue);
end;

procedure TSfzParser.RegionEnd;
begin
  if assigned(OnRegionEnd) then OnRegionEnd(self);
end;

procedure TSfzParser.RegionStart;
begin
  if assigned(OnRegionStart) then OnRegionStart(self);
end;

procedure TSfzParser.LoadFileData(FileName: string);
begin
  FileData.Clear;
  FileData.LoadFromFile(FileName);

  //Reset read locators.
  LinePos   := 0; //First Line 
  StringPos := 1; //First Charactor
end;


procedure TSfzParser.ReadNextToken(out TokenType: TSfzToken; out TokenText: string);
var
  Text : string;
begin
  //Set output values to their defaults..
  TokenType := stNil;
  TokenText := '';

  while LinePos < FileData.Count do
  begin
    Text := FileData[LinePos];
    if StringPos < Length(Text) then
    begin
      RegEx.Subject := Text;
      RegEx.RegEx   := '/{2,2}|[^\s]+';
      RegEx.Start   := StringPos;

      if RegEx.MatchAgain then
      begin
        TokenText := RegEx.MatchedText;
        StringPos := RegEx.MatchedOffset + RegEx.MatchedLength;

        if TokenText = '//' then
        begin
          // Comment has been found, so:
          // #1: Return comment text
          TokenType := stComment;
          TokenText := '//' + Copy(Text, StringPos, Length(Text)-StringPos);
          // #2: Increment read locator to next line.
          inc(LinePos);
          StringPos := 1;
          exit; //=================================================>>
        end else
        if LowerCase(TokenText) = '<group>' then
        begin
          TokenType := stGroup;
          TokenText := '';
          exit; //=================================================>>
        end else
        if LowerCase(TokenText) = '<region>' then
        begin
          TokenType := stRegion;
          TokenText := '';
          exit; //=================================================>>
        end else
        if Pos('=',TokenText) <> -1 then
        begin
          TokenType := stOpcode;
          exit; //=================================================>>
        end else
        begin
          TokenType := stUnknown;
          exit; //=================================================>>
        end;
      end;

    end;

    // If the loop makes it this far, increment the LinePos and reset StringPos
    inc(LinePos);
    StringPos := 1;
  end;
end;



procedure TSfzParser.Parse(Filename: string);
var
  TokenType    : TSfzToken;
  TokenText    : string;
  IsRegionOpen : boolean;
  IsGroupOpen  : boolean;
  OpcodeName   : string;
  OpcodeValue  : string;
begin
  IsRegionOpen := false;
  IsGroupOpen  := false;

  LoadFileData(FileName);

  //=======================================
  FileStart;
  //=======================================

  ReadNextToken(Tokentype, TokenText);
  while TokenType <> stNil do
  begin
    case TokenType of
      stGroup:
      begin
        if IsGroupOpen then GroupEnd; //Call GroupEnd event handler...
        GroupStart;                   //Call GroupStart event handler...
        IsGroupOpen := true;
      end;

      stRegion:
      begin
        if IsRegionOpen then RegionEnd; //Call RegionEnd event handler...
        RegionStart;                    //Call RegionStart event handler...
        IsRegionOpen := true;
      end;

      stOpcode:
      begin
        OpcodeName  := GetKeyName(TokenText);
        OpcodeValue := GetKeyValue(TokenText);
        Opcode(OpcodeName, OpcodeValue);        //Call opcode event handler....
      end;
    end;

    ReadNextToken(Tokentype, TokenText);
  end;


  if IsGroupOpen then
  begin
    //IsGroupOpen := false;
    GroupEnd;
  end;

  if IsRegionOpen then
  begin
    //IsRegionOpen := false;
    RegionEnd;
  end;


  //=======================================
  FileEnd;
  //=======================================
end;


end.
