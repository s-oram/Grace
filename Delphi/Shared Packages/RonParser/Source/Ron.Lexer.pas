unit Ron.Lexer;

{
  This unit contains low level parsing functions.
}

{
  NOTES:

  TOML Spec
  https://github.com/toml-lang/toml

  Building a parser
  http://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python-part-1

  How to build a parser by hand.
  http://jayconrod.com/posts/65/how-to-build-a-parser-by-hand

  http://onoffswitch.net/building-a-custom-lexer/

  http://www.cse.chalmers.se/edu/course/DAT150/lectures/proglang-04.html


  // PCRE Regex Tester:
  // https://regex101.com/

  // Go Regex Tester:
  // https://regex-golang.appspot.com/

}

interface

uses
  SysUtils;

const
  // NOTE: from http://stackoverflow.com/a/254997/395461
  EndOfLine = {$IFDEF LINUX}     AnsiChar(#10)      {$ENDIF}
              {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};

type
  TTokenType = (
    ttComment,
    ttWhiteSpace,
    ttMultiObjectOpen,
    ttObjectOpen,
    ttObjectClose,
    ttObjectKey,
    ttIntegerValue,
    ttFloatValue,
    ttBooleanValue,
    ttUntypedValue,
    ttBasicString,
    ttMultiLineString,
    ttLiteralString,
    ttMultiLineLiteralString,
    ttSymbol,
    ttEndOfFile,
    ttUndefined
  );

  TToken = record
    Position  : integer;
    TokenType : TTokenType;
    TokenData : string;
  end;

  TTokenizer = record
  private
    const rex_EndOfValue = '[\h]*(?:[;,\)\]]|[\v]|$)'; //Values end with optional whitespace, followed by a semi-colon, comma, or closing bracket.
    const rex_MultiObjectOpen = '((?:[a-zA-Z]+\w*)(?:\.[a-zA-Z]+\w*)*(?:\:\:[a-zA-Z]+\w*)?)(,[\s]*)[\w\.\:,\s]*{';
    const rex_ObjectOpen = '((?:[a-zA-Z]+\w*)(?:\.[a-zA-Z]+\w*)*(?:\:\:[a-zA-Z]+\w*)?)[\h]*{';
  public
    // The match functions are used to locate tokens. If a match is found,
    // - MatchedText will contain the value of the token.
    // - PosIncrement is the distance to the next token. It will often, but not always be eaual to length(MatchedText).
    // You may not need to call any of the match functions directly. Their behaviour has been encapsulated in the NextToken() function below.
    class function Match_Comment(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_MultiLineComment(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_WhiteSpace(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_MultiObjectOpen(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_ObjectOpen(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_ObjectClose(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_ObjectKey(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_IntegerValue(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_FloatValue(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_BooleanValue(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_UntypedValue(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_BasicString(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_MultiLineString(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_LiteralString(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_MultiLineLiteralString(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;
    class function Match_Symbol(const Text : string; const StartPos : integer; out MatchedText : string; out PosIncrement : integer):boolean; static;

    // Use NextToken() to advance through a text, matching tokens one-by-one.
    class function NextToken(const Text : string;  var IndexPos : integer; out Token : TToken): boolean; static;
  end;


  // NOTE: This syntax checker works but it's a bit messy. There's probably a better way.
  // Right now the syntax is keeping track of the document context by storing
  // a bunch of different state variables.
  TSyntaxChecker = record
  private
    type TArrayType = (atNotSet, atUntyped, atInteger, atFloat, atBoolean, atString);
  private
    // State variables...
    IsStartOfFile : boolean;
    IsValueNeeded : boolean;
    IsArrayOpen : boolean;
    IsMultiGroupOpening : boolean;
    IsMultiGroupOpen    : boolean;
    ObjectDepth : integer;
    ActiveArrayType : TArrayType;

    procedure _Comment(const Token : TToken);
    procedure _WhiteSpace(const Token : TToken);
    procedure _MultiObjectOpen(const Token : TToken);
    procedure _ObjectOpen(const Token : TToken);
    procedure _ObjectClose(const Token : TToken);
    procedure _ObjectKey(const Token : TToken);
    procedure _IntegerValue(const Token : TToken);
    procedure _FloatValue(const Token : TToken);
    procedure _BooleanValue(const Token : TToken);
    procedure _UntypedValue(const Token : TToken);
    procedure _BasicString(const Token : TToken);
    procedure _MultiLineString(const Token : TToken);
    procedure _LiteralString(const Token : TToken);
    procedure _MultiLineLiteralString(const Token : TToken);
    procedure _Symbol(const Token : TToken);
    procedure _EndOfFile(const Token : TToken);
    procedure _Undefined(const Token : TToken);
  public
    procedure ResetState; //Call before scanning a new document.
    function IsSyntaxValid(const Token : TToken; out ErrorMsg : string):boolean;
  end;

  ERonParserExcpetion = class(Exception);
  ERonSyntaxException = class(ERonParserExcpetion);


type
  TRonString = record
  public
    // Ron strings use the same formating and escape sequences as TOML v0.4.0
    class function UnescapeBasicString(str : string):string; static;
    class function UnescapeMultilineString(str : string):string; static;
    class function UnescapeLiteralString(str : string):string; static;
    class function UnescapeMultilineLiteralString(str : string):string; static;
  end;

  TRonHelper = record
  public
    class function TokenToString(const Token : TToken):string; static;
    class function TokenToFloat(const Token : TToken):single; static;
    class function TokenToInteger(const Token : TToken):integer; static;
    class function TokenToBoolean(const Token : TToken):boolean; static;

    // Extract the object path from a Ron object name.
    // For example: GetRonObjectPath("Panel1.Knob1.Label:TVstLabel") = "Panel1.Knob1.Label"
    class function GetRonObjectPath(const ObjectName : string):string; static;

    // Extract the object type from a Ron object name.
    // For example: GetRonObjectType("Panel1.Knob1.Label:TVstLabel") = "TVstLabel"
    class function GetRonObjectType(const ObjectName : string):string; static;
  end;

implementation

uses
  RegularExpressions,
  StrUtils;

{ TTokenizer }

class function TTokenizer.Match_Comment(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('#.*\n?');
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_MultiLineComment(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('\/\*.*?\*\/', [roSingleLine]);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_MultiObjectOpen(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_MultiObjectOpen);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Groups[1].Length + mr.Groups[2].Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_ObjectOpen(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_ObjectOpen);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_ObjectClose(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('}');
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := '';
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_WhiteSpace(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('[\s]+');
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := '';
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_ObjectKey(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('((?:[a-zA-Z]+\w*))[\h]*:');
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_IntegerValue(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
const
  rex_IntegerValue = '(?<=^|\W)((?:[-+]?0)|(?:[-+]?[1-9][0-9]*))';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_IntegerValue + rex_EndOfValue);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Groups[1].Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_FloatValue(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
const
  rex_FloatValue = '([-+]?[0-9]+\.[0-9]+)';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_FloatValue + rex_EndOfValue);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Groups[1].Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_BooleanValue(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
const
  rex_BooleanValue = '(?i)((?:true)|(?:false))';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_BooleanValue + rex_EndOfValue);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Groups[1].Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_UntypedValue(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
const
  rex_UntypedValue = '([a-zA-Z_][\w]*)';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_UntypedValue + rex_EndOfValue);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Groups[1].Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_BasicString(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('"([^"](?:\\"|(?!").)+[^"])"');
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;



class function TTokenizer.Match_MultiLineString(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('"""((?>\s|.)*?)"""');
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_LiteralString(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
const
  _LiteralString : string = '(?<!'')''([^''].*?[^''])''(?:[^'']|$)';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(_LiteralString);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;

class function TTokenizer.Match_MultiLineLiteralString(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
const
  _MultiLineLiteralString : string = '''{3}(.*?)''{3}';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(_MultiLineLiteralString);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;


class function TTokenizer.Match_Symbol(const Text: string; const StartPos: integer; out MatchedText: string; out PosIncrement: integer): boolean;
const
  _ValidSymbols : string = '([=;,\[\]])';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(_ValidSymbols);
  mr := rex.Match(Text, StartPos);
  if (mr.Success) and (mr.Index = StartPos) then
  begin
    MatchedText  := mr.Groups[1].Value;
    PosIncrement := mr.Groups[1].Length;
    result := true;
  end else
  begin
    result := false;
  end;
end;


class function TTokenizer.NextToken(const Text: string; var IndexPos: integer; out Token: TToken): boolean;
label
  SkipTokenCheck; //Let's use GOTOs!!
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  if IndexPos <= 0 then IndexPos := 1;


  //==== Skip Token Check ====
  SkipTokenCheck:
  begin
    if (IndexPos < Length(Text)) and (Match_WhiteSpace(Text, IndexPos, MatchedText, PosIncrement)) then
    begin
      inc(IndexPos, PosIncrement);
      GOTO SkipTokenCheck;  //------- GOTO --->
    end;

    if (IndexPos < Length(Text)) and (Match_Symbol(Text, IndexPos, MatchedText, PosIncrement)) and (MatchedText = ';') then
    begin
      inc(IndexPos, PosIncrement);
      GOTO SkipTokenCheck;  //------- GOTO --->
    end;
  end;


  // Set token position after skipping whitespace.
  Token.Position := IndexPos;


  //==== End of file check ====
  if IndexPos = Length(Text)+1 then
  begin
    Token.TokenType := TTokenType.ttEndOfFile;
    Token.TokenData := '';
    inc(IndexPos);
    exit(true);
  end else if IndexPos > Length(Text)+1 then
  begin
    Token.TokenType := TTokenType.ttEndOfFile;
    Token.TokenData := '';
    inc(IndexPos);
    exit(false);
  end;

  if Match_Comment(Text, IndexPos, MatchedText, PosIncrement)                then Token.TokenType := TTokenType.ttComment         else
  if Match_MultiLineComment(Text, IndexPos, MatchedText, PosIncrement)       then Token.TokenType := TTokenType.ttComment         else
  if Match_MultiObjectOpen(Text, IndexPos, MatchedText, PosIncrement)        then Token.TokenType := TTokenType.ttMultiObjectOpen else
  if Match_ObjectOpen(Text, IndexPos, MatchedText, PosIncrement)             then Token.TokenType := TTokenType.ttObjectOpen      else
  if Match_ObjectClose(Text, IndexPos, MatchedText, PosIncrement)            then Token.TokenType := TTokenType.ttObjectClose     else
  if Match_ObjectKey(Text, IndexPos, MatchedText, PosIncrement)              then Token.TokenType := TTokenType.ttObjectKey       else
  if Match_IntegerValue(Text, IndexPos, MatchedText, PosIncrement)           then Token.TokenType := TTokenType.ttIntegerValue    else
  if Match_FloatValue(Text, IndexPos, MatchedText, PosIncrement)             then Token.TokenType := TTokenType.ttFloatValue      else
  if Match_BooleanValue(Text, IndexPos, MatchedText, PosIncrement)           then Token.TokenType := TTokenType.ttBooleanValue    else
  if Match_UntypedValue(Text, IndexPos, MatchedText, PosIncrement)           then Token.TokenType := TTokenType.ttUntypedValue    else
  if Match_BasicString(Text, IndexPos, MatchedText, PosIncrement)            then Token.TokenType := TTokenType.ttBasicString     else
  if Match_MultiLineString(Text, IndexPos, MatchedText, PosIncrement)        then Token.TokenType := TTokenType.ttMultiLineString else
  if Match_LiteralString(Text, IndexPos, MatchedText, PosIncrement)          then Token.TokenType := TTokenType.ttLiteralString   else
  if Match_MultiLineLiteralString(Text, IndexPos, MatchedText, PosIncrement) then Token.TokenType := TTokenType.ttLiteralString   else
  if Match_Symbol(Text, IndexPos, MatchedText, PosIncrement)                 then Token.TokenType := TTokenType.ttSymbol          else
  begin
    // If we make it here, the next token can't be identified. It might be a syntax error.
    Token.TokenType := TTokenType.ttUndefined;
    Token.TokenData := '';    //It would be good to get the line of text at the current position, to serve as an error report.
    // Increment IndexPos so the NextToken() function won't get stuck in an infiniate loop if called again.
    IndexPos := Length(Text)+2;
    exit(true);
  end;

  Token.TokenData := MatchedText;
  inc(IndexPos, PosIncrement);
  result := true;
end;




{ TSyntaxChecker }

procedure TSyntaxChecker.ResetState;
begin
  IsStartOfFile := true;
  IsValueNeeded := false;
  IsArrayOpen := false;

  IsMultiGroupOpening := false;
  IsMultiGroupOpen    := false;

  ObjectDepth := 0;

  ActiveArrayType := atNotSet;
end;

procedure TSyntaxChecker._Undefined(const Token: TToken);
begin
  raise ERonSyntaxException.Create('Undefined value.');
end;

procedure TSyntaxChecker._WhiteSpace(const Token: TToken);
begin
  // do nothing. White space should always be acceptable.
end;

procedure TSyntaxChecker._Comment(const Token: TToken);
begin
  if IsValueNeeded then raise ERonSyntaxException.Create('Value expected but found comment.');
end;

procedure TSyntaxChecker._MultiObjectOpen(const Token: TToken);
begin
  if IsValueNeeded then raise ERonSyntaxException.Create('Value expected. Multi-object tag start found.');

  if IsMultiGroupOpen then raise ERonSyntaxException.Create('Multi-object definitions can not contain child objects.');

  IsMultiGroupOpening := true;
end;

procedure TSyntaxChecker._ObjectOpen(const Token: TToken);
begin
  if IsValueNeeded then raise ERonSyntaxException.Create('Value expected but found Object tag.');

  if IsMultiGroupOpen then raise ERonSyntaxException.Create('Multi-object definitions can not contain child objects.');

  if IsMultiGroupOpening then
  begin
    IsMultiGroupOpen := true;
  end;

  inc(ObjectDepth);
end;

procedure TSyntaxChecker._ObjectClose(const Token: TToken);
begin
  if IsValueNeeded then raise ERonSyntaxException.Create('Value expected but found Object close tag.');
  if ObjectDepth <= 0 then raise ERonSyntaxException.Create('Unexpected object close tag.');

  if IsMultiGroupOpen then
  begin
    IsMultiGroupOpening := false;
    IsMultiGroupOpen := false;
  end;

  dec(ObjectDepth);
end;

procedure TSyntaxChecker._ObjectKey(const Token: TToken);
begin
  if IsValueNeeded then raise ERonSyntaxException.Create('Value expected but found Object tag.');

  // A value will be expected after an object key.
  IsValueNeeded := true;
end;

procedure TSyntaxChecker._IntegerValue(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected integer value.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atInteger) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atInteger;
  end;

end;

procedure TSyntaxChecker._FloatValue(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected float value.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atFloat) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atFloat;
  end;
end;

procedure TSyntaxChecker._BooleanValue(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected boolean value.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atBoolean) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atBoolean;
  end;
end;

procedure TSyntaxChecker._UntypedValue(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected value.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atUntyped) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atUntyped;
  end;
end;

procedure TSyntaxChecker._BasicString(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected string.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atString) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atString;
  end;
end;

procedure TSyntaxChecker._MultiLineString(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected string.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atString) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atString;
  end;
end;

procedure TSyntaxChecker._LiteralString(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected string.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atString) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atString;
  end;
end;

procedure TSyntaxChecker._MultiLineLiteralString(const Token: TToken);
begin
  if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected string.');

  if (IsArrayOpen) and (ActiveArrayType <> atNotSet) and (ActiveArrayType <> atString) then raise ERonSyntaxException.Create('Mixed type arrays are not allowed.');

  IsValueNeeded := false;

  if (IsArrayOpen) and (ActiveArrayType = atNotSet) then
  begin
    ActiveArrayType := atString;
  end;
end;

procedure TSyntaxChecker._Symbol(const Token: TToken);
begin
  if Token.TokenData = '[' then
  begin
    if not IsValueNeeded then raise ERonSyntaxException.Create('Unexpected "[" symbol found.');
    if IsArrayOpen then raise ERonSyntaxException.Create('Unexpected "[" symbol found.');
    IsValueNeeded := true;
    IsArrayOpen := true;
    ActiveArrayType := atNotSet;
  end;

  if Token.TokenData = ']' then
  begin
    if not IsArrayOpen then raise ERonSyntaxException.Create('Unexpected "]" symbol found.');
    IsArrayOpen := false;
    IsValueNeeded := false;
    ActiveArrayType := atNotSet;
  end;

  if Token.TokenData = ',' then
  begin
    if not IsArrayOpen then raise ERonSyntaxException.Create('Unexpected "," symbol found.');
    if IsValueNeeded then raise ERonSyntaxException.Create('Value expected but "," symbol found.');
    IsValueNeeded := true;
  end;


end;

procedure TSyntaxChecker._EndOfFile(const Token: TToken);
begin
  if IsArrayOpen then raise ERonSyntaxException.Create('Expected "]" but found end of file.');
  if IsValueNeeded then raise ERonSyntaxException.Create('Expected value but found end of file.');
  if ObjectDepth > 0 then raise ERonSyntaxException.Create('Expected "}" but found end of file');
end;


function TSyntaxChecker.IsSyntaxValid(const Token: TToken; out ErrorMsg : string): boolean;
begin
  try
    case Token.TokenType of
      ttComment:                  _Comment(Token);
      ttWhiteSpace:               _WhiteSpace(Token);
      ttMultiObjectOpen:          _MultiObjectOpen(Token);
      ttObjectOpen:               _ObjectOpen(Token);
      ttObjectClose:              _ObjectClose(Token);
      ttObjectKey:                _ObjectKey(Token);
      ttIntegerValue:             _IntegerValue(Token);
      ttFloatValue:               _FloatValue(Token);
      ttBooleanValue:             _BooleanValue(Token);
      ttUntypedValue:             _UntypedValue(Token);
      ttBasicString:              _BasicString(Token);
      ttMultiLineString:          _MultiLineString(Token);
      ttLiteralString:            _LiteralString(Token);
      ttMultiLineLiteralString:   _MultiLineLiteralString(Token);
      ttSymbol:                   _Symbol(Token);
      ttEndOfFile:                _EndOfFile(Token);
      ttUndefined:                _Undefined(Token);
    else
      raise ERonParserExcpetion.Create('Syntax check for this token not implemented.');
    end;

    result := true;
  except
    on E:ERonSyntaxException do
    begin
      ErrorMsg := E.Message;
      exit(false); //=======================>> exit =============>>
    end else raise;
  end;
end;




{ TRonString }

class function TRonString.UnescapeBasicString(str: string): string;
var
  rex : TRegEx;
begin
  // Basic strings are surrounded by quotation marks. Any Unicode character
  // may be used except those that must be escaped: quotation mark, backslash,
  // and the control characters (U+0000 to U+001F).
  //
  // str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."
  //
  // For convenience, some popular characters have a compact escape sequence.
  //   \b         - backspace       (U+0008)
  //   \t         - tab             (U+0009)
  //   \n         - linefeed        (U+000A)
  //   \f         - form feed       (U+000C)
  //   \r         - carriage return (U+000D)
  //   \"         - quote           (U+0022)
  //   \\         - backslash       (U+005C)
  //   \uXXXX     - unicode         (U+XXXX)
  //   \UXXXXXXXX - unicode         (U+XXXXXXXX)
  //
  // Any Unicode character may be escaped with the \uXXXX or \UXXXXXXXX forms.
  // The escape codes must be valid Unicode scalar values.
  //
  // All other escape sequences not listed above are reserved and, if used, TOML should produce an error.


  //   \b - backspace       (U+0008)
  rex := TRegEx.Create('\\b', [roIgnoreCase]);
  str := rex.Replace(str, Char(StrToInt('$0008')));

  //   \t - tab             (U+0009)
  rex := TRegEx.Create('\\t', [roIgnoreCase]);
  str := rex.Replace(str, Char(StrToInt('$0009')));

  //   \n - linefeed        (U+000A)
  rex := TRegEx.Create('\\n', [roIgnoreCase]);
  str := rex.Replace(str, Char(StrToInt('$000A')));

  //   \f - form feed       (U+000C)
  rex := TRegEx.Create('\\f', [roIgnoreCase]);
  str := rex.Replace(str, Char(StrToInt('$000C')));

  //   \r - carriage return (U+000D)
  rex := TRegEx.Create('\\r', [roIgnoreCase]);
  str := rex.Replace(str, Char(StrToInt('$000D')));

  //   \" - quote           (U+0022)
  rex := TRegEx.Create('\\"', [roIgnoreCase]);
  str := rex.Replace(str, Char(StrToInt('$0022')));

  //   \\ - backslash       (U+005C)
  rex := TRegEx.Create('\\', [roIgnoreCase]);
  str := rex.Replace(str, Char(StrToInt('$005C')));

  // TODO:MED Need to do unicode replacements.
  //   \uXXXX - unicode      (U+XXXX)
  //rex := TRegEx.Create('\\b');
  //str := rex.Replace(str, StrToInt('$0008'));

  //   \UXXXXXXXX - unicode         (U+XXXXXXXX)
  //rex := TRegEx.Create('\\b');
  //str := rex.Replace(str, StrToInt('$0008'));

  result := str;
end;

class function TRonString.UnescapeMultilineString(str: string): string;
var
  rex : TRegEx;
begin
  // Sometimes you need to express passages of text (e.g. translation files)
  // or would like to break up a very long string into multiple lines. TOML
  // makes this easy. Multi-line basic strings are surrounded by three quotation
  // marks on each side and allow newlines. A newline immediately following the
  // opening delimiter will be trimmed. All other whitespace and newline characters
  // remain intact.
  //
  //      str1 = """
  //      Roses are red
  //      Violets are blue"""
  //
  // For writing long strings without introducing extraneous whitespace, end a line
  // with a \. The \ will be trimmed along with all whitespace (including newlines)
  // up to the next non-whitespace character or closing delimiter. If the first
  // characters after the opening delimiter are a backslash and a newline, then they
  // will both be trimmed along with all whitespace and newlines up to the next
  // non-whitespace character or closing delimiter. All of the escape sequences that
  // are valid for basic strings are also valid for multi-line basic strings.
  //
  //      # The following strings are byte-for-byte equivalent:
  //      str1 = "The quick brown fox jumps over the lazy dog."
  //
  //      str2 = """
  //      The quick brown \
  //
  //
  //        fox jumps over \
  //          the lazy dog."""
  //
  //      key3 = """\
  //             The quick brown \
  //             fox jumps over \
  //             the lazy dog.\
  //             """
  //


  // replace the initial line feed.
  rex := TRegEx.Create('^[\h]*[\v]');
  str := rex.Replace(str, '');
  result := str;

  // trim white space delimited with trailing backslashes.
  rex := TRegEx.Create('\\[\h]*(' + EndOfLine + '|[\v])[\h]*');
  str := rex.Replace(str, '');
  result := str;

  result := TRonString.UnescapeBasicString(str);
end;

class function TRonString.UnescapeLiteralString(str: string): string;
begin
  // If you're a frequent specifier of Windows paths or regular expressions,
  // then having to escape backslashes quickly becomes tedious and error prone.
  // To help, TOML supports literal strings where there is no escaping allowed
  // at all. Literal strings are surrounded by single quotes. Like basic strings,
  // they must appear on a single line:

  result := str;
end;

class function TRonString.UnescapeMultilineLiteralString(str: string): string;
var
  rex : TRegEx;
begin
  // Since there is no escaping, there is no way to write a single quote inside a
  // literal string enclosed by single quotes. Luckily, TOML supports a multi-line
  // version of literal strings that solves this problem. Multi-line literal strings
  // are surrounded by three single quotes on each side and allow newlines. Like
  // literal strings, there is no escaping whatsoever. A newline immediately following
  // the opening delimiter will be trimmed. All other content between the delimiters
  // is interpreted as-is without modification.
  //
  //      regex2 = '''I [dw]on't need \d{2} apples'''
  //      lines  = '''
  //      The first newline is
  //      trimmed in raw strings.
  //         All other whitespace
  //         is preserved.
  //      '''
  //

  // replace the initial line feed.
  rex := TRegEx.Create('^[\h]*[\v]');
  str := rex.Replace(str, '');
  result := str;
end;



{ TRonHelper }

class function TRonHelper.GetRonObjectPath(const ObjectName: string): string;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('^([a-zA-Z][\w.]*[^.])(?:::|$|\s)');
  mr := rex.Match(ObjectName);
  if (mr.Success)
    then result := mr.Groups[1].Value
    else result := '';
end;

class function TRonHelper.GetRonObjectType(const ObjectName: string): string;
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create('^(?:[a-zA-Z][\w.]*[^.])?::([a-zA-Z][\w.]*)(?:|$|\s)');
  mr := rex.Match(ObjectName);
  if (mr.Success)
    then result := mr.Groups[1].Value
    else result := '';
end;

class function TRonHelper.TokenToBoolean(const Token: TToken): boolean;
begin
  case Token.TokenType of
    ttBooleanValue:
    begin
      if Lowercase(Token.TokenData) = 'true'
        then result := true
        else result := false;
    end;
  else
    raise ERonParserExcpetion.Create('Cannot convert this token to a boolean.');
  end;
end;

class function TRonHelper.TokenToFloat(const Token: TToken): single;
var
  fs : TFormatSettings;
begin
  // NOTE: We don't use any error handling here because it's assumed only valid float strings
  // will be passed to this function. Invalid float strings should have been
  // weeded out by the syntax checker.
  fs := TFormatSettings.Create(1033); // NOTE: Use US locale for float format settings. https://msdn.microsoft.com/en-us/goglobal/bb895996.aspx
  case Token.TokenType of
    ttFloatValue: result := StrToFloat(Token.TokenData, fs);
  else
   raise ERonParserExcpetion.Create('Cannot convert this token to a float.');
  end;
end;

class function TRonHelper.TokenToInteger(const Token: TToken): integer;
begin
  // NOTE: We don't use any error handling here because it's assumed only valid integer strings
  // will be passed to this function. Invalid integer strings should have been
  // weeded out by the syntax checker.
  case Token.TokenType of
    ttIntegerValue: result := StrToInt(Token.TokenData);
  else
    raise ERonParserExcpetion.Create('Cannot convert this token to a integer.');
  end;
end;

class function TRonHelper.TokenToString(const Token: TToken): string;
begin
 case Token.TokenType of
   ttUntypedValue:           result := Token.TokenData;
   ttBasicString:            result := TRonString.UnescapeBasicString(Token.TokenData);
   ttMultiLineString:        result := TRonString.UnescapeMultilineString(Token.TokenData);
   ttLiteralString:          result := TRonString.UnescapeLiteralString(Token.TokenData);
   ttMultiLineLiteralString: result := TRonString.UnescapeMultilineLiteralString(Token.TokenData);
 else
   raise ERonParserExcpetion.Create('Cannot convert this token to a string.');
 end;
end;

end.
