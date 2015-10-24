unit Ron.Parser;

interface

{
  This unit contains wraps the low level parsing functions into a nice, easy to use class.
}

uses
  SysUtils,
  Classes,
  Ron.Lexer;

type
  TParserValueType = (
    pvNoValue,
    pvInteger,
    pvFloat,
    pvBoolean,
    pvUntyped,
    pvString,
    pvEmptyArray,
    pvIntegerArray,
    pvFloatArray,
    pvBooleanArray,
    pvUntypedArray,
    pvStringArray
  );

  TParserValue = class;

  PParserSyntaxError = ^TParserSyntaxError;
  TParserSyntaxError = record
    ErrorMsg : string;
    ErrorLineNumber   : integer;
    ErrorCharPosition : integer;
  end;

  TOnValueEvent = reference to procedure(Sender : TObject; const ParserValue : TParserValue);

  TParser = class
  private
    FLastSyntaxError: PParserSyntaxError;
    FOnValue: TOnValueEvent;

    ObjectPaths : TStringList;
    ObjectTypes : TStringList;
    MultiObjects : TStringList;

    IsMultiGroupOpening : boolean;
    IsMultiGroupOpen    : boolean;

    CurrentValue : TParserValue;
    FOnObjectOpen: TOnValueEvent;

    //(const Text: string; var IndexPos: integer; out Token: TToken)
    procedure Parse_MultiObjectOpen(const Text: string; const Token: TToken);
    procedure Parse_ObjectOpen(const Text: string; const Token: TToken);
    procedure Parse_ObjectClose(const Text: string; const Token: TToken);
    procedure Parse_ObjectKey(const Text: string; const Token: TToken);

    procedure TriggerOnObjectOpenEvent(const value : TParserValue);
    procedure TriggerOnValueEvent(const value : TParserValue);

    function GetCombinedObjectPath:string;
  public
    constructor Create;
    destructor Destroy; override;

    // The Parse..() functions return true if parsing was successful.
    // If false, check the LastSyntaxError property to find out what went wrong.
    function ParseText(const Text : string):boolean;
    function ParseFile(const FileName : string):boolean;

    property LastSyntaxError : PParserSyntaxError read FLastSyntaxError;

    property OnObjectOpen : TOnValueEvent read FOnObjectOpen write FOnObjectOpen;
    property OnValue : TOnValueEvent read FOnValue write FOnValue;
  end;

  TParserValue = class
  private
    fKeyName: string;
    FValueType: TParserValueType;
    FArrayLength: integer;

    vInteger : integer;
    vFloat   : single;
    vBoolean : boolean;
    vString  : string;

    vIntegerArray : array of integer;
    vFloatArray   : array of single;
    vBooleanArray : array of boolean;
    vStringArray  : array of string;
    fObjectType: string;
    fObjectPath: string;
    function GetObjectName: string;
  protected
    procedure SetValueType(const aValueType: TParserValueType; const aArrayLength : integer);

    procedure SetValueAsInteger(const Value: integer);
    procedure SetValueAsFloat(const Value: single);
    procedure SetValueAsBoolean(const Value: boolean);
    procedure SetValueAsUntyped(const Value: string);
    procedure SetValueAsString(const Value: string);

    procedure SetValueAsIntegerArray(const Value: array of integer);
    procedure SetValueAsFloatArray(const Value: array of single);
    procedure SetValueAsBooleanArray(const Value: array of boolean);
    procedure SetValueAsUntypedArray(const Value: array of string);
    procedure SetValueAsStringArray(const Value: array of string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property ObjectName : string read GetObjectName;
    property ObjectPath : string read fObjectPath;  // Must be set.
    property ObjectType : string read fObjectType;  // optional.
    property KeyName    : string read fKeyName;
    property ValueType  : TParserValueType read FValueType;

    function ValueAsInteger : integer; overload;
    function ValueAsFloat   : single; overload;
    function ValueAsBoolean : boolean; overload;
    function ValueAsUntyped : string; overload;
    function ValueAsString  : string; overload;

    function ValueAsInteger (const Index :integer): integer; overload;
    function ValueAsFloat   (const Index :integer): single; overload;
    function ValueAsBoolean (const Index :integer): boolean; overload;
    function ValueAsUntyped (const Index :integer): string; overload;
    function ValueAsString  (const Index :integer): string; overload;

    property ArrayLength : integer read FArrayLength;
  end;



  TParserHelper = record
  private
    class procedure GetRonValueArrayProperties(const Text : string; const ArrayStartPos : integer; out ArrayLength : integer; out ArrayType : TParserValueType); static;
    class procedure ReadIntegerArray(const Text : string; IndexPos : integer; var arr : array of integer); static;
    class procedure ReadFloatArray(const Text : string; IndexPos : integer; var arr : array of single); static;
    class procedure ReadBooleanArray(const Text : string; IndexPos : integer; var arr : array of boolean); static;
    class procedure ReadUntypedArray(const Text : string; IndexPos : integer; var arr : array of string); static;
    class procedure ReadStringArray(const Text : string; IndexPos : integer; var arr : array of string); static;
  public
    class procedure ReadRonValue(const Text : string; IndexPos : integer; var Output : TParserValue); static;
  end;

implementation


{ TParserValue }

constructor TParserValue.Create;
begin
  SetValueType(pvNoValue, 0);
end;

destructor TParserValue.Destroy;
begin
  SetLength(vIntegerArray, 0);
  SetLength(vFloatArray, 0);
  SetLength(vBooleanArray, 0);
  SetLength(vStringArray, 0);
  inherited;
end;

procedure TParserValue.Clear;
begin
  FObjectPath := '';
  FObjectType := '';
  FKeyName    := '';

  vInteger := 0;
  vFloat   := 0;
  vBoolean := false;
  vString  := '';

  SetValueType(pvNoValue, 0);
end;

function TParserValue.GetObjectName: string;
begin
  assert(fObjectPath <> '');
  if fObjectType <> ''
    then result := fObjectPath + ':' + fObjectType
    else result := fObjectPath;
end;

procedure TParserValue.SetValueAsBoolean(const Value: boolean);
begin
  SetValueType(TParserValueType.pvBoolean, 0);
  vBoolean := Value;
end;

procedure TParserValue.SetValueAsBooleanArray(const Value: array of boolean);
var
  c1: Integer;
begin
  SetValueType(TParserValueType.pvBooleanArray, Length(Value));

  for c1 := 0 to Length(Value)-1 do
  begin
    vBooleanArray[c1] := Value[c1];
  end;
end;

procedure TParserValue.SetValueAsFloat(const Value: single);
begin
  SetValueType(TParserValueType.pvFloat, 0);
  vFloat := Value;
end;

procedure TParserValue.SetValueAsFloatArray(const Value: array of single);
var
  c1: Integer;
begin
  SetValueType(TParserValueType.pvFloatArray, Length(Value));

  for c1 := 0 to Length(Value)-1 do
  begin
    vFloatArray[c1] := Value[c1];
  end;
end;

procedure TParserValue.SetValueAsInteger(const Value: integer);
begin
  SetValueType(TParserValueType.pvInteger, 0);
  vInteger := Value;
end;

procedure TParserValue.SetValueAsIntegerArray(const Value: array of integer);
var
  c1: Integer;
begin
  SetValueType(TParserValueType.pvIntegerArray, Length(Value));

  for c1 := 0 to Length(Value)-1 do
  begin
    vIntegerArray[c1] := Value[c1];
  end;
end;

procedure TParserValue.SetValueAsString(const Value: string);
begin
  SetValueType(TParserValueType.pvString, 0);
  vString := Value;
end;

procedure TParserValue.SetValueAsStringArray(const Value: array of string);
var
  c1: Integer;
begin
  SetValueType(TParserValueType.pvStringArray, Length(Value));

  for c1 := 0 to Length(Value)-1 do
  begin
    vStringArray[c1] := Value[c1];
  end;
end;

procedure TParserValue.SetValueAsUntyped(const Value: string);
begin
  SetValueType(TParserValueType.pvUntyped, 0);
  vString := Value;
end;

procedure TParserValue.SetValueAsUntypedArray(const Value: array of string);
var
  c1: Integer;
begin
  SetValueType(TParserValueType.pvUntypedArray, Length(Value));

  for c1 := 0 to Length(Value)-1 do
  begin
    vStringArray[c1] := Value[c1];
  end;
end;

procedure TParserValue.SetValueType(const aValueType: TParserValueType; const aArrayLength : integer);
begin
  FValueType := aValueType;
  FArrayLength := aArrayLength;

  SetLength(vIntegerArray, 0);
  SetLength(vFloatArray, 0);
  SetLength(vBooleanArray, 0);
  SetLength(vStringArray, 0);

  case aValueType of
    pvIntegerArray: SetLength(vIntegerArray, aArrayLength);
    pvFloatArray:   SetLength(vFloatArray,   aArrayLength);
    pvBooleanArray: SetLength(vBooleanArray, aArrayLength);
    pvUntypedArray: SetLength(vStringArray,  aArrayLength);
    pvStringArray:  SetLength(vStringArray,  aArrayLength);
  end;
end;

function TParserValue.ValueAsBoolean: boolean;
begin
  assert(self.ValueType = TParserValueType.pvBoolean);
  result := vBoolean;
end;

function TParserValue.ValueAsBoolean(const Index: integer): boolean;
begin
  assert(self.ValueType = TParserValueType.pvBooleanArray);
  result := vBooleanArray[Index];
end;

function TParserValue.ValueAsFloat: single;
begin
  assert(self.ValueType = TParserValueType.pvFloat);
  result := vFloat;
end;

function TParserValue.ValueAsFloat(const Index: integer): single;
begin
  assert(self.ValueType = TParserValueType.pvFloatArray);
  result := vFloatArray[Index];
end;

function TParserValue.ValueAsInteger(const Index: integer): integer;
begin
  assert(self.ValueType = TParserValueType.pvIntegerArray);
  result := vIntegerArray[Index];
end;

function TParserValue.ValueAsInteger: integer;
begin
  assert(self.ValueType = TParserValueType.pvInteger);
  result := vInteger;
end;

function TParserValue.ValueAsString(const Index: integer): string;
begin
  assert(self.ValueType = TParserValueType.pvStringArray);
  result := vStringArray[Index];
end;

function TParserValue.ValueAsString: string;
begin
  assert(self.ValueType = TParserValueType.pvString);
  result := vString;
end;

function TParserValue.ValueAsUntyped: string;
begin
  assert(self.ValueType = TParserValueType.pvUntyped);
  result := vString;
end;

function TParserValue.ValueAsUntyped(const Index: integer): string;
begin
  assert(self.ValueType = TParserValueType.pvUntypedArray);
  result := vStringArray[Index];
end;


{ TParser }

constructor TParser.Create;
begin
  New(FLastSyntaxError);

  CurrentValue := TParserValue.Create;

  ObjectPaths  := TStringList.Create;
  ObjectTypes  := TStringList.Create;
  MultiObjects := TStringList.Create;

  IsMultiGroupOpening := false;
  IsMultiGroupOpen    := false;
end;

destructor TParser.Destroy;
begin
  Dispose(FLastSyntaxError);
  ObjectPaths.Free;
  MultiObjects.Free;
  ObjectTypes.Free;
  CurrentValue.Free;
  inherited;
end;

function TParser.GetCombinedObjectPath: string;
var
  c1: Integer;
  text : string;
begin
  text := '';
  for c1 := 0 to ObjectPaths.Count-2 do
  begin
    Text := Text + ObjectPaths[c1] + '.';
  end;

  if ObjectPaths.Count > 0 then
  begin
    Text := Text  + ObjectPaths[ObjectPaths.Count-1];
  end;

  result := Text;
end;

function TParser.ParseFile(const FileName: string): boolean;
var
  strings : TStringList;
begin
  strings := TStringList.Create;
  try
    strings.LoadFromFile(FileName);
    result := ParseText(strings.Text);
  finally
    strings.Free;
  end;
end;

function TParser.ParseText(const Text: string): boolean;
var
  IndexPos : integer;
  Token : TToken;
  Tokenizer : TTokenizer;
  SyntaxChecker : TSyntaxChecker;
  ErrorMsg : string;
begin
  // Check the syntax.
  SyntaxChecker.ResetState;
  IndexPos := 0;
  while(Tokenizer.NextToken(Text, IndexPos, Token)) do
  begin
    if not SyntaxChecker.IsSyntaxValid(Token, ErrorMsg) then
    begin
      // figure out error handling stuff.
      exit(false); //=========================>> syntax error. Exit now. =============>>
    end;
  end;


  // Do the parsing for real..
  IndexPos := 0;
  while(Tokenizer.NextToken(Text, IndexPos, Token)) do
  begin
    case Token.TokenType of
      ttEndOfFile: ;  //do nothing.
      ttUndefined: ;  //do nothing. This should cause a syntax error and be handled earlier.
      ttComment: ;    //do nothing.
      ttWhiteSpace: ; //do nothing.
      ttMultiObjectOpen: Parse_MultiObjectOpen(Text, Token);
      ttObjectOpen:      Parse_ObjectOpen(Text, Token);
      ttObjectClose:     Parse_ObjectClose(Text, Token);
      ttObjectKey:       Parse_ObjectKey(Text, Token);
    end;
  end;


  result := true;
end;

procedure TParser.Parse_MultiObjectOpen(const Text: string; const Token: TToken);
begin
  IsMultiGroupOpening := true;
  MultiObjects.Add(Token.TokenData);
end;

procedure TParser.Parse_ObjectOpen(const Text: string; const Token: TToken);
var
  ObjectPath : string;
  ObjectType : string;
  c1 : integer;
  combinedPath : string;
  multiPath : string;
  multiType : string;
begin
  if IsMultiGroupOpening then
  begin
    // 1) Process the new object open.
    MultiObjects.Add(Token.TokenData);
    IsMultiGroupOpen := true;

    // 2) Trigger object open events.
    combinedPath := GetCombinedObjectPath;
    if combinedPath <> '' then combinedPath := combinedPath + '.';

    for c1 := 0 to MultiObjects.Count-1 do
    begin
      CurrentValue.Clear;

      multiPath := TRonHelper.GetRonObjectPath(MultiObjects[c1]);
      multiType := TRonHelper.GetRonObjectType(MultiObjects[c1]);

      CurrentValue.fObjectPath := combinedPath + multiPath;
      CurrentValue.fObjectType := multiType;

      TriggerOnObjectOpenEvent(CurrentValue);
    end;
  end else
  begin
    CurrentValue.Clear;

    // 1) Process the new object open.
    ObjectPath := TRonHelper.GetRonObjectPath(Token.TokenData);
    ObjectPaths.Add(ObjectPath);

    ObjectType := TRonHelper.GetRonObjectType(Token.TokenData);
    ObjectTypes.Add(ObjectType);

    // 2) Trigger object open events.
    CurrentValue.Clear;
    CurrentValue.fObjectPath := GetCombinedObjectPath;
    CurrentValue.fObjectType := ObjectTypes[ObjectTypes.Count-1];
    TriggerOnObjectOpenEvent(CurrentValue);
  end;

  CurrentValue.Clear;
end;

procedure TParser.TriggerOnObjectOpenEvent(const value: TParserValue);
begin
  if assigned(OnObjectOpen) then OnObjectOpen(self, value);
end;

procedure TParser.TriggerOnValueEvent(const value: TParserValue);
begin
  if assigned(OnValue) then OnValue(self, value);
end;

procedure TParser.Parse_ObjectClose(const Text: string; const Token: TToken);
begin
  if IsMultiGroupOpen then
  begin
    IsMultiGroupOpening := false;
    IsMultiGroupOpen    := false;
    MultiObjects.Clear;
  end else
  begin
    ObjectPaths.Delete(ObjectPaths.Create.Count-1);
    ObjectTypes.Delete(ObjectTypes.Create.Count-1);
  end;
end;

procedure TParser.Parse_ObjectKey(const Text: string; const Token: TToken);
var
  c1: Integer;
  combinedPath : string;
  multiPath : string;
  multiType : string;
begin
  CurrentValue.Clear;

  TParserHelper.ReadRonValue(Text, Token.Position, CurrentValue);

  combinedPath := GetCombinedObjectPath;

  if IsMultiGroupOpen then
  begin
    if combinedPath <> ''
      then combinedPath := combinedPath + '.';

    for c1 := 0 to MultiObjects.Count-1 do
    begin
      multiPath := TRonHelper.GetRonObjectPath(MultiObjects[c1]);
      multiType := TRonHelper.GetRonObjectType(MultiObjects[c1]);

      CurrentValue.fObjectPath := combinedPath + multiPath;
      CurrentValue.fObjectType := multiType;

      TriggerOnValueEvent(CurrentValue);
    end;
  end else
  begin
    CurrentValue.fObjectPath := combinedPath;
    if ObjectTypes.Count > 0
      then CurrentValue.fObjectType := ObjectTypes[ObjectTypes.Count-1]
      else CurrentValue.fObjectType := '';

    TriggerOnValueEvent(CurrentValue);
  end;
end;










{ TParserHelper }

class procedure TParserHelper.GetRonValueArrayProperties(const Text: string; const ArrayStartPos: integer; out ArrayLength: integer; out ArrayType: TParserValueType);
var
  Tokenizer : TTokenizer;
  IndexPos : integer;
  nt : TToken;
begin
  // Check for array start.
  IndexPos := ArrayStartPos;
  if not Tokenizer.NextToken(Text, IndexPos, nt) then raise ERonParserExcpetion.Create('Unable to read next array token.');
  if not ((nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = '[')) then raise ERonParserExcpetion.Create('Array start token not found.');

  // Check for the next token. (It should be a value or array end.)
  if not Tokenizer.NextToken(Text, IndexPos, nt) then raise ERonParserExcpetion.Create('Unable to read next array token.');

  if (nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = ']') then
  begin
    ArrayLength := 0;
    ArrayType := TParserValueType.pvEmptyArray;
    exit; //==============================================>>
  end;

  case nt.TokenType of
    ttIntegerValue: ArrayType := TParserValueType.pvIntegerArray;
    ttFloatValue:   ArrayType := TParserValueType.pvFloatArray;
    ttBooleanValue: ArrayType := TParserValueType.pvBooleanArray;
    ttUntypedValue: ArrayType := TParserValueType.pvUntypedArray;
    ttBasicString:  ArrayType := TParserValueType.pvStringArray;
    ttMultiLineString: ArrayType := TParserValueType.pvStringArray;
    ttLiteralString:   ArrayType := TParserValueType.pvStringArray;
    ttMultiLineLiteralString: ArrayType := TParserValueType.pvStringArray;
  else
    raise ERonParserExcpetion.Create('Unexpected array token type.');
  end;


  ArrayLength := 0;
  IndexPos := ArrayStartPos;
  while Tokenizer.NextToken(Text, IndexPos, nt) do
  begin
    case nt.TokenType of
      ttIntegerValue,
      ttFloatValue,
      ttBooleanValue,
      ttUntypedValue,
      ttBasicString,
      ttMultiLineString,
      ttLiteralString,
      ttMultiLineLiteralString: inc(ArrayLength);
    end;

    if (nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = ']') then exit; //===================>> exit >>===========>>
  end;

  // If we make it this far, something has gone wrong.
  raise ERonParserExcpetion.Create('Array close tag not found.');
end;


class procedure TParserHelper.ReadRonValue(const Text: string; IndexPos: integer; var Output: TParserValue);
var
  Tokenizer : TTokenizer;
  Token : TToken;
  IntegerValue : integer;
  StringValue  : string;
  BooleanValue : boolean;
  FloatValue : single;
  ArrayLength: integer;
  ArrayType: TParserValueType;
begin
  Tokenizer.NextToken(Text, IndexPos, Token);
  if Token.TokenType <> TTokenType.ttObjectKey then raise ERonParserExcpetion.Create('Object key expected.');

  Output.FKeyName := Token.TokenData;

  Tokenizer.NextToken(Text, IndexPos, Token);

  // If the value is a single value, parse it and return....
  case Token.TokenType of
    ttIntegerValue:
    begin
      IntegerValue := TRonHelper.TokenToInteger(Token);
      Output.SetValueAsInteger(IntegerValue);
      exit; //====================================>> exit >>=============>>
    end;
    ttFloatValue:
    begin
      FloatValue := TRonHelper.TokenToFloat(Token);
      Output.SetValueAsFloat(FloatValue);
      exit; //====================================>> exit >>=============>>
    end;
    ttBooleanValue:
    begin
      BooleanValue := TRonHelper.TokenToBoolean(Token);
      Output.SetValueAsBoolean(BooleanValue);
      exit; //====================================>> exit >>=============>>
    end;
    ttUntypedValue:
    begin
      StringValue := TRonHelper.TokenToString(Token);
      Output.SetValueAsUntyped(StringValue);
      exit; //====================================>> exit >>=============>>
    end;
    ttBasicString,
    ttMultiLineString,
    ttLiteralString,
    ttMultiLineLiteralString:
    begin
      StringValue := TRonHelper.TokenToString(Token);
      Output.SetValueAsString(StringValue);
      exit; //====================================>> exit >>=============>>
    end;
  end;


  // check if the next token is the start of an array.
  if (Token.TokenType = TTokenType.ttSymbol) and (Token.TokenData = '[') then
  begin
    TParserHelper.GetRonValueArrayProperties(Text, Token.Position, ArrayLength, ArrayType);
    Output.SetValueType(ArrayType, ArrayLength);

    case ArrayType of
      pvIntegerArray: ReadIntegerArray(Text, Token.Position, Output.vIntegerArray);
      pvFloatArray:   ReadFloatArray(Text, Token.Position, Output.vFloatArray);
      pvBooleanArray: ReadBooleanArray(Text, Token.Position, Output.vBooleanArray);
      pvUntypedArray: ReadUntypedArray(Text, Token.Position, Output.vStringArray);
      pvStringArray:  ReadStringArray(Text, Token.Position, Output.vStringArray);
    else
      raise ERonParserExcpetion.Create('Array type expected.');
    end;

    exit; //===========================>> exit >>====================>>
  end;

  // if we make it this far, something has gone wrong.
  raise ERonParserExcpetion.Create('Unable to read value.');
end;

class procedure TParserHelper.ReadUntypedArray(const Text: string; IndexPos: integer; var arr: array of string);
var
  WriteIndex : integer;
  Tokenizer : TTokenizer;
  nt : TToken;
begin
  WriteIndex := 0;
  while Tokenizer.NextToken(Text, IndexPos, nt) do
  begin
    if (nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = ']') then exit; //===================>> exit >>===========>>
    if (nt.TokenType = TTokenType.ttUntypedValue) then
    begin
      arr[WriteIndex] := nt.TokenData;
      inc(WriteIndex);
    end;
  end;
end;

class procedure TParserHelper.ReadStringArray(const Text: string; IndexPos: integer; var arr: array of string);
var
  WriteIndex : integer;
  Tokenizer : TTokenizer;
  nt : TToken;
begin
  WriteIndex := 0;
  while Tokenizer.NextToken(Text, IndexPos, nt) do
  begin
    if (nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = ']') then exit; //===================>> exit >>===========>>

    case nt.TokenType of
      ttBasicString:
      begin
        arr[WriteIndex] := TRonString.UnescapeBasicString(nt.TokenData);
        inc(WriteIndex);
      end;

      ttMultiLineString:
      begin
        arr[WriteIndex] := TRonString.UnescapeMultilineString(nt.TokenData);
        inc(WriteIndex);
      end;

      ttLiteralString:
      begin
        arr[WriteIndex] := TRonString.UnescapeLiteralString(nt.TokenData);
        inc(WriteIndex);
      end;

      ttMultiLineLiteralString:
      begin
        arr[WriteIndex] := TRonString.UnescapeMultilineLiteralString(nt.TokenData);
        inc(WriteIndex);
      end;
    end;
  end;
end;

class procedure TParserHelper.ReadBooleanArray(const Text: string; IndexPos: integer; var arr: array of boolean);
var
  WriteIndex : integer;
  Tokenizer : TTokenizer;
  nt : TToken;
begin
  WriteIndex := 0;
  while Tokenizer.NextToken(Text, IndexPos, nt) do
  begin
    if (nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = ']') then exit; //===================>> exit >>===========>>
    if (nt.TokenType = TTokenType.ttBooleanValue) then
    begin
      arr[WriteIndex] := TRonHelper.TokenToBoolean(nt);
      inc(WriteIndex);
    end;
  end;
end;

class procedure TParserHelper.ReadFloatArray(const Text: string; IndexPos: integer; var arr: array of single);
var
  WriteIndex : integer;
  Tokenizer : TTokenizer;
  nt : TToken;
begin
  WriteIndex := 0;
  while Tokenizer.NextToken(Text, IndexPos, nt) do
  begin
    if (nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = ']') then exit; //===================>> exit >>===========>>
    if (nt.TokenType = TTokenType.ttFloatValue) then
    begin
      arr[WriteIndex] := TRonHelper.TokenToFloat(nt);
      inc(WriteIndex);
    end;
  end;
end;

class procedure TParserHelper.ReadIntegerArray(const Text: string; IndexPos: integer; var arr: array of integer);
var
  WriteIndex : integer;
  Tokenizer : TTokenizer;
  nt : TToken;
begin
  WriteIndex := 0;
  while Tokenizer.NextToken(Text, IndexPos, nt) do
  begin
    if (nt.TokenType = TTokenType.ttSymbol) and (nt.TokenData = ']') then exit; //===================>> exit >>===========>>
    if (nt.TokenType = TTokenType.ttIntegerValue) then
    begin
      arr[WriteIndex] := TRonHelper.TokenToInteger(nt);
      inc(WriteIndex);
    end;
  end;
end;




end.
