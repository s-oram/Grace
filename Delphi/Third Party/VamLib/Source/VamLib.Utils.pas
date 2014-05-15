unit VamLib.Utils;

interface

type
  PObject = ^TObject;

function AutoFree(const aObject: PObject): IUnknown;

function LowestValue(const x1, x2 : integer):integer;
function HighestValue(const x1, x2 : integer):integer; overload;
function HighestValue(const x1, x2 : single):single; overload;


// a couple of methods to help with removing the 'combining signed and unsigned' types...
function CastToInteger(Value : cardinal):integer;
function CastToCardinal(Value : integer):cardinal;

function Clamp(const Value, MinValue, MaxValue : integer):integer; overload; inline;
function Clamp(const Value, MinValue, MaxValue : single):single; overload; inline;

// The Wrap() function forces a value to overflow around an arbitary minimum
// and maximum.
function Wrap(Input : single; const MinValue, MaxValue : single):single; inline;

function InRange(const Value, MinValue, MaxValue : single):boolean; inline;

procedure SwapValues(var x1, x2:single); inline; overload;
procedure SwapValues(var x1, x2:integer); inline; overload;

// Expands a 0-1 ranged float to an arbitary integer range.
function ExpandFloat(const Value : single; MinValue, MaxValue : integer):integer;

function MemoryUsed: cardinal;

function BytesToMegaBytes(const Value : single):single;




function TrimFileExt(FileName:string):string;



//==============================================================
// Use the DataIO functions to convert values to and from strings for
// storing into save files. The DataIO functions ensure values are valid
// when converting back from strings.
function DataIO_BoolToStr(Value:boolean):string;
function DataIO_FloatToStr(Value:single):string;
function DataIO_IntToStr(Value:integer):string;

function DataIO_StrToBool(Value:string; FallbackValue:boolean):boolean;
function DataIO_StrToFloat(Value:string; FallbackValue:single):single;
function DataIO_StrToInt(Value:string; FallbackValue:integer):integer;
//==============================================================

function FloatToBoolean(Value:single):boolean; inline;
function BooleanToFloat(Value:boolean):single; inline;

function IntToStrB(Int:integer; MinDigits:integer):string;
function IncrementFileName(FileName:string; MinDigits:integer = 2):string;
function IncrementName(Name:string; MinDigits:integer = 2):string;
function RandomString(const CharacterCount : integer):string;




//==============================================================
//    Variable Handling
//==============================================================

 // x := StaggeredExpand(0.5, 100,200,400,800);
 // InputValue range is 0..1.
 // The x1..x4 values are the min-max scaling values.
function StaggeredExpand(InputValue, x1, x2, x3, x4 : single):single;

implementation

uses
  {$IFDEF VER230}
  System.Regularexpressionscore,
  {$ELSE}
  PerlRegEx,
  {$ENDIF}
  Math,
  SysUtils;


//==============================================================================
//   Auto Free
//==============================================================================

type
  TAutoFree = class(TInterfacedObject, IUnknown)
  private
    fObject: PObject;
  public
    constructor Create(const aObject: PObject);
    destructor Destroy; override;
  end;

constructor TAutoFree.Create(const aObject: PObject);
begin
  inherited Create;
  fObject := aObject;
end;

destructor TAutoFree.Destroy;
begin
  FreeAndNIL(fObject^);
  inherited;
end;

function AutoFree(const aObject: PObject): IUnknown;
begin
  result := TAutoFree.Create(aObject);
end;


//==============================================================================
//==============================================================================

function CastToInteger(Value : cardinal):integer;
const
  kMaxInt = 2147483647;
begin
  if Value > kMaxInt then raise Exception.Create('Cannot convert type to integer. The result will overflow.');
  result := Integer(Value);
end;

function CastToCardinal(Value : integer):cardinal;
begin
  if Value < 0 then raise Exception.Create('Cannot convert type to cardinal. The result will overflow.');
  result := Cardinal(Value);
end;




function Clamp(const Value, MinValue, MaxValue : integer):integer; overload;
begin
  assert(MinValue <= MaxValue);

  if Value < MinValue then result := MinValue
  else
  if Value > MaxValue then result := MaxValue
  else
    result := Value;
end;

function Clamp(const Value, MinValue, MaxValue : single):single; overload;
begin
  assert(MinValue <= MaxValue);

  if Value < MinValue then result := MinValue
  else
  if Value > MaxValue then result := MaxValue
  else
    result := Value;
end;

function Wrap(Input : single; const MinValue, MaxValue : single):single; overload;
begin
  while Input < MinValue do
  begin
    Input := Input + (MaxValue - MinValue);
  end;

  while Input > MaxValue do
  begin
    Input := Input - (MaxValue - MinValue);
  end;

  result := Input;
end;



function InRange(const Value, MinValue, MaxValue : single):boolean; inline;
begin
  if (Value >= MinValue) and (Value <= MaxValue)
    then result := true
    else result := false;
end;

procedure SwapValues(var x1, x2:single); inline; overload;
var
  tx : single;
begin
  tx := x1;
  x1 := x2;
  x2 := tx;
end;


procedure SwapValues(var x1, x2:integer); inline; overload;
var
  tx : integer;
begin
  tx := x1;
  x1 := x2;
  x2 := tx;
end;


function ExpandFloat(const Value : single; MinValue, MaxValue : integer):integer;
var
  Dist : integer;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  Dist := MaxValue - MinValue;

  result := MinValue + round(Dist * Value);


  assert(result >= MinValue);
  assert(result <= MaxValue);

end;

function MemoryUsed: cardinal;
var
    st: TMemoryManagerState;
    sb: TSmallBlockTypeState;
begin
    GetMemoryManagerState(st);
    result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
    for sb in st.SmallBlockTypeStates do begin
        result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
    end;
end;

function BytesToMegaBytes(const Value : single):single;
begin
  // http://www.matisse.net/bitcalc/?input_amount=1&input_units=megabytes&notation=legacy
  result := Value / 1048576;
end;





function TrimFileExt(FileName:string):string;
var
  Ext:string;
  Index:integer;
  cc:integer;
begin
  FileName := ExtractFileName(FileName); //Remove path information, if there is any.

  Ext := ExtractFileExt(FileName);

  if Ext ='' then
  begin
    //There is no extension.
    result := FileName;
    exit; //==================================================>
  end;

  cc := Length(Ext);
  Index := Pos(Ext,FileName);

  Delete(FileName,Index,cc);

  result := FileName;
end;




function DataIO_BoolToStr(Value:boolean):string;
begin
  if Value
    then result := 'true'
    else result := 'false';
end;

function DataIO_FloatToStr(Value:single):string;
var
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  result := FloatToStr(Value, fs);
end;

function DataIO_IntToStr(Value:integer):string;
begin
  result := IntToStr(Value);
end;





function DataIO_StrToBool(Value:string; FallbackValue:boolean):boolean;
begin
  Value := Trim(Value);
  try
    if SameText(Value, '1')     then exit(true);
    if SameText(Value, 'true')  then exit(true);
    if SameText(Value, 'T')     then exit(true);
    if SameText(Value, 'Y')     then exit(true);
    if SameText(Value, 'Yes')   then exit(true);

    if SameText(Value, '0')     then exit(false);
    if SameText(Value, 'false') then exit(false);
    if SameText(Value, 'F')     then exit(false);
    if SameText(Value, 'N')     then exit(false);
    if SameText(Value, 'No')    then exit(false);

    if Value = '' then exit(false);

    //if we've made it this far, exit with the fall back value.
    result := FallBackValue;
  except
    result := FallBackValue;
  end;
end;

function DataIO_StrToFloat(Value:string; FallbackValue:single):single;
var
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  try
    result := StrToFloat(Value, fs)
  except
    result := FallBackValue;
  end;
end;

function DataIO_StrToInt(Value:string; FallbackValue:integer):integer;
begin
  try
    result := StrToInt(Value)
  except
    result := FallBackValue;
  end;
end;

function FloatToBoolean(Value:single):boolean; inline;
begin
  result := (Value >= 0.5);
end;

function BooleanToFloat(Value:boolean):single; inline;
begin
  if Value
    then result := 1
    else result := 0;
end;

function IntToStrB(Int:integer; MinDigits:integer):string;
begin
  result := IntToStr(Int);
  while Length(result) < MinDigits do result := '0' + result;
end;


function IncrementFileName(FileName:string; MinDigits:integer = 2):string;
const
  Space = ' ';
var
  Path,Name,Ext:string;
  RegEx:TPerlRegEx;
  FileNumber:integer;
  NumberString:string;
begin
  //RegEx := TPerlRegEx.Create(nil);
  RegEx := TPerlRegEx.Create;
  try
    Path := ExtractFilePath(FileName);
    Ext := ExtractFileExt(FileName);

    FileName := StringReplace(FileName, Path, '', []);
    Name := StringReplace(FileName, Ext, '', []);


    //Check to see if there is an integer at the end of the filename.
    //NOTE: The regex string also matches integers with preceding
    //whitespace charactors if there are any.
    RegEx.Subject := UTF8String(Name);
    RegEx.RegEx := '(([\s]+)?[0-9]+)$';
    if RegEx.Match then
    begin
      //If there is...
      //FileNumber := StrToInt(RegEx.MatchedExpression);
      FileNumber := StrToInt(String(RegEx.MatchedText));
      inc(FileNumber);

      //SetLength(Name, RegEx.MatchedExpressionOffset-1);
      SetLength(Name, RegEx.MatchedOffset-1);
    end else
    begin
      FileNumber := 1;
    end;

    NumberString := IntToStrB(FileNumber, MinDigits);

    Name := Name + Space + NumberString;

    result := Path + Name + Ext;
  finally
    RegEx.Free;
  end;
end;

function IncrementName(Name:string; MinDigits:integer = 2):string;
const
  Space = ' ';
var
  RegEx:TPerlRegEx;
  FileNumber:integer;
  NumberString:string;
begin
  //RegEx := TPerlRegEx.Create(nil);
  RegEx := TPerlRegEx.Create;
  try
    //Check to see if there is an integer at the end of the filename.
    //NOTE: The regex string also matches integers with preceding
    //whitespace charactors if there are any.
    RegEx.Subject := UTF8String(Name);
    RegEx.RegEx := '(([\s]+)?[0-9]+)$';
    if RegEx.Match then
    begin
      //If there is...
      //FileNumber := StrToInt(RegEx.MatchedExpression);
      FileNumber := StrToInt(String(RegEx.MatchedText));
      inc(FileNumber);

      //SetLength(Name, RegEx.MatchedExpressionOffset-1);
      SetLength(Name, RegEx.MatchedOffset-1);
    end else
    begin
      FileNumber := 1;
    end;

    NumberString := IntToStrB(FileNumber, MinDigits);

    result := Name + Space + NumberString;
  finally
    RegEx.Free;
  end;
end;

function RandomString(const CharacterCount : integer):string;
var
  s : string;
  c1: Integer;
  x : integer;
begin
  // Ansi charactor set reference.
  // http://www.alanwood.net/demos/ansi.html
  s := '';
  for c1 := 0 to CharacterCount-1 do
  begin
    x := 97 + random(122-97);
    s := s + Char(x);
  end;
  result := s;
end;


function LowestValue(const x1, x2 : integer):integer;
begin
  if x1 < x2
    then result := x1
    else result := x2;
end;

function HighestValue(const x1, x2 : integer):integer; overload;
begin
  if x1 > x2
    then result := x1
    else result := x2;
end;

function HighestValue(const x1, x2 : single):single; overload;
begin
  if x1 > x2
    then result := x1
    else result := x2;
end;

function StaggeredExpand(InputValue, x1, x2, x3, x4 : single):single;
var
  Index : integer;
begin
  assert(InputValue >= 0);
  assert(InputValue <= 1);

  Index := floor(InputValue * 3);

  case Index of
  0: result := x1 + (InputValue - 0/3) * 3 * (x2 - x1);
  1: result := x2 + (InputValue - 1/3) * 3 * (x3 - x2);
  2: result := x3 + (InputValue - 2/3) * 3 * (x4 - x3);
  3: result := x4;
  else
    raise Exception.Create('Unexpected Index Value.');
  end;

  assert(result >= x1);
  assert(result <= x4);
end;


end.
