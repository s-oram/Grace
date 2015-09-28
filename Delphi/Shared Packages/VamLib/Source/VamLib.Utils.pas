unit VamLib.Utils;

interface

uses
  SysUtils,
  Classes;

const
    // NOTE: End of line definition sourced from http://stackoverflow.com/a/254997/395461
    EndOfLine = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
              {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};

type
  // kChar wraps some ansi charactor constants.
  // http://www.alanwood.net/demos/ansi.html
  kChar = record
  const
    Space = ' ';
    NoBreakSpace = AnsiChar(160);
    Bullet       = AnsiChar(149);
    MultiplySign = AnsiChar(215);
    DivideSign   = AnsiChar(247);
  end;


type
  PObject = ^TObject;

//==============================================================
//    General Utility Functions
//==============================================================
function LowestValue(const x1, x2 : integer):integer;
function HighestValue(const x1, x2 : integer):integer; overload;
function HighestValue(const x1, x2 : single):single; overload;

// a couple of methods to help with removing the 'combining signed and unsigned' types...
function CastToInteger(Value : cardinal):integer;  //Is this needed?
function CastToCardinal(Value : integer):cardinal; //Is this needed?

function Clamp(const Value, MinValue, MaxValue : integer):integer; overload; inline;
function Clamp(const Value, MinValue, MaxValue : single):single; overload; inline;

// The Wrap() function forces a value to overflow around an arbitary minimum
// and maximum.
function Wrap(Input : single; const MinValue, MaxValue : single):single; inline;

function InRange(const Value, MinValue, MaxValue : single):boolean; inline;

procedure SwapValues(var x1, x2:single); inline; overload;
procedure SwapValues(var x1, x2:integer); inline; overload;

function FloatToBoolean(Value:single):boolean; inline;
function BooleanToFloat(Value:boolean):single; inline;

function DistanceBetweenTwoPoints(const x1, y1, x2, y2:single):single;

//==============================================================
//    String Handling
//==============================================================
// user IOUtils.pas TPath.GetFileNameWithoutExtension()
function TrimFileExt(FileName:string):string; deprecated;
function RemoveFileExt(FileName:string):string; deprecated;

function IntToStrB(Int:integer; MinDigits:integer):string;
function IncrementFileName(FileName:string; MinDigits:integer = 2):string;
function IncrementName(Name:string; MinDigits:integer = 2):string;
function RandomString(const CharacterCount : integer):string;

function Occurrences(const Substring, Text: string): integer;
procedure ExplodeString(Delimiter: Char; Str: string; ListOfStrings: TStrings);

// Use the DataIO functions to convert values to and from strings for
// storing into save files. The DataIO functions ensure values are valid
// when converting back from strings.
function DataIO_BoolToStr(Value:boolean):string;
function DataIO_FloatToStr(Value:single):string;
function DataIO_IntToStr(Value:integer):string;

function DataIO_StrToBool(Value:string):boolean; overload;
function DataIO_StrToFloat(Value:string):single; overload;
function DataIO_StrToInt(Value:string):integer; overload;

function DataIO_StrToBool(Value:string; FallbackValue:boolean):boolean; overload;
function DataIO_StrToFloat(Value:string; FallbackValue:single):single; overload;
function DataIO_StrToInt(Value:string; FallbackValue:integer):integer; overload;

function DataIO_StrIsBool(const Value : string):boolean;
function DataIO_StrIsFloat(const Value : string):boolean;
function DataIO_StrIsInt(const Value : string):boolean;

procedure Sanitise(var Value : integer; const LowValue, HighValue : integer); overload;
procedure Sanitise(var Value : single;  const LowValue, HighValue : single); overload;

function IsIntegerString(const Value : string):boolean;
function IsMidiKeyNameString(Value : string):boolean; overload;
function IsMidiKeyNameString(Value : string; out MidiNoteNumber : integer):boolean; overload;
function MidiKeyNameToNoteNumber(Value : string):integer;



//==============================================================
//    Variable Handling
//==============================================================

 // x := StaggeredExpand(0.5, 100,200,400,800);
 // InputValue range is 0..1.
 // The x1..x4 values are the min-max scaling values.
function StaggeredExpand(InputValue, x1, x2, x3, x4 : single):single;

// Expands a 0-1 ranged float to an arbitary integer range.
function ExpandFloat(const Value : single; MinValue, MaxValue : integer):integer;

// TODO:MED ValueSplit was copied from eeFunctions.pas. It propbably overlaps with ExpandFloat. Check and convert if it
// does. Otherwise remove the deprecated tag.
function ValueSplit(IndexValue:single; SplitCount:integer):integer; deprecated;


//==============================================================
//    Utilities
//==============================================================
function ObjectArray(const objs : array of TObject):TArray<TObject>;

function AutoFree(const aObject: PObject): IUnknown; overload;
function AutoFree(const aObject: Pointer): IUnknown; overload;

function MemoryUsed: cardinal;
function BytesToMegaBytes(const Value : single):single;

type
  GuidEx = record
    class function NewGuid : TGuid; static;
    class function EmptyGuid : TGuid; static;
    class function IsEmptyGuid(Guid : TGuid) : boolean; static;
    class function ToUnicodeString(Guid : TGuid) : string; static;
    class function ToQuotedString(Guid : TGuid) : string; static;
    class function FromString(Value : string) : TGuid; static;
    class function EqualGuids(Guid1, Guid2 : TGuid) : boolean; static;
  end;


// ***************************************************************

// NOTE: ProcedureToMethod() and MethodToProcedure() have both been
// copied from madTools.pas.
// Documentation: http://help.madshi.net/MethodToProc.htm

// converts a procedure/function to a method
function ProcedureToMethod (self: TObject; procAddr: pointer) : TMethod;

// converts a method to a procedure/function
// CAUTION: this works only for stdcall methods!!
// you should free the procedure pointer when you don't need it anymore using:
//   VirtualFree(ProcedurePointer, 0, MEM_RELEASE);
function MethodToProcedure (self: TObject; methodAddr: pointer; maxParamCount: integer = 32) : pointer; overload;
function MethodToProcedure (method: TMethod; maxParamCount: integer = 32) : pointer; overload;

// ***************************************************************


function MoveFile(ExistingFileName, NewFileName:string):boolean;
function CopyFile(ExistingFileName, NewFileName:string):boolean;


//==============================================================
//    Enum Helpers
//==============================================================

type
  EDataIOError = class(Exception);

implementation

uses
  {$IFDEF VER230}
  System.Regularexpressionscore,
  {$ELSE}
  PerlRegEx,
  {$ENDIF}
  Windows,
  Math,
  StrUtils;



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

function ObjectArray(const objs : array of TObject):TArray<TObject>;
var
  c1: Integer;
begin
  SetLength(result, Length(Objs));
  for c1 := 0 to Length(objs)-1 do
  begin
    result[c1] := objs[c1];
  end;
end;


function AutoFree(const aObject: PObject): IUnknown;
begin
  result := TAutoFree.Create(aObject);
end;

function AutoFree(const aObject: Pointer): IUnknown; overload;
begin
  result := TAutoFree.Create(PObject(aObject));
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

function ValueSplit(IndexValue:single; SplitCount:integer):integer;
var
  x:integer;
begin
  assert(IndexValue >= 0);
  assert(IndexValue <= 1);
  x := floor(IndexValue * SplitCount);
  if x = SplitCount then x := x-1;
  result := x;
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

function RemoveFileExt(FileName:string):string;
begin
  result := TrimFileExt(FileName);
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


function DataIO_StrIsBool(const Value : string):boolean;
begin
  try
    DataIO_StrToBool(Value);
    result := true;
  except
    on EDataIOError do result := false;
    else raise;
  end;
end;

function DataIO_StrIsFloat(const Value : string):boolean;
begin
  try
    DataIO_StrToFloat(Value);
    result := true;
  except
    on EDataIOError do result := false;
    else raise;
  end;
end;

function DataIO_StrIsInt(const Value : string):boolean;
begin
  try
    DataIO_StrToInt(Value);
    result := true;
  except
    on EDataIOError do result := false;
    else raise;
  end;
end;

function DataIO_StrToBool(Value:string):boolean; overload;
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

    raise EDataIOError.Create('Cannot not convert string to boolean value.');
  except
    raise EDataIOError.Create('Cannot not convert string to boolean value.');
  end;
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

function DataIO_StrToFloat(Value:string):single; overload;
var
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  try
    result := StrToFloat(Value, fs)
  except
    raise EDataIOError.Create('Cannot not convert string to float value.');
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

function DataIO_StrToInt(Value:string):integer; overload;
begin
  try
    result := StrToInt(Value)
  except
    raise EDataIOError.Create('Cannot not convert string to integer value.');
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

procedure ExplodeString(Delimiter: Char; Str: string; ListOfStrings: TStrings);
// Source: http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
var
  dx : integer;
  ns : string;
  txt : string;
  delta : integer;
begin
  delta := Length(delimiter) ;
  txt := Str + delimiter;
  ListOfStrings.BeginUpdate;
  ListOfStrings.Clear;
  try
    while Length(txt) > 0 do
    begin
      dx := Pos(delimiter, txt) ;
      ns := Copy(txt,0,dx-1) ;
      ListOfStrings.Add(ns) ;
      txt := Copy(txt,dx+delta,MaxInt) ;
    end;
  finally
    ListOfStrings.EndUpdate;
  end;
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


function DistanceBetweenTwoPoints(const x1, y1, x2, y2:single):single;
// http://www.mathwarehouse.com/algebra/distance_formula/index.php
begin
  result := Sqrt( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) );
end;


function IsIntegerString(const Value : string):boolean;
var
  x : integer;
begin
  result := TryStrToInt(Value, x);
end;


function MidiKeyNameToNoteNumber(Value : string):integer; 
var
  x : integer;
begin
  if IsMidiKeyNameString(Value, x) 
    then result := x
    else raise EConvertError.Create('Value is a not a MIDI Key Name string.');  
end;

function IsMidiKeyNameString(Value : string):boolean; overload;
var
  x : integer;
begin
  result := IsMidiKeyNameString(Value, x);
end;


function IsMidiKeyNameString(Value : string; out MidiNoteNumber : integer):boolean; overload;
// NOTES:
//   http://www.electronics.dit.ie/staff/tscarff/Music_technology/midi/midi_note_numbers_for_octaves.htm
var
  NoteName : string;
  NoteNumber : integer;
  OctaveString : string;
  CopyStartIndex : integer;
  CopyLength : integer;
  OctaveNumber : integer;
begin
  if Length(Value) < 2 then exit(false);

  Value := Lowercase(value);

  // Convert note name to note number.

  NoteName := Value[1];

  if Value[2] = '#' then
  begin
    // MIDI Note is a sharp.    
    if NoteName = 'c' then NoteNumber := 1 else
    if NoteName = 'd' then NoteNumber := 3 else
    if NoteName = 'f' then NoteNumber := 6 else
    if NoteName = 'g' then NoteNumber := 8 else
    if NoteName = 'a' then NoteNumber := 10
      else exit(false); // Invalid note name //=================>> exit >>=======>>
  end else
  begin
    // MIDI Note is *not* a sharp.    
    if NoteName = 'c' then NoteNumber := 0 else
    if NoteName = 'd' then NoteNumber := 2 else
    if NoteName = 'e' then NoteNumber := 4 else
    if NoteName = 'f' then NoteNumber := 5 else
    if NoteName = 'g' then NoteNumber := 7 else
    if NoteName = 'a' then NoteNumber := 9 else
    if NoteName = 'b' then NoteNumber := 11
      else exit(false); // Invalid note name //=================>> exit >>=======>>
  end;

  if (Value[2] = '#') or (Value[2] = '_')
    then CopyStartIndex := 3
    else CopyStartIndex := 2;

  try  
    CopyLength := Length(Value) - CopyStartIndex + 1;
    OctaveString := Copy(Value, CopyStartIndex, CopyLength);
  except
    // something went wrong. assume note is invalid.
    exit(false);
  end;

  if TryStrToInt(OctaveString, OctaveNumber) then
  begin
    // Check octave boundary. 
    if OctaveNumber < -2 then exit(false);
    if OctaveNumber > 8  then exit(false);    
    
    // if we make it this far, the string is a MIDI key name. phew! 
    result := true;
    MidiNoteNumber := (OctaveNumber + 2)  * 12 + NoteNumber; //Important: don't forget to set the result variable!
    assert(MidiNoteNumber >= 0);
    assert(MidiNoteNumber <= 127);  
  end else
  begin
    // It's not an octave number if it can't be converted to an integer. 
    exit(false);
  end;
end;

procedure Sanitise(var Value : integer; const LowValue, HighValue : integer);
begin
  Value := Clamp(Value, LowValue, HighValue);
end;


procedure Sanitise(var Value : single;  const LowValue, HighValue : single);
begin
  Value := Clamp(Value, LowValue, HighValue);
end;


{ GuidEx }

{ TGuidEx }

class function GuidEx.EmptyGuid: TGuid;
begin
  result := FromString('{00000000-0000-0000-0000-000000000000}');
end;

class function GuidEx.EqualGuids(Guid1, Guid2: TGuid): boolean;
begin
  result := IsEqualGUID(Guid1, Guid2);
end;

class function GuidEx.FromString(Value: string): TGuid;
begin
  result := StringToGuid(Value);
end;

class function GuidEx.IsEmptyGuid(Guid : TGuid): boolean;
begin
  result := EqualGuids(Guid,EmptyGuid);
end;

class function GuidEx.NewGuid: TGuid;
var
  Guid : TGuid;
begin
  CreateGUID(Guid);
  Result := Guid;
end;

class function GuidEx.ToQuotedString(Guid: TGuid): string;
begin
  result := QuotedStr(ToUnicodeString(Guid));
end;

class function GuidEx.ToUnicodeString(Guid: TGuid): string;
begin
  result := GuidToString(Guid);
end;


// ***************************************************************

{$ifdef win64}

  function MethodToProcedure(self: TObject; methodAddr: pointer; maxParamCount: integer = 32) : pointer;
  var stackSpace : integer;
      s1, s2     : AnsiString;
      pos        : integer;
      i1         : integer;
  begin
    if maxParamCount < 4 then
      maxParamCount := 4;
    if odd(maxParamCount) then
      stackSpace := (maxParamCount + 2) * 8   // parameters + self + localVar
    else
      stackSpace := (maxParamCount + 3) * 8;  // parameters + self + localVar + alignment
    s1 := #$48 + #$81 + #$ec +        #0#0#0#0 +  // sub     rsp, $118
          #$48 + #$89 + #$84 + #$24 + #0#0#0#0 +  // mov     [rsp+$110], rax
          #$48 + #$8b + #$84 + #$24 + #0#0#0#0 +  // mov     rax, [rsp+$120]    // read 1st original stack parameter
          #$48 + #$89 + #$44 + #$24 + #$08     +  // mov     [rsp+8], rax       // store as 2nd new stack parameter
          #$48 + #$8b + #$84 + #$24 + #0#0#0#0 +  // mov     rax, [rsp+$128]    // read 2nd original stack parameter
          #$48 + #$89 + #$44 + #$24 + #$10     +  // mov     [rsp+$10], rax     // store as 3rd new stack parameter
          #$48 + #$8b + #$84 + #$24 + #0#0#0#0 +  // mov     rax, [rsp+$130]    // read 3rd original stack parameter
          #$48 + #$89 + #$44 + #$24 + #$18     +  // mov     [rsp+$18], rax     // store as 4th new stack parameter
          #$4c + #$89 + #$4c + #$24 + #$20     +  // mov     [rsp+$20], r9      // store 4th original register parameter as 5th new stack parameter
          #$4d + #$89 + #$c1                   +  // mov     r9, r8             // cycle the register parameters (rcx -> rdx -> r8 -> r9)
          #$49 + #$89 + #$d0                   +  // mov     r8, rdx
          #$48 + #$89 + #$ca                   +  // mov     rdx, rcx
          #$66 + #$0f + #$6f + #$da            +  // movdqa  xmm3, xmm2         // cycle the register parameters (xmm0 -> xmm1 -> xmm2 -> xmm3)
          #$66 + #$0f + #$6f + #$d1            +  // movdqa  xmm2, xmm1
          #$66 + #$0f + #$6f + #$c8;              // movdqa  xmm1, xmm0
    integer(pointer(@s1[ 4])^) := stackSpace;
    integer(pointer(@s1[12])^) := stackSpace -  $8;
    integer(pointer(@s1[20])^) := stackSpace +  $8;
    integer(pointer(@s1[33])^) := stackSpace + $10;
    integer(pointer(@s1[46])^) := stackSpace + $18;
    pos := Length(s1) + 1;
    SetLength(s1, Length(s1) + (maxParamCount - 4) * 16);
    s2 := #$48 + #$8b + #$84 + #$24 + #0#0#0#0 +  // mov     rax, [rsp+$140]
          #$48 + #$89 + #$84 + #$24 + #0#0#0#0;   // mov     [rsp+$28], rax
    for i1 := 1 to maxParamCount - 4 do begin
      integer(pointer(@s2[ 5])^) := $20 + i1 * 8 + stackSpace;
      integer(pointer(@s2[13])^) := $20 + i1 * 8;
      Move(s2[1], s1[pos], Length(s2));
      inc(pos, Length(s2));
    end;
    s2 := #$48 + #$8b + #$84 + #$24 + #0#0#0#0 +  // mov     rax, [rsp+$110]
          #$48 + #$b9       + #0#0#0#0#0#0#0#0 +  // mov     rcx, methodAddr
          #$48 + #$89 + #$8c + #$24 + #0#0#0#0 +  // mov     [rsp+$110], rcx
          #$48 + #$b9       + #0#0#0#0#0#0#0#0 +  // mov     rcx, self
          #$48 + #$89 + #$0c + #$24            +  // mov     [rsp], rcx         // store "self" as 1st new stack parameter
          #$ff + #$94 + #$24        + #0#0#0#0 +  // call    [rsp+$110]
          #$48 + #$81 + #$c4        + #0#0#0#0 +  // add     rsp, $118
          #$c3;                                   // ret
    integer(pointer(@s2[ 5])^) := stackSpace - $8;
    pointer(pointer(@s2[11])^) := methodAddr;
    integer(pointer(@s2[23])^) := stackSpace - $8;
    pointer(pointer(@s2[29])^) := self;
    integer(pointer(@s2[44])^) := stackSpace - $8;
    integer(pointer(@s2[51])^) := stackSpace;
    s1 := s1 + s2;
    result := VirtualAlloc(nil, Length(s1), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Move(s1[1], result^, Length(s1));
  end;

{$else}

  function MethodToProcedure(self: TObject; methodAddr: pointer; maxParamCount: integer = 32) : pointer;
  type
    TMethodToProc = packed record
      popEax   : byte;                  // $58      pop EAX
      pushSelf : record                 //          push self
                   opcode  : byte;      // $B8
                   self    : pointer;   // self
                 end;
      pushEax  : byte;                  // $50      push EAX
      jump     : record                 //          jmp [target]
                   opcode  : byte;      // $FF
                   modRm   : byte;      // $25
                   pTarget : ^pointer;  // @target
                   target  : pointer;   //          @MethodAddr
                 end;
    end;
  var mtp : ^TMethodToProc absolute result;
  begin
    mtp := VirtualAlloc(nil, sizeOf(mtp^), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    with mtp^ do begin
      popEax          := $58;
      pushSelf.opcode := $68;
      pushSelf.self   := self;
      pushEax         := $50;
      jump.opcode     := $FF;
      jump.modRm      := $25;
      jump.pTarget    := @jump.target;
      jump.target     := methodAddr;
    end;
  end;

{$endif}

function MethodToProcedure(method: TMethod; maxParamCount: integer = 32) : pointer;
begin
  result := MethodToProcedure(TObject(method.data), method.code);
end;

function ProcedureToMethod(self: TObject; procAddr: pointer) : TMethod;
begin
  result.Data := self;
  result.Code := procAddr;
end;

// ***************************************************************


function MoveFile(ExistingFileName, NewFileName:string):boolean;
begin
  result := Windows.MoveFile(@(ExistingFileName[1]), @(NewFileName[1]));
end;

function CopyFile(ExistingFileName, NewFileName:string):boolean;
begin
  if ExistingFileName = NewFileName then
  begin
    result := true;
    exit;
  end;

  result := Windows.CopyFile(@(ExistingFileName[1]), @(NewFileName[1]), false);
end;

end.

