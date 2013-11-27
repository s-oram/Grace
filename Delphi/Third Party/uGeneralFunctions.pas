unit uGeneralFunctions;

interface

uses
  ShLwApi, SysUtils, Classes, Types, MoreTypes;

//== String Conversion functions ==============================================

function ToPAnsiChar(Text:string):PAnsiChar;

function InRect_Quick(aPoint:TPoint; aRect:TRect):boolean; inline;
function InRect(aPoint:TPoint; aRect:TRect):boolean; inline;

procedure Wait(Seconds:single);


function HighWord(value:LongWord):LongWord;
function LowWord(value:LongWord):LongWord;

//Number functions...

procedure Swap(var Value1,Value2:single); //depreciate this method because there is already a 'swap' in dephi that does something different.
procedure SwapValues(var Value1,Value2:single); inline;

function LowestCount(const Value1, Value2:integer):integer;

function IsEven(Value:integer):boolean; inline;


//== String handling functions ==============================================

function Explode(Delimiter,  Text : string):TArrayOfString;
procedure Explode_ToStringList(Delimiter, Text:string; out result:TStringList);

procedure DecodeDateString(Date:string; var Year, Month, Day:word);
function EncodeDateString(Year, Month, Day:word):string;

function IntToStrB(Int:integer; MinDigits:integer):string;

function RemoveFileExt(FileName:string):string;

function AddQuotes(Text:string):string;

function StrToFloat_Safe(Text:String; fs:TFormatSettings; FallBackValue:single = 0):single;
function StrToBool_Safe(Text:String; FallBackValue:boolean = false):boolean;
function StrToInt_Safe(Text:string; FallBackValue:integer = 0):integer;


function ExpandFileNameRelBaseDir(const FileName, BaseDir: string): string;

function IsNumeric(aChar:Char): boolean;
function IsInteger(aString : string):boolean;


implementation

uses
  Windows;



function InRect_Quick(aPoint:TPoint; aRect:TRect):boolean;
begin
  //NOTE: aRect must have it's points properly sorted so
  //the top value is really the top, otherwise the function will
  //return a false answer.
  assert(aRect.Top <= aRect.Bottom);
  assert(aRect.Left <= aRect.Right);

  result := (aPoint.X > aRect.Left) and (aPoint.X < aRect.Right) and (aPoint.Y > aRect.Top) and (aPoint.Y < aRect.Bottom);
end;

function InRect(aPoint:TPoint; aRect:TRect):boolean;
var
  t:integer;
begin
  //Check to see if the rectangle is properly alligned.

  if aRect.Top > aRect.Bottom then
  begin
    t := aRect.Top;
    aRect.Top := aRect.Bottom;
    aRect.Bottom := t;
  end;

  if aRect.Left > aRect.Right then
  begin
    t := aRect.Left;
    aRect.Left := aRect.Right;
    aRect.Right := t;
  end;

  //Check if the point is inside.

  result := (aPoint.X > aRect.Left) and (aPoint.X < aRect.Right) and (aPoint.Y > aRect.Top) and (aPoint.Y < aRect.Bottom);
end;

function HighWord(value:LongWord):LongWord;
begin
  result := (Value and $FFFFFFFF);
end;

function LowWord(value:LongWord):LongWord;
begin
  result := (Value and $0000FFFF);
end;

procedure Wait(Seconds:single);
var
  ct,xt:double;
begin
  ct := Time;
  xt := Time + Seconds / (24 * 60 * 60);
  while xt > ct do
  begin
    ct := Time;
  end;

end;

procedure Swap(var Value1,Value2:single);
var
  TempValue:single;
begin
  TempValue := Value1;
  Value1    := Value2;
  Value2    := TempValue;
end;

procedure SwapValues(var Value1,Value2:single);
var
  TempValue:single;
begin
  TempValue := Value1;
  Value1    := Value2;
  Value2    := TempValue;
end;

function LowestCount(const Value1, Value2:integer):integer;
begin
  if Value1 < Value2
    then result := Value1
    else result := Value2;
end;

function IsEven(Value:integer):boolean; inline;
begin
  if (Value mod 2) = 0
    then result := true
    else result := false;
end;


function Explode(Delimiter,  Text : string):TArrayOfString;
var
  s:string;
  c1,p:integer;
begin
  s  := Text;
  c1 := 0;

  while length(s) > 0 do
  begin
    inc(c1);
    SetLength(result, c1);
    p := pos(Delimiter,s);

    if ( p > 0 ) then
    begin
      result[c1 - 1] := copy(s,0,p-1);

      {updated, thanks Irfan}
      s := copy(s,p + length(Delimiter),length(s));
    end else
    begin
      result[c1 - 1] := s;
      s :=  '';
    end;
  end;

end;

procedure Explode_ToStringList(Delimiter, Text:string; out result:TStringList);
begin
  Assert(Assigned(result));

  result.Clear;
  result.Delimiter := Delimiter[1];
  result.DelimitedText := Text;
end;


procedure DecodeDateString(Date:string; var Year, Month, Day:word);
var
  ed:TArrayOfString;
begin
  ed := Explode('/', Date);
  if Length(ed) <> 3 then
  begin
    raise Exception.Create('Invalid date string.');
  end;                                             

  Day   := StrToInt(ed[0]);
  Month := StrToInt(ed[1]);
  Year  := StrToInt(ed[2]);

  SetLength(ed, 0);  
end;

function EncodeDateString(Year, Month, Day:word):string;
begin
  result := IntToStr(Day) + '/' + IntToStr(Month) + '/' + IntToStr(Year);  
end;


function IntToStrB(Int:integer; MinDigits:integer):string;
begin
  result := IntToStr(Int);
  while Length(result) < MinDigits do result := '0' + result;
end;


function RemoveFileExt(FileName:string):string;
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

function AddQuotes(Text:string):string;
begin
  result := '"' + Text + '"';
end;








// StrToFloat_Safe() will always return a valid float value.
// Exceptions will be caught silently.
function StrToFloat_Safe(Text:String; fs:TFormatSettings; FallBackValue:single = 0):single;
begin
  try
    result := StrToFloat(Text, fs);
  except
    result := FallBackValue;
  end;
end;

function StrToBool_Safe(Text:String; FallBackValue:boolean = false):boolean;
begin
  try
    result := StrToBool(Text);
  except
    result := FallBackValue;
  end;
end;

function StrToInt_Safe(Text:string; FallBackValue:integer = 0):integer;
begin
  try
    result := StrToInt(Text);
  except
    result := FallBackValue;
  end;
end;


function ToPAnsiChar(Text:string):PAnsiChar;
begin
  result := PAnsiChar(PAnsiString(AnsiString(Text)));
end;

function ExpandFileNameRelBaseDir(const FileName, BaseDir: string): string;
var
  Buffer: array [0..MAX_PATH-1] of Char;
begin
  if PathIsRelative(PChar(FileName)) then begin
    Result := IncludeTrailingPathDelimiter(BaseDir) + FileName;
  end else begin
    Result := FileName;
  end;
  if PathCanonicalize(@Buffer[0], PChar(Result)) then begin
    Result := Buffer;
  end;
end;


function IsNumeric(aChar:Char): boolean;
var
  CharCode : cardinal;
begin
  CharCode := Ord(aChar);
  if (CharCode >= 48) and (CharCode <=57)
    then result := true
    else result := false;

end;

function IsInteger(aString : string):boolean;
begin
  try
    StrToInt(aString);
    result := true;
  except
    result := false;
  end;
end;


end.
