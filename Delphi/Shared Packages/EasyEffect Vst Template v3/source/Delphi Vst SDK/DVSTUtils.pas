//******************************************************************************
//
//  DVstUtils.pas
//  19 January 2006
//
//  Part of the VST 2.4 SDK for Delphi
//  by Frederic Vanmol
//     http://www.axiworld.be
//     frederic@axiworld.be
//
//------------------------------------------------------------------------------
//
//  Functions : FourCharToLong
//              FMod
//              dB2string
//              db2stringRound
//              float2string
//              long2string
//              float2stringAsLong
//              Hz2string (new)
//              ms2string (new)
//              gapSmallValue
//              invGapSmallValue
//
//  Note : This unit requires the Delphi Math unit, which can be found on the
//         Delphi cd if you don't have it installed yet
//
//******************************************************************************
unit
    DVstUtils;

interface

function FourCharToLong(C1, C2, C3, C4: AnsiChar): Longint;
function FourCharToLong2(FC: array of AnsiChar): Longint;

function FMod(d1, d2: Double): Double;

procedure dB2string(value: Single; text: PAnsiChar); deprecated;
procedure dB2stringRound(value: single; text: PAnsiChar); deprecated;
procedure float2string(value: Single; text: PAnsiChar); deprecated;
procedure long2string(value: Longint; text: PAnsiChar); deprecated;
procedure float2stringAsLong(value: Single; text: PAnsiChar);  deprecated;
procedure Hz2string(samples, sampleRate: Single; text: PAnsiChar); deprecated;
procedure ms2string(samples, sampleRate: Single; text: PAnsiChar); deprecated;

function gapSmallValue(value, maxValue: Double): Double;
function invGapSmallValue(value, maxValue: Double): Double;



implementation

uses
    Math, SysUtils;

{ this function converts four AnsiChar variables to one longint. }
function FourCharToLong(C1, C2, C3, C4: AnsiChar): Longint;
begin
  Result := Ord(C4)  + (Ord(C3) shl 8) + (Ord(C2) shl 16) + (Ord(C1) shl 24);
end;

function FourCharToLong2(FC: array of AnsiChar): Longint;
begin
  Result := Ord(FC[3])  + (Ord(FC[2]) shl 8) + (Ord(FC[1]) shl 16) + (Ord(FC[0]) shl 24);
end;

function FMod(d1, d2: Double): Double;
var
   i: Integer;
begin
  try
    i := Trunc(d1 / d2);
  except
    on EInvalidOp do i := High(Longint);
  end;
  Result := d1 - (i * d2);
end;            

procedure dB2string(value: Single; text: PAnsiChar);
begin
  //if (value <= 0) then
  //  StrCopy(text, '   -oo  ')
  //else
  //  float2string(20 * log10(value), text);
end;

procedure dB2stringRound(value: single; text: PAnsiChar);
begin
  //if (value <= 0) then
  //  StrCopy(text, '    -96 ')
  //else
  //  long2string(Round(20 * log10(value)), text);
end;

procedure float2string(value: Single; text: PAnsiChar);
begin
  //StrCopy(text, PAnsiChar(Format('%f', [value])));
end;

procedure long2string(value: Longint; text: PAnsiChar);
begin
  //if (value >= 100000000) then
  //begin
  //  StrCopy(text, ' Huge!  ');
  //  Exit;
  //end;
  //
  //StrCopy(text, PAnsiChar(Format('%7d', [Value])));  // sprintf(aString, '%7d', value);
end;

procedure float2stringAsLong(value: Single; text: PAnsiChar);
begin
  // NOTE: Deprecated while transitioning to XE2 due to strange string conversion routines.
  // I don't think this method is used anywhere...

  //if (value >= 100000000) then
  //begin
  //  StrCopy(text, ' Huge!  ');
  //  Exit;
  //end;
  //
  //StrCopy(text, PAnsiChar(Format('%7.0f', [value])));  // sprintf(aString, '%7d', value);

end;

procedure Hz2string(samples, sampleRate: single; text: PAnsiChar);
begin
  //if (samples = 0) then
  //  float2string(0, text)
  //else
  //  float2string(sampleRate / samples, text);
end;

procedure ms2string(samples, sampleRate: single; text: PAnsiChar);
begin
  //float2string(samples * 1000 / sampleRate, text);
end;

function gapSmallValue(value, maxValue: double): double;
begin
  Result := Power(maxValue, value);
end;

function invGapSmallValue(value, maxValue: double): double;
var
   r: Double;
begin
  r := 0;
  if (value <> 0) then
    r := logN(maxValue, value);
  Result :=  r;
end;

end.
