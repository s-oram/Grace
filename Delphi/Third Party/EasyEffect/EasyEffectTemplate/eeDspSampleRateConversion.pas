{
  Windowed Sinc Interpolation

  This unit contains functions for windowed sinc interpolation as described by
  Julius Orion Smith at
    https://ccrma.stanford.edu/~jos/pasp/Bandlimited_Interpolation.html

  The method is also detailed in
    ``Physical Audio Signal Processing'', by Julius O. Smith III, W3K Publishing, 2010, ISBN 978-0-9745607-2-4.



}

unit eeDspSampleRateConversion;

interface

uses
  MoreTypes;





// WindowSinc Rate converstion
//
// --Best settings for downsampling.
//     ZeroCrossings    = 33
//     OverSampleFactor = 512
//     Alpha            = 10
//
//
// --Best settings for upsampling
//     ZeroCrossings    = 33 or higher.. (43+)
//     OverSampleFactor = 512
//     Alpha            = 15
//
//
// This method seems somewhat slow. Different methods of resampling need to be investigated.

function CreateTables_WindowedSinc(aZeroCrossings:integer = 33; aOverSampleFactor:integer = 512; aEnvAlpha:single = 10):boolean;
procedure FreeTables_WindowedSinc;

// RateConvert_WindowedSinc()
// Aliasing at approximate -140 db ?? Need to test this in more depth..
procedure RateConvert_WindowedSinc_Float(Input:PSingle;   InputSampleFrames:integer; Output:PSingle;   OutputSampleFrames:integer);
procedure RateConvert_WindowedSinc_Int  (Input:PSmallInt; InputSampleFrames:integer; Output:PSmallInt; OutputSampleFrames:integer);




// RateConvert_Linear()
// Aliasing at approximate -80 db.
procedure RateConvert_Linear(Input:PSingle; InputSampleFrames:integer; Output:PSingle; OutputSampleFrames:integer);

// RateConvert_Hermite()
// Aliasing at approximate -110 db.
procedure RateConvert_Hermite(Input:PSingle; InputSampleFrames:integer; Output:PSingle; OutputSampleFrames:integer);


implementation

uses
  vamDebugMessage, SysUtils, Math, eeDSP, eeDspWindows;

var
  AreTablesInitialised:boolean;


var
  ZeroCrossings    :integer;
  OverSampleFactor :integer;
  EnvAlpha         :single;
  TableLength      :integer;
  WindowedSincTable:array of double;



function CreateTables_WindowedSinc(aZeroCrossings:integer = 33; aOverSampleFactor:integer = 512; aEnvAlpha:single = 10):boolean;
var
  c1: Integer;
  SincIndex:double;
begin
  //Only initialise the tables if the settings have changed (or table is not already intialised).
  if (AreTablesInitialised)
  and (aZeroCrossings    = ZeroCrossings)
  and (aOverSampleFactor = OverSampleFactor)
  and (aEnvAlpha         = EnvAlpha)
    then
  begin
    result := false;
    exit; // ============================>>
  end;


  ZeroCrossings        := aZeroCrossings;
  OverSampleFactor     := aOverSampleFactor;
  EnvAlpha             := aEnvAlpha;
  TableLength          := ZeroCrossings * OverSampleFactor;
  AreTablesInitialised := false;



  try
    SetLength(WindowedSincTable, TableLength + 1);
  except
    on EOutOfMemory do
    begin
      result := false;
      exit;
    end;
  end;

  for c1 := 0 to TableLength - 1 do
  begin
    SincIndex := c1 / OverSampleFactor;
    WindowedSincTable[c1] := Sinc(SincIndex) * KaiserBesselWindow(c1, TableLength, EnvAlpha);
  end;

  AreTablesInitialised := true;

  //If we've made it this far, the function must have been successful. return true. :)
  result := true;
end;

procedure FreeTables_WindowedSinc;
begin
  SetLength(WindowedSincTable, 0);
  AreTablesInitialised := false;
end;


// Still have doubts about windowed sinc method. doesn't seem to perform well when downsampling...
// It's okay when upsampling with not too much frequency content near nyquist.

procedure RateConvert_WindowedSinc_Float(Input:PSingle; InputSampleFrames:integer; Output:PSingle; OutputSampleFrames:integer);
var
  InData:TArrayOfSingle  absolute Input;
  OutData:TArrayOfSingle absolute Output;
  c1        : Integer;
  x         : double;
  ReadIndex : integer;
  Frac      : double;
  c2        : Integer;

  SincTablePos       : double;
  stAIndex, stBIndex :integer;
  stFrac             : double;
  stCoeff            : double;

  SampleValue        : double;
  ReadIndex2         : integer;
  ReadValue          : double;
  RateChange         : double;
  ScaleFactor        : double;
  tx                : double;
begin
  if AreTablesInitialised = false then raise Exception.Create('Interpolation tables have not been created.');

  if OutputSampleFrames > InputSampleFrames then
  begin
    //tx := (OutputSampleFrames / InputSampleFrames);
    //tx := (tx - 1) * 0.19 + 1;
    //RateChange := 1 / tx;
    //ScaleFactor := InputSampleFrames / OutputSampleFrames;
    //ScaleFactor := RateChange;
    RateChange := 1;
    ScaleFactor := 1;
  end else
  begin
    //RateChange := 1;
    //ScaleFactor := 1;

    tx := (InputSampleFrames / OutputSampleFrames);
    RateChange := 1 / tx;
    ScaleFactor := RateChange;
  end;

  for c1 := 0 to OutputSampleFrames - 1 do
  begin
    //Find the read position in the source data.
    x         := (c1 / (OutputSampleFrames-1)) * (InputSampleFrames-1);
    ReadIndex := Floor(x);
    Frac      := x - ReadIndex;

    //  Now do the sinc interpolation...
    //Because the table only contains half of the filter kernal we need to apply it in two runs.
    SampleValue := 0;

    //Compute the first run moving forwards through the table.
    c2 := 0;
    SincTablePos := (c2 + (1-Frac)) * OverSampleFactor * RateChange;
    while SincTablePos <= TableLength-1 do
    begin
      stAIndex := floor(SincTablePos);
      stBIndex := stAIndex + 1;
      stFrac   := SincTablePos - stAIndex;

      stCoeff := LinearInterpolation(WindowedSincTable[stAIndex], WindowedSincTable[stBIndex], stFrac);

      //find the current sample value.
      ReadIndex2 := ReadIndex + 1 + c2;
      if (ReadIndex2 >= 0) and (ReadIndex2 < InputSampleFrames)
        then ReadValue := InData[ReadIndex2]
        else ReadValue := 0;

      //filter and add to the sample value sum.
      SampleValue := SampleValue + (ReadValue * stCoeff);

      inc(c2);
      SincTablePos := (c2 + (1-Frac)) * OverSampleFactor * RateChange;
    end;



    //Now compute the second run through the table.
    c2 := 0;
    SincTablePos := (c2 + Frac) * OverSampleFactor * RateChange;
    while SincTablePos <= TableLength-1 do
    begin
      //Calculate the filter coefficient for the current sample.
      stAIndex := floor(SincTablePos);
      stBIndex := stAIndex + 1;
      stFrac   := SincTablePos - stAIndex;

      stCoeff := LinearInterpolation(WindowedSincTable[stAIndex], WindowedSincTable[stBIndex], stFrac);

      //find the current sample value.
      ReadIndex2 := ReadIndex - c2;
      if (ReadIndex2 >= 0) and (ReadIndex2 < InputSampleFrames)
        then ReadValue := InData[ReadIndex2]
        else ReadValue := 0;

      //filter and add to the sample value sum.
      SampleValue := SampleValue + (ReadValue * stCoeff);

      inc(c2);
      SincTablePos := (c2 + Frac) * OverSampleFactor * RateChange;
    end;

    OutData[c1] := SampleValue * ScaleFactor;
  end;
end;



procedure RateConvert_WindowedSinc_Int(Input:PSmallInt; InputSampleFrames:integer; Output:PSmallInt; OutputSampleFrames:integer);
const
  SizeOfSmallInt = 32767;
  OneOver32767   = 1 / 32767; //One over size(SmallInt)
var
  InData:array of SmallInt  absolute Input;
  OutData:array of SmallInt absolute Output;
  c1        : Integer;
  x         : double;
  ReadIndex : integer;
  Frac      : double;
  c2        : Integer;

  SincTablePos       : double;
  stAIndex, stBIndex :integer;
  stFrac             : double;
  stCoeff            : double;

  SampleValue        : double;
  ReadIndex2         : integer;
  ReadValue          : double;
  RateChange         : double;
  ScaleFactor        : double;
  tx                : double;
begin
  if AreTablesInitialised = false then raise Exception.Create('Interpolation tables have not been created.');

  if OutputSampleFrames > InputSampleFrames then
  begin
    //tx := (OutputSampleFrames / InputSampleFrames);
    //tx := (tx - 1) * 0.19 + 1;
    //RateChange := 1 / tx;
    //ScaleFactor := InputSampleFrames / OutputSampleFrames;
    //ScaleFactor := RateChange;
    RateChange := 1;
    ScaleFactor := 1;
  end else
  begin
    //RateChange := 1;
    //ScaleFactor := 1;

    tx := (InputSampleFrames / OutputSampleFrames);
    RateChange := 1 / tx;
    ScaleFactor := RateChange;
  end;

  for c1 := 0 to OutputSampleFrames - 1 do
  begin
    //Find the read position in the source data.
    x         := (c1 / (OutputSampleFrames-1)) * (InputSampleFrames-1);
    ReadIndex := Floor(x);
    Frac      := x - ReadIndex;

    //  Now do the sinc interpolation...
    //Because the table only contains half of the filter kernal we need to apply it in two runs.
    SampleValue := 0;

    //Compute the first run moving forwards through the table.
    c2 := 0;
    SincTablePos := (c2 + (1-Frac)) * OverSampleFactor * RateChange;
    while SincTablePos <= TableLength-1 do
    begin
      stAIndex := floor(SincTablePos);
      stBIndex := stAIndex + 1;
      stFrac   := SincTablePos - stAIndex;

      stCoeff := LinearInterpolation(WindowedSincTable[stAIndex], WindowedSincTable[stBIndex], stFrac);

      //find the current sample value.
      ReadIndex2 := ReadIndex + 1 + c2;
      if (ReadIndex2 >= 0) and (ReadIndex2 < InputSampleFrames)
        then ReadValue := InData[ReadIndex2] * OneOver32767
        else ReadValue := 0;

      //filter and add to the sample value sum.
      SampleValue := SampleValue + (ReadValue * stCoeff);

      inc(c2);
      SincTablePos := (c2 + (1-Frac)) * OverSampleFactor * RateChange;
    end;



    //Now compute the second run through the table.
    c2 := 0;
    SincTablePos := (c2 + Frac) * OverSampleFactor * RateChange;
    while SincTablePos <= TableLength-1 do
    begin
      //Calculate the filter coefficient for the current sample.
      stAIndex := floor(SincTablePos);
      stBIndex := stAIndex + 1;
      stFrac   := SincTablePos - stAIndex;

      stCoeff := LinearInterpolation(WindowedSincTable[stAIndex], WindowedSincTable[stBIndex], stFrac);

      //find the current sample value.
      ReadIndex2 := ReadIndex - c2;
      if (ReadIndex2 >= 0) and (ReadIndex2 < InputSampleFrames)
        then ReadValue := InData[ReadIndex2]  * OneOver32767
        else ReadValue := 0;

      //filter and add to the sample value sum.
      SampleValue := SampleValue + (ReadValue * stCoeff);

      inc(c2);
      SincTablePos := (c2 + Frac) * OverSampleFactor * RateChange;
    end;

    OutData[c1] := round(SampleValue * ScaleFactor * SizeOfSmallInt);
  end;


//  for c1 := 0 to OutputSampleFrames - 1 do
//  begin
//    OutData[c1] := round(random * SizeOfSmallInt);
//  end;
end;





procedure RateConvert_Linear(Input:PSingle; InputSampleFrames:integer; Output:PSingle; OutputSampleFrames:integer);
var
  InData    : TArrayOfSingle absolute Input;
  OutData   : TArrayOfSingle absolute Output;
  c1        : Integer;
  x         : double;
  ReadIndex : integer;
  a,b       : integer;
  Frac      : double;
begin
  for c1 := 0 to OutputSampleFrames - 1 do
  begin
    //Find the read position in the source data.
    x         := (c1 / (OutputSampleFrames-1)) * (InputSampleFrames-1);
    ReadIndex := Floor(x);
    Frac      := x - ReadIndex;

    a := ReadIndex;
    b := ReadIndex + 1;



    if Frac = 0 then OutData[c1] := InData[a]
    else
    if Frac = 1 then OutData[c1] := InData[b]
    else
      OutData[c1] := LinearInterpolation(InData[a], InData[b], Frac);

  end;
end;


procedure RateConvert_Hermite(Input:PSingle; InputSampleFrames:integer; Output:PSingle; OutputSampleFrames:integer);
var
  InData:TArrayOfSingle absolute Input;
  OutData:TArrayOfSingle absolute Output;
  c1          : Integer;
  x           : double;
  ReadIndex   : integer;
  a,b,c,d     : integer;
  va,vb,vc,vd : double;
  Frac        : double; 
begin
  for c1 := 0 to OutputSampleFrames - 1 do
  begin
    //Find the read position in the source data.
    x         := (c1 / (OutputSampleFrames-1)) * (InputSampleFrames-1);
    ReadIndex := Floor(x);
    Frac      := x - ReadIndex;

    a := ReadIndex - 1;
    b := ReadIndex;
    c := ReadIndex + 1;
    d := ReadIndex + 2;


    if (a >= 0) and (a < InputSampleFrames) then va := InData[a] else va := 0;
    if (b >= 0) and (b < InputSampleFrames) then vb := InData[b] else vb := 0;
    if (c >= 0) and (c < InputSampleFrames) then vc := InData[c] else vc := 0;
    if (d >= 0) and (d < InputSampleFrames) then vd := InData[d] else vd := 0;

    OutData[c1] := Hermite4Interpolation(Frac,va,vb,vc,vd);

  end;
end;


initialization
  AreTablesInitialised := false;
  ZeroCrossings        := 0;
  OverSampleFactor     := 0;
  EnvAlpha             := 0;
  CreateTables_WindowedSinc;

finalization
  OutputDebugMessage('Enter: eeDspSampleRateConversion.pas Finalization');
  if AreTablesInitialised then FreeTables_WindowedSinc;


end.
