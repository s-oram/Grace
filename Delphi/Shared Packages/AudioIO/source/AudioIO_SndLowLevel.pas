unit AudioIO_SndLowLevel;

interface

uses
  AudioIO;

procedure Unpack16BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
procedure Unpack16BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);

procedure Unpack16BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
procedure Unpack16BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);

implementation

procedure Unpack16BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
const
  Scale16Bit:single = 2 / 65535;      //    2 / Power(2,16)
var
  c1:integer;
  psi:PSmallInt;
begin
  psi := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := psi^ * Scale16Bit;

    inc(psi);
    inc(Left);
  end;

end;



procedure Unpack16BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
const
  Scale16Bit:single = 2 / 65535;
var
  c1:integer;
  psi:PSmallInt;
begin
  psi := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := psi^ * Scale16Bit;

    inc(psi);
    inc(Left);

    Right^ := psi^ * Scale16Bit;

    inc(psi);
    inc(Right);
  end;

end;

procedure Unpack16BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  psi:PSmallInt;
begin
  psi := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := psi^;

    inc(psi);
    inc(Left);
  end;

end;


procedure Unpack16BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  psi:PSmallInt;
begin
  psi := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := psi^;

    inc(psi);
    inc(Left);

    Right^ := psi^;

    inc(psi);
    inc(Right);
  end;

end;



end.
