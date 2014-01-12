unit AudioIO_AiffLowLevel;

interface

uses
  AudioIO;

procedure Unpack8BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
procedure Unpack16BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
procedure Unpack24BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
procedure Unpack32BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);

procedure Unpack8BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
procedure Unpack16BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
procedure Unpack24BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
procedure Unpack32BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);


procedure Unpack8BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
procedure Unpack16BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
procedure Unpack24BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
procedure Unpack32BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);

procedure Unpack8BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
procedure Unpack16BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
procedure Unpack24BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
procedure Unpack32BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);



implementation


uses
  AudioIO_EndianConversion;

procedure Unpack8BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
const
  Scale8Bit:single = 2 / 256;         //    2 / Power(2,8)
var
  c1:integer;
  ps:PShortInt;
begin
  ps := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := ps^ * Scale8Bit;

    inc(ps);
    inc(Left);

    Right^ := ps^ * Scale8Bit;

    inc(ps);
    inc(Right);
  end;

end;

procedure Unpack8BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
const
  Scale8Bit:single = 2 / 256;         //    2 / Power(2,8)
var
  c1:integer;
  ps:PShortInt;
begin
  ps := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := ps^ * Scale8Bit;

    inc(ps);
    inc(Left);
  end;

end;

procedure Unpack16BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
const
  Scale16Bit:single = 2 / 65535;
var
  c1:integer;
  psi:PSmallInt;
  pw:PWord;
begin
  pw := SampleData;
  for c1 := 0 to (SampleFrames * 2) - 1 do
  begin
    pw^ := ByteSwap(pw^);
    inc(pw);
  end;


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

procedure Unpack16BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
const
  Scale16Bit:single = 2 / 65535;      //    2 / Power(2,16)
var
  c1:integer;
  psi:PSmallInt;
  pw:PWord;
begin
  pw := SampleData;
  for c1 := 0 to SampleFrames - 1 do
  begin
    pw^ := ByteSwap(pw^);
    inc(pw);
  end;

  psi := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := psi^ * Scale16Bit;

    inc(psi);
    inc(Left);
  end;

end;

procedure Unpack24BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
var
  c1:integer;
  pb:PByte;
  b1,b2,b3:byte;
  tv:LongInt;
  x:single;
begin

  pb := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    //--- left channel ----
    b1 := pb^;
    inc(pb);
    b2 := pb^;
    inc(pb);
    b3 := pb^;
    inc(pb);

    tv := b3 + b2 * (1 shl 8) + b1 * (1 shl 16);

    if tv >= (1 shl 23) then tv := tv - (1 shl 24);

    x := tv;

    if x > 0
      then x := x / (1 shl 23 - 1)
      else
    if x < 0
      then x := x / (1 shl 23);

    Left^ := x;
    inc(Left);

    //--- Right channel ----
    b1 := pb^;
    inc(pb);
    b2 := pb^;
    inc(pb);
    b3 := pb^;
    inc(pb);


    tv := b3 + b2 * (1 shl 8) + b1 * (1 shl 16);

    if tv >= (1 shl 23) then tv := tv - (1 shl 24);

    x := tv;

    if x > 0
      then x := x / (1 shl 23 - 1)
      else
    if x < 0
      then x := x / (1 shl 23);

    Right^ := x;
    inc(Right);

  end;



end;

procedure Unpack24BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
const
  Scale24Bit:single = 2 / 16777211;
var
  c1:integer;
  pb:PByte;
  b1,b2,b3:byte;
  tv:LongInt;
  x:single;
begin
  pb := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    //--- left channel ----

    b1 := pb^;
    inc(pb);
    b2 := pb^;
    inc(pb);
    b3 := pb^;
    inc(pb);

    tv := b3 + b2 * (1 shl 8) + b1 * (1 shl 16);

    if tv >= (1 shl 23) then tv := tv - (1 shl 24);

    x := tv;

    if x > 0
      then x := x / (1 shl 23 - 1)
      else
    if x < 0
      then x := x / (1 shl 23);

    Left^ := x;
    inc(Left);
  end;

end;


procedure Unpack32BitPCM_Stereo(SampleData:Pointer; Left,Right:PSingle; SampleFrames:integer);
const
  Scale32Bit:single = 2 / 4294967296;      //    2 / Power(2,32)
var
  c1:integer;
  pv:PLongInt;
  plw:PLongWord;
begin
  plw := SampleData;

  for c1 := 0 to (SampleFrames * 2) - 1 do
  begin
    plw^ := ByteSwap(plw^);
    inc(plw);
  end;


  pv := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := pv^ * Scale32Bit;

    inc(pv);
    inc(Left);

    Right^ := pv^ * Scale32Bit;

    inc(pv);
    inc(Right);
  end;

end;

procedure Unpack32BitPCM_Mono(SampleData:Pointer; Left:PSingle; SampleFrames:integer);
const
  Scale32Bit:single = 2 / 4294967296;      //    2 / Power(2,32)
var
  c1:integer;
  pv:PLongInt;
  plw:PLongWord;
begin
  plw := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    plw^ := ByteSwap(plw^);
    inc(plw);
  end;

  pv := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := pv^ * Scale32Bit;

    inc(pv);
    inc(Left);
  end;

end;



procedure Unpack8BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  ps:PShortInt;
begin
  ps := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := ps^ * 256;

    inc(ps);
    inc(Left);
  end;

end;



procedure Unpack16BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  psi:PSmallInt;
  pw:PWord;
begin
  pw := SampleData;
  for c1 := 0 to SampleFrames - 1 do
  begin
    pw^ := ByteSwap(pw^);
    inc(pw);
  end;

  psi := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := psi^;

    inc(psi);
    inc(Left);
  end;

end;


procedure Unpack24BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  pb:PByte;
  b1,b2,b3:byte;
  tv:LongInt;
begin
  pb := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    //--- left channel ----

    b1 := pb^;
    inc(pb);
    b2 := pb^;
    inc(pb);
    b3 := pb^;
    inc(pb);

    tv := b3 + b2 * (1 shl 8) + b1 * (1 shl 16);

    if tv >= (1 shl 23) then tv := tv - (1 shl 24);

    Left^ := tv div 256;

    inc(Left);
  end;

end;


procedure Unpack32BitPCM_Mono_Int(SampleData:Pointer; Left:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  pv:PLongInt;
  plw:PLongWord;
begin
  plw := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    plw^ := ByteSwap(plw^);
    inc(plw);
  end;

  pv := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := pv^ div 65536;

    inc(pv);
    inc(Left);
  end;

end;


procedure Unpack8BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  ps:PShortInt;
begin
  ps := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := ps^ * 256;

    inc(ps);
    inc(Left);

    Right^ := ps^ * 256;

    inc(ps);
    inc(Right);
  end;

end;


procedure Unpack16BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  psi:PSmallInt;
  pw:PWord;
begin
  pw := SampleData;
  for c1 := 0 to (SampleFrames * 2) - 1 do
  begin
    pw^ := ByteSwap(pw^);
    inc(pw);
  end;


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


procedure Unpack24BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  pb:PByte;
  b1,b2,b3:byte;
  tv:LongInt;
begin

  pb := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    //--- left channel ----
    b1 := pb^;
    inc(pb);
    b2 := pb^;
    inc(pb);
    b3 := pb^;
    inc(pb);

    tv := b3 + b2 * (1 shl 8) + b1 * (1 shl 16);

    if tv >= (1 shl 23) then tv := tv - (1 shl 24);

    Left^ := tv div 256;

    inc(Left);

    //--- Right channel ----
    b1 := pb^;
    inc(pb);
    b2 := pb^;
    inc(pb);
    b3 := pb^;
    inc(pb);


    tv := b3 + b2 * (1 shl 8) + b1 * (1 shl 16);

    if tv >= (1 shl 23) then tv := tv - (1 shl 24);

    Right^ := tv div 256;

    inc(Right);

  end;



end;



procedure Unpack32BitPCM_Stereo_Int(SampleData:Pointer; Left,Right:PSmallInt; SampleFrames:integer);
var
  c1:integer;
  pv:PLongInt;
  plw:PLongWord;
begin
  plw := SampleData;

  for c1 := 0 to (SampleFrames * 2) - 1 do
  begin
    plw^ := ByteSwap(plw^);
    inc(plw);
  end;


  pv := SampleData;

  for c1 := 0 to SampleFrames - 1 do
  begin
    Left^ := pv^ div 65536;

    inc(pv);
    inc(Left);

    Right^ := pv^ div 65536;

    inc(pv);
    inc(Right);
  end;

end;








end.
