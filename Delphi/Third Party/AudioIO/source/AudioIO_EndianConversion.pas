unit AudioIO_EndianConversion;

interface
uses
  AudioIO_ExtendedX87,
  SysUtils;



function ByteSwap(Data:integer):integer;  overload;
function ByteSwap(Data:cardinal):cardinal;  overload;
function ByteSwap(Data:SmallInt):SmallInt; overload;
function ByteSwap(Data:word):word; overload;


function ByteSwap(Data:single):single; overload;
function ByteSwap(Data:Double):Double; overload;

procedure ByteSwap(Data:Pointer; Bytes:integer); overload;

// NOTE: Extended is a 10 byte type on 32-bit platforms and and 8 byte type on 64-bit platforms.
function ByteSwap(Data:Extended):Extended; overload;

// NOTE: TExtendedX87 functions as a 10 byte type on both 32 and 64 bit platforms.
// On 32-bit platforms it aliases to the Extended type. Because of the aliasing on
// 32-bit platforms we can't have another overloaded function like:
//     function ByteSwap(Data:TExtendedX87):TExtendedX87; overload;
function ByteSwap_TExtendedX87(Data:TExtendedX87):TExtendedX87;





implementation

type
  PFourByteArray = ^TFourByteArray;
  TFourByteArray = array[0..3] of byte;

function ByteSwap(Data:integer):integer;  overload;
{$IFDEF CPUX64}
var
  DataPtr,ResultPtr:PFourByteArray;
begin
  DataPtr   := @Data;
  ResultPtr := @Result;

  ResultPtr^[0] := DataPtr^[3];
  ResultPtr^[1] := DataPtr^[2];
  ResultPtr^[2] := DataPtr^[1];
  ResultPtr^[3] := DataPtr^[0];
end;
{$ELSE}
asm
  bswap EAX
end;
{$ENDIF}

function ByteSwap(Data:cardinal):cardinal;  overload;
{$IFDEF CPUX64}
var
  DataPtr,ResultPtr:PFourByteArray;
begin
  DataPtr   := @Data;
  ResultPtr := @Result;

  ResultPtr^[0] := DataPtr^[3];
  ResultPtr^[1] := DataPtr^[2];
  ResultPtr^[2] := DataPtr^[1];
  ResultPtr^[3] := DataPtr^[0];
end;
{$ELSE}
asm
  bswap EAX
end;
{$ENDIF}

function ByteSwap(Data:SmallInt):SmallInt; overload;
begin
  //result := (Data shr 8) or (Data shl 8);
  result := ((Data and $FF00) shr 8) + ((Data and $00FF) shl 8);
end;

function ByteSwap(Data:word):word; overload;
begin
  //result := (Data shr 8) or (Data shl 8);
  result := ((Data and $FF00) shr 8) + ((Data and $00FF) shl 8);
end;

function ByteSwap(Data:single):single; overload;
var
  pInt:^integer;
begin
  pInt := @Data;
  ByteSwap(pInt^);
  result := pInt^;
end;

function ByteSwap(Data:Double):Double; overload;
var
  pInt:^integer;
begin
  pInt := @Data;
  ByteSwap(pInt^);
  inc(pInt);
  ByteSwap(pInt^);
  dec(pInt);
  result := pInt^;
end;


procedure ByteSwap(Data:Pointer; Bytes:integer); overload;
var
  c1:integer;
  pInt:^integer;
begin
  pInt := Data;
  for c1 := 0 to (Bytes div 4) - 1 do
  begin
    pInt^ := ByteSwap(pInt^);
    inc(pInt);
  end;
end;

function ByteSwap(Data:Extended):Extended; overload;
type
  TTenBits = array[0..9] of byte;
  PTenBits = ^TTenBits;
var
  t1,t2:PTenBits;
begin
  // NOTE: Extended is 10 bits on 32 bit platforms and 8 bits on 64 bit platforms.
  {$IFDEF CPUX64}
    assert(SizeOf(Extended) = 8);

    t1 := @Data;
    t2 := @Result;

    t2^[7] := t1^[0];
    t2^[6] := t1^[1];
    t2^[5] := t1^[2];
    t2^[4] := t1^[3];
    t2^[3] := t1^[4];
    t2^[2] := t1^[5];
    t2^[1] := t1^[6];
    t2^[0] := t1^[7];
  {$ELSE}
    assert(SizeOf(Extended) = 10);

    t1 := @Data;
    t2 := @Result;

    t2^[9] := t1^[0];
    t2^[8] := t1^[1];
    t2^[7] := t1^[2];
    t2^[6] := t1^[3];
    t2^[5] := t1^[4];
    t2^[4] := t1^[5];
    t2^[3] := t1^[6];
    t2^[2] := t1^[7];
    t2^[1] := t1^[8];
    t2^[0] := t1^[9];
  {$ENDIF}
end;

function ByteSwap_TExtendedX87(Data:TExtendedX87):TExtendedX87; overload;
begin
  {$IFDEF CPUX64}
    result.AsBytes[0] := Data.AsBytes[9];
    result.AsBytes[1] := Data.AsBytes[8];
    result.AsBytes[2] := Data.AsBytes[7];
    result.AsBytes[3] := Data.AsBytes[6];
    result.AsBytes[4] := Data.AsBytes[5];
    result.AsBytes[5] := Data.AsBytes[4];
    result.AsBytes[6] := Data.AsBytes[3];
    result.AsBytes[7] := Data.AsBytes[2];
    result.AsBytes[8] := Data.AsBytes[1];
    result.AsBytes[9] := Data.AsBytes[0];
  {$ELSE}
    result := ByteSwap(Data);
  {$ENDIF}
end;




end.
