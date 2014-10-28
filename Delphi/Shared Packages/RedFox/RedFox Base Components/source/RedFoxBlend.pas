unit RedFoxBlend;

interface

{.$DEFINE Inline}

uses
  RedFox;

//==============================================================================
//    Pixel Blend Functions
//==============================================================================

// NOTE:
// - The BasePixel is the 'destination' layer.
// - The TopPixel is the 'source' layer.
// - The source layer is render 'on top' of the base layer.
function AlphaBlend(const BasePixel, TopPixel : T32BitPixel):T32BitPixel; //AlphaBlend is taken from the GR32 project.
function AlphaBlend2(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
function AlphaBlend3(const BasePixel, TopPixel : T32BitPixel; TopAlpha:byte):T32BitPixel;

function CopySource(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;

function RandomPixel(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
function Copy_InvertAlpha(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
function Copy_WinText(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;


//==============================================================================
//    BlitImage()
//==============================================================================

type
  TBlitDestInfo = record
    Buffer       : Pointer;
    BufferWidth  : integer;
    BufferHeight : integer;
    x1           : integer;
    y1           : integer;
  end;

  TBlitSrcInfo = record
    Buffer       : Pointer;
    BufferWidth  : integer;
    BufferHeight : integer;
    x1           : integer;
    y1           : integer;
    x2           : integer;
    y2           : integer;
  end;

  TPixelBlendFunction = reference to function(const BasePixel,TopPixel : T32BitPixel):T32BitPixel;

procedure BlitImage(Dest:TBlitDestInfo; Src:TBlitSrcInfo; const BlendFunc:TPixelBlendFunction); {$IFDEF Inline}inline;{$ENDIF}

function ByteCrossFade(const x1, x2, f : byte):byte;

implementation

uses
  Math;

{ A fixed-point type }

type
  // This type has data bits arrangement compatible with Windows.TFixed
  PFixed = ^TFixed;
  TFixed = type Integer;

  PFixedRec = ^TFixedRec;
  TFixedRec = packed record
    case Integer of
      0: (Fixed: TFixed);
      1: (Frac: Word; Int: SmallInt);
  end;

  PFixedArray = ^TFixedArray;
  TFixedArray = array [0..0] of TFixed;
  PArrayOfFixed = ^TArrayOfFixed;
  TArrayOfFixed = array of TFixed;
  PArrayOfArrayOfFixed = ^TArrayOfArrayOfFixed;
  TArrayOfArrayOfFixed = array of TArrayOfFixed;

  // TFloat determines the precision level for certain floating-point operations
  PFloat = ^TFloat;
  TFloat = Single;

type
  PByteArray = ^TByteArray;
  TByteArray = array [0..0] of Byte;
  PArrayOfByte = ^TArrayOfByte;
  TArrayOfByte = array of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array [0..0] of Word;
  PArrayOfWord = ^TArrayOfWord;
  TArrayOfWord = array of Word;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array [0..0] of Integer;
  PArrayOfInteger = ^TArrayOfInteger;
  TArrayOfInteger = array of Integer;
  PArrayOfArrayOfInteger = ^TArrayOfArrayOfInteger;
  TArrayOfArrayOfInteger = array of TArrayOfInteger;

  PSingleArray = ^TSingleArray;
  TSingleArray = array [0..0] of Single;
  PArrayOfSingle = ^TArrayOfSingle;
  TArrayOfSingle = array of Single;

  PFloatArray = ^TFloatArray;
  TFloatArray = array [0..0] of TFloat;
  PArrayOfFloat = ^TArrayOfFloat;
  TArrayOfFloat = array of TFloat;

const
  // Fixed point math constants
  FixedOne = $10000;
  FixedHalf = $7FFF;
  FixedPI  = Round(PI * FixedOne);
  FixedToFloat = 1/FixedOne;


var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;


function ByteCrossFade(const x1, x2, f : byte):byte;
begin
  result := ((x1 * (256 - f)) + (x2 * f)) shr 8;
end;



function AlphaBlend(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
var
  Af, Ab: PByteArray;
  FA : Byte;
begin
  if TopPixel.A = 0 then
  begin
    result := BasePixel;
    exit; //========================>> exit >>=================>>
  end;

  if TopPixel.A = $FF then
  begin
    result := TopPixel;
    exit; //========================>> exit >>=================>>
  end;

  result.A := TopPixel.A;

  FA := TopPixel.A;

  Af := @DivTable[FA];
  Ab := @DivTable[not FA];


  {$IFOPT R+}
    {$DEFINE ToggleRangeCheck}
    {$R-}
  {$ELSE}
    {$UNDEF ToggleRangeCheck}
  {$ENDIF}

  Result.B := Af[TopPixel.B] + Ab[BasePixel.B];
  Result.G := Af[TopPixel.G] + Ab[BasePixel.G];
  Result.R := Af[TopPixel.R] + Ab[BasePixel.R];

  {$IFDEF ToggleRangeCheck}
    {$R+}
    {$UNDEF ToggleRangeCheck}
  {$ENDIF}
end;

function AlphaBlend2(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
begin
  if TopPixel.A = 0 then
  begin
    result := BasePixel;
    exit; //========================>> exit >>=================>>
  end;

  if TopPixel.A = $FF then
  begin
    result := TopPixel;
    exit; //========================>> exit >>=================>>
  end;

  result.B := ByteCrossFade(BasePixel.B, TopPixel.B, TopPixel.A);
  result.G := ByteCrossFade(BasePixel.G, TopPixel.G, TopPixel.A);
  result.R := ByteCrossFade(BasePixel.R, TopPixel.R, TopPixel.A);
  result.A := ByteCrossFade(BasePixel.A, TopPixel.A, TopPixel.A);
end;

function AlphaBlend3(const BasePixel, TopPixel : T32BitPixel; TopAlpha:byte):T32BitPixel;
var
  ModifiedAlpha : byte;
begin
  ModifiedAlpha := TopPixel.A * TopAlpha div 255;

  if ModifiedAlpha = 0 then
  begin
    result := BasePixel;
    exit; //========================>> exit >>=================>>
  end;

  if ModifiedAlpha = $FF then
  begin
    result := TopPixel;
    exit; //========================>> exit >>=================>>
  end;

  result.B := ByteCrossFade(BasePixel.B, TopPixel.B, ModifiedAlpha);
  result.G := ByteCrossFade(BasePixel.G, TopPixel.G, ModifiedAlpha);
  result.R := ByteCrossFade(BasePixel.R, TopPixel.R, ModifiedAlpha);
  result.A := ByteCrossFade(BasePixel.A, TopPixel.A, ModifiedAlpha);
end;

function CopySource(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
begin
  result := TopPixel;
end;

function RandomPixel(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
begin
  //result.B := random(255);
  //result.G := random(255);
  //result.R := random(255);

  result.B := TopPixel.B;
  result.G := TopPixel.G;
  result.R := TopPixel.R;
  result.A := 255;
end;

function Copy_InvertAlpha(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
begin
  result.B := TopPixel.B;
  result.G := TopPixel.G;
  result.R := TopPixel.R;
  result.A := 255 - TopPixel.A;
end;

function Copy_WinText(const BasePixel, TopPixel : T32BitPixel):T32BitPixel;
//var
//  Alpha : byte;
begin
  result.R := 0;
  result.G := 0;
  result.B := 0;
  result.A := (255 - TopPixel.G);

  result := TopPixel;
  result.A := 255;


  {
  Alpha := 255 - TopPixel.G;

  result.B := ByteCrossFade(BasePixel.B, 0, topPixel.B);
  result.G := ByteCrossFade(BasePixel.G, 0, topPixel.G);
  result.R := ByteCrossFade(BasePixel.R, 0, topPixel.R);
  result.A := ByteCrossFade(BasePixel.A, 255, Alpha);
  }


  {
  Alpha := 255 - TopPixel.G;

  result.B := topPixel.B;
  result.G := topPixel.G;
  result.R := topPixel.R;
  result.A := 255 - topPixel.A;
  }
end;

{
procedure BlendMem_Pas(F: TColor32; var B: TColor32);
var
  FX: TColor32Entry absolute F;
  BX: TColor32Entry absolute B;
  Af, Ab: PByteArray;
  FA : Byte;
begin
  FA := FX.A;

  if FA = 0 then Exit;

  if FA = $FF then
  begin
    B := F;
    Exit;
  end;

  with BX do
  begin
    Af := @DivTable[FA];
    Ab := @DivTable[not FA];
    R := Af[FX.R] + Ab[R];
    G := Af[FX.G] + Ab[G];
    B := Af[FX.B] + Ab[B];
  end;
end;
}
//===========================================================


procedure MakeMergeTables;
var
  x : integer;
  I, J: Integer;
const
  OneByteth : Double = 1 / 255;
begin
  for J := 0 to 255 do
    for I := 0 to 255 do
    begin
      DivTable[I, J] := Round(I * J * OneByteth);
      if I > 0
        then x := Round(J * (255 / I))
        else x := 0;
      if x > 255
        then RcTable[I, J] := 255
        else RcTable[I, J] := x;
    end;
end;



procedure BlitImage(Dest:TBlitDestInfo; Src:TBlitSrcInfo; const BlendFunc:TPixelBlendFunction);
var
  SrcPixel, DstPixel : P32BitPixel;
  c2: Integer;
  c1: Integer;

  SrcLineIndex : P32BitPixel;
  DstLineIndex : P32BitPixel;

  SrcStride : Integer;
  DstStride : Integer;

  CopyBoundX1, CopyBoundY1, CopyBoundX2, CopyBoundY2 : integer;
  CopyWidth, CopyHeight : integer;
begin
  CopyBoundX1 := Src.X1;
  CopyBoundX2 := Src.X2;
  CopyBoundY1 := Src.Y1;
  CopyBoundY2 := Src.Y2;

  CopyWidth  := CopyBoundX2 - CopyBoundX1;
  CopyHeight := CopyBoundY2 - CopyBoundY1;

  if CopyWidth > Dest.BufferWidth - Dest.X1 then CopyWidth := Dest.BufferWidth - Dest.X1;
  if CopyHeight > Dest.BufferHeight - Dest.Y1 then CopyHeight := Dest.BufferHeight - Dest.Y1;

  if Dest.X1 < 0 then
  begin
    CopyWidth := CopyWidth + Dest.X1;
    Dest.X1 := Dest.X1 - Dest.X1;
    Dest.X1 := 0;
  end;

  if Dest.Y1 < 0 then
  begin
    CopyHeight := CopyHeight + Dest.Y1;
    Dest.Y1 := Dest.Y1 - Dest.Y1;
    Dest.Y1 := 0;
  end;

  SrcStride := -Src.BufferWidth;
  DstStride := -Dest.BufferWidth;

  SrcLineIndex := Src.Buffer;
  inc(SrcLineIndex, Src.BufferWidth * (Src.BufferHeight-1));
  inc(SrcLineIndex, Src.Y1 * SrcStride + Src.X1);

  DstLineIndex := Dest.Buffer;
  inc(DstLineIndex, Dest.BufferWidth * (Dest.BufferHeight-1));
  inc(DstLineIndex, Dest.Y1 * DstStride + Dest.X1);

  for c1 := 0 to CopyHeight-1 do
  begin
    SrcPixel := SrcLineIndex;
    DstPixel := DstLineIndex;
    inc(SrcLineIndex, SrcStride);
    inc(DstLineIndex, DstStride);
    for c2 := 0 to CopyWidth-1 do
    begin
      DstPixel^ := BlendFunc(DstPixel^, SrcPixel^);
      inc(SrcPixel);
      inc(DstPixel);
    end;
  end;
end;


initialization
  MakeMergeTables;


end.
