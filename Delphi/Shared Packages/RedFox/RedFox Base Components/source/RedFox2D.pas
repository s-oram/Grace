unit RedFox2D;

interface



uses
  Types,
  Graphics,
  Agg2D,
  AggWin32Bmp,
  RedFox,
  RedFoxColor;




type
   {============================================================================
   TRedFox2D has some similarities to the TAgg2D class. It provides methods
   for drawing on an arbitrary buffer of 32 bit pixel data.

   TRedFox2D uses a pixel alpha blending function taken from the Graphics32
   project.

   TRedFox2D's image blending functions have less functionality then the
   equivlent TAgg2D methods. TRedFox2D doesn't allow an image to be resized and
   only works on 32 bit pixel data.

   More positively TRedFox2D's image blending functions are close to three times
   quicker in my tests. I'm not sure why TRedFox2D is faster then TAgg2D. It
   could be my machine, the Graphics32 blend function, the reduceded
   functionality or something else entirely.

   I used the first Graphics32 pixel blend function I found. It would probably
   be worth investigating other blending functions. There may be more speedups
   or better blending behaviour available.
   =============================================================================}

  TRedFox2D = class
  private
    fBuffer : Pointer;
    fWidth  : integer;
    fHeight : integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Attach(aBuffer:Pointer; BufferWidth, BufferHeight:cardinal; Stride:integer); overload;

    procedure ClearAll(R, G, B: Cardinal; A: Cardinal = 255); overload;
    procedure ClearAll(Color : TRedFoxColor); overload;

    procedure SetImageAlpha(Alpha : cardinal);

    procedure InvertAlpha;

    //CopyImage() replaces the destination pixel data with the source pixel data.
    procedure CopyImage(const Source : TPixelMap; SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1 : integer);

    //BleadImage() blends the source image over the destination image using the source aplha layer.
    procedure BlendImage(const Source : TPixelMap;        SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1 : integer); overload;
    procedure BlendImage(const aBitmap: Graphics.TBitmap; SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1 : integer); overload;

    procedure BlendTo(const Dest: TRedFox2D; DestX1, DestY1 : integer); overload;
    procedure BlendTo(const Dest: TRedFox2D; const SrcX1, SrcY1, SrcX2, SrcY2, DestX1, DestY1: integer); overload;
    procedure DrawTo(const Dest:TRedFox2D; DestX1, DestY1 : integer);

    procedure DrawImage(const Source: Graphics.TBitmap);

    property Buffer : Pointer read fBuffer;
    property Width  : integer read fWidth;
    property Height : integer read fHeight;
  end;



// This blitting function was written to blit control images to main RedFoxContainer backbuffer.
procedure RedFox_AlphaBlit(const Dst, Src : TRedFox2d; SrcX1, SrcY1, SrcX2, SrcY2, DestX, DestY : integer; const SrcAlpha : byte);

implementation

uses
  AggBasics, AggPixelFormat, AggColor,
  SysUtils, RedFoxBlend;


procedure RedFox_AlphaBlit(const Dst, Src : TRedFox2d; SrcX1, SrcY1, SrcX2, SrcY2, DestX, DestY : integer; const SrcAlpha : byte);
var
  BlitSrc : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;

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
  BlitSrc.Buffer       := Src.Buffer;
  BlitSrc.BufferWidth  := Src.Width;
  BlitSrc.BufferHeight := Src.Height;
  BlitSrc.x1     := SrcX1;
  BlitSrc.y1     := SrcY1;
  BlitSrc.x2     := SrcX2;
  BlitSrc.y2     := SrcY2;

  BlitDest.Buffer       := Dst.Buffer;
  BlitDest.BufferWidth  := Dst.Width;
  BlitDest.BufferHeight := Dst.Height;
  BlitDest.x1           := DestX;
  BlitDest.y1           := DestY;




  CopyBoundX1 := BlitSrc.X1;
  CopyBoundX2 := BlitSrc.X2;
  CopyBoundY1 := BlitSrc.Y1;
  CopyBoundY2 := BlitSrc.Y2;

  CopyWidth  := CopyBoundX2 - CopyBoundX1;
  CopyHeight := CopyBoundY2 - CopyBoundY1;

  if CopyWidth  > BlitDest.BufferWidth - BlitDest.X1 then CopyWidth := BlitDest.BufferWidth - BlitDest.X1;
  if CopyHeight > BlitDest.BufferHeight - BlitDest.Y1 then CopyHeight := BlitDest.BufferHeight - BlitDest.Y1;

  if BlitDest.X1 < 0 then
  begin
    CopyWidth := CopyWidth + BlitDest.X1;
    BlitDest.X1 := BlitDest.X1 - BlitDest.X1;
    BlitDest.X1 := 0;
  end;

  if BlitDest.Y1 < 0 then
  begin
    CopyHeight := CopyHeight + BlitDest.Y1;
    BlitDest.Y1 := BlitDest.Y1 - BlitDest.Y1;
    BlitDest.Y1 := 0;
  end;

  SrcStride := -BlitSrc.BufferWidth;
  DstStride := -BlitDest.BufferWidth;

  SrcLineIndex := BlitSrc.Buffer;
  inc(SrcLineIndex, BlitSrc.BufferWidth * (BlitSrc.BufferHeight-1));
  inc(SrcLineIndex, BlitSrc.Y1 * SrcStride + BlitSrc.X1);

  DstLineIndex := BlitDest.Buffer;
  inc(DstLineIndex, BlitDest.BufferWidth * (BlitDest.BufferHeight-1));
  inc(DstLineIndex, BlitDest.Y1 * DstStride + BlitDest.X1);

  for c1 := 0 to CopyHeight-1 do
  begin
    SrcPixel := SrcLineIndex;
    DstPixel := DstLineIndex;
    inc(SrcLineIndex, SrcStride);
    inc(DstLineIndex, DstStride);
    for c2 := 0 to CopyWidth-1 do
    begin
      DstPixel^ := AlphaBlend3(DstPixel^, SrcPixel^, SrcAlpha);
      inc(SrcPixel);
      inc(DstPixel);
    end;
  end;


end;


{ TRedFox2D }

constructor TRedFox2D.Create;
begin
  fBuffer := nil;
end;

destructor TRedFox2D.Destroy;
begin

  inherited;
end;

procedure TRedFox2D.SetImageAlpha(Alpha: cardinal);
var
  DstPixel : P32BitPixel;
  c1: Integer;
begin
  assert(Alpha <= 255);

  DstPixel := fBuffer;
  for c1 := 0 to Height * Width -1 do
  begin
    DstPixel^.A := Alpha;
    inc(DstPixel);
  end;
end;


procedure TRedFox2D.InvertAlpha;
var
  DstPixel : P32BitPixel;
  c1: Integer;
begin
  DstPixel := fBuffer;
  for c1 := 0 to Height * Width -1 do
  begin
    DstPixel^.A := 255 - DstPixel^.A;
    inc(DstPixel);
  end;
end;

procedure TRedFox2D.Attach(aBuffer: Pointer; BufferWidth, BufferHeight: cardinal; Stride: integer);
begin
  fBuffer := aBuffer;
  fWidth := BufferWidth;
  fHeight := BufferHeight;
end;

procedure TRedFox2D.BlendImage(const aBitmap: Graphics.TBitmap; SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1: integer);
var
  BlitSrc : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
begin
  BlitSrc.Buffer := aBitmap.ScanLine[aBitmap.Height-1];
  BlitSrc.BufferWidth  := aBitmap.Width;
  BlitSrc.BufferHeight := aBitmap.Height;
  BlitSrc.x1     := SrcX1;
  BlitSrc.y1     := SrcY1;
  BlitSrc.x2     := SrcX2;
  BlitSrc.y2     := SrcY2;

  BlitDest.Buffer := Buffer;
  BlitDest.BufferWidth  := Width;
  BlitDest.BufferHeight := Height;
  BlitDest.x1     := DstX1;
  BlitDest.y1     := DstY1;

  BlitImage(BlitDest, BlitSrc, RedFoxBlend.AlphaBlend);
end;



procedure TRedFox2D.BlendImage(const Source: TPixelMap; SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1: integer);
var
  BlitSrc : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
begin
  BlitSrc.Buffer := Source.Buffer;
  BlitSrc.BufferWidth  := Source.Width;
  BlitSrc.BufferHeight := Source.Height;
  BlitSrc.x1     := SrcX1;
  BlitSrc.y1     := SrcY1;
  BlitSrc.x2     := SrcX2;
  BlitSrc.y2     := SrcY2;

  BlitDest.Buffer := Buffer;
  BlitDest.BufferWidth  := Width;
  BlitDest.BufferHeight := Height;
  BlitDest.x1     := DstX1;
  BlitDest.y1     := DstY1;

  BlitImage(BlitDest, BlitSrc, RedFoxBlend.AlphaBlend);
end;

procedure TRedFox2D.BlendTo(const Dest: TRedFox2D; DestX1, DestY1: integer);
var
  BlitSrc : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
begin
  BlitSrc.Buffer := Buffer;
  BlitSrc.BufferWidth  := Width;
  BlitSrc.BufferHeight := Height;
  BlitSrc.x1     := 0;
  BlitSrc.y1     := 0;
  BlitSrc.x2     := Width;
  BlitSrc.y2     := Height;

  BlitDest.Buffer := Dest.Buffer;
  BlitDest.BufferWidth  := Dest.Width;
  BlitDest.BufferHeight := Dest.Height;
  BlitDest.x1     := DestX1;
  BlitDest.y1     := DestY1;

  BlitImage(BlitDest, BlitSrc, RedFoxBlend.AlphaBlend);
end;

procedure TRedFox2D.BlendTo(const Dest: TRedFox2D; const SrcX1, SrcY1, SrcX2, SrcY2, DestX1, DestY1: integer);
var
  BlitSrc  : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
begin

  BlitSrc.Buffer := Buffer;
  BlitSrc.BufferWidth  := Width;
  BlitSrc.BufferHeight := Height;
  BlitSrc.x1     := SrcX1;
  BlitSrc.y1     := SrcY1;
  BlitSrc.x2     := SrcX2;
  BlitSrc.y2     := SrcY2;

  BlitDest.Buffer := Dest.Buffer;
  BlitDest.BufferWidth  := Dest.Width;
  BlitDest.BufferHeight := Dest.Height;
  BlitDest.x1     := DestX1;
  BlitDest.y1     := DestY1;

  BlitImage(BlitDest, BlitSrc, RedFoxBlend.AlphaBlend);
end;

procedure TRedFox2D.ClearAll(R, G, B, A: Cardinal);
var
  DstPixel : P32BitPixel;
  c1: Integer;
begin
  DstPixel := Buffer;
  for c1 := 0 to Height * Width -1 do
  begin
    DstPixel^.R := R;
    DstPixel^.G := G;
    DstPixel^.B := B;
    DstPixel^.A := A;
    inc(DstPixel);
  end;
end;

procedure TRedFox2D.ClearAll(Color: TRedFoxColor);
begin
  ClearAll(Color.R, Color.G, Color.B, Color.A);
end;

procedure TRedFox2D.CopyImage(const Source: TPixelMap; SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1: integer);
var
  BlitSrc : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
begin
  BlitSrc.Buffer := Source.Buffer;
  BlitSrc.BufferWidth  := Source.Width;
  BlitSrc.BufferHeight := Source.Height;
  BlitSrc.x1     := SrcX1;
  BlitSrc.y1     := SrcY1;
  BlitSrc.x2     := SrcX2;
  BlitSrc.y2     := SrcY2;

  BlitDest.Buffer := Buffer;
  BlitDest.BufferWidth  := Width;
  BlitDest.BufferHeight := Height;
  BlitDest.x1     := DstX1;
  BlitDest.y1     := DstY1;

  BlitImage(BlitDest, BlitSrc, RedFoxBlend.CopySource);
end;

procedure TRedFox2D.DrawImage(const Source: Graphics.TBitmap);
var
  BlitSrc : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
begin
  BlitSrc.Buffer       := Source.ScanLine[Source.Height-1];
  BlitSrc.BufferWidth  := Source.Width;
  BlitSrc.BufferHeight := Source.Height;
  BlitSrc.x1           := 0;
  BlitSrc.y1           := 0;
  BlitSrc.x2           := Source.Width;
  BlitSrc.y2           := Source.Height;

  BlitDest.Buffer       := Buffer;
  BlitDest.BufferWidth  := Width;
  BlitDest.BufferHeight := Height;
  BlitDest.x1           := 0;
  BlitDest.y1           := 0;

  //BlitImage(BlitDest, BlitSrc, RedFoxBlend.CopySource);
  //BlitImage(BlitDest, BlitSrc, RedFoxBlend.RandomPixel);
  BlitImage(BlitDest, BlitSrc, RedFoxBlend.Copy_WinText);

end;

procedure TRedFox2D.DrawTo(const Dest: TRedFox2D; DestX1, DestY1: integer);
var
  BlitSrc : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
begin
  BlitSrc.Buffer := Buffer;
  BlitSrc.BufferWidth  := Width;
  BlitSrc.BufferHeight := Height;
  BlitSrc.x1     := 0;
  BlitSrc.y1     := 0;
  BlitSrc.x2     := Width;
  BlitSrc.y2     := Height;

  BlitDest.Buffer := Dest.Buffer;
  BlitDest.BufferWidth  := Dest.Width;
  BlitDest.BufferHeight := Dest.Height;
  BlitDest.x1     := DestX1;
  BlitDest.y1     := DestY1;

  BlitImage(BlitDest, BlitSrc, RedFoxBlend.CopySource);
end;






end.
