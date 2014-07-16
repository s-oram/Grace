unit RedFoxImageBuffer;

interface

uses
  WinApi.Windows,
  Graphics,
  Agg2D,
  AggWin32Bmp,
  RedFox,
  RedFox2D,
  RedFoxColor;

type
  TRedFoxImageBuffer = class
  private
    fBufferInterface: TAgg2D;
    fBufferAsImage: TAgg2dImage;
    fWidth: Cardinal;
    fHeight: Cardinal;
    fRedFox2D: TRedFox2D;
    fBitmap : TBitmap;
    fHandle: HDC;

    procedure SetHeight(const aHeight: Cardinal);
    procedure SetWidth(const aWidth: Cardinal);
  protected
    procedure Internal_TransformImage(aBitmap: Graphics.TBitmap; SrcX1, SrcY1, SrcX2, SrcY2:integer; DstX1, DstY1:double); overload; inline;
    procedure Internal_TransformImage(aBitmap: TAgg2dImage; SrcX1, SrcY1, SrcX2, SrcY2:integer; DstX1, DstY1:double); overload; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(aWidth, aHeight: Cardinal);

    //======== Text Drawing Methods =======================
    function CalcActualTextBounds(const Text:string; const Font:TFont; const HorzAlign, VertAlign:TRedFoxAlign; const TextBounds:TRect):TRect;
    procedure DrawText(const Text:string; const Font:TFont; const HorzAlign, VertAlign:TRedFoxAlign; const TextBounds:TRect); overload;
    procedure DrawText(const Text:string; const Font:TFont; const HorzAlign, VertAlign:TRedFoxAlign; const TextBounds:TRect; const Color:TRedFoxColor); overload;
    // TODO: Should consider implementing the method below.
    //procedure DrawText(const Text:string; const Font:TFont; const HorzAlign, VertAlign:TRedFoxAlignment; const TextBounds:TRect; const Color:TColor; const Opacity :byte = 255);

    // NOTE: These text methods are used by some components but generally the
    // DrawText() methods should be used where possible. (Perhaps these will be
    // made protected at some stage.
    procedure UpdateFont(Source:TFont); overload;
    procedure TextOut(x, y: single; Text:string); overload; //TODO: This needs work!!
    procedure TextOut(const x, y:single; const Text : string; const Font:TFont; const aColor : TRedFoxColor) overload;
    procedure TextOutAlt(x, y: single; Text:string);

    function TextWidth(Text:string):double; overload; inline;
    function TextWidth(const Text:string; const Font : TFont):double; overload; inline;
    function TextHeight:double; inline;

    function AutoTrimTextToFitBufferWidth(const Text : string; const TextMargin : integer = 0):string;


    //======== Bitmap Drawing Methods =======================
    procedure TransformImage(aBitmap: Graphics.TBitmap); overload;
    procedure TransformImage(aBitmap: Graphics.TBitmap; DstX1, DstY1:single); overload;
    procedure TransformImage(aBitmap: Graphics.TBitmap; SrcX1, SrcY1, SrcX2, SrcY2:integer; DstX1, DstY1:double); overload;
    procedure TransformImage(aBitmap: TAgg2dImage); overload;
    procedure TransformImage(const aBitmap: TAgg2dImage; const DstX1, DstY1:single); overload;

    //NOTE: Maybe Draw() could be depreciated?....
    procedure Draw(x,y:integer; aBitmap:Graphics.TBitmap);  deprecated;

    // Draw the internal PixelMap data to an external canvas object.
    procedure DrawTo(CanvasHandleDC: HDC); overload;
    procedure DrawTo(CanvasHandleDC: HDC; DestX, DestY : integer); overload;

    // Alpha blends the image buffer to a canvas.
    // This is a temporary method and will be replaced.
    procedure AlphaBlendTo(DestHandle: HDC; DestX, DestY, DestWidth, DestHeight:integer); deprecated;


    property BufferInterface : TAgg2D      read fBufferInterface write fBufferInterface;
    property RedFoxInterface : TRedFox2D   read fRedFox2D        write fRedFox2D;
    property AsImage         : TAgg2dImage read fBufferAsImage   write fBufferAsImage;

    property Handle : HDC read fHandle;

    property Width  : Cardinal read fWidth  write SetWidth;
    property Height : Cardinal read fHeight write SetHeight;
  end;

implementation

uses
  VamLib.Utils,
  Types,
  SysUtils, AggBasics, AggPixelFormat, AggColor,
  RedFoxBlend;

function LastError: string;
// NOTE: This method was originally copied from TobyBears DIBControls library.
// Source: cDIB.pas
var
  OutputMessage: PChar;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    nil,
    GetLastError,
    0, @OutputMessage,
    0,
    nil);

  Result := string(OutputMessage);
end;

{ TRedFoxBackBuffer }

procedure CopyBitmapPixelToPixelMap(SrcPixel, DstPixel:PInt8u);
begin
  DstPixel^ := SrcPixel^;     //Blue (i think)
  inc(SrcPixel);
  inc(DstPixel);

  DstPixel^ := SrcPixel^;    //Green (i think)
  inc(SrcPixel);
  inc(DstPixel);

  DstPixel^ := SrcPixel^;    //Red (i think)
  //inc(SrcPixel);
  inc(DstPixel);

  //Set the alpha level
  DstPixel^ := 255;
end;

constructor TRedFoxImageBuffer.Create;
begin
  fWidth := 0;
  fHeight := 0;
  fHandle := 0;

  fBitmap := TBitmap.Create;
  RedFoxInterface := TRedFox2D.Create;
  BufferInterface := TAgg2d.Create;

  SetSize(1,1);
end;

destructor TRedFoxImageBuffer.Destroy;
begin
  if assigned(fBufferAsImage) then fBufferAsImage.Free;
  BufferInterface.Free;
  RedFoxInterface.Free;
  fBitmap.Free;

  inherited;
end;


procedure TRedFoxImageBuffer.DrawTo(CanvasHandleDC: HDC);
var
  aCanvas : TCanvas;
begin
  if (CanvasHandleDC <> 0) and (assigned(fBitmap)) and (fBitmap.Width > 0) and (fBitmap.Height > 0) then
  begin
    //BitBlt(CanvasHandleDC,0,0,Width,Height, fBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    aCanvas := TCanvas.Create;
    aCanvas.Handle := CanvasHandleDC;
    aCanvas.Lock;
    try
      aCanvas.Draw(0,0,fBitmap);
    finally
      aCanvas.Unlock;
      aCanvas.Free;
    end;
  end;
end;

procedure TRedFoxImageBuffer.DrawTo(CanvasHandleDC: HDC; DestX, DestY: integer);
var
  aCanvas : TCanvas;
begin
  if (CanvasHandleDC <> 0) and (assigned(fBitmap)) and (fBitmap.Width > 0) and (fBitmap.Height > 0) then
  begin
    //BitBlt(CanvasHandleDC,0,0,Width,Height, fBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    aCanvas := TCanvas.Create;
    aCanvas.Handle := CanvasHandleDC;
    aCanvas.Lock;
    try
      aCanvas.Draw(DestX,DestY,fBitmap);
    finally
      aCanvas.Unlock;
      aCanvas.Free;
    end;
  end;
end;








procedure TRedFoxImageBuffer.SetHeight(const aHeight: Cardinal);
begin
  if aHeight <> fHeight then
  begin
    fHeight := aHeight;
    SetSize(fWidth, fHeight);
  end;
end;

procedure TRedFoxImageBuffer.SetSize(aWidth, aHeight: Cardinal);
var
  Buffer : PInt8u;
  Wx, Hx : cardinal;
  Stride : integer;
begin
  if aWidth  < 1 then aWidth  := 1;
  if aHeight < 1 then aHeight := 1;
  fWidth  := aWidth;
  fHeight := aHeight;

  fBitmap.PixelFormat := pf32Bit;
  fBitmap.SetSize(aWidth, aHeight);

  fHandle := fBitmap.Canvas.Handle;

  Buffer := fBitmap.ScanLine[fBitmap.Height-1];
  Wx := fBitmap.Width;
  Hx := fBitmap.Height;
  // AFAIK Stride is the pixel width * pixel byte size. (4 bytes for 32 bit pixels)
  Stride := -Wx * 4;

  RedFoxInterface.Attach(Buffer, wx, hx, Stride);
  BufferInterface.Attach(Buffer, wx, hx, Stride);

  if not assigned(AsImage)
    then AsImage := TAgg2dImage.Create(Buffer, wx, hx, Stride)
    else AsImage.Attach(Buffer, wx, hx, Stride);


end;

procedure TRedFoxImageBuffer.SetWidth(const aWidth: Cardinal);
begin
  if aWidth <> fWidth then
  begin
    fWidth := aWidth;
    SetSize(fWidth, fHeight);
  end;
end;


function TRedFoxImageBuffer.TextHeight: double;
begin
  result := BufferInterface.FontHeight;
end;

procedure TRedFoxImageBuffer.TextOut(x, y: single; Text: string); deprecated;
begin
  // This method doesn't always work because UpdateFont() needs to be called somewhere.
  // Maybe it would be possible to only call UpdateFont() when the font changes.
  // At the moment it is being called each time text is drawn to the screen.
  // There is also the native windows text out methods which would
  // be worth investigating. The native windows methods have better anti-alaising.
  if Text <> '' then BufferInterface.Text(x, y, AnsiString(Text));
end;

procedure TRedFoxImageBuffer.TextOut(const x, y: single; const Text : string; const Font: TFont; const aColor: TRedFoxColor);
begin
  if Text <> '' then
  begin
    UpdateFont(Font);
    BufferInterface.LineColor := aColor;
    BufferInterface.FillColor := aColor;
    BufferInterface.TextAlignment(tahLeft, tavTop);
    BufferInterface.Text(x, y, AnsiString(Text));
  end;
end;

procedure TRedFoxImageBuffer.TextOutAlt(x, y: single; Text: string);
begin
  //if Text <> '' then BufferInterface.TextAlt(x, y, Text);
end;

function TRedFoxImageBuffer.TextWidth(Text: string): double;
begin
  result := BufferInterface.TextWidth(AnsiString(Text));
end;

function TRedFoxImageBuffer.TextWidth(const Text: string; const Font: TFont): double;
begin
  UpdateFont(Font);
  result := BufferInterface.TextWidth(AnsiString(Text));
end;





procedure TRedFoxImageBuffer.Draw(x, y: integer; aBitmap: Graphics.TBitmap);
var
  Dstx1, Dstx2, Dsty1, Dsty2 : integer;
  CopyWidth, CopyHeight : integer;
  c1, c2 : integer;
  SrcPixel : PInt8u;
  DstPixel : PInt8u;
  SrcOffsetX, SrcOffsetY : integer;
begin
  assert(fBitmap.PixelFormat = pf32Bit);
  assert(assigned(aBitmap));
  assert(aBitmap.PixelFormat = TPixelFormat.pf32bit);
  if aBitmap.PixelFormat <> TPixelFormat.pf32bit then raise Exception.Create('aBitmap.PixelFormat must be 32 bits!');

  if (x + aBitmap.Width >= 0) and (y + aBitmap.Height >= 0) and (x < CastToInteger(Width)) and (y < CastToInteger(Height)) then
  begin
    // Calculate copy source x-y offset positions.
    if x < 0
      then SrcOffsetX := x * -1
      else SrcOffsetX := 0;

    if y < 0
      then SrcOffsetY := y * -1
      else SrcOffsetY := 0;

    // Calculate the bounds of where the bitmap will be copied to.
    Dstx1 := x;
    Dsty1 := y;
    Dstx2 := x + aBitmap.Width;
    Dsty2 := y + aBitmap.Height;

    if Dstx1 < 0 then Dstx1 := 0;
    if Dsty1 < 0 then Dsty1 := 0;
    if Dstx2 > CastToInteger(Width)  then Dstx2 := CastToInteger(Width);
    if Dsty2 > CastToInteger(Height) then Dsty2 := CastToInteger(Height);

    CopyWidth  := Dstx2 - Dstx1;
    CopyHeight := Dsty2 - Dsty1;

    //==== Copy the image ====
    for c1 := 0 to CopyHeight-1 do
    begin
      //Find the source pixel to start copying from.
      SrcPixel := aBitmap.ScanLine[SrcOffsetY + c1];
      inc(SrcPixel, SrcOffsetX * 4);

      //Find the destination pixel to start copying to.
      DstPixel := BufferInterface.Row[Dsty1 + c1];
      inc(DstPixel, Dstx1 * 4);

      //copy the scan line from source to destination.
      for c2 := 0 to CopyWidth-1 do
      begin
        CopyBitmapPixelToPixelMap(SrcPixel, DstPixel);
        inc(SrcPixel, 4);
        inc(DstPixel, 4);
      end;
    end;

  end;
end;

procedure TRedFoxImageBuffer.TransformImage(aBitmap: TAgg2dImage);
begin
  Internal_TransformImage(aBitmap, 0, 0, aBitmap.Width, aBitmap.Height, 0, 0);
end;

procedure TRedFoxImageBuffer.TransformImage(const aBitmap: TAgg2dImage; const DstX1, DstY1: single);
begin
  Internal_TransformImage(aBitmap, 0, 0, aBitmap.Width, aBitmap.Height, DstX1, DstY1);
end;


procedure TRedFoxImageBuffer.TransformImage(aBitmap: Graphics.TBitmap);
begin
  Internal_TransformImage(aBitmap, 0, 0, aBitmap.Width, aBitmap.Height, 0, 0);
end;

procedure TRedFoxImageBuffer.TransformImage(aBitmap: Graphics.TBitmap; DstX1, DstY1: single);
begin
  Internal_TransformImage(aBitmap, 0, 0, aBitmap.Width, aBitmap.Height, DstX1, DstY1);
end;

procedure TRedFoxImageBuffer.TransformImage(aBitmap: Graphics.TBitmap; SrcX1, SrcY1, SrcX2, SrcY2:integer; DstX1, DstY1:double);
begin
  Internal_TransformImage(aBitmap, SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1);
end;

procedure TRedFoxImageBuffer.Internal_TransformImage(aBitmap: TAgg2dImage; SrcX1, SrcY1, SrcX2, SrcY2: integer; DstX1, DstY1: double);
var
  CopyWidth, CopyHeight : integer;
begin
  CopyWidth  := SrcX2 - SrcX1;
  CopyHeight := SrcY2 - SrcY1;
  BufferInterface.TransformImage(aBitmap, SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1, DstX1 + CopyWidth, DstY1 + CopyHeight);
end;




procedure TRedFoxImageBuffer.AlphaBlendTo(DestHandle: HDC; DestX, DestY, DestWidth, DestHeight:integer);
var
  TempBitmap: TBitmap;
  BlendFunction: TBlendFunction;
begin
  // NOTE: This function copies the bitmap data to a temporary TBitmap object.
  // Then it sets "AlphaFormat" so the bitmap is using premultiplied alpha color
  // values. From there it is blit'ed to the destination.
  //
  // If RedFoxImage buffer was using pre-multiplied alpha color values - or -
  // there was a way to copy and translate the pixel data in one step, the duplication
  // step could be avoided.


  TempBitmap := TBitmap.Create;
  try
    TempBitmap.PixelFormat := pf32bit;
    TempBitmap.Width := self.Width;
    TempBitmap.Height := self.Height;
    DrawTo(TempBitmap.Canvas.Handle);
    //Important: Set afPreMultiplied after setting pixel data.
    TempBitmap.AlphaFormat := afPreMultiplied;

    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 255;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;
    WinAlphaBlend(DestHandle, DestX, DestY, DestWidth, DestHeight, TempBitmap.Canvas.Handle, 0, 0, Width, Height, BlendFunction);
  finally
    TempBitmap.Free;
  end;

end;

procedure TRedFoxImageBuffer.Internal_TransformImage(aBitmap: Graphics.TBitmap; SrcX1, SrcY1, SrcX2, SrcY2: integer; DstX1, DstY1: double);
var
  AggImage: TAgg2dImage;
  PixelBuffer : PInt8u;
  PixelWidth, PixelHeight : cardinal;
  PixelStride : integer;
  CopyWidth, CopyHeight : integer;
begin
  assert(assigned(aBitmap));
  assert(aBitmap.PixelFormat = TPixelFormat.pf32bit);
  if aBitmap.PixelFormat <> TPixelFormat.pf32bit then raise Exception.Create('aBitmap.PixelFormat must be 32 bits!');

  PixelWidth  := aBitmap.Width;
  PixelHeight := aBitmap.Height;

  {
  case aBitmap.PixelFormat of
    pf1bit:   PixelByteSize := 1;
    pf4bit:   PixelByteSize := 1;
    pf8bit:   PixelByteSize := 1;
    pf15bit:  PixelByteSize := 1;
    pf16bit:  PixelByteSize := 2;
    pf24bit:  PixelByteSize := 1;
    pf32bit:  PixelByteSize := 4;
  else
    raise Exception.Create('Unsupported pixel format.');
  end;
  PixelStride := -(PixelWidth * PixelByteSize);
  }

  PixelStride := -(PixelWidth * 4);

  PixelBuffer := aBitmap.ScanLine[aBitmap.Height-1];

  AggImage := TAgg2dImage.Create(PixelBuffer, PixelWidth, PixelHeight, PixelStride);
  try
    CopyWidth  := SrcX2 - SrcX1;
    CopyHeight := SrcY2 - SrcY1;
    BufferInterface.TransformImage(AggImage, SrcX1, SrcY1, SrcX2, SrcY2, DstX1, DstY1, DstX1+CopyWidth, DstY1+CopyHeight);
  finally
    AggImage.Free;
  end;
end;

function TRedFoxImageBuffer.AutoTrimTextToFitBufferWidth(const Text: string; const TextMargin : integer = 0): string;
var
  tw : single;
  TestString : string;
  HorizontalEllipsis : string;
begin
  // IMPORTANT: Call UpdateFont() before this method to ensure the buffer is using the
  // correct font and font size.

  if Width > 1 then
  begin
    TestString := Text;

    tw := TextWidth(TestString);

    if (tw + TextMargin >= Width) then
    begin
      HorizontalEllipsis := AnsiChar(133); // ...

      while (tw + TextMargin >= Width) do
      begin
        if Length(TestString) = 0 then break;
        Delete(TestString, Length(TestString), 1); //delete the last charactor.
        tw := TextWidth(TestString + HorizontalEllipsis);
      end;
      TestString := Trim(TestString);
      TestString := TestString + HorizontalEllipsis;
    end;
  end else
  begin
    TestString := '';
  end;

  result := TestString;
end;

function TRedFoxImageBuffer.CalcActualTextBounds(const Text: string; const Font: TFont; const HorzAlign, VertAlign: TRedFoxAlign; const TextBounds: TRect): TRect;
var
  th, tw:single;
  xPos, yPos : single;
begin
  UpdateFont(Font);
  BufferInterface.TextAlignment(tahLeft, tavCenter);

  th := self.TextHeight;
  tw := self.TextWidth(Text);

  case HorzAlign of
    AlignNear:   xPos := TextBounds.Left;
    AlignCenter: xPos := (TextBounds.Width - tw) * 0.5 + TextBounds.Left;
    AlignFar:    xPos := TextBounds.Right - tw;
  else
    xPos := 0;
  end;

  case VertAlign of
    AlignNear:   yPos := TextBounds.Top + (th * 0.5);
    AlignCenter: yPos := (TextBounds.Height) * 0.5 + TextBounds.Top;
    AlignFar:    yPos := TextBounds.Bottom - (th * 0.5);
  else
    yPos := 0;
  end;

  result.Left   := round(xPos);
  result.Right  := round(xPos + tw);
  result.Top    := round(yPos - (th * 0.5));
  result.Bottom := round(yPos + (th * 0.5));
end;




procedure TRedFoxImageBuffer.DrawText(const Text: string; const Font: TFont; const HorzAlign, VertAlign: TRedFoxAlign; const TextBounds:TRect);
var
  th, tw:single;
  xPos, yPos : single;
begin
  UpdateFont(Font);
  BufferInterface.TextAlignment(tahLeft, tavCenter);

  th := self.TextHeight;
  tw := self.TextWidth(Text);

  case HorzAlign of
    AlignNear:   xPos := TextBounds.Left;
    AlignCenter: xPos := (TextBounds.Width - tw) * 0.5 + TextBounds.Left;
    AlignFar:    xPos := TextBounds.Right - tw;
  else
    xPos := 0;
  end;

  case VertAlign of
    AlignNear:   yPos := TextBounds.Top + (th * 0.5);
    AlignCenter: yPos := (TextBounds.Height) * 0.5 + TextBounds.Top;
    AlignFar:    yPos := TextBounds.Bottom - (th * 0.5);
  else
    yPos := 0;
  end;

  BufferInterface.Text(xPos, yPos, AnsiString(Text));
end;

procedure TRedFoxImageBuffer.DrawText(const Text: string; const Font: TFont; const HorzAlign, VertAlign: TRedFoxAlign; const TextBounds: TRect; const Color: TRedFoxColor);
var
  th, tw:single;
  xPos, yPos : single;
begin
  UpdateFont(Font);
  BufferInterface.LineColor := Color.AsAggRgba8;
  BufferInterface.FillColor := Color.AsAggRgba8;
  BufferInterface.TextAlignment(tahLeft, tavCenter);

  th := self.TextHeight;
  tw := self.TextWidth(Text);

  case HorzAlign of
    AlignNear:   xPos := TextBounds.Left;
    AlignCenter: xPos := (TextBounds.Width - tw) * 0.5 + TextBounds.Left;
    AlignFar:    xPos := TextBounds.Right - tw;
  else
    xPos := 0;
  end;

  case VertAlign of
    AlignNear:   yPos := TextBounds.Top + (th * 0.5);
    AlignCenter: yPos := (TextBounds.Height) * 0.5 + TextBounds.Top;
    AlignFar:    yPos := TextBounds.Bottom - (th * 0.5);
  else
    yPos := 0;
  end;

  BufferInterface.Text(xPos, yPos, AnsiString(Text));
end;




procedure TRedFoxImageBuffer.UpdateFont(Source: TFont);
var
  IsItalic, IsBold : boolean;
  Buffer : PInt8u;
  Wx, Hx : cardinal;
  Stride : integer;
begin
  Buffer := fBitmap.ScanLine[fBitmap.Height-1];
  Wx := fBitmap.Width;
  Hx := fBitmap.Height;
  // AFAIK Stride is the pixel width * pixel byte size. (4 bytes for 32 bit pixels)
  Stride := -Wx * 4;

  BufferInterface.Attach(Buffer, wx, Hx, Stride);

  BufferInterface.FlipText  := true;
  BufferInterface.LineColor := GetAggColor(Source.Color);
  BufferInterface.FillColor := GetAggColor(Source.Color);
  IsItalic := (TFontStyle.fsItalic in Source.Style);
  IsBold   := (TFontStyle.fsBold   in Source.Style);
  BufferInterface.Font(ToPAnsiChar(Source.Name), abs(Source.Height), IsBold, IsItalic, TAggFontCache.fcRaster, 0);
  BufferInterface.FlipText  := true;
end;





end.
