unit RedFox.PngImageLoader;

interface

uses
  Graphics;

procedure LoadPngImageToBitmap(SourcePngFileName:string; var Dest:TBitmap);


implementation

uses
  SysUtils,
  Vcl.Imaging.PngImage;

procedure SwapPixelByteOrder(aBitmap:TBitmap);
type
  // Define a generic 4 byte pixel so we can access the individual bytes of
  // each 4 byte pixel.
  PPixel = ^TPixel;
  TPixel = record
    b1, b2, b3, b4 : byte;
  end;
var
  c1: Integer;
  c2: Integer;
  SrcPixel  : PPixel;
  TempPixel : TPixel;
begin
  // TBitmap and AggPasMod use a different pixel order.
  // I think AggPasMod is BGRA and TBitmap is RGBA.
  // If not, it's the other way round.
  // When using AggPasMod, we need to re-order the pixel bytes so the pixel
  // order is compatible.
  for c1 := 0 to aBitmap.Height-1 do
  begin
    SrcPixel := aBitmap.ScanLine[c1];
    for c2 := 0 to aBitmap.Width-1 do
    begin
      TempPixel := SrcPixel^;

      //Notice the byte re-ordering here!
      SrcPixel^.b1 := TempPixel.b3;
      SrcPixel^.b2 := TempPixel.b2;
      SrcPixel^.b3 := TempPixel.b1;
      SrcPixel^.b4 := TempPixel.b4;

      inc(SrcPixel);
    end;
  end;
end;

procedure ResetAlphaMask(aBitmap:TBitmap);
type
  // Define a generic 4 byte pixel so we can access the individual bytes of
  // each 4 byte pixel.
  PPixel = ^TPixel;
  TPixel = record
    b1, b2, b3, b4 : byte;
  end;
var
  c1: Integer;
  c2: Integer;
  SrcPixel  : PPixel;
begin
  for c1 := 0 to aBitmap.Height-1 do
  begin
    SrcPixel := aBitmap.ScanLine[c1];
    for c2 := 0 to aBitmap.Width-1 do
    begin
      SrcPixel^.b4 := 255;
      inc(SrcPixel);
    end;
  end;
end;



procedure LoadPngImageToBitmap(SourcePngFileName:string; var Dest:TBitmap);
var
  PngImage : TPngImage;
begin
  PngImage := TPngImage.Create;
  try
    if FileExists(SourcePngFileName) then
    begin
      PngImage.LoadFromFile(SourcePngFileName);
      PngImage.AssignTo(Dest);

      if Dest.PixelFormat <> TPixelFormat.pf32bit then
      begin
        Dest.PixelFormat := TPixelFormat.pf32bit;
        ResetAlphaMask(Dest);
      end;

      SwapPixelByteOrder(Dest);
    end;

  finally
    PngImage.Free;
  end;
end;

end.
