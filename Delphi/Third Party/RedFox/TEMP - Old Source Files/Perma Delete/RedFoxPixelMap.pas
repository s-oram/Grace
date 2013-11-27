unit RedFoxPixelMap;

interface

uses
  Windows,
  AggWin32Bmp;

type
  TRedFoxPixelMap = class(TPixelMap)
  private
  public
     procedure BuildFromBitmap(Bmp: PBITMAPINFO);
  end;

implementation

{ TRedFoxPixelMap }

procedure TRedFoxPixelMap.BuildFromBitmap(Bmp: PBITMAPINFO);
begin
  FreeBitmap;
  FBitsPerPixel := Bmp^.bmiHeader.biBitCount;
  CreateFromBitmap(Bmp);
  CreateGrayScalePalette(Bmp);
  FIsInternal := false;
end;

end.
