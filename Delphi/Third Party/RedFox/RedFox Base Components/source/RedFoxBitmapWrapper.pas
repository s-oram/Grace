unit RedFoxBitmapWrapper;

interface

uses
  Graphics, Agg2D, RedFox2D;

type
  TRedFoxBitmapWrapper = class
  private
    fWidth: integer;
    fHeight: integer;
    fRedFox2D: TRedFox2D;
    fBufferInterface: TAgg2D;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Wrap(const aBitmap : Graphics.TBitmap);

    property BufferInterface : TAgg2D      read fBufferInterface write fBufferInterface;
    property RedFoxInterface : TRedFox2D   read fRedFox2D        write fRedFox2D;

    property Width : integer read fWidth;
    property Height : integer read fHeight;
  end;

implementation

uses
  AggBasics;

{ TRedFoxBitmapWrapper }

constructor TRedFoxBitmapWrapper.Create;
begin
  fWidth  := 0;
  fHeight := 0;

  RedFoxInterface := TRedFox2D.Create;
  BufferInterface := TAgg2d.Create;
end;

destructor TRedFoxBitmapWrapper.Destroy;
begin
  BufferInterface.Free;
  RedFoxInterface.Free;

  inherited;
end;

procedure TRedFoxBitmapWrapper.Wrap(const aBitmap: Graphics.TBitmap);
var
  Buffer : PInt8u;
  Wx, Hx : cardinal;
  Stride : integer;
begin
  aBitmap.PixelFormat := pf32Bit;

  Buffer := aBitmap.ScanLine[aBitmap.Height-1];
  Wx := aBitmap.Width;
  Hx := aBitmap.Height;
  // AFAIK Stride is the pixel width * pixel byte size. (4 bytes for 32 bit pixels)
  Stride := -Wx * 4;

  RedFoxInterface.Attach(Buffer, wx, hx, Stride);
  BufferInterface.Attach(Buffer, wx, hx, Stride);
end;

end.
