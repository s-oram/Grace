unit Lucidity.SampleImageRenderer;

interface

uses
  Lucidity.SampleMap,
  VamLib.Graphics;

type
  TSampleImageRenderer = class
  private
  public
    constructor Create;
    destructor Destroy; override;

    function RenderSample(const aSampleRegion : IRegion; const aWidth, aHeight:integer):IInterfacedBitmap;
  end;

implementation

uses
  Graphics;

{ TSampleImageRenderer }

constructor TSampleImageRenderer.Create;
begin

end;

destructor TSampleImageRenderer.Destroy;
begin

  inherited;
end;

function TSampleImageRenderer.RenderSample(const aSampleRegion: IRegion; const aWidth, aHeight:integer): IInterfacedBitmap;
var
  Bitmap : IInterfacedBitmap;
begin
  Bitmap := TInterfacedBitmap.Create;

  Bitmap.Bitmap.PixelFormat := pf32Bit;
  Bitmap.Bitmap.SetSize(aWidth, aHeight);

  Bitmap.Bitmap.Canvas.Pen.Color := clRed;
  Bitmap.Bitmap.Canvas.Pen.Style := TPenStyle.psSolid;
  Bitmap.Bitmap.Canvas.Pen.Width := 2;

  Bitmap.Bitmap.Canvas.MoveTo(0,0);
  Bitmap.Bitmap.Canvas.LineTo(aWidth, aHeight);

  Bitmap.Bitmap.Canvas.MoveTo(aWidth,0);
  Bitmap.Bitmap.Canvas.LineTo(0, aHeight);


  result := Bitmap;
end;

end.
