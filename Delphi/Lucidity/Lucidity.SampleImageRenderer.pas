unit Lucidity.SampleImageRenderer;

interface

uses
  RedFoxColor,
  Lucidity.SampleMap,
  VamLib.Graphics;

type
  TSampleImageRenderer = class
  private
    fWidth: cardinal;
    fHeight: cardinal;
    fLineColor: TRedFoxColorString;
  public
    constructor Create;
    destructor Destroy; override;

    function RenderSample(const aSampleRegion : IRegion):IInterfacedBitmap;


    // Image properties.
    property Width  : cardinal read fWidth  write fWidth;
    property Height : cardinal read fHeight write fHeight;
    property LineColor : TRedFoxColorString read fLineColor write fLineColor;
  end;

implementation

uses
  Graphics;

{ TSampleImageRenderer }

constructor TSampleImageRenderer.Create;
begin
  fWidth := 0;
  fHeight := 0;
  fLineColor := '$FF000000';
end;

destructor TSampleImageRenderer.Destroy;
begin

  inherited;
end;

function TSampleImageRenderer.RenderSample(const aSampleRegion: IRegion): IInterfacedBitmap;
var
  Bitmap : IInterfacedBitmap;
begin
  Bitmap := TInterfacedBitmap.Create;

  Bitmap.Bitmap.PixelFormat := pf32Bit;
  Bitmap.Bitmap.SetSize(Width, Height);

  Bitmap.Bitmap.Canvas.Pen.Color := clRed;
  Bitmap.Bitmap.Canvas.Pen.Style := TPenStyle.psSolid;
  Bitmap.Bitmap.Canvas.Pen.Width := 2;

  Bitmap.Bitmap.Canvas.MoveTo(0,0);
  Bitmap.Bitmap.Canvas.LineTo(Width, Height);

  Bitmap.Bitmap.Canvas.MoveTo(Width,0);
  Bitmap.Bitmap.Canvas.LineTo(0, Height);


  result := Bitmap;
end;

end.
