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
  VamLib.Utils,
  RedFoxBitmapWrapper,
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
  Wrapper : TRedfoxBitmapWrapper;
begin
  Bitmap := TInterfacedBitmap.Create;

  Bitmap.Bitmap.PixelFormat := pf32Bit;
  Bitmap.Bitmap.SetSize(Width, Height);

  Wrapper := TRedFoxBitmapWrapper.Create;
  AutoFree(@Wrapper);
  Wrapper.Wrap(Bitmap.Bitmap);



  {

  Wrapper.BufferInterface.FillColor := GetAggColor(clGreen);
  Wrapper.BufferInterface.LineColor := GetAggColor(clBlue);

  Wrapper.BufferInterface.ClearAll(66,66,66,255);
  Wrapper.BufferInterface.Rectangle(0,0,60,60);
  Wrapper.BufferInterface.Line(0,0,Width,Height);
  Wrapper.BufferInterface.Line(Width,0,0,Height);
  }



  result := Bitmap;
end;

end.
