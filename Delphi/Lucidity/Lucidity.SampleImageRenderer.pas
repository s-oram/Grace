unit Lucidity.SampleImageRenderer;

interface

uses
  Types,
  RedFoxColor,
  RedFoxBitmapWrapper,
  VamSamplePeakBuffer,

  Lucidity.SampleMap,
  VamLib.Graphics;

type
  TSampleImageRenderer = class
  private
    fImageWidth: cardinal;
    fImageHeight: cardinal;
    fLineColor: TRedFoxColorString;
    fBackgroundColor: TRedFoxColorString;

  protected
    procedure DrawPeakBufferData(const ImageWrapper : TRedFoxBitmapWrapper; const Peaks : TPeakBufferData; const PeakFrames : integer; const DestBounds : TRectF);
  public
    constructor Create;
    destructor Destroy; override;

    function RenderSample(const aSampleRegion : IRegion):IInterfacedBitmap;


    // Image properties.
    property ImageWidth  : cardinal           read fImageWidth  write fImageWidth;
    property ImageHeight : cardinal           read fImageHeight write fImageHeight;
    property LineColor   : TRedFoxColorString read fLineColor   write fLineColor;
    property BackgroundColor : TRedFoxColorString read fBackgroundColor write fBackgroundColor;
  end;

implementation

uses
  Math,
  AggPixelFormat,
  VamLib.Utils,
  Graphics;

{ TSampleImageRenderer }

constructor TSampleImageRenderer.Create;
begin
  fImageWidth := 0;
  fImageHeight := 0;
  fLineColor       := '$FFFF0000';
  fBackgroundColor := '$FF000000';
end;

destructor TSampleImageRenderer.Destroy;
begin

  inherited;
end;

function TSampleImageRenderer.RenderSample(const aSampleRegion: IRegion): IInterfacedBitmap;
var
  Bitmap : IInterfacedBitmap;
  Wrapper : TRedfoxBitmapWrapper;
  DestBounds : TRectF;
  PeakBuffer : IPeakBuffer;
begin
  Bitmap := TInterfacedBitmap.Create;

  Bitmap.Bitmap.PixelFormat := pf32Bit;
  Bitmap.Bitmap.SetSize(ImageWidth, ImageHeight);

  Wrapper := TRedFoxBitmapWrapper.Create;
  AutoFree(@Wrapper);
  Wrapper.Wrap(Bitmap.Bitmap);


  Wrapper.BufferInterface.ClearAll(GetRedFoxColor(BackgroundColor));
  Wrapper.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
  Wrapper.BufferInterface.LineColor := GetRedFoxColor(LineColor);


  if (aSampleRegion.GetSample^.Properties.IsValid) and (aSampleRegion.GetSample^.Properties.SampleFrames > ImageWidth) then
  begin
    PeakBuffer := aSampleRegion.GetPeakBuffer;

    if (PeakBuffer.GetDataChannels = 1)  then
    begin
      DestBounds.Left := 0;
      DestBounds.Top  := 0;
      DestBounds.Height := ImageHeight;
      DestBounds.Width  := ImageWidth;

      DrawPeakBufferData(Wrapper, PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);
    end;

    if PeakBuffer.GetDataChannels = 2 then
    begin
      DestBounds.Left   := 0;
      DestBounds.Width  := ImageWidth;
      DestBounds.Top    := 0;
      DestBounds.Height := ImageHeight * 0.5;

      DrawPeakBufferData(Wrapper, PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);

      DestBounds.Left   := 0;
      DestBounds.Width  := ImageWidth;
      DestBounds.Top    := ImageHeight * 0.5;
      DestBounds.Height := ImageHeight * 0.5;

      DrawPeakBufferData(Wrapper, PeakBuffer.GetDataB^, PeakBuffer.GetDataFrames, DestBounds);
    end;
  end;



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


procedure TSampleImageRenderer.DrawPeakBufferData(const ImageWrapper : TRedFoxBitmapWrapper; const Peaks: TPeakBufferData; const PeakFrames: integer; const DestBounds: TRectF);
var
  xOffset : integer;
  x1, y1, y2 : single;
  MidPoint : single;
  c1: Integer;
  UnityGainPixelHeight : single;
begin
  if PeakFrames <> DestBounds.Width then exit;

  xOffset := floor(DestBounds.Left);
  MidPoint := DestBounds.Top + (DestBounds.Height * 0.5);

  UnityGainPixelHeight := (DestBounds.Height * 0.5);

  ImageWrapper.BufferInterface.FillColor := GetRedFoxColor(LineColor);
  ImageWrapper.BufferInterface.LineColor := GetRedFoxColor(LineColor);
  ImageWrapper.BufferInterface.LineWidth := 1;

  //BackBuffer.BufferInterface.FillColor := GetAggColor(clRed);
  //BackBuffer.BufferInterface.LineColor := GetAggColor(clRed);
  //BackBuffer.BufferInterface.LineWidth := 1;


  for c1 := 0 to PeakFrames-1 do
  begin
    x1 := xOffset + c1 + 0.5;
    y1 := MidPoint - (Peaks[c1].MaxValue * UnityGainPixelHeight);
    y2 := MidPoint - (Peaks[c1].MinValue * UnityGainPixelHeight);

    if IsNAN(y1) then y1 := 0;
    if IsNAN(y2) then y2 := 0;

    if y1 <= DestBounds.Top    then y1 := DestBounds.Top    + 1;
    if y1 >= DestBounds.Bottom then y1 := DestBounds.Bottom - 1;

    if y2 <= DestBounds.Top    then y2 := DestBounds.Top    + 1;
    if y2 >= DestBounds.Bottom then y2 := DestBounds.Bottom - 1;

    ImageWrapper.BufferInterface.Line(x1, y1, x1, y2);
  end;
end;



end.
