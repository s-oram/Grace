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
    procedure DrawSampleUsingPoints(const aSampleRegion : IRegion; const ImageWrapper : TRedFoxBitmapWrapper);
    procedure DrawPoints(const ImageWrapper : TRedFoxBitmapWrapper; smps:PSingle; SampleFrames:integer; const DestBounds : TRectF);

    procedure DrawSampleUsingPeakBuffer(const aSampleRegion : IRegion; const ImageWrapper : TRedFoxBitmapWrapper);
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

  if (aSampleRegion.GetSample^.Properties.IsValid) then
  begin
    if (aSampleRegion.GetSample^.Properties.SampleFrames > ImageWidth)
      then DrawSampleUsingPeakBuffer(aSampleRegion, Wrapper)
      else DrawSampleUsingPoints(aSampleRegion, Wrapper);
  end;




  result := Bitmap;
end;

procedure TSampleImageRenderer.DrawSampleUsingPeakBuffer(const aSampleRegion: IRegion; const ImageWrapper: TRedFoxBitmapWrapper);
var
  DestBounds : TRectF;
  PeakBuffer : IPeakBuffer;
begin
  PeakBuffer := aSampleRegion.GetPeakBuffer;

  assert(PeakBuffer.GetDataFrames = ImageWidth);
  assert(aSampleRegion.GetSample^.Properties.SampleFrames > ImageWidth);

  if (PeakBuffer.GetDataChannels = 1)  then
  begin
    DestBounds.Left := 0;
    DestBounds.Top  := 0;
    DestBounds.Height := ImageHeight;
    DestBounds.Width  := ImageWidth;

    DrawPeakBufferData(ImageWrapper, PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);
  end;

  if PeakBuffer.GetDataChannels = 2 then
  begin
    DestBounds.Left   := 0;
    DestBounds.Width  := ImageWidth;
    DestBounds.Top    := 0;
    DestBounds.Height := ImageHeight * 0.5;

    DrawPeakBufferData(ImageWrapper, PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);

    DestBounds.Left   := 0;
    DestBounds.Width  := ImageWidth;
    DestBounds.Top    := ImageHeight * 0.5;
    DestBounds.Height := ImageHeight * 0.5;

    DrawPeakBufferData(ImageWrapper, PeakBuffer.GetDataB^, PeakBuffer.GetDataFrames, DestBounds);
  end;
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


procedure TSampleImageRenderer.DrawSampleUsingPoints(const aSampleRegion: IRegion; const ImageWrapper: TRedFoxBitmapWrapper);
var
  DestBounds : TRectF;
begin
  if aSampleRegion.GetSample^.Properties.ChannelCount = 1 then
  begin
    DestBounds.Left := 0;
    DestBounds.Top  := 0;
    DestBounds.Height := ImageHeight;
    DestBounds.Width  := ImageWidth;

    self.DrawPoints(ImageWrapper, aSampleRegion.GetSample^.Properties.Ch1, aSampleRegion.GetSample^.Properties.SampleFrames, DestBounds);
  end;

  if aSampleRegion.GetSample^.Properties.ChannelCount = 2 then
  begin
    DestBounds.Left   := 0;
    DestBounds.Width  := ImageWidth;
    DestBounds.Top    := 0;
    DestBounds.Height := ImageHeight * 0.5;

    self.DrawPoints(ImageWrapper, aSampleRegion.GetSample^.Properties.Ch1, aSampleRegion.GetSample^.Properties.SampleFrames, DestBounds);

    DestBounds.Left   := 0;
    DestBounds.Width  := ImageWidth;
    DestBounds.Top    := ImageHeight * 0.5;
    DestBounds.Height := ImageHeight * 0.5;

    self.DrawPoints(ImageWrapper, aSampleRegion.GetSample^.Properties.Ch2, aSampleRegion.GetSample^.Properties.SampleFrames, DestBounds);
  end;
end;

procedure TSampleImageRenderer.DrawPoints(const ImageWrapper : TRedFoxBitmapWrapper; smps: PSingle; SampleFrames: integer; const DestBounds: TRectF);
var
  c1: Integer;
  x1,y1,x2,y2:single;
  OffsetX, OffsetY : integer;
  dw, dh : integer;
  ab : integer;

  aColor : TRedFoxColor;
begin
  //===== Draw the zero point reference line ======
  aColor := fLineColor;
  ab := aColor.A div 3;
  ImageWrapper.BufferInterface.FillColor := aColor.WithAlpha(ab).AsAggRgba8;
  ImageWrapper.BufferInterface.LineColor := aColor.WithAlpha(ab).AsAggRgba8;
  ImageWrapper.BufferInterface.LineWidth := 1;

  x1 := DestBounds.Left;
  x2 := DestBounds.Right;
  y1 := round(DestBounds.Top + (DestBounds.Height * 0.5)) + 0.5;
  y2 := y1;
  ImageWrapper.BufferInterface.Line(x1, y1, x2,y2);




  //=== draw the sample data line ===
  ImageWrapper.BufferInterface.LineColor := aColor;
  ImageWrapper.BufferInterface.LineWidth := 1;




  dw      := round(DestBounds.Width-2);
  OffsetX := round(DestBounds.Left + 1);
  dh      := round(DestBounds.Height-2);
  OffsetY := round(DestBounds.Top + 1);

  x1 := OffsetX;
  y1 := (-1 * smps^) * (dh * 0.5) + OffsetY + (dh * 0.5);
  if y1 < OffsetY then y1 := offsetY;
  if y1 > OffsetY + dh then y1 := OffsetY + dh;
  x2 := x1;
  y2 := y1;

  for c1 := 0 to SampleFrames-1 do
  begin
    x1 := round((c1 / (SampleFrames-1)) * dw + OffsetX);
    y1 := (-1 * smps^) * (dh * 0.5) + OffsetY + (dh * 0.5);
    if y1 < OffsetY then y1 := offsetY;
    if y1 > OffsetY + dh then y1 := OffsetY + dh;

    ImageWrapper.BufferInterface.Line(x1, y1, x2, y2);

    x2 := x1;
    y2 := y1;

    inc(smps);
  end;
end;







end.
