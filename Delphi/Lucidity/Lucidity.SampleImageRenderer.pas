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
  TSampleRenderParameters = record
    LineColor       : TRedFoxColor;
    BackgroundColor : TRedFoxColor;
    ImageWidth      : cardinal;
    ImageHeight     : cardinal;
    Zoom            : single; //Horizontal zoom - range 0..1 .. This should be changed to be 1+ with 1 being no zoom.
    Offset          : single; //Horizontal offset. IE. sample scrolling.
    VertGain        : single; //default = 1.
  end;


  TSampleImageRenderer = class
  private
    function CalcDestBounds(const ChannelIndex, ChannelCount : integer; const ImageWidth, ImageHeight : cardinal):TRectF;
  protected
    procedure DrawEmptySample(const Par:TSampleRenderParameters; const ImageWrapper : TRedFoxBitmapWrapper; ChannelCount : integer);
    procedure DrawSampleUsingPoints(const aSampleRegion : IRegion; const Par:TSampleRenderParameters; const ImageWrapper : TRedFoxBitmapWrapper);
    procedure DrawSampleUsingPeakBuffer(const aSampleRegion : IRegion; const Par:TSampleRenderParameters; const ImageWrapper : TRedFoxBitmapWrapper);

    procedure DrawZeroPointLine(const Par:TSampleRenderParameters; const ImageWrapper : TRedFoxBitmapWrapper; const DestBounds : TRectF);
  public
    constructor Create;
    destructor Destroy; override;

    function RenderSample(const Par:TSampleRenderParameters; const aSampleRegion : IRegion):IInterfacedBitmap;
  end;

implementation

uses
  SysUtils,
  Math,
  AggPixelFormat,
  VamLib.Utils,
  Graphics;

{ TSampleImageRenderer }


constructor TSampleImageRenderer.Create;
begin
end;

destructor TSampleImageRenderer.Destroy;
begin

  inherited;
end;

function TSampleImageRenderer.CalcDestBounds(const ChannelIndex, ChannelCount: integer; const ImageWidth, ImageHeight: cardinal): TRectF;
begin
  if ChannelCount = 1 then
  begin
    result := RectF(0,0,ImageWidth, ImageHeight);
  end else
  if (ChannelCount = 2) and (ChannelIndex = 1) then
  begin
    result := RectF(0,0,ImageWidth, round(ImageHeight*0.5));
  end else
  if (ChannelCount = 2) and (ChannelIndex = 2) then
  begin
    result := RectF(0, round(ImageHeight*0.5) ,ImageWidth, ImageHeight);
  end else
  begin
    raise Exception.Create('Unexpected ChannelIndex and ChannelCount combo.');
  end;
end;


function TSampleImageRenderer.RenderSample(const Par:TSampleRenderParameters; const aSampleRegion: IRegion): IInterfacedBitmap;
var
  Bitmap : IInterfacedBitmap;
  Wrapper : TRedfoxBitmapWrapper;

begin
  Bitmap := TInterfacedBitmap.Create;

  Bitmap.Bitmap.PixelFormat := pf32Bit;
  Bitmap.Bitmap.SetSize(Par.ImageWidth, Par.ImageHeight);

  Wrapper := TRedFoxBitmapWrapper.Create;
  AutoFree(@Wrapper);
  Wrapper.Wrap(Bitmap.Bitmap);

  Wrapper.BufferInterface.ClearAll(Par.BackgroundColor);
  Wrapper.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
  Wrapper.BufferInterface.LineColor := Par.LineColor;

  if (aSampleRegion.GetSample^.Properties.IsValid) then
  begin
    if (aSampleRegion.GetSample^.Properties.SampleFrames > CastToInteger(Par.ImageWidth))
      then DrawSampleUsingPeakBuffer(aSampleRegion, Par, Wrapper)
      else DrawSampleUsingPoints(aSampleRegion, Par, Wrapper);
  end else
  begin
    DrawEmptySample(Par, Wrapper, 1);
  end;




  result := Bitmap;
end;

procedure TSampleImageRenderer.DrawSampleUsingPeakBuffer(const aSampleRegion: IRegion; const Par:TSampleRenderParameters; const ImageWrapper: TRedFoxBitmapWrapper);
  procedure DrawPeakBufferData(const ImageWrapper : TRedFoxBitmapWrapper; const Peaks: TPeakBufferData; const PeakFrames: integer; const DestBounds: TRectF);
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

    ImageWrapper.BufferInterface.FillColor := Par.LineColor;
    ImageWrapper.BufferInterface.LineColor := Par.LineColor;
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


var
  DestBounds : TRectF;
  PeakBuffer : IPeakBuffer;
begin
  PeakBuffer := aSampleRegion.GetPeakBuffer;

  assert(PeakBuffer.GetDataFrames = CastToInteger(Par.ImageWidth));
  assert(aSampleRegion.GetSample^.Properties.SampleFrames > CastToInteger(Par.ImageWidth));

  if (PeakBuffer.GetDataChannels = 1)  then
  begin
    DestBounds := CalcDestBounds(1, 1, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
    DrawPeakBufferData(ImageWrapper, PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);
  end;

  if PeakBuffer.GetDataChannels = 2 then
  begin
    DestBounds := CalcDestBounds(1, 2, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
    DrawPeakBufferData(ImageWrapper, PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);

    DestBounds := CalcDestBounds(2, 2, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
    DrawPeakBufferData(ImageWrapper, PeakBuffer.GetDataB^, PeakBuffer.GetDataFrames, DestBounds);
  end;
end;


procedure TSampleImageRenderer.DrawEmptySample(const Par:TSampleRenderParameters; const ImageWrapper: TRedFoxBitmapWrapper; ChannelCount: integer);
var
  DestBounds : TRectF;
begin
  if ChannelCount = 1 then
  begin
    DestBounds := CalcDestBounds(1, 1, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
  end;

  if ChannelCount = 2 then
  begin
    DestBounds := CalcDestBounds(1, 2, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);

    DestBounds := CalcDestBounds(2, 2, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
  end;


end;


procedure TSampleImageRenderer.DrawSampleUsingPoints(const aSampleRegion: IRegion; const Par:TSampleRenderParameters; const ImageWrapper: TRedFoxBitmapWrapper);
  procedure DrawPoints(smps: PSingle; SampleFrames: integer; const DestBounds: TRectF);
  var
    c1: Integer;
    x1,y1,x2,y2:single;
    OffsetX, OffsetY : integer;
    dw, dh : integer;
    aColor : TRedFoxColor;
  begin
    aColor := Par.LineColor;

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
var
  DestBounds : TRectF;
  Smps : PSingle;
begin
  if aSampleRegion.GetSample^.Properties.ChannelCount = 1 then
  begin
    DestBounds := CalcDestBounds(1, 1, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
    Smps := PSingle(aSampleRegion.GetSample^.Properties.Ch1);
    DrawPoints(Smps, aSampleRegion.GetSample^.Properties.SampleFrames, DestBounds);
  end;

  if aSampleRegion.GetSample^.Properties.ChannelCount = 2 then
  begin
    DestBounds := CalcDestBounds(1, 2, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
    Smps := PSingle(aSampleRegion.GetSample^.Properties.Ch1);
    DrawPoints(Smps, aSampleRegion.GetSample^.Properties.SampleFrames, DestBounds);

    DestBounds := CalcDestBounds(2, 2, Par.ImageWidth, Par.ImageHeight);
    DrawZeroPointLine(Par, ImageWrapper, DestBounds);
    Smps := PSingle(aSampleRegion.GetSample^.Properties.Ch2);
    DrawPoints(Smps, aSampleRegion.GetSample^.Properties.SampleFrames, DestBounds);
  end;
end;

procedure TSampleImageRenderer.DrawZeroPointLine(const Par:TSampleRenderParameters; const ImageWrapper: TRedFoxBitmapWrapper; const DestBounds: TRectF);
var
  ab : integer;
  aColor : TRedFoxColor;
  x1,y1,x2,y2:single;
begin
  //===== Draw the zero point reference line ======
  aColor := Par.LineColor;
  ab := aColor.A div 3;
  ImageWrapper.BufferInterface.FillColor := aColor.WithAlpha(ab).AsAggRgba8;
  ImageWrapper.BufferInterface.LineColor := aColor.WithAlpha(ab).AsAggRgba8;
  ImageWrapper.BufferInterface.LineWidth := 1;

  x1 := DestBounds.Left;
  x2 := DestBounds.Right;
  y1 := round(DestBounds.Top + (DestBounds.Height * 0.5)) + 0.5;
  y2 := y1;
  ImageWrapper.BufferInterface.Line(x1, y1, x2,y2);
end;






end.
