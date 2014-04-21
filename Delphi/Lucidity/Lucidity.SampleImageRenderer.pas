unit Lucidity.SampleImageRenderer;

interface

uses
  Lucidity.Interfaces,
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
    procedure DrawSampleUsingPeakBuffer(const aSampleRegion : IRegion; PeaksData: IPeakBuffer;  const Par:TSampleRenderParameters; const ImageWrapper : TRedFoxBitmapWrapper);

    procedure DrawZeroPointLine(const Par:TSampleRenderParameters; const ImageWrapper : TRedFoxBitmapWrapper; const DestBounds : TRectF);
  public
    constructor Create;
    destructor Destroy; override;

    function RenderSample(const Par:TSampleRenderParameters; const aSampleRegion : IRegion; PeaksData: IPeakBuffer):IInterfacedBitmap;
  end;

implementation

uses
  SysUtils,
  Math,
  AggPixelFormat,
  VamLib.Utils,
  Graphics,
  Lucidity.FlexSampleRenderer;

{ TSampleImageRenderer }


constructor TSampleImageRenderer.Create;
begin
end;

destructor TSampleImageRenderer.Destroy;
begin

  inherited;
end;

function TSampleImageRenderer.CalcDestBounds(const ChannelIndex, ChannelCount: integer; const ImageWidth, ImageHeight: cardinal): TRectF;
var
  r : TRectF;
begin
  if ChannelCount = 1 then
  begin
    r := RectF(0,0,ImageWidth, ImageHeight);
  end else
  if (ChannelCount = 2) and (ChannelIndex = 1) then
  begin
    r := RectF(0,0,ImageWidth, round(ImageHeight*0.5));
  end else
  if (ChannelCount = 2) and (ChannelIndex = 2) then
  begin
    r := RectF(0, round(ImageHeight*0.5) ,ImageWidth, ImageHeight);
  end else
  begin
    raise Exception.Create('Unexpected ChannelIndex and ChannelCount combo.');
  end;

  r.Inflate(0, -1);
  result := r;
end;


function TSampleImageRenderer.RenderSample(const Par:TSampleRenderParameters; const aSampleRegion: IRegion; PeaksData: IPeakBuffer): IInterfacedBitmap;
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
    if (Par.Zoom = 0) then
    begin
      if (PeaksData <> nil) and (PeaksData.GetDataFrames = Integer(Par.ImageWidth)) then
      begin
        DrawSampleUsingPeakBuffer(aSampleRegion, PeaksData, Par, Wrapper)
      end else
      if (aSampleRegion.GetSample^.Properties.SampleFrames > CastToInteger(Par.ImageWidth)) then
      begin
        GenerateRegionPeaks(aSampleRegion, Par.ImageWidth, PeaksData);
        DrawSampleUsingPeakBuffer(aSampleRegion, PeaksData, Par, Wrapper)
      end else
      begin
        DrawSampleUsingPoints(aSampleRegion, Par, Wrapper);
      end;
    end;
  end else
  begin
    DrawEmptySample(Par, Wrapper, 1);
  end;

  result := Bitmap;
end;

procedure TSampleImageRenderer.DrawSampleUsingPeakBuffer(const aSampleRegion: IRegion; PeaksData: IPeakBuffer; const Par:TSampleRenderParameters; const ImageWrapper: TRedFoxBitmapWrapper);
  procedure DrawPeakBufferData(const ImageWrapper : TRedFoxBitmapWrapper; const Peaks: TPeakBufferData; const PeakFrames: integer; const DestBounds: TRectF);
  var
    xOffset : integer;
    x1, y1, y2 : single;
    MidPoint : single;
    c1: Integer;
    UnityGainPixelHeight : single;
  begin
    if PeakFrames <> DestBounds.Width then exit;

    ImageWrapper.BufferInterface.FillColor := Par.LineColor;
    ImageWrapper.BufferInterface.LineColor := Par.LineColor;
    ImageWrapper.BufferInterface.LineWidth := 1;

    xOffset := floor(DestBounds.Left);
    MidPoint := DestBounds.Top + (DestBounds.Height * 0.5);
    UnityGainPixelHeight := (DestBounds.Height * 0.5) * Par.VertGain;

    for c1 := 0 to PeakFrames-1 do
    begin
      x1 := xOffset + c1 + 0.5;
      y1 := MidPoint - (Peaks[c1].MaxValue * UnityGainPixelHeight);
      y2 := MidPoint - (Peaks[c1].MinValue * UnityGainPixelHeight);

      if IsNAN(y1) then y1 := 0;
      if IsNAN(y2) then y2 := 0;

      if abs(y1-y2) < 1 then
      begin
        y1 := y1 - 0.5;
        y2 := y2 + 0.5;
      end;

      y1 := Clamp(y1, DestBounds.Top, DestBounds.Bottom-1);
      y2 := Clamp(y2, DestBounds.Top+1, DestBounds.Bottom);

      ImageWrapper.BufferInterface.Line(x1, y1, x1, y2);
    end;
  end;
var
  DestBounds : TRectF;
  PeakBuffer : IPeakBuffer;
begin
  //PeakBuffer := aSampleRegion.GetPeakBuffer;
  PeakBuffer := PeaksData;

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
    OffsetX : integer;
    dw : integer;
    aColor : TRedFoxColor;
    MidPoint : single;
    UnityGainPixelHeight : single;
  begin
    aColor := Par.LineColor;

    ImageWrapper.BufferInterface.LineColor := aColor;
    ImageWrapper.BufferInterface.LineWidth := 1;

    dw      := round(DestBounds.Width-2);
    OffsetX := round(DestBounds.Left + 1);

    MidPoint := DestBounds.Top + (DestBounds.Height * 0.5);
    UnityGainPixelHeight := (DestBounds.Height * 0.5) * Par.VertGain;

    x1 := OffsetX;
    y1 := MidPoint - smps^ * UnityGainPixelHeight;
    y1 := Clamp(y1, DestBounds.Top, DestBounds.Bottom);

    x2 := x1;
    y2 := y1;

    for c1 := 0 to SampleFrames-1 do
    begin
      x1 := round((c1 / (SampleFrames-1)) * dw + OffsetX);
      y1 := MidPoint - smps^ * UnityGainPixelHeight;
      y1 := Clamp(y1, DestBounds.Top, DestBounds.Bottom);

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
