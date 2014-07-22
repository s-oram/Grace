unit Lucidity.FlexSampleRenderer;

interface

uses
  Lucidity.Interfaces,
  VamLib.Graphics,
  System.Classes, System.Types, Graphics,
  RedFox, RedFoxColor,
  RedFoxImageBuffer,
  Lucidity.SampleMap,
  VamSamplePeakBuffer;

type
  TFlexRenderPar = record
    LineColor       : TRedFoxColor;
    BackgroundColor : TRedFoxColor;
    ImageWidth      : cardinal;
    ImageHeight     : cardinal;
    Zoom            : single; //Horizontal zoom - range 0..1 .. This should be changed to be 1+ with 1 being no zoom.
    Offset          : single; //Horizontal offset. IE. sample scrolling.
    VertGain        : single; //default = 1.
  end;

  TFlexSampleImageRenderer = class
  private
    class function CalcDestBounds(const ChannelIndex, ChannelCount : integer; const ImageWidth, ImageHeight : cardinal):TRectF; static;
  protected
  public
    class function RenderSample(const aSampleRegion : IRegion; const Par:TFlexRenderPar):IInterfacedBitmap; static;
  end;

implementation

uses
  VamSampleDisplayBackBuffer,
  AggPixelFormat,
  VamLib.Utils,
  SysUtils,
  RedFoxbitmapWrapper;

//============ Sample standalone methods ===================

procedure GetMinMaxValuesFloat(Data:PSingle; FirstSample, LastSample:integer; Out MinValue, MaxValue:single); //inline;
var
  SampleData:array of single absolute Data;
  c2:integer;
  lv,hv:single;
  Value:single;
  //SampleDataLength : integer;
begin
  lv := 1;
  hv := -1;

  //Pixel to sample position.
  for c2 := FirstSample to LastSample do
  begin
    value := SampleData[c2];
    if lv > value then lv := Value;
    if hv < value then hv := Value;
  end;

  MinValue := lv;
  MaxValue := hv;
end;


function CalcPeakCount(const SampleFrames: integer; const ZoomFactor: double; const DisplayWidth:integer): integer;
var
  PeakCount    : integer;
  MaxPeakCount : integer;
begin
  assert(ZoomFactor >= 0);
  assert(ZoomFactor <= 1);

  MaxPeakCount := SampleFrames;

  if ZoomFactor < 1
    then PeakCount := round(DisplayWidth / (1-ZoomFactor))
    else PeakCount := SampleFrames;

  if PeakCount > MaxPeakCount then PeakCount := MaxPeakCount;

  result := PeakCount;
end;

function CalcFirstVisiblePeak(const Offset : single; const PeakCount, DisplayWidth : integer):integer;
var
  VisiblePeaks : integer;
  CenterPeak : integer;
  FirstPeak  : integer;
begin
  if PeakCount >= DisplayWidth then
  begin
    VisiblePeaks := DisplayWidth;
    CenterPeak   := round(Offset * (PeakCount- VisiblePeaks)) + (VisiblePeaks div 2);
    FirstPeak   := CenterPeak - (VisiblePeaks div 2);
    if FirstPeak < 0 then FirstPeak := 0;
    if FirstPeak > PeakCount - VisiblePeaks then FirstPeak := PeakCount - VisiblePeaks;

    result := FirstPeak;
  end else
  begin
    result := 0;
  end;
end;


function SamplePosToPixelPos(const x1:single; const SampleFrames, DisplayWidth:integer; const Zoom, Offset:single):single;
var
  PeakCount, FirstPeak : integer;
  ScaledX : single;
begin
  assert((Zoom >= 0) and (Zoom <= 1));
  assert((Offset >= 0) and (Offset <= 1));

  PeakCount := CalcPeakCount(SampleFrames, Zoom, DisplayWidth);

  if PeakCount >= DisplayWidth then
  begin
    FirstPeak := CalcFirstVisiblePeak(Offset, PeakCount, DisplayWidth);
    ScaledX := x1 / (SampleFrames-1) * PeakCount - FirstPeak;
    result := ScaledX;
  end else
  begin
    ScaledX := x1 / (SampleFrames-1) * DisplayWidth;
    result := ScaledX;
  end;
end;

function PixelPosToSamplePos(const x1:single; const SampleFrames, DisplayWidth:integer; const Zoom, Offset:single):integer;
var
  PeakCount, FirstPeak : integer;
  ScaledX : single;
  smpX : integer;
begin
  assert((Zoom >= 0) and (Zoom <= 1));
  assert((Offset >= 0) and (Offset <= 1));

  PeakCount := CalcPeakCount(SampleFrames, Zoom, DisplayWidth);

  if PeakCount >= DisplayWidth then
  begin
    FirstPeak := CalcFirstVisiblePeak(Offset, PeakCount, DisplayWidth);
    ScaledX   := FirstPeak + x1;
    //if ScaledX < 0 then ScaledX := 0;
    //if ScaledX > PeakCount then ScaledX := PeakCount;
    result := round((SampleFrames-1) * (ScaledX / PeakCount));
  end else
  begin
    smpX := round(x1 / DisplayWidth * (SampleFrames-1));
    result := smpX;
  end;
end;




procedure DrawPeaks(const BackBuffer : TRedfoxBitmapWrapper; const Par: TFlexRenderPar; const PeakData : TArrayOfPeak; const aFirstPeak, aPeakCount : integer; const DestBounds : TRectF);
const
  OneOver255 : double = 1 / 255;
var
  c1: Integer;
  Index : integer;
  pt1, pt2 : TPointF;
  NewY1, NewY2 : single;
  OldY1, OldY2 : single;
  AreaHeight   : single;
  x1, y1, x2, y2: double;
  ab : integer;
begin
  assert(aFirstPeak >= 0);

  AreaHeight := DestBounds.Height-1;

  //===== Draw the zero point reference line ======
  ab := Par.LineColor.A div 4;
  BackBuffer.BufferInterface.FillColor := Par.LineColor.WithAlpha(ab).AsAggRgba8;
  BackBuffer.BufferInterface.LineColor := Par.LineColor.WithAlpha(ab).AsAggRgba8;
  BackBuffer.BufferInterface.LineWidth := 1;

  x1 := DestBounds.Left;
  x2 := DestBounds.Right;
  y1 := round(DestBounds.Top + (AreaHeight * 0.5)) + 0.5;
  y2 := y1;
  BackBuffer.BufferInterface.Line(x1, y1, x2,y2);


  //===== Draw the top and bottom lines ======

  //  ab := fLineColor.A div 8;
  //  BackBuffer.BufferInterface.FillColor := fLineColor.WithAlpha(ab).AsAggRgba8;
  //  BackBuffer.BufferInterface.LineColor := fLineColor.WithAlpha(ab).AsAggRgba8;
  //  BackBuffer.BufferInterface.LineWidth := 1;
  //
  //  x1 := DestBounds.Left;
  //  x2 := DestBounds.Right;
  //  y1 := round(DestBounds.Top) + 0.5;
  //  y2 := y1;
  //  BackBuffer.BufferInterface.Line(x1, y1, x2,y2);
  //
  //  x1 := DestBounds.Left;
  //  x2 := DestBounds.Right;
  //  y1 := round(DestBounds.Top + AreaHeight) + 0.5;
  //  y2 := y1;
  //  BackBuffer.BufferInterface.Line(x1, y1, x2,y2);
  //



  //===== Draw the sample data =======

  BackBuffer.BufferInterface.FillColor := Par.LineColor.AsAggRgba8;
  BackBuffer.BufferInterface.LineColor := Par.LineColor.AsAggRgba8;
  BackBuffer.BufferInterface.LineWidth := 1;

  if aPeakCount > 0 then
  begin
    // Get the value of the first sample...
    Index := aFirstPeak;
    OldY1 := (1 - (PeakData[Index].MinValue * OneOver255)) * AreaHeight + DestBounds.Top;
    OldY2 := (1 - (PeakData[Index].MaxValue * OneOver255)) * AreaHeight + DestBounds.Top;
  end else
  begin
    OldY1 := 0.5 * AreaHeight + DestBounds.Top;
    OldY2 := 0.5 * AreaHeight + DestBounds.Top;
  end;

  for c1 := 0 to aPeakCount-1 do
  begin
    Index := aFirstPeak + c1;
    pt1.X := c1 + 0.5;
    pt2.X := c1 + 0.5;
    NewY1 := (1 - (PeakData[Index].MinValue * OneOver255)) * AreaHeight + DestBounds.Top;
    NewY2 := (1 - (PeakData[Index].MaxValue * OneOver255)) * AreaHeight + DestBounds.Top;

    // In some cases, the minimum peak level will be higher then the previous peak maximum. When that
    // happens, the waveform level (as drawn) will appear to jump to a new value. (The same happens
    // when the maximum peak level is smaller than the previous minimum peak level.) To avoid this
    // minor problem, check the new min-max peak levels against the last min-max peak levels. In the
    // case of jumps, extend either the min or max peak level as appropiate.
    if (NewY1 < OldY2) then NewY1 := OldY2;
    if (NewY2 > OldY1) then NewY2 := OldY1;

    pt1.Y := NewY1;
    pt2.Y := NewY2;

    OldY1 := NewY1;
    OldY2 := NewY2;

    // If the top of line and the bottom of the line are very close together, no line will be drawn.
    if abs(pt1.Y - pt2.Y) >= 1 then
    begin
      BackBuffer.BufferInterface.Line(pt1.X, pt1.Y, pt2.X, pt2.Y);
    end else
    begin
      pt1.Y := pt1.Y - 0.35;
      pt2.Y := pt2.Y + 0.35;
      BackBuffer.BufferInterface.Line(pt1.X, pt1.Y, pt2.X, pt2.Y);
    end;
  end;
end;


{ TFlexSampleImageRenderer }


class function TFlexSampleImageRenderer.CalcDestBounds(const ChannelIndex, ChannelCount: integer; const ImageWidth, ImageHeight: cardinal): TRectF;
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

class function TFlexSampleImageRenderer.RenderSample(const aSampleRegion: IRegion; const Par: TFlexRenderPar): IInterfacedBitmap;
  procedure DrawSample_UsingPeaks(SampleData : TSampleDisplayInfo; const Wrapper : TRedfoxBitmapWrapper);
  var
    DestBounds : TRectF;
    x : integer;
    PeakData : TPeakData;
    VisiblePeaks : integer;
    FirstPeak : integer;
  begin
    x := CalcPeakCount(SampleData.SampleFrames, Par.Zoom, Par.ImageWidth);
    assert(x >= CastToInteger(Par.ImageWidth));

    PeakData := TPeakData.Create;
    AutoFree(@PeakData);

    PeakData.PeakCount := x;
    PeakData.CalcPeaks(SampleData, Par.VertGain);

    VisiblePeaks := Par.ImageWidth;
    FirstPeak := CalcFirstVisiblePeak(Par.Offset, PeakData.PeakCount, Par.ImageWidth);

    if SampleData.ChannelCount = 1 then
    begin
      DestBounds := CalcDestBounds(1,1,Par.ImageWidth, Par.ImageHeight);
      DrawPeaks(Wrapper, Par, PeakData.PeaksL, FirstPeak, VisiblePeaks, DestBounds);
    end;

    if SampleData.ChannelCount = 2 then
    begin
      DestBounds := CalcDestBounds(1,2,Par.ImageWidth, Par.ImageHeight);
      DrawPeaks(Wrapper, Par, PeakData.PeaksL, FirstPeak, VisiblePeaks, DestBounds);

      DestBounds := CalcDestBounds(2,2,Par.ImageWidth, Par.ImageHeight);
      DrawPeaks(Wrapper, Par, PeakData.PeaksR, FirstPeak, VisiblePeaks, DestBounds);
    end;
  end;
var
  Bitmap : IInterfacedBitmap;
  Wrapper : TRedfoxBitmapWrapper;
  x : integer;
  SampleData : TSampleDisplayInfo;
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
    SampleData.IsValid := true;
    SampleData.ChannelCount := aSampleRegion.GetSample^.Properties.ChannelCount;
    SampleData.SampleFrames := aSampleRegion.GetSample^.Properties.SampleFrames;
    SampleData.Ch1          := aSampleRegion.GetSample^.Properties.Ch1;
    SampleData.Ch2          := aSampleRegion.GetSample^.Properties.Ch2;

    x := CalcPeakCount(SampleData.SampleFrames, Par.Zoom, Par.ImageWidth);

    if x >= CastToInteger(Par.ImageWidth)
      then DrawSample_UsingPeaks(SampleData, Wrapper);
      //else DrawSample_UsingPoints;
  end;

  result := Bitmap;
end;





end.
