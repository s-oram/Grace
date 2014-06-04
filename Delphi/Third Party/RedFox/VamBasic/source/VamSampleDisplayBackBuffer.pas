unit VamSampleDisplayBackBuffer;

interface

uses
  System.Classes, System.Types, Graphics,
  RedFox, RedFoxColor,
  RedFoxImageBuffer,
  VamSamplePeakBuffer;

type
  TSampleImageBuffer = class;

  ISampleImageBuffer = interface
    ['{16E34EBA-9613-49CB-B324-BF910A37FFC3}']
    function GetObject:TSampleImageBuffer;
  end;

  TSampleDisplayInfo = record
    IsValid        : boolean; // set to false to clear sample.
    ChannelCount   : integer; // 1 or 2.
    SampleFrames   : integer;
    Ch1            : Pointer; // Pointer to sample data.
    Ch2            : Pointer; // Pointer to sample data.
  end;

  TPeak = record
    MinValue : byte;
    MaxValue : byte;
  end;

  TArrayOfPeak = array of TPeak;

  TPeakData = class
  private
    fPeakCount: integer;
    fPeaksL : TArrayOfPeak;
    fPeaksR : TArrayOfPeak;
    procedure SetPeakCount(const Value: integer);
  public
    destructor Destroy; override;
    procedure CalcPeaks(SDI:TSampleDisplayInfo);
    property PeakCount : integer read fPeakCount write SetPeakCount;

    property PeaksL : TArrayOfPeak read fPeaksL write fPeaksL;
    property PeaksR : TArrayOfPeak read fPeaksR write fPeaksR;
  end;

  TSampleImageBuffer = class(TInterfacedObject, ISampleImageBuffer)
  private
    fOffset: double;
    fZoom: double;
    fBackBuffer  : TRedFoxImageBuffer;
    procedure SetZoom(const Value: double);
    procedure SetOffset(const Value: double);
    function GetLineColor: TRedFoxColorString;
    procedure SetLineColor(const Value: TRedFoxColorString);
    function GetObject:TSampleImageBuffer;
  protected
    SampleData        : TSampleDisplayInfo;
    fPeakData         : TPeakData;
    fFirstPeak        : integer;
    fVisiblePeaks     : integer;
    DisplayWidth      : integer;
    DisplayHeight     : integer;
    fLineColor        : TRedFoxColor;
    procedure DrawPeaks(const PeakData : TArrayOfPeak; const aFirstPeak, aPeakCount : integer; const DestBounds : TRectF);
    procedure DrawPoints(smps:PSingle; SampleFrames:integer; const DestBounds : TRectF);

    procedure DrawPeakBufferData(const Peaks : TPeakBufferData; const PeakFrames : integer; const DestBounds : TRectF);

    procedure DrawSample_UsingPeaks;
    procedure DrawSample_UsingPoints;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DrawSample(PeakBuffer : IPeakBuffer); overload;
    procedure DrawSample(SDI:TSampleDisplayInfo); overload;
    procedure DrawSample; overload;
    procedure ClearSample(InvalidateDisplay : boolean = true);

    property BackBuffer : TRedFoxImageBuffer read fBackBuffer write fBackBuffer;
    property Zoom       : double             read fZoom       write SetZoom;   //range 0..1. 0 = no zoom.
    property Offset     : double             read fOffset     write SetOffset; //range 0..1.

    //Call resize to set the size of the sample display back buffer.
    procedure Resize(Width, Height:integer);

    property PeakData : TPeakData read fPeakData;

    property FirstPeak    : integer read fFirstPeak;
    property VisiblePeaks : integer read fVisiblePeaks;

    property LineColor : TRedFoxColorString read GetLineColor write SetLineColor;
  end;


function CalcPeakCount(const SampleFrames: integer; const ZoomFactor: double; const DisplayWidth:integer): integer;
function CalcFirstVisiblePeak(const Offset : single; const PeakCount, DisplayWidth : integer):integer;

function SamplePosToPixelPos(const x1:single; const SampleFrames, DisplayWidth:integer; const Zoom, Offset:single):single;
function PixelPosToSamplePos(const x1:single; const SampleFrames, DisplayWidth:integer; const Zoom, Offset:single):integer;

implementation

uses
  AggPixelFormat,
  Math;

const
  Msg_DrawSample = 1;
  Msg_SampleBitmapReady = 2;
  Msg_PaintFinished     = 3;

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



{ TPeakData }

destructor TPeakData.Destroy;
begin
  PeakCount := 0;
  inherited;
end;

procedure TPeakData.SetPeakCount(const Value: integer);
begin
  fPeakCount := Value;
  SetLength(fPeaksL, Value);
  SetLength(fPeaksR, Value);
end;

procedure TPeakData.CalcPeaks(SDI: TSampleDisplayInfo);
var
  c1: Integer;
  FirstSample : integer;
  LastSample  : integer;
  SamplesPerPeak : integer;
  MinValue, MaxValue : single;
begin
  SamplesPerPeak := floor(SDI.SampleFrames / PeakCount);
  for c1 := 0 to PeakCount-1 do
  begin
    FirstSample := round(c1 / PeakCount * (SDI.SampleFrames - 1));
    LastSample  := FirstSample + SamplesPerPeak;
    assert(LastSample < SDI.SampleFrames);

    if SDI.ChannelCount = 1 then
    begin
      GetMinMaxValuesFloat(SDI.Ch1, FirstSample, LastSample, MinValue, MaxValue);
      if MinValue < -1 then MinValue := -1;
      if MaxValue > 1  then MaxValue := 1;
      PeaksL[c1].MinValue := round((MinValue + 1) * 127.5);
      PeaksL[c1].MaxValue := round((MaxValue + 1) * 127.5);
    end;

    if SDI.ChannelCount = 2 then
    begin
      GetMinMaxValuesFloat(SDI.Ch1, FirstSample, LastSample, MinValue, MaxValue);
      if MinValue < -1 then MinValue := -1;
      if MaxValue > 1  then MaxValue := 1;
      PeaksL[c1].MinValue := round((MinValue + 1) * 127.5);
      PeaksL[c1].MaxValue := round((MaxValue + 1) * 127.5);

      GetMinMaxValuesFloat(SDI.Ch2, FirstSample, LastSample, MinValue, MaxValue);
      if MinValue < -1 then MinValue := -1;
      if MaxValue > 1  then MaxValue := 1;
      PeaksR[c1].MinValue := round((MinValue + 1) * 127.5);
      PeaksR[c1].MaxValue := round((MaxValue + 1) * 127.5);
    end;
  end;
end;





{ TSampleImageBuffer }

constructor TSampleImageBuffer.Create;
begin
  BackBuffer := TRedFoxImageBuffer.Create;

  fPeakData   := TPeakData.Create;

  fFirstPeak    := 0;
  fVisiblePeaks := 0;
end;

destructor TSampleImageBuffer.Destroy;
begin
  BackBuffer.Free;
  fPeakData.Free;

  inherited;
end;

procedure TSampleImageBuffer.Resize(Width, Height:integer);
begin
  DisplayWidth  := Width;
  DisplayHeight := Height;
  BackBuffer.SetSize(Width,Height);
end;

procedure TSampleImageBuffer.DrawSample(SDI: TSampleDisplayInfo);
begin
  SampleData := SDI;
  DrawSample;
end;

procedure TSampleImageBuffer.ClearSample(InvalidateDisplay : boolean = true);
begin
  SampleData.IsValid := false;
  if InvalidateDisplay then DrawSample;
end;

procedure TSampleImageBuffer.DrawSample;
var
  x : integer;
begin
  if SampleData.IsValid = false then
  begin
    PeakData.PeakCount := 0;
    BackBuffer.BufferInterface.ClearAll(fLineColor.R, fLineColor.G, fLineColor.B, 0);
    BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
  end;

  if SampleData.IsValid = true then
  begin
    BackBuffer.BufferInterface.ClearAll(fLineColor.R, fLineColor.G, fLineColor.B, 0);
    BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

    x := CalcPeakCount(SampleData.SampleFrames, Zoom, DisplayWidth);

    if x >= DisplayWidth
      then DrawSample_UsingPeaks
      else DrawSample_UsingPoints;
  end;
end;

procedure TSampleImageBuffer.DrawSample(PeakBuffer: IPeakBuffer);
var
  DestBounds : TRectF;
begin
  BackBuffer.BufferInterface.ClearAll(fLineColor.R, fLineColor.G, fLineColor.B, 0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  //if PeakBuffer then

  if (PeakBuffer.GetDataChannels = 1)  then
  begin
    DestBounds.Left := 0;
    DestBounds.Top  := 0;
    DestBounds.Height := BackBuffer.Height;
    DestBounds.Width  := BackBuffer.Width;

    DrawPeakBufferData(PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);
  end;

  if PeakBuffer.GetDataChannels = 2 then
  begin
    DestBounds.Left   := 0;
    DestBounds.Width  := BackBuffer.Width;
    DestBounds.Top    := 0;
    DestBounds.Height := BackBuffer.Height * 0.5;

    DrawPeakBufferData(PeakBuffer.GetDataA^, PeakBuffer.GetDataFrames, DestBounds);

    DestBounds.Left   := 0;
    DestBounds.Width  := BackBuffer.Width;
    DestBounds.Top    := BackBuffer.Height * 0.5;
    DestBounds.Height := BackBuffer.Height * 0.5;

    DrawPeakBufferData(PeakBuffer.GetDataB^, PeakBuffer.GetDataFrames, DestBounds);
  end;


  //BackBuffer.BufferInterface.FillColor := GetAggColor(clRed);
  //BackBuffer.BufferInterface.LineColor := GetAggColor(clRed);
  //BackBuffer.BufferInterface.LineWidth := 1;

  //BackBuffer.BufferInterface.Line(0,0,100,100);




end;

procedure TSampleImageBuffer.DrawPeakBufferData(const Peaks: TPeakBufferData; const PeakFrames : integer; const DestBounds: TRectF);
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

  BackBuffer.BufferInterface.FillColor := fLineColor.AsAggRgba8;
  BackBuffer.BufferInterface.LineColor := fLineColor.AsAggRgba8;
  BackBuffer.BufferInterface.LineWidth := 1;

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

    BackBuffer.BufferInterface.Line(x1, y1, x1, y2);
  end;
end;





procedure TSampleImageBuffer.DrawSample_UsingPeaks;
var
  DestBounds : TRectF;
  x : integer;
begin
  x := CalcPeakCount(SampleData.SampleFrames, Zoom, DisplayWidth);
  assert(x >= DisplayWidth);

  if x <> PeakData.PeakCount then
  begin
    PeakData.PeakCount := x;
  end;
  PeakData.CalcPeaks(SampleData);
  fVisiblePeaks := DisplayWidth;
  fFirstPeak := CalcFirstVisiblePeak(Offset, PeakData.PeakCount, DisplayWidth);

  if SampleData.ChannelCount = 1 then
  begin
    DestBounds.Left := 0;
    DestBounds.Top  := 0;
    DestBounds.Height := BackBuffer.Height;
    DestBounds.Width  := BackBuffer.Width;

    DrawPeaks(PeakData.PeaksL, fFirstPeak, fVisiblePeaks, DestBounds);
  end;

  if SampleData.ChannelCount = 2 then
  begin
    DestBounds.Left := 0;
    DestBounds.Top  := 0;
    DestBounds.Height := BackBuffer.Height * 0.5;
    DestBounds.Width  := BackBuffer.Width;
    DrawPeaks(PeakData.PeaksL, fFirstPeak, fVisiblePeaks, DestBounds);

    DestBounds.Left := 0;
    DestBounds.Top  := BackBuffer.Height * 0.5;
    DestBounds.Height := BackBuffer.Height * 0.5;
    DestBounds.Width  := BackBuffer.Width;
    DrawPeaks(PeakData.PeaksR, fFirstPeak, fVisiblePeaks, DestBounds);
  end;
end;

procedure TSampleImageBuffer.DrawSample_UsingPoints;
var
  DestBounds : TRectF;
begin
  assert(CalcPeakCount(SampleData.SampleFrames, Zoom, DisplayWidth) < DisplayWidth);

  if SampleData.ChannelCount = 1 then
  begin
    DestBounds.Left := 0;
    DestBounds.Top  := 0;
    DestBounds.Height := BackBuffer.Height;
    DestBounds.Width  := BackBuffer.Width;
    DrawPoints(SampleData.Ch1, SampleData.SampleFrames, DestBounds);
  end;

  if SampleData.ChannelCount = 2 then
  begin
    DestBounds.Left := 0;
    DestBounds.Top  := 0;
    DestBounds.Height := BackBuffer.Height * 0.5;
    DestBounds.Width  := BackBuffer.Width;
    DrawPoints(SampleData.Ch1, SampleData.SampleFrames, DestBounds);

    DestBounds.Left := 0;
    DestBounds.Top  := BackBuffer.Height * 0.5;
    DestBounds.Height := BackBuffer.Height * 0.5;
    DestBounds.Width  := BackBuffer.Width;
    DrawPoints(SampleData.Ch2, SampleData.SampleFrames, DestBounds);
  end;

end;

function TSampleImageBuffer.GetLineColor: TRedFoxColorString;
begin
  result := fLineColor.AsString;
end;

function TSampleImageBuffer.GetObject: TSampleImageBuffer;
begin
  result := self;
end;

procedure TSampleImageBuffer.DrawPeaks(const PeakData: TArrayOfPeak; const aFirstPeak, aPeakCount: integer; const DestBounds: TRectF);
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
  ab := fLineColor.A div 4;
  BackBuffer.BufferInterface.FillColor := fLineColor.WithAlpha(ab).AsAggRgba8;
  BackBuffer.BufferInterface.LineColor := fLineColor.WithAlpha(ab).AsAggRgba8;
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

  BackBuffer.BufferInterface.FillColor := fLineColor.AsAggRgba8;
  BackBuffer.BufferInterface.LineColor := fLineColor.AsAggRgba8;
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

procedure TSampleImageBuffer.DrawPoints(smps: PSingle; SampleFrames: integer; const DestBounds: TRectF);
var
  c1: Integer;
  x1,y1,x2,y2:single;
  OffsetX, OffsetY : integer;
  dw, dh : integer;
  ab : integer;
begin
  //===== Draw the zero point reference line ======
  ab := fLineColor.A div 3;
  BackBuffer.BufferInterface.FillColor := fLineColor.WithAlpha(ab).AsAggRgba8;
  BackBuffer.BufferInterface.LineColor := fLineColor.WithAlpha(ab).AsAggRgba8;
  BackBuffer.BufferInterface.LineWidth := 1;

  x1 := DestBounds.Left;
  x2 := DestBounds.Right;
  y1 := round(DestBounds.Top + (DestBounds.Height * 0.5)) + 0.5;
  y2 := y1;
  BackBuffer.BufferInterface.Line(x1, y1, x2,y2);





  BackBuffer.BufferInterface.LineColor := fLineColor;
  BackBuffer.BufferInterface.LineWidth := 1;

  //BackBuffer.BufferInterface.Line(DestBounds.Left, DestBounds.Top, DestBounds.Right, DestBounds.Bottom);


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

    BackBuffer.BufferInterface.Line(x1, y1, x2, y2);

    x2 := x1;
    y2 := y1;

    inc(smps);
  end;
end;

procedure TSampleImageBuffer.SetLineColor(const Value: TRedFoxColorString);
begin
  fLineColor.SetColor(Value);
end;

procedure TSampleImageBuffer.SetOffset(const Value: double);
begin
  fOffset := Value;
end;

procedure TSampleImageBuffer.SetZoom(const Value: double);
begin
  fZoom := Value;
end;

end.
