unit RedFox.SampleDisplay.BackBuffer;

interface

uses
  System.Classes, System.Types, Graphics,
  RedFox,
  RedFoxImageBuffer;

type
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


  TBackBufferController = class
  private
    fOffset: double;
    fZoom: double;
    fBackBuffer  : TRedFoxImageBuffer;
    procedure SetZoom(const Value: double);
    procedure SetOffset(const Value: double);
    function GetLineColor: TRedFoxColorString;
    procedure SetLineColor(const Value: TRedFoxColorString);
  protected
    SampleData        : TSampleDisplayInfo;
    fPeakData         : TPeakData;
    fFirstPeak        : integer;
    fVisiblePeaks     : integer;
    DisplayWidth      : integer;
    DisplayHeight     : integer;
    fLineColor        : TRedFoxColor;

    function CalcPeakCount(SampleFrames:integer; ZoomFactor : double):integer;
    procedure DrawPeaks(const PeakData : TArrayOfPeak; const aFirstPeak, aPeakCount : integer; const DestBounds : TRectF);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DrawSample(SDI:TSampleDisplayInfo); overload;
    procedure DrawSample; overload;
    procedure ClearSample(InvalidateDisplay : boolean = true);



    property BackBuffer : TRedFoxImageBuffer read fBackBuffer write fBackBuffer;
    property Zoom   : double read fZoom write SetZoom;
    property Offset : double read fOffset write SetOffset;

    //Call resize to set the size of the sample display back buffer.
    procedure Resize(Width, Height:integer);


    function SamplePosToPixelPos(x1:integer):single;
    function PixelPosToSamplePos(x1:single):integer;

    property PeakData : TPeakData read fPeakData;

    property FirstPeak    : integer read fFirstPeak;
    property VisiblePeaks : integer read fVisiblePeaks;

    property LineColor : TRedFoxColorString read GetLineColor          write SetLineColor;
  end;

implementation

uses
  Math;

const
  Msg_DrawSample = 1;
  Msg_SampleBitmapReady = 2;
  Msg_PaintFinished     = 3;

procedure GetMinMaxValuesFloat(Data:PSingle; FirstSample, LastSample:integer; Out MinValue, MaxValue:single); inline;
var
  SampleData:array of single absolute Data;
  c2:integer;
  lv,hv:single;
  Value:single;
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





{ TBackBufferController }

constructor TBackBufferController.Create;
begin
  BackBuffer := TRedFoxImageBuffer.Create;

  fPeakData   := TPeakData.Create;

  fFirstPeak    := 0;
  fVisiblePeaks := 0;
end;

destructor TBackBufferController.Destroy;
begin
  BackBuffer.Free;
  fPeakData.Free;

  inherited;
end;

procedure TBackBufferController.Resize(Width, Height:integer);
begin
  DisplayWidth  := Width;
  DisplayHeight := Height;
  BackBuffer.SetSize(Width,Height);
end;

function TBackBufferController.CalcPeakCount(SampleFrames: integer; ZoomFactor: double): integer;
var
  MinPeakCount : integer;
  MaxPeakCount : integer;
begin
  assert(ZoomFactor >= 0);
  assert(ZoomFactor <= 1);

  MinPeakCount := DisplayWidth;
  MaxPeakCount := SampleFrames;

  result := round(MinPeakCount * (1 - ZoomFactor) + (MaxPeakCount * ZoomFactor));
end;

procedure TBackBufferController.DrawSample(SDI: TSampleDisplayInfo);
begin
  SampleData := SDI;
  DrawSample;
end;

procedure TBackBufferController.ClearSample(InvalidateDisplay : boolean = true);
begin
  SampleData.IsValid := false;
  if InvalidateDisplay then DrawSample;
end;

procedure TBackBufferController.DrawSample;
var
  DestBounds : TRectF;
  x : integer;
  CenterPeak : integer;
begin
  if SampleData.IsValid = false then
  begin
    PeakData.PeakCount := 0;
    BackBuffer.BufferInterface.ClearAll(0,0,0,0);
  end;

  if SampleData.IsValid = true then
  begin
    x := CalcPeakCount(SampleData.SampleFrames, Zoom);
    if x <> PeakData.PeakCount then
    begin
      PeakData.PeakCount := x;
    end;
    PeakData.CalcPeaks(SampleData);
    fVisiblePeaks := DisplayWidth;
    CenterPeak   := round(Offset * (PeakData.PeakCount- fVisiblePeaks)) + (fVisiblePeaks div 2);
    fFirstPeak    := CenterPeak - (fVisiblePeaks div 2);
    if fFirstPeak < 0 then fFirstPeak := 0;
    if fFirstPeak > PeakData.PeakCount - fVisiblePeaks then fFirstPeak := PeakData.PeakCount - fVisiblePeaks;

    BackBuffer.BufferInterface.ClearAll(0,0,0,0);
    BackBuffer.BufferInterface.FillColor := fLineColor.AsAggRgba8;
    BackBuffer.BufferInterface.LineColor := fLineColor.AsAggRgba8;
    BackBuffer.BufferInterface.LineWidth := 2;

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
end;


function TBackBufferController.GetLineColor: TRedFoxColorString;
begin
  result := fLineColor.AsString;
end;

procedure TBackBufferController.DrawPeaks(const PeakData: TArrayOfPeak; const aFirstPeak, aPeakCount: integer; const DestBounds: TRectF);
const
  OneOver255 : double = 1 / 255;
var
  c1: Integer;
  Index : integer;
  pt1, pt2 : TPointF;
  NewY1, NewY2 : single;
  OldY1, OldY2 : single;
  AreaHeight   : single;
begin
  assert(aFirstPeak >= 0);

  AreaHeight := DestBounds.Height-1;

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

procedure TBackBufferController.SetLineColor(const Value: TRedFoxColorString);
begin
  fLineColor.SetColor(Value);
end;

procedure TBackBufferController.SetOffset(const Value: double);
begin
  fOffset := Value;
end;

procedure TBackBufferController.SetZoom(const Value: double);
begin
  fZoom := Value;
end;

function TBackBufferController.PixelPosToSamplePos(x1: single): integer;
var
  ScaledX : single;
begin
  if SampleData.IsValid then
  begin
    ScaledX := fFirstPeak + x1;
    if ScaledX < 0 then ScaledX := 0;
    if ScaledX > PeakData.PeakCount then ScaledX := PeakData.PeakCount;

    result := round((SampleData.SampleFrames-1) * (ScaledX / PeakData.PeakCount));
  end else
  begin
    result := 0;
  end;
end;

function TBackBufferController.SamplePosToPixelPos(x1: integer): single;
var
  ScaledX : single;
begin
  if SampleData.IsValid then
  begin
    ScaledX := x1 / (SampleData.SampleFrames-1) * PeakData.PeakCount - fFirstPeak;
    result := ScaledX;
  end else
  begin
    result := 0;
  end;
end;

end.
