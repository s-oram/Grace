unit LucidityGui.Scope.SignalRecorder;

interface

uses
  Types,
  RedFoxColor,
  RedFoxImageBuffer,
  VamLib.MoreTypes;

type
  IScopeSignalRecorder = interface
    ['{A01F1BD5-F4DD-48A2-A466-5AD7C9195647}']
    procedure GetReadPointer(out WriteIndex, BufferSize : integer; out Buffer : PArrayOfSingle);
  end;

  TSignalDisplay = class
  private
    BackBuffer: TRedFoxImageBuffer;
    ResetRequired : boolean;
    ReadIndex  : integer;
    WriteIndex : integer;
    BufferSize : integer;
    DrawXIndex : integer;
    fLineColor: TRedFoxColor;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(w,h:integer);

    procedure ProcessSignal(Dest : TRedFoxImageBuffer; const DestRect:TRect; const Source: IScopeSignalRecorder);
    procedure DrawTo(Dest : TRedFoxImageBuffer; const DestRect:TRect);

    property LineColor : TRedFoxColor read fLineColor write fLineColor;
  end;

implementation

uses
  AggBasics,
  AggPixelFormat,
  VamLib.Utils,
  Graphics;

{ TSignalDisplay }

constructor TSignalDisplay.Create;
begin
  BackBuffer := TRedFoxImageBuffer.Create;
  ResetRequired := true;

  ReadIndex  := 0;
  WriteIndex := 0;
  BufferSize := 0;
end;

destructor TSignalDisplay.Destroy;
begin
  BackBuffer.Free;
  inherited;
end;

procedure TSignalDisplay.SetSize(w, h: integer);
begin
  if (w > 0) and (h > 0) then
  begin
    BackBuffer.SetSize(w, h);
    BackBuffer.BufferInterface.ClearAll(0,0,0,0);
  end;
end;

procedure TSignalDisplay.ProcessSignal(Dest : TRedFoxImageBuffer; const DestRect:TRect; const Source: IScopeSignalRecorder);
  function CalcSamplesAvailable(OldWriteIndex, CurrentWriteIndex, BufferSize : integer):integer;
  begin
    if OldWriteIndex < CurrentWriteIndex
      then exit(CurrentWriteIndex - OldWriteIndex);

    if OldWriteIndex > CurrentWriteIndex
      then exit(BufferSize - OldWriteIndex - 1 + CurrentWriteIndex);

    // If we've made it this far...
    result := 0;
  end;

  procedure FindMinMaxValues(var ReadIndex :integer; const SampleFrames, BufferSize : integer; Buffer : PArrayOfSingle; out MinBuffer, MaxBuffer : single);
  var
    tMin, tMax : single;
    c1: Integer;
  begin
    tMin := 10;
    tMax := -10;

    for c1 := 0 to SampleFrames-1 do
    begin
      if tMin > Buffer^[ReadIndex] then tMin := Buffer^[ReadIndex];
      if tMax < Buffer^[ReadIndex] then tMax := Buffer^[ReadIndex];

      inc(ReadIndex);
      if ReadIndex >= BufferSize then ReadIndex := 0;
    end;

    MinBuffer := tMin;
    MaxBuffer := tMax;
  end;
const
  SamplesPerPixel : integer = 256;
var
  CurrentWriteIndex : integer;
  Buffer : PArrayOfSingle;
  SamplesAvailable : integer;
  MinBuffer, MaxBuffer : single;

  y1, y2 : single;
  x1, x2 : single;
begin
  BackBuffer.BufferInterface.LineColor := LineColor;

  if ResetRequired then
  begin
    ResetRequired := false;
    BackBuffer.BufferInterface.ClearAll(0,0,0,0);

    Source.GetReadPointer(WriteIndex, BufferSize, Buffer);
    ReadIndex := WriteIndex;
    DrawXIndex := 0;

    BackBuffer.BufferInterface.LineWidth := 1;
    BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;
  end else
  begin
    Source.GetReadPointer(CurrentWriteIndex, BufferSize, Buffer);

    SamplesAvailable := CalcSamplesAvailable(WriteIndex, CurrentWriteIndex, BufferSize);

    while SamplesAvailable >= SamplesPerPixel do
    begin
      dec(SamplesAvailable, SamplesPerPixel);

      FindMinMaxValues(ReadIndex, SamplesPerPixel, BufferSize, Buffer, MinBuffer, MaxBuffer);

      y1 := (1 - Clamp((MaxBuffer * 0.5 + 0.5), 0, 1)) * BackBuffer.Height;
      y2 := (1 - Clamp((MinBuffer * 0.5 + 0.5), 0, 1)) * BackBuffer.Height;

      if abs(y1 - y2) <= 2 then
      begin
        y1 := y1 - 1;
        y2 := y2 + 1;
      end;

      BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmClear;
      BackBuffer.BufferInterface.Line(DrawXIndex + 0.5, 0, DrawXIndex + 0.5, BackBuffer.Height);

      BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmAlpha2;
      Backbuffer.BufferInterface.Line(DrawXIndex + 0.5, y1, DrawXIndex + 0.5, y2);

      inc(DrawXIndex);
      if DrawXIndex >= BackBuffer.Width then
      begin
        DrawXIndex := 0;
      end;

      inc(WriteIndex, SamplesPerPixel);
      if WriteIndex >= BufferSize then WriteIndex := WriteIndex - BufferSize;
    end;
  end;
end;

procedure TSignalDisplay.DrawTo(Dest: TRedFoxImageBuffer; const DestRect: TRect);
begin
  BackBuffer.RedFoxInterface.BlendTo(Dest.RedFoxInterface, DestRect.Left, DestRect.Top);
end;





end.
