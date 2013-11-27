unit B2.DelayLine.StereoDelayBuffer;

interface

uses
  MoreTypes;

type
  TStereoDelayBuffer = class
  private
    fBufferSize: cardinal;
    fBuffer: T2dArrayOfSingle;
    fWriteIndex: integer;
    procedure SetBufferSize(const Value: cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure StepInput(In1, In2 : single); inline;
    procedure ReadTap(const ReadIndex : single; out Out1, Out2: single); inline;
    procedure ReadRelativeTap(const SampleDelay : single; out Out1, Out2:single); inline;

    property BufferSize : cardinal         read fBufferSize write SetBufferSize;
    property Buffer     : T2dArrayOfSingle read fBuffer     write fBuffer;

    property WriteIndex : integer read fWriteIndex write fWriteIndex;
  end;

implementation

uses
  Math,
  B2.Dsp;

{ TStereoDelayLine }

constructor TStereoDelayBuffer.Create;
begin
  fBufferSize := 0;
  SetLength(fBuffer, 2);
end;

destructor TStereoDelayBuffer.Destroy;
begin
  SetLength(fBuffer[0], 0);
  SetLength(fBuffer[1], 0);
  SetLength(fBuffer, 0);
  inherited;
end;

procedure TStereoDelayBuffer.SetBufferSize(const Value: cardinal);
begin
  // NOTE: Internally the buffer array is BufferSize+1. The extra
  // one is added to ease reading from the delay line with
  // linear interpolation.

  if Value <> fBufferSize then
  begin
    fBufferSize := Value;
    SetLength(fBuffer[0], Value + 1);
    SetLength(fBuffer[1], Value + 1);

    WriteIndex := BufferSize;
    Clear;
  end;
end;

procedure TStereoDelayBuffer.Clear;
var
  c1: Integer;
begin
  for c1 := 0 to BufferSize-1 do
  begin
    Buffer[0, c1] := 0;
    Buffer[1, c1] := 0;
  end;

  //Clear the extra array value.
  Buffer[0, BufferSize] := 0;
  Buffer[1, BufferSize] := 0;
end;

procedure TStereoDelayBuffer.StepInput(In1, In2: single);
begin
  // NOTE: The buffer contains an extra element at the end. When the Write position
  // loops around, the same value is written to the start of the array and the extra
  // element at the end. The extra element is useful when reading the buffer while
  // using linear interpolation.

  inc(fWriteIndex);

  if WriteIndex >= BufferSize then
  begin
    WriteIndex := 0;
    Buffer[0, 0] := In1;
    Buffer[1, 0] := In2;
    Buffer[0, BufferSize] := In1;
    Buffer[1, BufferSize] := In2;
  end else
  begin
    Buffer[0, WriteIndex] := In1;
    Buffer[1, WriteIndex] := In2;
  end;

end;

procedure TStereoDelayBuffer.ReadTap(const ReadIndex: single; out Out1, Out2: single);
var
  a, b   : integer;
  f      : single;
  y0, y1 : single;
begin
  assert(ReadIndex>= 0);
  assert(ReadIndex <= BufferSize-1);

  a := floor(ReadIndex);
  b := a + 1;
  f := ReadIndex - a;

  y0 := Buffer[0, a];
  y1 := Buffer[0, b];
  Out1 := LinearInterpolation(f, y0, y1);

  y0 := Buffer[1, a];
  y1 := Buffer[1, b];
  Out2 := LinearInterpolation(f, y0, y1);
end;

procedure TStereoDelayBuffer.ReadRelativeTap(const SampleDelay: single; out Out1, Out2: single);
var
  a, b   : integer;
  f      : single;
  y0, y1 : single;
  ReadIndex : single;
begin
  assert(SampleDelay < BufferSize-1);
  assert(SampleDelay >= 0);

  ReadIndex := WriteIndex - SampleDelay;
  if ReadIndex < 0 then ReadIndex := ReadIndex + BufferSize;

  a := floor(ReadIndex);
  b := a + 1;
  f := ReadIndex - a;

  y0 := Buffer[0, a];
  y1 := Buffer[0, b];
  Out1 := LinearInterpolation(f, y0, y1);

  y0 := Buffer[1, a];
  y1 := Buffer[1, b];
  Out2 := LinearInterpolation(f, y0, y1);
end;





end.
