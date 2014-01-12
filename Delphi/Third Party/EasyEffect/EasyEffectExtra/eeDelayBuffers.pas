{
  This unit contains a collection of delay buffer classes. These classes should
  all be designed with direct implementation into production code in mide.
  Ie. efficient where possible.
}

unit eeDelayBuffers;

interface

uses
  VamLib.MoreTypes;

type
  TFixedSizeBuffer256Mono = class
  private
  public
    constructor Create;
	  destructor Destroy; override;
  end;

  TFixedSizeBuffer1024Mono = class
  private
    fBuffer: TArrayOfSingle;
    fDelayTime: integer;
    fOutput: single;
    procedure SetDelayTime(const Value: integer);
  protected
    ReadIndex:word;
    WriteIndex:word;
    StepSize:integer;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure StepInput(const In1:single); inline;
    procedure InitBuffer(const Value:single = 0);

    property Buffer : TArrayOfSingle read fBuffer write fBuffer;

    property DelayTime  : integer read fDelayTime write SetDelayTime; //In samples..
    property Output : single read fOutput;
  end;



  // A 2048 element delay buffer. 2048 is enough for 10 milli-seconds of delay at
  TFixedSizeBuffer2048Stereo = class
  private
    fDelayTime: integer;
    fBuffer2: TArrayOfSingle;
    fBuffer1: TArrayOfSingle;

    procedure SetDelayTime(const Value: integer);
  protected
    ReadIndex  : word;
    WriteIndex : word;
    StepSize   : integer;
    property Buffer1 : TArrayOfSingle read fBuffer1 write fBuffer1;
    property Buffer2 : TArrayOfSingle read fBuffer2 write fBuffer2;
  public
    constructor Create;
	  destructor Destroy; override;
    procedure InitBuffer(const Value:single = 0);
    procedure Step(const In1, In2:single; out Out1, Out2:single); inline;
    property DelayTime : integer read fDelayTime write SetDelayTime; //In samples...
  end;


implementation

{ TFixedSizeBuffer256Mono }

constructor TFixedSizeBuffer256Mono.Create;
begin

end;

destructor TFixedSizeBuffer256Mono.Destroy;
begin

  inherited;
end;

{ TFixedSizeBuffer1024Mono }

constructor TFixedSizeBuffer1024Mono.Create;
begin
  SetLength(fBuffer, 1024);
  StepSize     := 65536 div 1024; //equals 64.
  WriteIndex   := 0;
  DelayTime    := 100;
  fOutput      := 0;
end;

destructor TFixedSizeBuffer1024Mono.Destroy;
begin
  SetLength(fBuffer, 0);
  inherited;
end;

procedure TFixedSizeBuffer1024Mono.InitBuffer(const Value: single);
const
  BufferSize = 1024;
var
  c1: Integer;
begin
  for c1 := 0 to BufferSize - 1 do
  begin
    fBuffer[c1] := Value;
  end;
  fOutput      := Value;
end;

procedure TFixedSizeBuffer1024Mono.SetDelayTime(const Value: integer);
const
  BufferSize = 1024;
begin
  assert(Value < BufferSize);
  fDelayTime := Value;

  if (StepSize * Value) > WriteIndex
    then ReadIndex  := High(Word) - ((StepSize * Value) - WriteIndex)
    else ReadIndex  := WriteIndex - (StepSize * Value);
end;

procedure TFixedSizeBuffer1024Mono.StepInput(const In1: single);
begin
  fBuffer[WriteIndex shr 6] := In1;
  fOutput := fBuffer[ReadIndex shr 6];

  {$ifopt Q+}
     {$define TOGGLE_OVERFLOW_CHECK}
     {$Q-}
  {$endif}
    // NOTE: Turn off overflow checking so we can take advantage of the integer overflow
    // for automatic wraparound.
    inc(WriteIndex, StepSize);
    inc(ReadIndex, StepSize);
  {$ifdef TOGGLE_OVERFLOW_CHECK}
     {$undef TOGGLE_OVERFLOW_CHECK}
     {$Q+}
  {$endif}
end;

{ TFixedSizeBuffer2048Stereo }

constructor TFixedSizeBuffer2048Stereo.Create;
begin
  SetLength(fBuffer1, 2048);
  SetLength(fBuffer2, 2048);

  StepSize := 65536 div 2048;  //equals 32.

  WriteIndex := 0;
  ReadIndex  := 0;
end;

destructor TFixedSizeBuffer2048Stereo.Destroy;
begin
  SetLength(fBuffer1, 0);
  SetLength(fBuffer2, 0);
  inherited;
end;

procedure TFixedSizeBuffer2048Stereo.InitBuffer(const Value: single);
const
  BufferSize = 2048;
var
  c1:integer;
begin
  for c1 := 0 to BufferSize - 1 do
  begin
    fBuffer1[c1] := Value;
    fBuffer2[c1] := Value;
  end;
end;

procedure TFixedSizeBuffer2048Stereo.SetDelayTime(const Value: integer);
const
  BufferSize = 2048;
begin
  assert(Value < BufferSize);

  fDelayTime := Value;

  {$ifopt Q+}
     {$define TOGGLE_OVERFLOW_CHECK}
     {$Q-}
  {$endif}
    // NOTE: Turn off overflow checking so we can take advantage of the integer overflow
    // for automatic wraparound.
    //ReadIndex  := WriteIndex - (StepSize * Value);
  {$ifdef TOGGLE_OVERFLOW_CHECK}
     {$undef TOGGLE_OVERFLOW_CHECK}
     {$Q+}
  {$endif}

  if (StepSize * Value) > WriteIndex
    then ReadIndex  := High(Word) - ((StepSize * Value) - WriteIndex)
    else ReadIndex  := WriteIndex - (StepSize * Value);
end;

procedure TFixedSizeBuffer2048Stereo.Step(const In1, In2: single; out Out1, Out2: single);
begin
  fBuffer1[WriteIndex shr 5] := In1;
  fBuffer2[WriteIndex shr 5] := In2;

  Out1 := fBuffer1[ReadIndex shr 5];
  Out2 := fBuffer2[ReadIndex shr 5];

  {$ifopt Q+}
     {$define TOGGLE_OVERFLOW_CHECK}
     {$Q-}
  {$endif}
    // NOTE: Turn off overflow checking so we can take advantage of the integer overflow
    // for automatic wraparound.
    inc(WriteIndex, StepSize);
    inc(ReadIndex, StepSize);
  {$ifdef TOGGLE_OVERFLOW_CHECK}
     {$undef TOGGLE_OVERFLOW_CHECK}
     {$Q+}
  {$endif}
end;

end.
