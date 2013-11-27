{
  The varaible Speed Delay class is very similar to a regular delay line class. Except the rate at which
  the read & write heads move though the delay buffer can be modified. This is conceptully similar to changing
  the tape speed in an old style tape delay effect.

  The Tape speed changes speed whilst the read and write heads remain in the same position. This has several
  effects, the echos get closer or further apart, and the audio already in the delay buffer changes pitch.

  The implementation of a variable speed delay is complicated because the read and write heads are both
  moving (relative to the buffer) at variable, non-integer rates. So the output values need to be interpolated, as
  do the values which need to be written to the buffer.


  This delay could be improve with a better interpolation scheme. At the moment it is using linear interpolation,
  which is noisy!! :(

}

unit eeVariableSpeedDelay_Stereo;

interface

uses
  eeDsp;

type
  TVariableSpeedDelay_Stereo = class
  private
    fStepSize: double;
    fRate: double;
    fDelayTime: double;
    fBufferSize: integer;
    procedure SetStepSize(const Value: double);
    procedure SetRate(const Value: double);
    procedure SetDelayTime(const Value: double);
    procedure SetBufferSize(const aBufferSize:integer);
  protected
    LastIn1, LastIn2:single;
    Buffer1, Buffer2:array of single;
    ReadIndex:double;
    WriteIndex:integer;
    Offset:single;
    SuspendProcessing:boolean;

    InBuf:TSingleSixElementStereoBuffer;  //Interpolation buffer.
    OutBuf:TSingleSixElementStereoBuffer;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure StepWrite(In1, In2:single); inline;
    procedure StepRead(out Out1, Out2:single);inline;

    //DelayTime is set in SampleFrames, always relative to the buffer size.
    property DelayTime :double  read fDelayTime write SetDelayTime; //range 0..aBufferSize-1
    property Rate      :double  read fRate      write SetRate;      //range 0..aMaxRate

    //NOTE: Will possibily need to reset the delay time after changing buffer size.
    property BufferSize:integer read fBufferSize write SetBufferSize;
  end;


implementation

uses
  SysUtils, Math;

{ TVariableSpeedDelay_Stereo }

constructor TVariableSpeedDelay_Stereo.Create;
begin
  fBufferSize   := 0;
  WriteIndex    := 0;
  ReadIndex     := 0;
  Rate          := 1;
  LastIn1       := 0;
  LastIn2       := 0;
  Offset        := 0;

  SuspendProcessing := false;

  InBuf.Init(0);
  OutBuf.Init(0);
end;

destructor TVariableSpeedDelay_Stereo.Destroy;
begin
  SetLength(Buffer1, 0);
  SetLength(Buffer2, 0);
  inherited;
end;

procedure TVariableSpeedDelay_Stereo.SetBufferSize(const aBufferSize: integer);
begin
  SetLength(Buffer1, aBufferSize);
  SetLength(Buffer2, aBufferSize);
  fBufferSize := aBufferSize;

  // NOTE: Will possibily need to reset the delay time after changing buffer size.
  // I decided not to handle the situation here as it's a low level component. Let the classes
  // using this decide what is appropiate.

  if fBufferSize <= WriteIndex then WriteIndex := 0;
  if fBufferSize <= ReadIndex  then ReadIndex  := 0;

end;

procedure TVariableSpeedDelay_Stereo.SetDelayTime(const Value: double);
begin
  assert(Value < BufferSize-2);

  fDelayTime := Value;

  ReadIndex := (WriteIndex+Offset) - fDelayTime;
  if ReadIndex < 0 then ReadIndex := ReadIndex + BufferSize;

  //Error checking...
  if (ReadIndex < 0) or (ReadIndex >= BufferSize) then
  begin
    raise Exception.Create('ReadIndex out of range');
    ReadIndex := 0;
  end;
end;

procedure TVariableSpeedDelay_Stereo.SetRate(const Value: double);
begin
  fRate := Value;
end;

procedure TVariableSpeedDelay_Stereo.SetStepSize(const Value: double);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fStepSize := Value;
end;

procedure TVariableSpeedDelay_Stereo.StepRead(out Out1, Out2: single);
var
  a, b:integer;
  f:single;
  ax0, ax1, ax2, ax3, ax4, ax5 :integer;
  x0, x1, x2, x3, x4, x5 :single;
begin
  a := floor(ReadIndex);
  f := ReadIndex-a;

  a := floor(ReadIndex + 3);   if a >= BufferSize then a := a - BufferSize;    ax0 := a;
  a := floor(ReadIndex + 2);   if a >= BufferSize then a := a - BufferSize;    ax1 := a;
  a := floor(ReadIndex + 1);   if a >= BufferSize then a := a - BufferSize;    ax2 := a;
  a := floor(ReadIndex + 0);                                                   ax3 := a;
  a := floor(ReadIndex - 1);   if a <= 0 then a := a + BufferSize;             ax4 := a;
  a := floor(ReadIndex - 2);   if a <= 0 then a := a + BufferSize;             ax5 := a;


  x0 := Buffer1[ax0];
  x1 := Buffer1[ax1];
  x2 := Buffer1[ax2];
  x3 := Buffer1[ax3];
  x4 := Buffer1[ax4];
  x5 := Buffer1[ax5];

  //Out1 := Hermite4Interpolation(1-f, x1, x2, x3, x4);
  Out1 := SplineInterpolation6Point5thOrder(1-f, x0,x1,x2,x3,x4,x5);


  x0 := Buffer2[ax0];
  x1 := Buffer2[ax1];
  x2 := Buffer2[ax2];
  x3 := Buffer2[ax3];
  x4 := Buffer2[ax4];
  x5 := Buffer2[ax5];

  //Out2 := Hermite4Interpolation(1-f, x1, x2, x3, x4);
  Out2 := SplineInterpolation6Point5thOrder(1-f, x0,x1,x2,x3,x4,x5);



  {
  a := floor(ReadIndex);
  b := a + 1;
  if b >= bufferSize then b := b - BufferSize;
  f := ReadIndex-a;

  Out1 := LinearInterpolation(Buffer1[a], Buffer1[b], f);
  Out2 := LinearInterpolation(Buffer2[a], Buffer2[b], f);
  }


  //ReadIndex := ReadIndex + Rate;
  ReadIndex := ReadIndex + 0.25;
  if ReadIndex >= BufferSize then ReadIndex := ReadIndex - BufferSize;


end;

procedure TVariableSpeedDelay_Stereo.StepWrite(In1, In2: single);
var
  x1, x2:single;
  x:integer;
  f:single;
begin
  if SuspendProcessing then exit;

  InBuf.Add(In1,In2);

  try
    x := 0;

    while (x + Offset) < Rate do
    begin
      f := (x + Offset) / Rate;
      assert(f <= 1);

      //x1 := LinearInterpolation(LastIn1, In1, f);
      //x2 := LinearInterpolation(LastIn2, In2, f);
      //x1 := Hermite4Interpolation(f, InBuf.ax1, InBuf.ax2, InBuf.ax3, InBuf.ax4);
      //x2 := Hermite4Interpolation(f, InBuf.bx1, InBuf.bx2, InBuf.bx3, InBuf.bx4);
      //x1 := Hermite4Interpolation(f, InBuf.ax4, InBuf.ax3, InBuf.ax2, InBuf.ax1);
      //x2 := Hermite4Interpolation(f, InBuf.bx4, InBuf.bx3, InBuf.bx2, InBuf.bx1);

      x1 := SplineInterpolation6Point5thOrder(1-f, InBuf.ax1, InBuf.ax2, InBuf.ax3, InBuf.ax4, InBuf.ax5, InBuf.ax6);
      x2 := SplineInterpolation6Point5thOrder(1-f, InBuf.bx1, InBuf.bx2, InBuf.bx3, InBuf.bx4, InBuf.bx5, InBuf.bx6);

      Buffer1[WriteIndex] := x1;
      Buffer2[WriteIndex] := x2;

      inc(x);
      inc(WriteIndex);
      if WriteIndex >= Buffersize then dec(WriteIndex, BufferSize);
    end;

    Offset := (x + Offset) - Rate;
    assert(Offset >= 0);
    assert(Offset <= 1);  // <-- error here but i don't see why...

    LastIn1 := In1;
    LastIn2 := In2;
  except
    SuspendProcessing := true;
  end;

end;

end.
