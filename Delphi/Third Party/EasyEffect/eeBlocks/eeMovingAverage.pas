unit eeMovingAverage;

interface

uses
  eeDelayBuffers, MoreTypes;

type
  TMovingAverageFilter = class
  private
    fAverageTime: integer;
    fLevel1: double;
    procedure SetAverageTime(const Value: integer);
  protected
    DelayBuffer:TFixedSizeBuffer1024Mono;
    z1:single;
    OneOverDelayTime:double;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Step(const In1:single); overload; inline;

    //The Average time is set in samples and has a maximum average time of 1023 samples.
    // The maximum time can be increase by dropping in a new delay buffer class.
    property AverageTime : integer read fAverageTime write SetAverageTime; //Samples...

    property Level1  : double read fLevel1;
  end;

implementation


{ TMovingAverageFilter }

// TMovingAverageFilter NOTES
// The moving average filter uses a CIC filter to track the running sum.
// The CIC filter is a comb filter followed by an integrator stage.
// The CIC filter is described on page 398 of "Understanding Digital signal processing".

constructor TMovingAverageFilter.Create;
begin
  DelayBuffer := TFixedSizeBuffer1024Mono.Create;
  z1 := 0;
  AverageTime := 6;
  fLevel1 := 0;
end;

destructor TMovingAverageFilter.Destroy;
begin
  DelayBuffer.Free;
  inherited;
end;

procedure TMovingAverageFilter.SetAverageTime(const Value: integer);
begin
  fAverageTime := Value;

  assert(Value < 1024);
  DelayBuffer.DelayTime := Value;
  OneOverDelayTime      := 1 / Value;
end;

procedure TMovingAverageFilter.Step(const In1: single);
var
  x:double;
begin
  DelayBuffer.StepInput(In1);

  x       := In1 - DelayBuffer.Output;
  x       := x + z1;
  z1      := x;

  fLevel1 := x * OneOverDelayTime;
end;


end.
