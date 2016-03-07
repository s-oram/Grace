unit VamAudio.RealTimeInterpolationFilter;

interface

// This unit contains interpolation filters. They increase the sampling rate and filter
// out the resulting spectral images.
//
// Internally, the interpolation filters are polyphase IIR filters. Polyphase IIR filters are relatively efficient
// because they operate at the lower, input samplerate. Therefore they only need to process half as many samples.
//
// The implementation here is informed by Dave Muon's polyphase filters found on www.musicdsp.org.
//
// References:
//
//   "Digital Signal Processing Schemes for Efficient Interpolation and Decimation"
//   - Reinaldo A. Valenzuela, A. G. Constantinides
//
//   "High Performance IIR Filters for Interpolation and Decimation"
//   - Dr David Wheeler.

uses
  VamLib.MoreTypes;

type
  TAllpassFilterStage = record
  public
    x0, x2 : double;   // filter inputs.
    y0, y2 : double;   // filter outputs.
    a : double;
    procedure Reset;
    function Process(const Input : double):double;
  end;

  // Increase samplerate by two. Use 6th order halfband filter to reduce alaising.
  TInterpolationFilter2x6thOrder = class
  private
    c1f1 : TAllpassFilterStage; // cascade 1 - filter 1.
    c1f2 : TAllpassFilterStage; // cascade 1 - filter 2.
    c1f3 : TAllpassFilterStage; // cascade 1 - filter 3.

    c2f1 : TAllpassFilterStage; // cascade 2 - filter 1.
    c2f2 : TAllpassFilterStage; // cascade 2 - filter 2.
    c2f3 : TAllpassFilterStage; // cascade 2 - filter 3.
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure Process(Input, Output: PSingle; const InputSampleFrames : integer; out OutputSampleFrames: integer);
    procedure ProcessDouble(Input, Output: PDouble; const InputSampleFrames : integer; out OutputSampleFrames: integer);
  end;

implementation

{ TAllpassFilterStage }

function TAllpassFilterStage.Process(const Input: double): double;
begin
  x2 := x0;
  x0 := Input;
  y2 := y0;
  y0 := x2 + ((Input - y2) * a);
  result := y0;
end;

procedure TAllpassFilterStage.Reset;
begin
  x0 := 0;
  x2 := 0;
  y0 := 0;
  y2 := 0;
end;


{ TInterpolationFilter2x6thOrder }

constructor TInterpolationFilter2x6thOrder.Create;
begin
  // 6th order polyphase allpass halfband filter coefficients. (rejection=80dB, transition band=0.05)
  c1f1.a := 0.06029739095712437;
  c1f2.a := 0.4125907203610563;
  c1f3.a := 0.7727156537429234;

  c2f1.a := 0.21597144456092948;
  c2f2.a := 0.6043586264658363;
  c2f3.a := 0.9238861386532906;

  Reset;
end;

destructor TInterpolationFilter2x6thOrder.Destroy;
begin

  inherited;
end;

procedure TInterpolationFilter2x6thOrder.Reset;
begin
  c1f1.Reset;
  c1f2.Reset;
  c1f3.Reset;

  c2f1.Reset;
  c2f2.Reset;
  c2f3.Reset;
end;


procedure TInterpolationFilter2x6thOrder.Process(Input, Output: PSingle; const InputSampleFrames: integer; out OutputSampleFrames: integer);
var
  x : double;
  y : double;
  c1 : integer;
begin
  assert(InputSampleFrames >= 1);

  OutputSampleFrames := InputSampleFrames * 2;

  for c1 := 0 to InputSampleFrames-1 do
  begin
    // cascade A
    c1f1.x2 := c1f1.x0;
    c1f1.x0 := Input^; //<-- filter input
    c1f1.y2 := c1f1.y0;
    c1f1.y0 := c1f1.x2 + ((c1f1.x0 - c1f1.y2) * c1f1.a);

    c1f2.x2 := c1f2.x0;
    c1f2.x0 := c1f1.y0; //<-- filter input
    c1f2.y2 := c1f2.y0;
    c1f2.y0 := c1f2.x2 + ((c1f2.x0 - c1f2.y2) * c1f2.a);

    c1f3.x2 := c1f3.x0;
    c1f3.x0 := c1f2.y0; //<-- filter input
    c1f3.y2 := c1f3.y0;
    c1f3.y0 := c1f3.x2 + ((c1f3.x0 - c1f3.y2) * c1f3.a);

    // cascade B
    c2f1.x2 := c2f1.x0;
    c2f1.x0 := Input^; //<-- filter input
    c2f1.y2 := c2f1.y0;
    c2f1.y0 := c2f1.x2 + ((c2f1.x0 - c2f1.y2) * c2f1.a);

    c2f2.x2 := c2f2.x0;
    c2f2.x0 := c2f1.y0; //<-- filter input
    c2f2.y2 := c2f2.y0;
    c2f2.y0 := c2f2.x2 + ((c2f2.x0 - c2f2.y2) * c2f2.a);

    c2f3.x2 := c2f3.x0;
    c2f3.x0 := c2f2.y0; //<-- filter input
    c2f3.y2 := c2f3.y0;
    c2f3.y0 := c2f3.x2 + ((c2f3.x0 - c2f3.y2) * c2f3.a);

    // Grab the final cascade outputs.
    x := c1f3.y0;
    y := c2f3.y0;

    inc(Input);

    Output^ := x;
    inc(Output);

    Output^ := y;
    inc(Output);
  end;
end;

procedure TInterpolationFilter2x6thOrder.ProcessDouble(Input, Output: PDouble; const InputSampleFrames: integer; out OutputSampleFrames: integer);
var
  x : double;
  y : double;
  c1 : integer;
begin
  assert(InputSampleFrames >= 1);

  OutputSampleFrames := InputSampleFrames * 2;

  for c1 := 0 to InputSampleFrames-1 do
  begin
    // cascade A
    c1f1.x2 := c1f1.x0;
    c1f1.x0 := Input^; //<-- filter input
    c1f1.y2 := c1f1.y0;
    c1f1.y0 := c1f1.x2 + ((c1f1.x0 - c1f1.y2) * c1f1.a);

    c1f2.x2 := c1f2.x0;
    c1f2.x0 := c1f1.y0; //<-- filter input
    c1f2.y2 := c1f2.y0;
    c1f2.y0 := c1f2.x2 + ((c1f2.x0 - c1f2.y2) * c1f2.a);

    c1f3.x2 := c1f3.x0;
    c1f3.x0 := c1f2.y0; //<-- filter input
    c1f3.y2 := c1f3.y0;
    c1f3.y0 := c1f3.x2 + ((c1f3.x0 - c1f3.y2) * c1f3.a);

    // cascade B
    c2f1.x2 := c2f1.x0;
    c2f1.x0 := Input^; //<-- filter input
    c2f1.y2 := c2f1.y0;
    c2f1.y0 := c2f1.x2 + ((c2f1.x0 - c2f1.y2) * c2f1.a);

    c2f2.x2 := c2f2.x0;
    c2f2.x0 := c2f1.y0; //<-- filter input
    c2f2.y2 := c2f2.y0;
    c2f2.y0 := c2f2.x2 + ((c2f2.x0 - c2f2.y2) * c2f2.a);

    c2f3.x2 := c2f3.x0;
    c2f3.x0 := c2f2.y0; //<-- filter input
    c2f3.y2 := c2f3.y0;
    c2f3.y0 := c2f3.x2 + ((c2f3.x0 - c2f3.y2) * c2f3.a);

    // Grab the final cascade outputs.
    x := c1f3.y0;
    y := c2f3.y0;

    inc(Input);

    Output^ := x;
    inc(Output);

    Output^ := y;
    inc(Output);
  end;
end;




end.
