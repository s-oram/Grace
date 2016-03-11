unit VamAudio.RealTimeDecimationFilter;

interface

// This unit contains decimation filters. They reduce the bandwidth and sampling rate of
// a signal together in one step.
// Internally, they are using polyphase IIR filters. These filters are relatively efficient
// because they operate at the lower, output samplerate. Therefore they only need to process
// half as many samples.
//
// The implementation here is informed by Dave Muon's polyphase filters found on www.musicdsp.org.
//
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

  // Downsample by two. Use 6th order halfband filter to reduce alaising.
  TDecimationFilter2x6thOrder = class
  private
    Input_Delay : double;

    c1f1 : TAllpassFilterStage;
    c1f2 : TAllpassFilterStage;
    c1f3 : TAllpassFilterStage;

    c2f1 : TAllpassFilterStage;
    c2f2 : TAllpassFilterStage;
    c2f3 : TAllpassFilterStage;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure Process(Input, Output: PSingle; const InputSampleFrames : integer; out OutputSampleFrames: integer);
    procedure ProcessDouble(Input, Output: PDouble; const InputSampleFrames : integer; out OutputSampleFrames: integer);
  end;

  // Downsample by two. Use 8th order halfband filter to reduce alaising.
  TDecimationFilter2x8thOrder = class
  private
    Input_Delay : double;

    c1f1 : TAllpassFilterStage;
    c1f2 : TAllpassFilterStage;
    c1f3 : TAllpassFilterStage;
    c1f4 : TAllpassFilterStage;

    c2f1 : TAllpassFilterStage;
    c2f2 : TAllpassFilterStage;
    c2f3 : TAllpassFilterStage;
    c2f4 : TAllpassFilterStage;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure Process(Input, Output: PSingle; const InputSampleFrames : integer; out OutputSampleFrames: integer);
    procedure ProcessDouble(Input, Output: PDouble; const InputSampleFrames : integer; out OutputSampleFrames: integer);
  end;

implementation

uses
  VamAudio.Constants;

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

{ TDecimationFilter2x6thOrder }

constructor TDecimationFilter2x6thOrder.Create;
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

destructor TDecimationFilter2x6thOrder.Destroy;
begin
  inherited;
end;

procedure TDecimationFilter2x6thOrder.Reset;
begin
  c1f1.Reset;
  c1f2.Reset;
  c1f3.Reset;

  c2f1.Reset;
  c2f2.Reset;
  c2f3.Reset;

  Input_Delay := 0;
end;

procedure TDecimationFilter2x6thOrder.Process(Input, Output: PSingle; const InputSampleFrames : integer; out OutputSampleFrames: integer);
var
  x : double;
  y : double;
  c1 : integer;
begin
  assert(not odd(InputSampleFrames));
  assert(InputSampleFrames >= 2);

  OutputSampleFrames := InputSampleFrames div 2;

  for c1 := 0 to OutputSampleFrames-1 do
  begin
    // cascade A
    c1f1.x2 := c1f1.x0;
    c1f1.x0 := Input^ + kDenormal; //<-- filter input
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
    c2f1.x0 := Input_Delay; //<-- filter input
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
    Input_Delay  := Input^ + kDenormal;
    inc(Input);

    Output^ := (x + y) * 0.5;
    inc(Output);
  end;
end;

procedure TDecimationFilter2x6thOrder.ProcessDouble(Input, Output: PDouble; const InputSampleFrames: integer; out OutputSampleFrames: integer);
var
  x : double;
  y : double;
  c1 : integer;
begin
  assert(not odd(InputSampleFrames));
  assert(InputSampleFrames >= 2);

  OutputSampleFrames := InputSampleFrames div 2;

  for c1 := 0 to OutputSampleFrames-1 do
  begin
    // cascade A
    c1f1.x2 := c1f1.x0;
    c1f1.x0 := Input^ + kDenormal; //<-- filter input
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
    c2f1.x0 := Input_Delay; //<-- filter input
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
    Input_Delay  := Input^ + kDenormal;
    inc(Input);

    Output^ := (x + y) * 0.5;
    inc(Output);
  end;
end;


{ TDecimationFilter2x8thOrder }

constructor TDecimationFilter2x8thOrder.Create;
begin
  // 8th order polyphase allpass halfband filter coefficients. (rejection=106dB, transition band=0.05)
  c1f1.a := 0.03583278843106211;
  c1f2.a := 0.2720401433964576;
  c1f3.a := 0.5720571972357003;
  c1f4.a := 0.827124761997324;

  c2f1.a := 0.1340901419430669;
  c2f2.a := 0.4243248712718685;
  c2f3.a := 0.7062921421386394;
  c2f4.a := 0.9415030941737551;

  Reset;
end;

destructor TDecimationFilter2x8thOrder.Destroy;
begin
  inherited;
end;

procedure TDecimationFilter2x8thOrder.Reset;
begin
  c1f1.Reset;
  c1f2.Reset;
  c1f3.Reset;
  c1f4.Reset;

  c2f1.Reset;
  c2f2.Reset;
  c2f3.Reset;
  c2f4.Reset;

  Input_Delay := 0;
end;

procedure TDecimationFilter2x8thOrder.Process(Input, Output: PSingle; const InputSampleFrames : integer; out OutputSampleFrames: integer);
var
  x : double;
  y : double;
  c1 : integer;
begin
  assert(not odd(InputSampleFrames));
  assert(InputSampleFrames >= 2);

  OutputSampleFrames := InputSampleFrames div 2;

  for c1 := 0 to OutputSampleFrames-1 do
  begin
    // cascade A
    c1f1.x2 := c1f1.x0;
    c1f1.x0 := Input^ + kDenormal; //<-- filter input
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

    c1f4.x2 := c1f4.x0;
    c1f4.x0 := c1f3.y0; //<-- filter input
    c1f4.y2 := c1f4.y0;
    c1f4.y0 := c1f4.x2 + ((c1f4.x0 - c1f4.y2) * c1f4.a);

    // cascade B
    c2f1.x2 := c2f1.x0;
    c2f1.x0 := Input_Delay; //<-- filter input
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

    c2f4.x2 := c2f4.x0;
    c2f4.x0 := c2f3.y0; //<-- filter input
    c2f4.y2 := c2f4.y0;
    c2f4.y0 := c2f4.x2 + ((c2f4.x0 - c2f4.y2) * c2f4.a);

    // Grab the final cascade outputs.
    x := c1f4.y0;
    y := c2f4.y0;

    inc(Input);
    Input_Delay  := Input^ + kDenormal;
    inc(Input);

    Output^ := (x + y) * 0.5;
    inc(Output);
  end;
end;

procedure TDecimationFilter2x8thOrder.ProcessDouble(Input, Output: PDouble; const InputSampleFrames: integer; out OutputSampleFrames: integer);
var
  x : double;
  y : double;
  c1 : integer;
begin
  assert(not odd(InputSampleFrames));
  assert(InputSampleFrames >= 2);

  OutputSampleFrames := InputSampleFrames div 2;

  for c1 := 0 to OutputSampleFrames-1 do
  begin
    // cascade A
    c1f1.x2 := c1f1.x0;
    c1f1.x0 := Input^ + kDenormal; //<-- filter input
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

    c1f4.x2 := c1f4.x0;
    c1f4.x0 := c1f3.y0; //<-- filter input
    c1f4.y2 := c1f4.y0;
    c1f4.y0 := c1f4.x2 + ((c1f4.x0 - c1f4.y2) * c1f4.a);

    // cascade B
    c2f1.x2 := c2f1.x0;
    c2f1.x0 := Input_Delay; //<-- filter input
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

    c2f4.x2 := c2f4.x0;
    c2f4.x0 := c2f3.y0; //<-- filter input
    c2f4.y2 := c2f4.y0;
    c2f4.y0 := c2f4.x2 + ((c2f4.x0 - c2f4.y2) * c2f4.a);

    // Grab the final cascade outputs.
    x := c1f4.y0;
    y := c2f4.y0;

    inc(Input);
    Input_Delay  := Input^ + kDenormal;
    inc(Input);

    Output^ := (x + y) * 0.5;
    inc(Output);
  end;
end;


end.
