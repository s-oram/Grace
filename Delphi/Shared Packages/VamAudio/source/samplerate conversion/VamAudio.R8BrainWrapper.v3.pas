unit VamAudio.R8BrainWrapper.v3;

interface

uses
  VamLib.MoreTypes, r8bsrc, r8bsrcEx;

type
  // MaxInLen = Maximum length of input buffer.
  // ReqTransBand = Required Transition Band. Units in percentage (%).
  //   Range:
  //      - 0.5-2% is extremely greedy and usually not necessary.
  //      - 2-3% is good for most cases.
  //      - 3-4% is relaxed but still offers a flat freqrency response up to 21kHz with 44.1k source or destination.
  //      - 5-30% is can be "good enough" when working with 88.2k or higher samplerates.
  PResampleConfig = ^TResampleConfig;
  TResampleConfig = record
    SourceRate           : Double;
    DestRate             : Double;
    MaxInputBufferFrames : LongInt;
    TransitionBand       : double;
    Res                  : TResampleResolution;
    procedure SetToDefault;
    procedure AssignFrom(Source : PResampleConfig);
  end;


  TR8BrainResampler = class
  private
    MaxInputFrames : integer;
    MaxOutputFrames : integer;
    rsHandle : CR8BResampler;
    PTempBuffer : PR8BDouble;
    fOutputLatency: integer;
    fInputLatency: integer;
  protected
    WorkBuffer : array of double;
    WorkBufferSize : integer;
    procedure PrimeFilterMemory;
    procedure ZeroFilterMemory;
    function MeasureLatency:integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup(Config : PResampleConfig);

    function ProcessDouble(InputBuffer : PDouble; const InputSampleFrames : integer; out OutputBuffer:PDouble):integer;

    procedure Reset;

    property InputLatency : integer read fInputLatency;
    property OutputLatency : integer read fOutputLatency;
    property Latency : integer read fOutputLatency; // Should replace this with output latency.
  end;

implementation

uses
  Math,
  SysUtils;

{ TResampleConfig }

procedure TResampleConfig.AssignFrom(Source: PResampleConfig);
begin
  self.SourceRate           := Source^.SourceRate;
  self.DestRate             := Source^.DestRate;
  self.MaxInputBufferFrames := Source^.MaxInputBufferFrames;
  Self.TransitionBand       := Source^.TransitionBand;
  Self.Res                  := Source^.Res;
end;

procedure TResampleConfig.SetToDefault;
begin
  self.SourceRate           := 1;
  self.DestRate             := 1;
  self.MaxInputBufferFrames := 512;
  //Self.TransitionBand       := ????;
  //Self.Res                  := ????;
end;

{ TR8BrainResampler }

constructor TR8BrainResampler.Create;
begin
  rsHandle := nil;
  fOutputLatency := 0;
end;

destructor TR8BrainResampler.Destroy;
begin
  if assigned(rsHandle) then
  begin
    r8b_delete(rsHandle);
  end;

  SetLength(WorkBuffer, 0);

  inherited;
end;

procedure TR8BrainResampler.Setup(Config: PResampleConfig);
var
  ResampleRes: LongInt;
begin
  if assigned(rsHandle) then r8b_delete(rsHandle);

  MaxInputFrames := Config^.MaxInputBufferFrames;
  MaxOutputFrames := ceil(Config^.MaxInputBufferFrames * Config^.DestRate / Config^.SourceRate);
  WorkBufferSize := max(MaxInputFrames, MaxOutputFrames);
  SetLength(WorkBuffer, WorkBufferSize);

  case Config^.Res of
    res16Bit:   ResampleRes := 0;
    res16BitIR: ResampleRes := 1;
    res24bit:   ResampleRes := 2;
  else
    raise Exception.Create('Type not handled.');
  end;

  rsHandle := r8b_create(Config^.SourceRate, Config^.DestRate, Config^.MaxInputBufferFrames, Config^.TransitionBand, ResampleRes);

  // NOTE: Measuring the latency also primes the filter object. The filter object
  // will not output any samples until the internal buffers are filled.
  fOutputLatency := MeasureLatency;
  fInputLatency := floor(fOutputLatency * Config.SourceRate / Config.DestRate);

  // Zero the filter memory after measuring the latency.
  ZeroFilterMemory;
end;

procedure TR8BrainResampler.PrimeFilterMemory;
var
  c1 : integer;
  x : integer;
  ProcessedSampleFrames : LongInt;
  ip0 : PR8BDouble;
  op0 : PR8BDouble;
  ReportedLatency : integer;
begin
  ip0 := @WorkBuffer[0];

  ReportedLatency := r8b_get_latency(rsHandle);

  // Prime the resampler. (feed it with zero's so there is data available in resampler when
  // it's needed to start work.)
  x := 0;
  while x < ReportedLatency do
  begin
    for c1 := 0 to MaxInputFrames-1 do WorkBuffer[c1] := 0;
    ProcessedSampleFrames := r8b_process(rsHandle, ip0, MaxInputFrames, op0);
    inc(x, ProcessedSampleFrames);
    for c1 := 0 to ProcessedSampleFrames-1 do
    begin
      if op0^ <> 0 then
      begin
        x := 0;
        break;
      end;
      inc(op0);
    end;
  end;
end;

procedure TR8BrainResampler.ZeroFilterMemory;
var
  c1 : integer;
  x : integer;
  ProcessedSampleFrames : LongInt;
  ip0 : PR8BDouble;
  op0 : PR8BDouble;
  ReportedLatency : integer;
begin
  ip0 := @WorkBuffer[0];

  ReportedLatency := r8b_get_latency(rsHandle);

  // Prime the resampler. (feed it with zero's so there is data available in resampler when
  // it's needed to start work.)
  x := 0;
  while x < ReportedLatency do
  begin
    for c1 := 0 to MaxInputFrames-1 do WorkBuffer[c1] := 0;
    ProcessedSampleFrames := r8b_process(rsHandle, ip0, MaxInputFrames, op0);
    inc(x, ProcessedSampleFrames);
  end;
end;



function TR8BrainResampler.MeasureLatency: integer;
var
  c1 : integer;
  x : integer;
  ProcessedSampleFrames : LongInt;
  ip0 : PR8BDouble;
  op0 : PR8BDouble;
begin
  assert(assigned(rsHandle));

  // First, prime the filter memory to ensure the oversampling filter is ready for work.
  PrimeFilterMemory;


  // ==== Now measure the latency ====
  ip0 := @WorkBuffer[0];

  x := 0;
  while true do
  begin
    for c1 := 0 to MaxInputFrames-1 do WorkBuffer[c1] := 1;
    ProcessedSampleFrames := r8b_process(rsHandle, ip0, MaxInputFrames, op0);
    for c1 := 0 to ProcessedSampleFrames-1 do
    begin
      if op0^ < 1
        then inc(x)
        else exit(x);
      inc(op0);
    end;
  end;
end;



procedure TR8BrainResampler.Reset;
begin
  ZeroFilterMemory;
end;

function TR8BrainResampler.ProcessDouble(InputBuffer: PDouble; const InputSampleFrames: integer; out OutputBuffer: PDouble): integer;
var
  OutputSampleFrames : integer;
begin
  OutputSampleFrames := r8b_process(rsHandle, PR8BDouble(InputBuffer), InputSampleFrames, PR8BDouble(OutputBuffer));
  result := OutputSampleFrames;
end;

end.
