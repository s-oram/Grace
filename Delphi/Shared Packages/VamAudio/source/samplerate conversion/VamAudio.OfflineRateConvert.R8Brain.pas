unit VamAudio.OfflineRateConvert.R8Brain;

interface

uses
  VamAudio.R8BrainWrapper.v3,
  r8bsrc, r8bsrcEx,
  VamLib.MoreTypes;

type
  PConfig = ^TConfig;
  TConfig = record
    SourceRate           : Double;
    DestRate             : Double;
    TransitionBand       : double;
    Res                  : TResampleResolution;
    InputSampleFrames    : integer;
  end;

  PRateConverterResult = ^TRateConverterResult;
  TRateConverterResult = record
    OutputSampleFrames : integer;
    Output1 : PDouble;
    Output2 : PDouble;
  end;

  TOfflineRateConverter = class
  private
  protected
    InputBuffer1, InputBuffer2 : array of double;
    OutputBuffer1, OutputBuffer2 : array of double;
    procedure Resample(const rs:TR8BrainResampler; const MaxInputBufferSize: integer; const Config: PConfig; Input, Output: PDouble);
  public
    constructor Create;
    destructor Destroy; override;

    function ProcessSingle(const Config:PConfig; Input1 : PSingle):TRateConverterResult; overload;
    function ProcessSingle(const Config:PConfig; Input1, Input2 : PSingle):TRateConverterResult; overload;
  end;

implementation

uses
  VamLib.Utils,
  Math;


// IMPORTANT: Don't forget to free the resampler after creating it.
function CreateResampler(const Config : PConfig; const MaxInputFrameBufferSize : integer):TR8BrainResampler;
var
  rs : TR8BrainResampler;
  RsConfig : TResampleConfig;
begin
  RsConfig.SourceRate           := Config.SourceRate;
  RsConfig.DestRate             := Config.DestRate;
  RsConfig.TransitionBand       := Config.TransitionBand;
  RsConfig.Res                  := Config.Res;
  RsConfig.MaxInputBufferFrames := MaxInputFrameBufferSize;

  rs := TR8BrainResampler.Create;
  rs.Setup(@RsConfig);
  result := rs;
end;

function CalcOutputSampleFrames(const SourceRate, DestRate : single; const InputSampleFrames : integer):integer;
begin
  result := ceil(DestRate / SourceRate * InputSampleFrames);
end;

{ TOfflineRateConverter }

constructor TOfflineRateConverter.Create;
begin


end;

destructor TOfflineRateConverter.Destroy;
begin
  SetLength(InputBuffer1, 0);
  SetLength(InputBuffer2, 0);
  SetLength(OutputBuffer1, 0);
  SetLength(OutputBuffer2, 0);
  inherited;
end;


function TOfflineRateConverter.ProcessSingle(const Config: PConfig; Input1: PSingle):TRateConverterResult;
const
  kMaxInputBufferSize = 1024;
var
  rs : TR8BrainResampler;
  OutputSampleFrames : integer;
  OutputBufferSize : integer;
  c1: Integer;
begin
  rs := CreateResampler(Config, kMaxInputBufferSize);
  AutoFree(@rs);

  OutputSampleFrames := CalcOutputSampleFrames(Config.SourceRate, Config.DestRate, Config.InputSampleFrames);
  OutputBufferSize := OutputSampleFrames + rs.OutputLatency * 2;

  SetLength(InputBuffer1, Config^.InputSampleFrames);
  SetLength(OutputBuffer1, OutputBufferSize);

  for c1 := 0 to Config^.InputSampleFrames-1 do
  begin
    InputBuffer1[c1] := Input1^;
    inc(Input1);
  end;


  Resample(rs, kMaxInputBufferSize, Config, @InputBuffer1[0], @OutputBuffer1[0]);

  result.OutputSampleFrames := OutputSampleFrames;
  result.Output1 := @OutputBuffer1[rs.OutputLatency];
  result.Output2 := nil;
end;

function TOfflineRateConverter.ProcessSingle(const Config: PConfig; Input1, Input2: PSingle):TRateConverterResult;
const
  kMaxInputBufferSize = 1024;
var
  rs : TR8BrainResampler;
  OutputSampleFrames : integer;
  OutputBufferSize : integer;
  c1: Integer;
begin
  rs := CreateResampler(Config, kMaxInputBufferSize);
  AutoFree(@rs);

  OutputSampleFrames := CalcOutputSampleFrames(Config.SourceRate, Config.DestRate, Config.InputSampleFrames);
  OutputBufferSize := OutputSampleFrames + rs.OutputLatency * 2;

  SetLength(InputBuffer1, Config^.InputSampleFrames);
  SetLength(OutputBuffer1, OutputBufferSize);

  SetLength(InputBuffer2, Config^.InputSampleFrames);
  SetLength(OutputBuffer2, OutputBufferSize);

  for c1 := 0 to Config^.InputSampleFrames-1 do
  begin
    InputBuffer1[c1] := Input1^;
    inc(Input1);

    InputBuffer2[c1] := Input2^;
    inc(Input2);
  end;

  rs.Reset;
  Resample(rs, kMaxInputBufferSize, Config, @InputBuffer1[0], @OutputBuffer1[0]);
  rs.Reset;
  Resample(rs, kMaxInputBufferSize, Config, @InputBuffer2[0], @OutputBuffer2[0]);

  result.OutputSampleFrames := OutputSampleFrames;
  result.Output1 := @OutputBuffer1[rs.OutputLatency];
  result.Output2 := @OutputBuffer2[rs.OutputLatency];
end;

procedure TOfflineRateConverter.Resample(const rs:TR8BrainResampler; const MaxInputBufferSize: integer; const Config: PConfig; Input, Output: PDouble);
var
  FrameCount : integer;
  SamplesToProcess : integer;
  SampleCount : integer;
  InPtr : PDouble;
  Outptr : PDouble;
  c1: Integer;
  TempBuffer : array of double;
  ExtraSamples : integer;
begin
  SetLength(TempBuffer, MaxInputBufferSize);

  // Process the input....
  FrameCount := 0;
  while FrameCount < Config.InputSampleFrames do
  begin
    SamplesToProcess := Min(1024, Config.InputSampleFrames-FrameCount);
    InPtr := Input;

    SampleCount := rs.ProcessDouble(InPtr, SamplesToProcess, OutPtr);

    for c1 := 0 to SampleCount-1 do
    begin
      Output^ := OutPtr^;
      inc(Output);
      inc(OutPtr);
    end;
    inc(Input, SamplesToProcess);
    inc(FrameCount, SamplesToProcess);
  end;


  //ExtraSamples := floor(rs.Latency * Config.SourceRate / Config.DestRate
  ExtraSamples := rs.InputLatency;
  FrameCount := 0;
  while FrameCount < rs.InputLatency do
  begin
    SamplesToProcess := Min(1024, rs.InputLatency-FrameCount);

    for c1 := 0 to MaxInputBufferSize-1 do
    begin
      TempBuffer[c1] := 0;
    end;

    InPtr := @TempBuffer[0];
    SampleCount := rs.ProcessDouble(InPtr, SamplesToProcess, OutPtr);

    for c1 := 0 to SampleCount-1 do
    begin
      Output^ := OutPtr^;
      inc(Output);
      inc(OutPtr);
    end;

    inc(FrameCount, SamplesToProcess);
  end;

  SetLength(TempBuffer, 0);
end;

end.
