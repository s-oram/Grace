unit VamAudio.R8BrainWrapper.v2;

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
    rsHandle : CR8BResampler;
    TempBuffer : array of double;
    PTempBuffer : PR8BDouble;
    fLatency: integer;
  protected
    procedure PrimeResampleObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup(Config : PResampleConfig; const UseTempBuffers : boolean); overload;

    procedure Setup(SrcSampleRate: Double; DstSampleRate: Double;	MaxInLen: LongInt; ReqTransBand: Double; Res: TResampleResolution); overload;

    // NOTE: InputBuffer and OutputBuffer may use the same buffer. IE. The process method can work in-place.
    // NOTE: ProcessDouble is preferred as it avoids copying to and from tempory buffers.
    function ProcessSingle(InputBuffer, OutputBuffer : PSingle; const InputSampleFrames : integer):integer;
    function ProcessDouble(InputBuffer : PDouble; const InputSampleFrames : integer; out OutputBuffer:PDouble):integer;

    procedure Reset;

    property Latency : integer read fLatency;
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
  fLatency := 0;
end;

destructor TR8BrainResampler.Destroy;
begin
  if assigned(rsHandle) then
  begin
    r8b_delete(rsHandle);
  end;

  SetLength(TempBuffer, 0);

  inherited;
end;


// MaxInLen = Maximum length of input buffer.
// ReqTransBand = Required Transition Band. Units in percentage (%).
//   Range:
//      - 0.5-2% is extremely greedy and usually not necessary.
//      - 2-3% is good for most cases.
//      - 3-4% is relaxed but still offers a flat freqrency response up to 21kHz with 44.1k source or destination.
//      - 5-30% is can be "good enough" when working with 88.2k or higher samplerates.
procedure TR8BrainResampler.Setup(SrcSampleRate, DstSampleRate: Double; MaxInLen: Integer; ReqTransBand: Double; Res: TResampleResolution);
var
  BufferSize : integer;
  x : integer;
  c1: Integer;
begin
  //assert(ReqTransBand >= 0.5);
  //assert(ReqTransBand <= 100);

  if assigned(rsHandle) then
  begin
    r8b_delete(rsHandle);
  end;

  case Res of
    res16Bit:   rsHandle := r8b_create(SrcSampleRate, DstSampleRate, MaxInLen, ReqTransBand, r8brr16);
    res16BitIR: rsHandle := r8b_create(SrcSampleRate, DstSampleRate, MaxInLen, ReqTransBand, r8brr16IR);
    res24bit:   rsHandle := r8b_create(SrcSampleRate, DstSampleRate, MaxInLen, ReqTransBand, r8brr24);
  else
    raise Exception.Create('Type not handled.');
  end;

  BufferSize := MaxInLen;

  SetLength(TempBuffer, BufferSize);
  PTempBuffer := @TempBuffer[0];

  fLatency := r8b_get_latency(rsHandle);


  // Prime the resampler. (feed it with zero's so there is data available in resampler when
  // it's needed to start work.)
  x := 0;
  while x < fLatency do
  begin
    for c1 := 0 to BufferSize-1 do
    begin
      TempBuffer[c1] := 0;
    end;
    r8b_process(rsHandle, PTempBuffer, MaxInLen, PTempBuffer);
    inc(x, BufferSize);
  end;
end;

procedure TR8BrainResampler.Setup(Config: PResampleConfig; const UseTempBuffers : boolean);
var
  DestSampleFrames : integer;
  ResampleRes: LongInt;
begin
  if assigned(rsHandle) then r8b_delete(rsHandle);

  MaxInputFrames := Config^.MaxInputBufferFrames;

  case Config^.Res of
    res16Bit:   ResampleRes := 0;
    res16BitIR: ResampleRes := 1;
    res24bit:   ResampleRes := 2;
  else
    raise Exception.Create('Type not handled.');
  end;

  rsHandle := r8b_create(Config^.SourceRate, Config^.DestRate, Config^.MaxInputBufferFrames, Config^.TransitionBand, ResampleRes);
  fLatency := r8b_get_latency(rsHandle);

  DestSampleFrames := ceil(Config^.MaxInputBufferFrames * (Config^.DestRate / Config^.SourceRate));

  if MaxInputFrames > DestSampleFrames then
  begin
    SetLength(TempBuffer, MaxInputFrames);
  end else
  begin
    SetLength(TempBuffer, DestSampleFrames);
  end;

  {*
  if UseTempBuffers then
  begin
    SetLength(TempInputBuffer,  Config^.MaxInputBufferFrames);
    SetLength(TempOutputBuffer, DestSampleFrames);
  end;
  *}

  PrimeResampleObject;
end;

procedure TR8BrainResampler.PrimeResampleObject;
var
  c1 : integer;
  x : integer;
  BufferSize : integer;
  TempBuffer : array of double;
  ProcessedSampleFrames : LongInt;
  ip0 : PR8BDouble;
  op0 : PR8BDouble;
begin
  assert(assigned(rsHandle));

  BufferSize := MaxInputFrames;
  SetLength(TempBuffer, BufferSize);
  ip0 := @TempBuffer[0];
  try
    // Prime the resampler. (feed it with zero's so there is data available in resampler when
    // it's needed to start work.)
    x := 0;
    while x < Latency do
    begin
      for c1 := 0 to BufferSize-1 do TempBuffer[c1] := 0;
      ProcessedSampleFrames := r8b_process(rsHandle, ip0, BufferSize, op0);
      inc(x, ProcessedSampleFrames);
    end;
  finally
    SetLength(TempBuffer, 0);
  end;
end;


function TR8BrainResampler.ProcessSingle(InputBuffer, OutputBuffer: PSingle; const InputSampleFrames: integer): integer;
var
  c1: Integer;
  OutputSampleFrames : integer;
begin
  for c1 := 0 to InputSampleFrames-1 do
  begin
    TempBuffer[c1] := InputBuffer^;
    inc(InputBuffer);
  end;

  PTempBuffer := @TempBuffer[0];
  OutputSampleFrames := r8b_process(rsHandle, PTempBuffer, InputSampleFrames, PTempBuffer);

  for c1 := 0 to OutputSampleFrames-1 do
  begin
    OutputBuffer^ := TempBuffer[c1];
    inc(OutputBuffer);
  end;

  result := OutputSampleFrames;
end;

procedure TR8BrainResampler.Reset;
begin
  PrimeResampleObject;
end;

function TR8BrainResampler.ProcessDouble(InputBuffer: PDouble; const InputSampleFrames: integer; out OutputBuffer: PDouble): integer;
var
  OutputSampleFrames : integer;
begin
  OutputSampleFrames := r8b_process(rsHandle, PR8BDouble(InputBuffer), InputSampleFrames, PR8BDouble(OutputBuffer));
  result := OutputSampleFrames;
end;

end.
