unit R8BrainWrapper;

interface


// https://code.google.com/p/r8brain-free-src/

uses
  VamLib.MoreTypes, r8bsrc;

type
  TResampleResolution = (res16Bit, res16BitIR, res24bit);

  TR8BrainResampler = class
  private
    rsHandle : CR8BResampler;
    TempBuffer : array of double;
    PTempBuffer : PR8BDouble;
    fLatency: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup(SrcSampleRate: Double; DstSampleRate: Double;	MaxInLen: LongInt; ReqTransBand: Double; Res: TResampleResolution);

    // InputBuffer and OutputBuffer may use the same buffer. IE. The process method can work in-place.
    function Process_Single(InputBuffer, OutputBuffer : PSingle; const InputSampleFrames : integer):integer;

    function Process(InputBuffer : PDouble; const InputSampleFrames : integer; out OutputBuffer:PDouble):integer;

    property Latency : integer read fLatency;
  end;

implementation

uses
  SysUtils;

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

function TR8BrainResampler.Process_Single(InputBuffer, OutputBuffer: PSingle; const InputSampleFrames: integer): integer;
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

function TR8BrainResampler.Process(InputBuffer: PDouble; const InputSampleFrames: integer; out OutputBuffer: PDouble): integer;
var
  OutputSampleFrames : integer;
begin
  OutputSampleFrames := r8b_process(rsHandle, InputBuffer, InputSampleFrames, OutputBuffer);
  result := OutputSampleFrames;
end;





end.
