unit r8Brain;

{$include r8Brain.inc}

interface

uses
  r8bsrc;

type
  PResampleConfig = ^TResampleConfig;
  TResampleConfig = record
    SourceRate           : Double;
    DestRate             : Double;
    MaxInputBufferFrames : LongInt;
    TransitionBand       : double;
    Res                  : LongInt;
    procedure SetToDefault;
    procedure AssignFrom(Source : PResampleConfig);
  end;

  TR8BrainSingleChannelResampler = class
  private
    ResampleObject : CR8BResampler;
    InputLatencySamples : LongInt;
    TempInputBuffer  : array of double;
    TempOutputBuffer : array of single;
    MaxInputFrames : integer;
  protected
    procedure PrimeResampleObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(Config : PResampleConfig);
    procedure Clear;

    procedure ProcessFloat32(const InputSamples : PSingle; const InputSampleFrames : LongInt; out OutputSamples : PSingle; out OutputSampleFrames : LongInt);
    procedure ProcessFloat64(const InputSamples : PDouble; const InputSampleFrames : LongInt; out OutputSamples : PDouble; out OutputSampleFrames : LongInt);

    property Latency : integer read InputLatencySamples;
  end;

implementation

uses
  Math;


{ TR8BrainSingleChannelResampler }

constructor TR8BrainSingleChannelResampler.Create;
begin

end;

destructor TR8BrainSingleChannelResampler.Destroy;
begin
  if assigned(ResampleObject) then r8b_delete(ResampleObject);
  SetLength(TempInputBuffer, 0);
  SetLength(TempOutputBuffer, 0);
  inherited;
end;

procedure TR8BrainSingleChannelResampler.Init(Config: PResampleConfig);
var
  DestSampleFrames : integer;
begin
  if assigned(ResampleObject) then r8b_delete(ResampleObject);

  MaxInputFrames := Config^.MaxInputBufferFrames;

  ResampleObject := r8b_create(Config^.SourceRate, Config^.DestRate, Config^.MaxInputBufferFrames, Config^.TransitionBand, Config^.Res);
  InputLatencySamples := r8b_get_latency(ResampleObject);

  DestSampleFrames := ceil(Config^.MaxInputBufferFrames * (Config^.DestRate / Config^.SourceRate));

  SetLength(TempInputBuffer,  Config^.MaxInputBufferFrames);
  SetLength(TempOutputBuffer, DestSampleFrames);

  PrimeResampleObject;
end;

procedure TR8BrainSingleChannelResampler.Clear;
begin
  if assigned(ResampleObject) then
  begin
    r8b_clear(ResampleObject);
    PrimeResampleObject;
  end;
end;

procedure TR8BrainSingleChannelResampler.PrimeResampleObject;
var
  c1 : integer;
  x : integer;
  BufferSize : integer;
  TempBuffer : array of double;
  ProcessedSampleFrames : LongInt;
  ip0 : PR8BDouble;
  op0 : PR8BDouble;
begin
  assert(assigned(ResampleObject));

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
      ProcessedSampleFrames := r8b_process(ResampleObject, ip0, BufferSize, op0);
      inc(x, ProcessedSampleFrames);
    end;
  finally
    SetLength(TempBuffer, 0);
  end;
end;

procedure TR8BrainSingleChannelResampler.ProcessFloat32(const InputSamples: PSingle; const InputSampleFrames: Integer; out OutputSamples: PSingle; out OutputSampleFrames: Integer);
var
  InSmps : PSingle;
  ip0 : PR8BDouble;
  op0 : PR8BDouble;
  c1: Integer;
begin
  assert(assigned(ResampleObject));

  InSmps := InputSamples;

  for c1 := 0 to InputSampleFrames-1 do
  begin
    TempInputBuffer[c1] := InSmps^;
    inc(InSmps);
  end;

  ip0 := @TempInputBuffer[0];

  OutputSampleFrames := r8b_process(ResampleObject, ip0, InputSampleFrames, op0);

  for c1 := 0 to OutputSampleFrames-1 do
  begin
    TempOutputBuffer[c1] := op0^;
    inc(op0);
  end;

  OutputSamples := @TempOutputBuffer[0];
end;

procedure TR8BrainSingleChannelResampler.ProcessFloat64(const InputSamples: PDouble; const InputSampleFrames: Integer; out OutputSamples: PDouble; out OutputSampleFrames: Integer);
var
  ip0 : PR8BDouble absolute InputSamples;
  op0 : PR8BDouble absolute OutputSamples;
begin
  assert(assigned(ResampleObject));
  //OutputSampleFrames := r8b_process(ResampleObject, InputSamples, InputSampleFrames, OutputSamples);
  OutputSampleFrames := r8b_process(ResampleObject, ip0, InputSampleFrames, op0);
end;


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

end.
