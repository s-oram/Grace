unit AudioIO_Resampler_r8brain;

interface

uses
  VamLib.MoreTypes, r8bsrc;

type
  Tr8bResampler = class
  public
    constructor Create;
    destructor Destroy; override;

    procedure Resample(SourceBuffer, DestBuffer :PSingle; const SourceSampleFrames, DestSampleFrames:integer);
  end;

implementation

uses
  SysUtils;

{ Tr8bResampler }

constructor Tr8bResampler.Create;
begin

end;

destructor Tr8bResampler.Destroy;
begin

  inherited;
end;

procedure Tr8bResampler.Resample(SourceBuffer, DestBuffer: PSingle; const SourceSampleFrames, DestSampleFrames: integer);
const
  MaxInputLength = 1024;
  ReqTransBand   = 3;
var
  rsHandle : CR8BResampler;
  SrcRate, DstRate : Double;
  TempSrcBuffer : array of Double;
  TempDestBuffer : array of Double;
  PSrcBuffer, PDestBuffer : PR8BDouble;
  //Latency : integer;
  SamplesProcessed : integer;
  BufferSize : integer;
  c1: Integer;
  OutputSampleFrames : integer;
begin
  // Calculate resampling ratios for R8Brain.
  if SourceSampleFrames < DestSampleFrames then
  begin
    SrcRate := 1;
    DstRate := DestSampleFrames / SourceSampleFrames;
  end else
  begin
    SrcRate := SourceSampleFrames / DestSampleFrames;
    DstRate := 1;
  end;

  rsHandle := r8b_create( SrcRate, DstRate, MaxInputLength, ReqTransBand, r8brr24);

  if not assigned(rsHandle) then raise Exception.Create('Can not create R8Brain resampler object.');

  try
    // Create some temporary buffers.
    SetLength(TempSrcBuffer,  round(MaxInputLength * SrcRate)+1);
    SetLength(TempDestBuffer, round(MaxInputLength * DstRate)+1);

    //Latency := r8b_get_latency(rsHandle);

    SamplesProcessed := 0;
    while SamplesProcessed < SourceSampleFrames do
    begin
      //Calculate how many samples need to be processed.
      BufferSize := SourceSampleFrames - SamplesProcessed;
      if (BufferSize > MaxInputLength) then BufferSize := MaxInputLength;

      //Copy source sample data into the temporary buffer.
      for c1 := 0 to BufferSize-1 do
      begin
        TempSrcBuffer[c1] := SourceBuffer^;
        inc(SourceBuffer);
      end;

      // Do the samplerate conversion...
      PSrcBuffer  := @TempSrcBuffer[0];
      OutputSampleFrames := r8b_process(rsHandle, PSrcBuffer, BufferSize, PDestBuffer);

      // Copy samples from temporary buffer to the output.
      for c1 := 0 to OutputSampleFrames-1 do
      begin
        TempDestBuffer[c1] := PDestBuffer^;
        inc(PDestBuffer);
      end;


      // Copy samples from temporary buffer to the output.
      for c1 := 0 to OutputSampleFrames-1 do
      begin
        DestBuffer^ := TempDestBuffer[c1];
        inc(DestBuffer);
      end;

      inc(SamplesProcessed, BufferSize);
    end;

  finally
    SetLength(TempSrcBuffer,  0);
    SetLength(TempDestBuffer, 0);
    r8b_delete(rsHandle);
  end;
end;

end.
