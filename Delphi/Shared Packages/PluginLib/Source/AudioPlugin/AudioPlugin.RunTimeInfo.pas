unit AudioPlugin.RunTimeInfo;

interface

type
  TRunTimeInfo = record
  private
  public
    InputCount       : integer; //Audio input count.
    OutputCount      : integer; //Audio output count.
    SampleRate       : integer;
    MaxSampleFrames  : integer;
    // 1) SlowControlBufferSize must be larger than FastControlBufferSize.
    // 2) SlowControlBufferSize must be a multiple of FastControlBufferSize.
    FastControlBufferSize : integer;
    SlowControlBufferSize : integer;

    procedure Init;
  end;

implementation

{ TRunTimeInfo }

procedure TRunTimeInfo.Init;
begin
  InputCount            := 0;
  OutputCount           := 0;
  SampleRate            := 0;
  MaxSampleFrames       := 0;
  FastControlBufferSize := 0;
  SlowControlBufferSize := 0;
end;

end.
