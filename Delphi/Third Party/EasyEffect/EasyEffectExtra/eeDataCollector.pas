unit eeDataCollector;

interface

type           
  TDataCollector = class
  private
    fDataBufferSize: integer;
    fSamplesPerValue: integer;
    procedure SetDataBufferSize(const Value: integer);
  protected
    DataBuffer:array of single;
    SampleCount:integer;
    SampleMax  :single;
    HasLooped:boolean;

    WriteDataIndex :integer;
    ReadDataIndex  :integer;
  public
    constructor Create;
	  destructor Destroy; override;

    //Call for each sample.
    procedure AddValue(Value:single); inline;
    function ReadValue(out MaxValue:single):boolean; inline;

    function ReadValueCount:integer;

    property SamplesPerValue:integer read fSamplesPerValue write fSamplesPerValue;

    property DataBufferSize:integer read fDataBufferSize write SetDataBufferSize;
  end;


implementation

{ TScopeDataCollector }

constructor TDataCollector.Create;
begin
  DataBufferSize  := 44100;
  SamplesPerValue := 100;
  SampleCount     := 0;
  HasLooped       := false;
  SampleMax       := -100;
  WriteDataIndex  := 0;
  ReadDataIndex   := 0;
end;

destructor TDataCollector.Destroy;
begin
  SetLength(DataBuffer, 0);
  inherited;
end;

procedure TDataCollector.SetDataBufferSize(const Value: integer);
begin
  fDataBufferSize := Value;
  SetLength(DataBuffer, Value);
end;

procedure TDataCollector.AddValue(Value: single);
begin
  if Value > SampleMax then SampleMax := Value;
  inc(SampleCount);

  if SampleCount >= SamplesPerValue then
  begin
    DataBuffer[WriteDataIndex] := SampleMax;

    inc(WriteDataIndex);
    if WriteDataIndex >= DataBufferSize then
    begin
      WriteDataIndex := 0;
      HasLooped      := true;
    end;

    SampleCount := 0;
    SampleMax   := -10;
  end;

end;

function TDataCollector.ReadValue(out MaxValue: single): boolean;
begin
  if (HasLooped = false) and (ReadDataIndex < WriteDataIndex) then
  begin
    MaxValue := DataBuffer[ReadDataIndex];
    inc(ReadDataIndex);
    result := true;
  end else
  if (HasLooped = true) then
  begin
    MaxValue := DataBuffer[ReadDataIndex];
    inc(ReadDataIndex);
    if ReadDataIndex >= DataBufferSize then
    begin
      ReadDataIndex := 0;
      HasLooped     := false;
    end;
    result := true;
  end else
  begin
    MaxValue := 0;
    result := false;
  end;
end;


function TDataCollector.ReadValueCount: integer;
begin
  if (HasLooped = false)
    then result := WriteDataIndex - ReadDataIndex
    else result := fDataBufferSize - ReadDataIndex + WriteDataIndex;
end;

end.
