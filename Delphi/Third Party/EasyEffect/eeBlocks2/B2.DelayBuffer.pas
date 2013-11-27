unit B2.DelayBuffer;

interface

type



  TDelayBuffer = class
  private
    fBuffer : array of single;
    fBufferSize: integer;
  protected
    WriteIndex : integer;
    procedure SetBufferSize(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetBuffer(Value : single = 0);



    // Use ReadWrite() to read a value at the same time as writing a value.
    // This will avoid calcuating if the ReadIndex is in range of the
    // buffer size.
    function ReadWrite(InputValue : single):single;

    procedure WriteInput(InputValue : single);
    function ReadAbsolute(ReadIndex : integer):single;
    function ReadRelative(ReadDelay : integer):single;

    property BufferSize : integer read fBufferSize write SetBufferSize;
  end;

implementation

{ TDelayBuffer }

constructor TDelayBuffer.Create;
begin
  fBufferSize := 0;
end;

destructor TDelayBuffer.Destroy;
begin
  SetLength(fBuffer, 0);
  inherited;
end;

procedure TDelayBuffer.ResetBuffer(Value: single);
var
  c1: Integer;
begin
  for c1 := 0 to fBufferSize-1 do
  begin
    fBuffer[c1] := Value;
  end;
end;

procedure TDelayBuffer.SetBufferSize(const Value: integer);
begin
  if Value <> fBufferSize then
  begin
    WriteIndex := 0;
    fBufferSize := Value;
    SetLength(fBuffer, Value + 1);
  end;

end;

procedure TDelayBuffer.WriteInput(InputValue: single);
begin
  inc(WriteIndex);
  fBuffer[WriteIndex] := InputValue;
  if WriteIndex = fBufferSize then
  begin
    WriteIndex := 0;
    fBuffer[0] := InputValue;
  end;
end;

function TDelayBuffer.ReadAbsolute(ReadIndex : integer): single;
begin
  result := fBuffer[ReadIndex];
end;

function TDelayBuffer.ReadRelative(ReadDelay : integer): single;
var
  ReadIndex : integer;
begin
  ReadIndex := WriteIndex - ReadDelay;
  if ReadIndex < 0 then ReadIndex := ReadIndex + fBufferSize;

  result := fBuffer[ReadIndex];
end;



function TDelayBuffer.ReadWrite(InputValue: single): single;
begin
  if WriteIndex < fBufferSize-1
    then inc(WriteIndex)
    else WriteIndex := 0;

  result := fBuffer[WriteIndex];

  fBuffer[WriteIndex] := InputValue;
end;

end.
