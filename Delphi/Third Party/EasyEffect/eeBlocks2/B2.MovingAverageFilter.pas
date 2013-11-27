unit B2.MovingAverageFilter;

interface

uses
  B2.DelayBuffer;

type
  // TMovingAverageFilter is described in "Streamlining Digital Signal Processing" page 284 (Section 26.1)
  TMovingAverageFilter = class
  private
    DelayBuffer : TDelayBuffer;
    function GetBufferSize: integer;
    procedure SetBufferSize(const Value: integer);
  protected
    b0 : double; // b0 is the scaling variable.
    UnitDelay : double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(const Value : single);

    function Step(Input : single):single;

    property BufferSize : integer read GetBufferSize write SetBufferSize;
  end;

implementation

{ TMovingAverageFilter }

constructor TMovingAverageFilter.Create;
begin
  DelayBuffer := TDelayBuffer.Create;
  BufferSize := 16;
end;

destructor TMovingAverageFilter.Destroy;
begin
  DelayBuffer.Free;
  inherited;
end;

function TMovingAverageFilter.GetBufferSize: integer;
begin
  result := DelayBuffer.BufferSize;
end;

procedure TMovingAverageFilter.Reset(const Value: single);
begin
  DelayBuffer.ResetBuffer(Value);
  UnitDelay := Value * DelayBuffer.BufferSize;
end;

procedure TMovingAverageFilter.SetBufferSize(const Value: integer);
begin
  DelayBuffer.BufferSize := Value;
  DelayBuffer.ResetBuffer(0);
  b0 := 1 / Value;
  UnitDelay := 0;
end;

function TMovingAverageFilter.Step(Input: single): single;
var
  x : single;
begin
  x := Input - DelayBuffer.ReadWrite(Input);
  x := x + UnitDelay;
  UnitDelay := x;
  result := x * b0;

  //result := Input - DelayBuffer.ReadWrite(Input);
end;

end.
