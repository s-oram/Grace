unit eeIntegerWaveTable;

interface

uses
  MoreTypes;

type
  TIntegerWaveTable = class
  private
    fTableLength: integer;
    fWaveData: TArrayOfInteger;
    fDeltaValues: TArrayOfInteger;
    fMaxFrequency: single;
    procedure SetTableLength(Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ZeroData;
    procedure CalcDeltaValues;

    property TableLength:integer read fTableLength write SetTableLength;
    property WaveData:TArrayOfInteger read fWaveData write fWaveData;
    property DeltaValues:TArrayOfInteger read fDeltaValues write fDeltaValues;
    property MaxFrequency:single read fMaxFrequency write fMaxFrequency;
  end;

implementation

{ TModWaveTable }

constructor TIntegerWaveTable.Create;
begin

end;

destructor TIntegerWaveTable.Destroy;
begin
  TableLength := 0;
  inherited;
end;

procedure TIntegerWaveTable.SetTableLength(Value: integer);
begin
  fTableLength := Value;
  SetLength(fWaveData, Value);
  SetLength(fDeltaValues, Value);
end;

procedure TIntegerWaveTable.ZeroData;
var c1:integer;
begin
  for c1 := 0 to TableLength - 1 do
  begin
    WaveData[c1] := 0;
    DeltaValues[c1] := 0;
  end;
end;

procedure TIntegerWaveTable.CalcDeltaValues;
var
  c1:integer;
  t:integer;
begin
  assert(TableLength >= 2);

  for c1 := 0 to TableLength - 2 do
  begin
    DeltaValues[c1] := WaveData[c1 + 1] - WaveData[c1];
  end;

  t := TableLength - 1;
  DeltaValues[t] := WaveData[0] - WaveData[t];

end;


end.
