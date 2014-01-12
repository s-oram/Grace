unit eeWaveTable;

interface

uses
  VamLib.MoreTypes;

type
  TWaveTable = class
  private
    fTableLength: integer;
    fWaveData: TArrayOfSingle;
    fDeltaValues: TArrayOfSingle;
    procedure SetTableLength(Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ZeroData;
    procedure CalcDeltaValues;

    property TableLength  : integer        read fTableLength  write SetTableLength;
    property WaveData     : TArrayOfSingle read fWaveData     write fWaveData;
    property DeltaValues  : TArrayOfSingle read fDeltaValues  write fDeltaValues;
  end;

implementation

{ TModWaveTable }

constructor TWaveTable.Create;
begin

end;

destructor TWaveTable.Destroy;
begin
  TableLength := 0;
  inherited;
end;

procedure TWaveTable.SetTableLength(Value: integer);
begin
  fTableLength := Value;
  SetLength(fWaveData, Value);
  SetLength(fDeltaValues, Value);
end;

procedure TWaveTable.ZeroData;
var c1:integer;
begin
  for c1 := 0 to TableLength - 1 do
  begin
    WaveData[c1] := 0;
    DeltaValues[c1] := 0;
  end;
end;

procedure TWaveTable.CalcDeltaValues;
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
