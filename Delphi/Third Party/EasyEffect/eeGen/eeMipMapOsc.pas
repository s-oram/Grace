unit eeMipMapOsc;

interface

uses
  MoreTypes, eeOscPhaseCounter, eeMipMapTable;


type
  TMipMapOsc = class
  private
    fFreq : single;
    fSampleRate: integer;
    fMipMapTable: TMipMapTable;
    fTableIndex: integer;
    procedure SetSampleRate(const Value: integer);
  protected
    PhaseCounter : TOscPhaseCounter;

    procedure UpdateStepSize; inline;

    property TableIndex : integer read fTableIndex write fTableIndex;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetFreq(NewFreq:single);

    function Step:single;

    property SampleRate  : integer      read fSampleRate  write SetSampleRate;
    property MipMapTable : TMipMapTable read fMipMapTable write fMipMapTable;

  end;

implementation

uses
  eeDsp;

{ TMipMapOsc }

constructor TMipMapOsc.Create;
begin
  PhaseCounter := TOscPhaseCounter.Create;
  TableIndex  := 0;
  fSampleRate := 44100;
end;

destructor TMipMapOsc.Destroy;
begin
  PhaseCounter.Free;
  inherited;
end;

procedure TMipMapOsc.SetFreq(NewFreq: single);
begin
  fFreq := NewFreq;
  UpdateStepSize;
end;

procedure TMipMapOsc.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  UpdateStepSize;
end;

procedure TMipMapOsc.UpdateStepSize;
var
  ms            : single;
  Samples       : single;
  BaseTableSize : integer;
  DivCount      : integer;
begin
  //Calc Table Index
  ms := 1000 / fFreq;
  Samples := MillisecondsToSamples(ms, SampleRate);

  BaseTableSize := Length(MipMapTable.MipMap[0]);
  DivCount := 0;

  while (Samples < BaseTableSize) and (DivCount < 6) do
  begin
    BaseTableSize := BaseTableSize div 2;
    inc(DivCount);
  end;

  //inc(DivCount);
  //BaseTableSize := BaseTableSize div 2;
  TableIndex              := DivCount;
  PhaseCounter.TableSize  := Length(MipMapTable.MipMap[TableIndex]);


  //PhaseCounter.TableSize := 1024;
  //TableIndex := 1;

  PhaseCounter.SampleRate := SampleRate;
  PhaseCounter.Freq       := fFreq;



  //=== OLD CODE ====

  {
  while (Samples < BaseTableSize) and (DivCount < 6) do
  begin
    BaseTableSize := BaseTableSize div 2;
    inc(DivCount);
  end;

  inc(DivCount);
  BaseTableSize := BaseTableSize div 2;

  //PhaseCounter.TableSize := BaseTableSize;
  //TableIndex := DivCount;

  TableIndex := DivCount;
  PhaseCounter.TableSize := Length(MipMapTable.MipMap[TableIndex]);



  PhaseCounter.TableSize  := 2048;
  TableIndex              := 0;

  PhaseCounter.SampleRate := SampleRate;
  PhaseCounter.Freq       := fFreq;

  }
end;



function TMipMapOsc.Step: single;
var
  sp : TSamplePhase;
  x1, x2 : single;
begin
  assert(assigned(fMipMapTable));
  sp := PhaseCounter.Step;

  //x1 := MipMapTable.MipMap[0,sp.A];
  //x2 := MipMapTable.MipMap[0,sp.B];
  x1 := MipMapTable.MipMap[TableIndex, sp.A];
  x2 := MipMapTable.MipMap[TableIndex, sp.B];

  result := LinearInterpolation(x1, x2, sp.Frac);
end;

end.
