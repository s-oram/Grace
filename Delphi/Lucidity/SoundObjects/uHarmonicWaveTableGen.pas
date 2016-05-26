unit uHarmonicWaveTableGen;

interface

uses
  VamLib.MoreTypes;

type
  THarmonic = record
    Magnitude : single; //Range 0..1
    Phase     : single; //range 0..1
  end;

  TArrayOfHarmonic = array of THarmonic;

  THarmonicMap = class
  private
    fCount: integer;
    fHarmonics: TArrayOfHarmonic;
    procedure SetCount(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    property Count : integer read fCount write SetCount;
    property Harmonics : TArrayOfHarmonic read fHarmonics write fHarmonics;
    // Harmonic 0 = DC offset
    // Harmonic 1 = fundamental harmonic. 1st.

    procedure GenerateWaveTable(Output : PSingle; OutputSampleFrames : integer; MaxHarmonics : integer);
  end;



implementation

// TODO:HIGH When moving this project to my new machine on 26th May 2016, I gutted the functionality
// to remove the dependence on MtxVec.
// Right now this unit is "broken". Which is OK because I don't believe it's actually being used.
// I need to follow the usage and remove the unit from the project properly.

{
uses
  MtxVec, Math387;
}
{ THarmonicMap }

constructor THarmonicMap.Create;
begin
  Count := 0;
end;

destructor THarmonicMap.Destroy;
begin
  Count := 0;
  inherited;
end;

procedure THarmonicMap.GenerateWaveTable(Output: PSingle; OutputSampleFrames, MaxHarmonics: integer);
begin

end;

procedure THarmonicMap.SetCount(const Value: integer);
var
  c1: Integer;
begin
  fCount := Value;
  SetLength(fHarmonics, Value);

  for c1 := 0 to Value-1 do
  begin
    Harmonics[c1].Magnitude := 0;
    Harmonics[c1].Phase     := 0;
  end;
end;

end.
