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

uses
  MtxVec, Math387;

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
var
  Data1, Data2 : TVec;
  c1: Integer;
begin
  Data1 := TVec.Create;
  Data1.Size(OutputSampleFrames, true);

  Data2 := TVec.Create;
  Data2.Size(OutputSampleFrames, false);


  //Zero the outputs
  for c1 := 0 to OutputSampleFrames-1 do
  begin
    Data1[c1 * 2]     := 0;
    Data1[c1 * 2 + 1] := 0;
  end;

  if MaxHarmonics >= OutputSampleFrames div 4 then MaxHarmonics := OutputSampleFrames div 4;
  if MaxHarmonics >  self.Count then MaxHarmonics := self.Count;

  //Calc inverse harmonics...
  for c1 := 0 to MaxHarmonics-1 do
  begin
    Data1[c1 * 2]     := cos(2 * pi * (Harmonics[c1].Phase - 0.25)) * Harmonics[c1].Magnitude;
    Data1[c1 * 2 + 1] := sin(2 * pi * (Harmonics[c1].Phase - 0.25)) * Harmonics[c1].Magnitude;
  end;

  Data1.IFFT(true);
  Data2.RealPart(Data1);


  for c1 := 0 to OutputSampleFrames-1 do
  begin
    Output^ := Data2[c1];
    inc(Output);
  end;

  Data1.Free;
  Data2.Free;
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
