unit soSawSquareOsc;

interface

uses
  uHarmonicWaveTableGen;

type
  TWaveData = array of single;

  TSawSquareOsc = class
  private
    fSampleRate: single;
    fFreq: single;
    fOscRootFreq: single;
    fShape: single;
    fPulseWidth: single;
    fTableSize: integer;
    fWaveData: TWaveData;
    fAmAmount: single;
    fFmAmount: single;
    procedure SetFreq(const Value: single);
    procedure SetOscRootFreq(const Value: single);
  protected
    CurPhase : single;
    StepSize : single;
    HarmonicMap : THarmonicMap;

    property WaveData  : TWaveData read fWaveData write fWaveData;
    property TableSize : integer read fTableSize;

    procedure SetWaveDataTableSize(NewTableSize:integer);

    procedure FillTableWithSineWave; // Fill the internal WaveData with a sine waveform.
    procedure FillBufferFrames;      // Must be called after changing the contents of WaveData.
  public
    constructor Create;
    destructor Destroy; override;

    //==========================================================================
    // These methods are designed to be used by the 'synthesis' generation
    // parts of an application.
    property RootFreq : single read fOscRootFreq write SetOscRootFreq;
    property Freq : single read fFreq write SetFreq;
    property SampleRate : single read fSampleRate write fSampleRate;
    function Step(const AudioIn:single):single;

    property FmAmount : single read fFmAmount write fFmAmount;
    property AmAmount : single read fAmAmount write fAmAmount;

    property Shape : single read fShape write fShape; //range 0..1
    property PulseWidth : single read fPulseWidth write fPulseWidth; //range 0..1
    //==========================================================================

  end;

implementation

uses
  Math, eeDsp;

const
  kBufferSampleFrames = 4;

{ TSawSquareOsc }

constructor TSawSquareOsc.Create;
var
  c1: Integer;
begin
  CurPhase := 0;
  SetWaveDataTableSize(2048);

  FillTableWithSineWave;
  FillBufferFrames;

  HarmonicMap := THarmonicMap.Create;
  HarmonicMap.Count := 512;

  for c1 := 1 to HarmonicMap.Count-1 do
  begin
    HarmonicMap.Harmonics[c1].Magnitude := 0.5 / c1;
    HarmonicMap.Harmonics[c1].Phase := 0;
  end;
end;

destructor TSawSquareOsc.Destroy;
begin
  SetLength(fWaveData, 0);
  HarmonicMap.Free;
  inherited;
end;

procedure TSawSquareOsc.FillBufferFrames;
var
  c1: Integer;
  Index1, Index2 : integer;
begin
  for c1 := 0 to kBufferSampleFrames-1 do
  begin
    Index1 := c1;
    Index2 := TableSize + c1;
    WaveData[Index2] := WaveData[Index1];
  end;
end;

procedure TSawSquareOsc.FillTableWithSineWave;
var
  c1 : integer;
begin
  for c1 := 0 to TableSize-1 do
  begin
    WaveData[c1] := Sin((c1/TableSize) * 2 * pi);
  end;
end;

procedure TSawSquareOsc.SetWaveDataTableSize(NewTableSize: integer);
begin
  SetLength(fWaveData, NewTableSize + kBufferSampleFrames);
  fTableSize := NewTableSize;

  FillTableWithSineWave;
  FillBufferFrames;
end;

procedure TSawSquareOsc.SetFreq(const Value: single);
begin
  fFreq := Value;
  StepSize := (1 / fSampleRate) * Value * fTableSize;
end;

procedure TSawSquareOsc.SetOscRootFreq(const Value: single);
var
  MaxHarmonics : integer;
begin
  fOscRootFreq := Value;

  StepSize := (1 / fSampleRate) * Value * fTableSize;

  MaxHarmonics := floor(fSampleRate / 2 / Value);

  HarmonicMap.GenerateWaveTable(@fWaveData[0], TableSize, MaxHarmonics);
  FillBufferFrames;
end;



function TSawSquareOsc.Step(const AudioIn:single): single;
var
  ax : integer;
  frac : single;
  sax, sbx : single;
  //Out1 : single;
  Saw1Out : single;
  Saw2Out : single;
  OffsetPhase : single;
begin
  //== Calc Saw Osc One ==
  ax := floor(CurPhase);
  //bx := ax + 1;
  frac := CurPhase - ax;

  sax := WaveData[ax];
  sbx := WaveData[ax+1];

  Saw1Out := LinearInterpolation(sax, sbx, frac);


  //== Calc Saw Osc Two ==
  OffsetPhase := CurPhase + (TableSize * PulseWidth);
  if OffsetPhase >= TableSize then OffsetPhase := OffsetPhase - TableSize;
  ax := floor(OffsetPhase);
  //bx := ax + 1;
  frac := OffsetPhase - ax;

  sax := WaveData[ax];
  sbx := WaveData[ax+1];

  Saw2Out := LinearInterpolation(sax, sbx, frac) * -1;







  //== Calc output ==
  result := Saw1Out + (Saw2Out * Shape);
  //result := Saw2Out;



  //== Increment Phase ==
  CurPhase := CurPhase + StepSize;
  if CurPhase >= TableSize then CurPhase := CurPhase - TableSize;
end;

end.
