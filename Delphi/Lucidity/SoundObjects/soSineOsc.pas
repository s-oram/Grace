unit soSineOsc;

interface

type
  TWaveData = array of single;

  TSineOsc = class
  private
    fSampleRate: single;
    fFreq: single;
    fWaveData: TWaveData;
    procedure SetFreq(const Value: single);
  protected
    CurPhase : single;
    StepSize : single;


    procedure FillTableWithSineWave; // Fill the internal WaveData with a sine waveform.

    property WaveData  : TWaveData read fWaveData write fWaveData;
  public
    constructor Create;
    destructor Destroy; override;

    property Freq : single read fFreq write SetFreq;
    property SampleRate : single read fSampleRate write fSampleRate;

    function AudioRateStep:single;

  end;

implementation

uses
  Math, eeDsp;

const
  kTableSize          = 2048;
  kBufferSampleFrames = 4;


{ TSineOsc }

constructor TSineOsc.Create;
begin
  SetLength(fWaveData, kTableSize + kBufferSampleFrames);
  FillTableWithSineWave;
end;

destructor TSineOsc.Destroy;
begin
  SetLength(fWaveData, 0);
  inherited;
end;

procedure TSineOsc.FillTableWithSineWave;
var
  c1 : integer;
  Index1, Index2 : integer;
begin
  // generate the sine wave
  for c1 := 0 to kTableSize-1 do
  begin
    WaveData[c1] := Sin((c1/kTableSize) * 2 * pi);
  end;

  // fill the buffer sample points
  for c1 := 0 to kBufferSampleFrames-1 do
  begin
    Index1 := c1;
    Index2 := kTableSize + c1;
    WaveData[Index2] := WaveData[Index1];
  end;
end;


procedure TSineOsc.SetFreq(const Value: single);
begin
  fFreq := Value;
  StepSize := (1 / fSampleRate) * Value * kTableSize;
end;

function TSineOsc.AudioRateStep: single;
var
  ax : integer;
  frac : single;
  sax, sbx : single;
begin
  //== Calc Osc Phase ==
  ax := floor(CurPhase);
  //bx := ax + 1;
  frac := CurPhase - ax;

  sax := WaveData[ax];
  sbx := WaveData[ax+1];

  result := LinearInterpolation(sax, sbx, frac);


  //== Increment Phase ==
  CurPhase := CurPhase + StepSize;
  if CurPhase >= kTableSize then CurPhase := CurPhase - kTableSize;
end;



end.
