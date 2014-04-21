unit soDynamicWaveTableOsc;

interface

type
  TWaveData = array of single;

  TDynamicWaveTableOsc = class
  private
    fSampleRate: single;
    fFreq: single;
    fWaveData: TWaveData;
    fTableSize: integer;
    procedure SetSampleRate(const Value: single);
    procedure SetFreq(const Value: single);
  protected
    CurPhase : single;
    StepSize : single;


    procedure SetWaveDataTableSize(NewTableSize:integer);
  public
    constructor Create;
    destructor Destroy; override;

    //==========================================================================
    // These methods are designed to be used by the 'synthesis' generation
    // parts of an application.
    property Freq : single read fFreq write SetFreq;
    property SampleRate : single read fSampleRate write SetSampleRate;
    function Step:single;
    //==========================================================================

    //==========================================================================
    // Generally these methods/properties are only used externally when
    // the WaveData contents are being updated.
    procedure FillTableWithSineWave; // Fill the internal WaveData with a sine waveform.
    procedure FillBufferFrames;      // Must be called after changing the contents of WaveData.
    property TableSize : integer read fTableSize;
    property WaveData : TWaveData read fWaveData write fWaveData;
    //==========================================================================




  end;

implementation

uses
  Math, eeDsp;

const
  kBufferSampleFrames = 4;

{ TDynamicWaveTableOsc }

constructor TDynamicWaveTableOsc.Create;
begin
  CurPhase := 0;
  SetWaveDataTableSize(2048);
end;

destructor TDynamicWaveTableOsc.Destroy;
begin
  SetLength(fWaveData, 0);
  inherited;
end;

procedure TDynamicWaveTableOsc.SetWaveDataTableSize(NewTableSize: integer);
begin
  SetLength(fWaveData, NewTableSize + kBufferSampleFrames);
  fTableSize := NewTableSize;

  FillTableWithSineWave;
  FillBufferFrames;
end;

procedure TDynamicWaveTableOsc.SetFreq(const Value: single);
begin
  fFreq := Value;

  StepSize := (1 / fSampleRate) * Value * fTableSize;
end;

procedure TDynamicWaveTableOsc.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
end;

procedure TDynamicWaveTableOsc.FillBufferFrames;
var
  c1: Integer;
  Index1, Index2 : integer;
begin
  for c1 := 0 to kBufferSampleFrames-1 do
  begin
    Index1 := c1;
    Index2 := TableSize - c1 - 1;
    WaveData[Index2] := WaveData[Index1];
  end;
end;

procedure TDynamicWaveTableOsc.FillTableWithSineWave;
var
  c1 : integer;
begin
  for c1 := 0 to TableSize-1 do
  begin
    WaveData[c1] := Sin((c1/TableSize) * 2 * pi);
  end;
end;



function TDynamicWaveTableOsc.Step: single;
var
  ax : integer;
  frac : single;
  sax, sbx : single;
  Out1 : single;
begin
  ax := floor(CurPhase);
  //bx := ax + 1;
  frac := CurPhase - ax;

  sax := WaveData[ax];
  sbx := WaveData[ax+1];

  Out1 := LinearInterpolation(sax, sbx, frac);

  result := Out1;

  CurPhase := CurPhase + StepSize;
  if CurPhase >= TableSize then CurPhase := CurPhase - TableSize;
end;

end.
