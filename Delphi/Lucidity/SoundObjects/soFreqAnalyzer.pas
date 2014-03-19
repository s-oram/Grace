unit soFreqAnalyzer;

interface

uses
  MtxVec,
  SignalUtils,
  VamLib.ZeroObject,
  VamLib.MoreTypes,
  eeDsp,
  LucidityGui.Scope.FreqAnalyzer;

type
  TFrequencyAnalyzer = class(TZeroObject, IFreqAnalyzer)
  private
    fWindowSize: integer;
    procedure SetWindowSize(const Value: integer);

    procedure GetAnalysisData(out MagnitudeData:PSingle; out SampleFrames : integer);
  protected
    BufferIndex : integer;
    BufferPos   : integer;

    InputA, InputB : TVec;
    OutputA, OutputB : TVec;

    FFTGains : array of single;

    WindowFunction  : TVec;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Process(Input1, Input2 : PSingle; SampleFrames : integer);

    property WindowSize : integer read fWindowSize write SetWindowSize;
  end;

implementation

{ TFrequencyAnalyzer }

constructor TFrequencyAnalyzer.Create;
begin
  fWindowSize := 1024;

  InputA := TVec.Create;
  InputA.Size(fWindowSize, false);

  InputB := TVec.Create;
  InputB.Size(fWindowSize, false);

  OutputA := TVec.Create;
  OutputA.Size(fWindowSize, true);

  OutputB := TVec.Create;
  OutputB.Size(fWindowSize, true);

  WindowFunction := TVec.Create;
  WindowFunction.Size(fWindowSize, false);

  //SignalUtils.Hamming(WindowFunction, TSignalWindowMode.wmSymmetric, 0, fWindowSize);

  BufferIndex := 0;
  BufferPos   := 0;


  SetLength(FFTGains, fWindowSize);
end;

destructor TFrequencyAnalyzer.Destroy;
begin

  InputA.Free;
  InputB.Free;

  OutputA.Free;
  OutputB.Free;

  WindowFunction.Free;

  SetLength(FFTGains, 0);

  inherited;
end;

procedure TFrequencyAnalyzer.GetAnalysisData(out MagnitudeData: PSingle; out SampleFrames: integer);
begin
  MagnitudeData := @FFTGains[0];
  SampleFrames  := fWindowSize div 2;
end;

procedure TFrequencyAnalyzer.SetWindowSize(const Value: integer);
begin
  fWindowSize := Value;
end;

procedure TFrequencyAnalyzer.Process(Input1, Input2: PSingle; SampleFrames: integer);
var
  c1, c2: Integer;
  InBuff, OutBuff : ^TVec;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    if BufferIndex = 0
      then InBuff := @InputA
      else InBuff := @InputB;

    InBuff^.Values[BufferPos] := Input1^ + Input2^;

    inc(Input1);
    inc(Input2);

    inc(BufferPos);
    if BufferPos >= fWindowSize then
    begin
      if BufferIndex = 0
        then OutBuff := @OutputA
        else OutBuff := @OutputB;

      //InBuff^.Mul(WindowFunction);
      OutBuff^.FFTFromReal(InBuff^);


      //Convert to magnitudes...
      for c2 := 0 to OutBuff^.Length div 2 - 1 do
      begin
        FFTGains[c2] := OutBuff^.Values[c2 * 2];
      end;

      BufferPos := 0;
      if BufferIndex = 0
        then BufferIndex := 1
        else BufferIndex := 0;


    end;
  end;
end;



end.
