unit eeWindowedSincFilter;

interface

uses
  MoreTypes;

  // Reference:
  // - The Scientist and Engineer's Guide to Digital Signal Processing
  // - Chapter 16 - Windowed Sinc Filters
  // - http://www.analog.com/static/imported-files/tech_docs/dsp_book_Ch16.pdf

  // NOTE:
  // This windowed sinc filter is intended for non-realtime usage.
  // The process method expects to process the entire audio buffer
  // in one pass.


type
  TWindowedSincFilter = class
  private
    fSincTaps: integer;
    fCutoff: single;
    procedure SetSincTaps(const Value: integer);
    procedure SetCutoff(const Value: single);
  protected
    SincKernel : array of single;
    procedure CalcSincKernel;

    function RunFilterStep(const ReadIndex : integer; const Input:TArrayOfSingle):single; //inline;
  public
    constructor Create;
    destructor Destroy; override;

    // SincTaps defaults to 128. It's a gives relatively good quality
    // roll-off when using the filter for bandlimiting when resampling signals.
    property SincTaps : integer read fSincTaps write SetSincTaps; //range ~16..~1024.
    property Cutoff   : single  read fCutoff   write SetCutoff;   //range 0..1. 1 = nyquist. 0.5 = half nyquist.

    procedure Process(px : PSingle; const SampleFrames : integer);
  end;

implementation

uses
  DelphiPlot,
  eeDsp,
  eeDspWindows;



{ TWindowedSincFilter }

constructor TWindowedSincFilter.Create;
begin
  fCutoff   := 0.5;
  fSincTaps := 128;

  CalcSincKernel;
end;

destructor TWindowedSincFilter.Destroy;
begin
  SetLength(SincKernel, 0);
  inherited;
end;

procedure TWindowedSincFilter.CalcSincKernel;
var
  ScaledCutoff : double;
  c1: Integer;

  x1, x2 : double;

  sum : double;
  GainFactor : double;

  fx1 : double;
  i, M : integer;
begin
  SetLength(SincKernel, SincTaps);

  ScaledCutoff := Cutoff * 0.5;
  assert((ScaledCutoff > 0) and (ScaledCutoff <= 0.5));

  for c1 := 0 to SincTaps-1 do
  begin
    SincKernel[c1] := Sinc(c1, Cutoff);
  end;

  // apply window function.
  for c1 := 0 to SincTaps-1 do
  begin
    x2 := BlackmanHarrisWindow((c1 / (SincTaps-1)) * 0.5 + 0.5);
    SincKernel[c1] := SincKernel[c1] * x2;
  end;


  sum := 0;
  for c1 := 0 to SincTaps-1 do
  begin
    sum := sum + SincKernel[c1];
  end;
  sum := sum * 2;
  GainFactor := 1 / Sum;

  for c1 := 0 to SincTaps-1 do
  begin
    SincKernel[c1] := SincKernel[c1] * GainFactor;
  end;
end;




procedure TWindowedSincFilter.SetCutoff(const Value: single);
begin
  assert((Value > 0) and (Value <= 1));
  fCutoff := Value;
  CalcSincKernel;
end;

procedure TWindowedSincFilter.SetSincTaps(const Value: integer);
begin
  fSincTaps := Value;
  CalcSincKernel;
end;


procedure TWindowedSincFilter.Process(px: PSingle; const SampleFrames: integer);
var
  tpx : PSingle;
  InputBuffer, OutputBuffer : TArrayOfSingle;
  c1: Integer;
begin
  SetLength(InputBuffer, SampleFrames);
  SetLength(OutputBuffer, SampleFrames);


  tpx := px;
  for c1 := 0 to SampleFrames-1 do
  begin
    InputBuffer[c1] := tpx^;
    inc(tpx);
  end;


  for c1 := 0 to SampleFrames-1 do
  begin
    OutputBuffer[c1] := RunFilterStep(c1, InputBuffer);
  end;

  tpx := px;
  for c1 := 0 to SampleFrames-1 do
  begin
    tpx^ := OutputBuffer[c1];
    inc(tpx);
  end;

  SetLength(InputBuffer, 0);
  SetLength(OutputBuffer, 0);
end;


function TWindowedSincFilter.RunFilterStep(const ReadIndex: integer; const Input: TArrayOfSingle): single;
var
  x : double;
  InputValue : double;
  c1: Integer;
  SampleFrames : single;
  Index2 : integer;
begin
  x := Input[ReadIndex] * SincKernel[0];

  SampleFrames := Length(Input);

  // calc the forward taps...
  for c1 := 1 to SincTaps-1 do
  begin
    Index2 := ReadIndex + c1;
    if (Index2 >= 0) and (Index2 < SampleFrames)
      then InputValue := Input[Index2]
      else InputValue := 0;

    x := x + InputValue * SincKernel[c1];
  end;

  // calc the backward taps...
  for c1 := 1 to SincTaps-1 do
  begin
    Index2 := ReadIndex - c1;
    if (Index2 >= 0) and (Index2 < SampleFrames)
      then InputValue := Input[Index2]
      else InputValue := 0;

    x := x + InputValue * SincKernel[c1];
  end;

  result := x;
end;

end.
