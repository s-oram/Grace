unit eeDSP.Oversampling;

interface

uses
  MoreTypes, eeFilters.AllPass;




procedure ZeroPadx2(InputBuffer, OutputBuffer:PSingle; const InputSampleFrames: integer); inline;
procedure ZeroPadx4(InputBuffer, OutputBuffer:PSingle; const InputSampleFrames: integer); inline;

//NOTE: The decimate functions do not do any filtering.
procedure Decimatex2_WithGainCorrection(InputBuffer, OutputBuffer:PSingle; const OutputSampleFrames: integer); inline;
procedure Decimatex4_WithGainCorrection(InputBuffer, OutputBuffer:PSingle; const OutputSampleFrames: integer); inline;


type
  // THalfBandLowPassFilter is a two-path filter as described in
  // "Streamlining Digital Signal Processing" by richard G.Lyons. Chapter 9.
  THalfBandLowPassFilter = class
  private
  protected
    x1 : double;
    ap0, ap1, ap2, ap3 : TSecondOrderAllPass;
  public
    constructor Create;
    destructor Destroy; override;

    function Step(x0: double):double; inline;

    procedure Process(In1: psingle; SampleFrames:integer); 
  end;


  

implementation

uses
  eeDsp, eeAsm;

procedure ZeroPadx2(InputBuffer, OutputBuffer:PSingle; const InputSampleFrames: integer); inline;
var
  c1: Integer;
begin
  for c1 := 0 to InputSampleFrames - 1 do
  begin
    //Copy sample value...
    OutputBuffer^ := InputBuffer^;
    inc(InputBuffer);
    inc(OutputBuffer);

    //zero pad.
    OutputBuffer^ := 0;
    inc(OutputBuffer);
  end;
end;

procedure ZeroPadx4(InputBuffer, OutputBuffer:PSingle; const InputSampleFrames: integer); inline;
var
  c1: Integer;
begin
  for c1 := 0 to InputSampleFrames - 1 do
  begin
    //Copy sample value...
    OutputBuffer^ := InputBuffer^;
    inc(InputBuffer);
    inc(OutputBuffer);

    //zero pad.
    OutputBuffer^ := 0;
    inc(OutputBuffer);

    OutputBuffer^ := 0;
    inc(OutputBuffer);

    OutputBuffer^ := 0;
    inc(OutputBuffer);

  end;
end;

procedure Decimatex2_WithGainCorrection(InputBuffer, OutputBuffer:PSingle; const OutputSampleFrames: integer); inline;
var
  c1: Integer;
begin
  for c1 := 0 to OutputSampleFrames - 1 do
  begin
    //Copy sample,
    OutputBuffer^ := InputBuffer^ * 0.5;
    inc(InputBuffer);
    inc(OutputBuffer);

    //extra incrment to perform decimation
    inc(InputBuffer);
  end;
end;

procedure Decimatex4_WithGainCorrection(InputBuffer, OutputBuffer:PSingle; const OutputSampleFrames: integer); inline;
var
  c1: Integer;
begin
  for c1 := 0 to OutputSampleFrames - 1 do
  begin
    //Copy sample,
    OutputBuffer^ := InputBuffer^;
    inc(InputBuffer);
    inc(OutputBuffer);

    //extra incrment to perform decimation
    inc(InputBuffer);
    inc(InputBuffer);
    inc(InputBuffer);
  end;
end;

{ THalfBandFilter }

constructor THalfBandLowPassFilter.Create;
begin
  ap0 := TSecondOrderAllPass.Create;
  ap1 := TSecondOrderAllPass.Create;
  ap2 := TSecondOrderAllPass.Create;
  ap3 := TSecondOrderAllPass.Create;

  x1 := 0;

  ap0.Coefficient1 := 0;
  ap0.Coefficient2 := 0.101467517;

  ap2.Coefficient1 := 0;
  ap2.Coefficient2 := 0.612422841;

  ap1.Coefficient1 := 0;
  ap1.Coefficient2 := 0.342095596;

  ap3.Coefficient1 := 0;
  ap3.Coefficient2 := 0.867647439;
end;

destructor THalfBandLowPassFilter.Destroy;
begin
  ap0.Free;
  ap1.Free;
  ap2.Free;
  ap3.Free;
  inherited;
end;

function THalfBandLowPassFilter.Step(x0: double): double;
var
  t0, t1, t2, t3: double;
begin
  t0 := ap0.Step(x0);
  t1 := ap1.Step(x1);
  t2 := ap2.Step(t0);
  t3 := ap3.Step(t1);
  x1 := x0;

  result := t2 + t3;
end;

procedure THalfBandLowPassFilter.Process(In1: psingle; SampleFrames: integer);
var
  c1 : integer;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    In1^ := Step(In1^ + kDenormal);
    inc(In1);
  end;

  emms;
end;



end.
