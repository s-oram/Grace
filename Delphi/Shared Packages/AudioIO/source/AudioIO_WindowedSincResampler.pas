unit AudioIO_WindowedSincResampler;

interface

uses
  VamLib.MoreTypes;

type
  // NOTE:
  // Windowed sinc resampler implements high quality resampling it aids in
  // converting between any samplerate to another.
  // It is designed to be used for offline processes where speed isn't
  // the number priority. There are a number of ways this class
  // could be optimised to improve CPU efficiency.

  TWindowedSincResampler = class
  private
    fSincTaps: integer;
    fOverSampleFactor: integer;
    procedure SetSincTaps(const Value: integer);
    procedure SetOverSampleFactor(const Value: integer);
  protected

    property OverSampleFactor : integer read fOverSampleFactor write SetOverSampleFactor; //16 by default.
    property SincTaps : integer read fSincTaps write SetSincTaps;  // range ~16..~1024+. //256 by default
  public
    constructor Create;
    destructor Destroy; override;

    procedure Resample(const SourceBuffer, DestBuffer :PSingle; const SourceSampleFrames, DestSampleFrames:integer);
  end;


implementation

uses
  SysUtils,
  Math,
  AudioIO_WindowedSincFilter;


function LinearInterpolation(const x0, x1, f:double):double; inline;
begin
  result := (x1 - x0) * f + x0;
end;

function ReadSampleFromBuffer(const Buffer:TArrayOfSingle; const ReadIndex : double):single; inline;
var
  a, b : integer;
  f : double;
  ax, bx : single;
begin
  a := floor(ReadIndex);
  b := a + 1;
  f := ReadIndex - a;
  ax := Buffer[a];
  bx := Buffer[b];
  result := LinearInterpolation(ax, bx, f);
end;

{ TWindowedSincResampler }

constructor TWindowedSincResampler.Create;
begin
  fSincTaps := 256;
  fOverSampleFactor := 16;
end;

destructor TWindowedSincResampler.Destroy;
begin
  inherited;
end;

procedure TWindowedSincResampler.SetOverSampleFactor(const Value: integer);
begin
  fOverSampleFactor := Value;
end;

procedure TWindowedSincResampler.SetSincTaps(const Value: integer);
begin
  fSincTaps := Value;
end;


procedure TWindowedSincResampler.Resample(const SourceBuffer, DestBuffer: PSingle; const SourceSampleFrames, DestSampleFrames: integer);
var
  InputSamples  : TArrayOfSingle absolute SourceBuffer;
  OutputSamples : TArrayOfSingle absolute DestBuffer;
  c1: Integer;
  ReadIndex : double;
  ReadIndexFactor : double;
  OverSampledInput : TArrayOfSingle;
  Filter : TWindowedSincFilter;
begin
  // NOTE: This function is implemented quite crudely. It oversamples the input in one step,
  // then reads out interpolated values to find the downsample output values.
  // A couple improvements could be:
  // - do the work in blocks, (8000 samples at a time. This will probably be
  //   necessary to work with long samples...
  // - the function could check for favourable integer resampling ratios ie. 2:1, 3:1, 4:1 etc.
  // - Use a better interpolator. Currently using linear interpolation. Better
  //   interpolation will allow for a lower oversampling factor.

  Filter := TWindowedSincFilter.Create;
  try
    Filter.SincTaps := SincTaps;
    Filter.Cutoff := 1 / OverSampleFactor;

    if SourceSampleFrames = DestSampleFrames then
    begin
      for c1 := 0 to DestSampleFrames-1 do
      begin
        OutputSamples[c1] := InputSamples[c1];
      end;
    end else
    begin
      SetLength(OverSampledInput, SourceSampleFrames * OverSampleFactor);

      for c1 := 0 to SourceSampleFrames * OverSampleFactor - 1 do
      begin
        OverSampledInput[c1] := 0;
      end;

      for c1 := 0 to SourceSampleFrames-1 do
      begin
        OverSampledInput[c1 * OverSampleFactor] := InputSamples[c1];
      end;

      Filter.Process(@OverSampledInput[0],@OverSampledInput[0],SourceSampleFrames * OverSampleFactor);

      for c1 := 0 to SourceSampleFrames * OverSampleFactor - 1 do
      begin
        OverSampledInput[c1] := OverSampledInput[c1] * OverSampleFactor;
      end;

      ReadIndexFactor := SourceSampleFrames / DestSampleFrames;
      for c1 := 0 to DestSampleFrames-1 do
      begin
        ReadIndex := c1 * ReadIndexFactor * OverSampleFactor;
        OutputSamples[c1] := ReadSampleFromBuffer(OverSampledInput, ReadIndex);
      end;
    end;


  finally
    SetLength(OverSampledInput, 0);
    Filter.Free;
  end;

end;










end.
