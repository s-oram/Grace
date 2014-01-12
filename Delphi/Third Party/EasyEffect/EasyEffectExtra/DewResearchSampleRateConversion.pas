{
  Dew Research Samplerate Conversion

}

unit DewResearchSampleRateConversion;

interface

uses
  VamLib.MoreTypes, SignalTools, SignalProcessing;

type
  TDownsampler = class
  private
    function GetDecimationFactor: integer;
    procedure SetDecimationFactor(const Value: integer);
  protected
    Signal:TSignal;
    SignalDecimator : TSignalDecimator;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    property DecimationFactor : integer read GetDecimationFactor write SetDecimationFactor;

    procedure Process(InBuffer, OutBuffer : PSingle; InputSampleFrames : integer);
  end;

implementation

uses
  Math;

{ TDownsampler }

constructor TDownsampler.Create;
begin
  Signal := TSignal.Create(nil);
  Signal.Complex := false;
  Signal.ChannelCount := 1;

  SignalDecimator := TSignalDecimator.Create(nil);
  SignalDecimator.Input := Signal;
  SignalDecimator.Factor := 2;
  SignalDecimator.Ripple := 0.00001;

end;

destructor TDownsampler.Destroy;
begin

  inherited;
end;

function TDownsampler.GetDecimationFactor: integer;
begin
  result := SignalDecimator.Factor;
end;

procedure TDownsampler.SetDecimationFactor(const Value: integer);
begin
  SignalDecimator.Factor := value;
end;

procedure TDownsampler.Process(InBuffer, OutBuffer: PSingle; InputSampleFrames: integer);
var
  c1: Integer;
  ps : PSingle;
  InternalBufferLength : integer;
  FilterDelay : integer;
begin
  FilterDelay := ceil(SignalDecimator.FilterDelay);
  InternalBufferLength := InputSampleFrames + (FilterDelay * SignalDecimator.Factor);
  if Signal.Length <> InternalBufferLength then Signal.Length := InternalBufferLength;

  //Zero Buffer.
  for c1 := 0 to InternalBufferLength - 1 do
  begin
    Signal[c1] := 0;
  end;

  //copy sample data.
  for c1 := 0 to InputSampleFrames - 1 do
  begin
    Signal[c1] := InBuffer^;
    inc(InBuffer);
  end;


  //Decimate...
  SignalDecimator.Update;


  //Copy output...
  for c1 := 0 to InputSampleFrames div DecimationFactor - 1 do
  begin
    OutBuffer^ := SignalDecimator[c1 + FilterDelay];
    inc(OutBuffer);
  end;



  //if InputSampleFrames <> Signal.Length then Signal.Length := InputSampleFrames;


  {
  ps := @Signal.Data.SData[0];
  for c1 := 0 to InputSampleFrames - 1 do
  begin
    ps^ := InBuffer^;
    inc(InBuffer);
    inc(ps);
  end;
  }

  {
  for c1 := 0 to InputSampleFrames - 1 do
  begin
    Signal[c1] := InBuffer^;
    inc(InBuffer);
  end;

  SignalDecimator.Update;

  for c1 := 0 to InputSampleFrames div DecimationFactor - 1 do
  begin
    OutBuffer^ := SignalDecimator[c1];
    inc(OutBuffer);
  end;
  }
end;




procedure TDownsampler.Reset;
begin
  SignalDecimator.Reset;
end;

end.
