{
  A class to add noise to demos...

}

unit eeDemoNoise;

interface

uses
  MoreTypes, eeGlobals;


type
  TDemoNoise = class
  private
    fSampleRate: single;
    fSilentPeriod: single;
    fNoisePeriod: single;
    fNoiseVolume: single;
    procedure SetNoisePeriod(const Value: single);
    procedure SetSilentPeriod(const Value: single);
  protected
    Globals:TGlobals;

    SampleCount   : integer;
    SilentSamples : single;
    NoiseSamples  : single;

    fIsNoiseState  : boolean;

    procedure OnSampleRateChange(Sender:TObject);

    property SampleRate:single read fSampleRate write fSampleRate;
  public
    constructor Create(aGlobals:TGlobals);
	  destructor Destroy; override;


    procedure Process(Out1, Out2:PSingle; SampleFrames:integer); inline;


    property SilentPeriod : single read fSilentPeriod write SetSilentPeriod; //milliseconds
    property NoisePeriod  : single read fNoisePeriod  write SetNoisePeriod;  //milliseconds
    property NoiseVolume  : single read fNoiseVolume  write fNoiseVolume;  //range 0..1

    property IsNoiseState : boolean read fIsNoiseState;
  end;


implementation

uses
  eeDsp;

{ TDemoNoise }

constructor TDemoNoise.Create(aGlobals: TGlobals);
begin
  Globals := aGlobals;
  Globals.SampleRateList.Add(OnSampleRateChange);

  SampleRate := Globals.SampleRate;

  SilentPeriod := 120 * 1000;
  NoisePeriod  := 15 * 1000;
  NoiseVolume  := 0.015;

  SampleCount   := 0;

  fIsNoiseState := false;
end;

destructor TDemoNoise.Destroy;
begin

  inherited;
end;

procedure TDemoNoise.OnSampleRateChange(Sender: TObject);
begin
  SampleRate := Globals.SampleRate;

  SilentSamples := MillisecondsToSamples(fSilentPeriod, SampleRate);
  NoiseSamples  := MillisecondsToSamples(fNoisePeriod, SampleRate);
end;

procedure TDemoNoise.SetNoisePeriod(const Value: single);
begin
  fNoisePeriod := Value;
  NoiseSamples  := MillisecondsToSamples(fNoisePeriod, SampleRate);
end;

procedure TDemoNoise.SetSilentPeriod(const Value: single);
begin
  fSilentPeriod := Value;
  SilentSamples := MillisecondsToSamples(fSilentPeriod, SampleRate);
end;


procedure TDemoNoise.Process(Out1, Out2: PSingle; SampleFrames: integer);
var
  c1: Integer;
begin
  inc(SampleCount, SampleFrames);

  if fIsNoiseState then
  begin
    if SampleCount > NoiseSamples then
    begin
      fIsNoiseState := false;
      SampleCount := 0;
    end;

    for c1 := 0 to SampleFrames - 1 do
    begin
      Out1^ := (Random * 2 - 1) * NoiseVolume;
      Out2^ := Out1^;
      inc(Out1);
      inc(Out2);
    end;
  end else
  begin
    if SampleCount > SilentSamples then
    begin
      fIsNoiseState := true;
      SampleCount := 0;
    end;
  end;
end;


end.
