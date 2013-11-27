unit eeAudioFilePreviewPlayerVoice;

interface

uses
  MoreTypes, eeSampleFloat, eeSimpleGate;

type
  TAudioFilePreviewPlayerVoice = class
  private
    fSampleRate: integer;
    fVolume: single;
    fActive: boolean;
  protected
    Gate:TSimpleGate;
    Sample:TSampleFloat;
    SampleFileName:string;

    ReadPos:cardinal;
    ReadEnd:cardinal;
    ReadStepSize:cardinal;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Trigger(fn:string);
    procedure Stop;
    procedure Kill;

    procedure Process(In1,In2:PSingle; Sampleframes:integer); inline;

    property SampleRate : integer read fSampleRate write fSampleRate;
    property Volume     : single  read fVolume     write fVolume;     //range 0..1

    property Active  :boolean read fActive;
  end;

implementation

uses
  SysUtils;

{ TAudioFilePreviewPlayerVoice }

constructor TAudioFilePreviewPlayerVoice.Create;
begin
  Sample := TSampleFloat.Create;
  Gate := TSimpleGate.Create;
end;

destructor TAudioFilePreviewPlayerVoice.Destroy;
begin
  if assigned(Sample) then FreeAndNil(Sample);
  Gate.Free;
  inherited;
end;


procedure TAudioFilePreviewPlayerVoice.Trigger(fn: string);
const
  kFullVelocity = 1;
begin
  fActive := false;
  SampleFileName := fn;

  if Sample.LoadFromFile(SampleFileName) then
  begin
    if Sample.Properties.SampleFrames > 2 then
    begin
      ReadPos      := 0;
      ReadStepSize := round(Sample.Properties.SampleRate / SampleRate * 1024);
      ReadEnd      := (Sample.Properties.SampleFrames-2) * 1024;

      Gate.AttackSamples  := 0;
      Gate.ReleaseSamples := round(SampleRate * 0.015);
      Gate.Trigger(kFullVelocity);

      fActive := true;
    end;
  end;
end;


procedure TAudioFilePreviewPlayerVoice.Stop;
begin
  if Gate.Stage = gsOff
    then fActive := false
    else Gate.Release;
end;

procedure TAudioFilePreviewPlayerVoice.Kill;
begin
  fActive := false;
end;

procedure TAudioFilePreviewPlayerVoice.Process(In1, In2: PSingle; Sampleframes: integer);
const
  OneOver1024 = 1 / 1024;
var
  c1: Integer;
  r1,r2:integer;
  f:single;
  x1,x2:single;
  GateValue:single;
begin
  if (Active) and (Sample.Properties.IsValid) and (Sample.Properties.ChannelCount = 1) then
  begin
    try
      for c1 := 0 to SampleFrames - 1 do
      begin
        GateValue := Gate.Step;

        r1 := ReadPos shr 10; //r1 := ReadPos div 1024;
        r2 := r1 + 1;
        f  := (ReadPos * OneOver1024) - r1;
        x1 := Sample.Ch1[r1] * (1 - f) + Sample.Ch1[r2] * f;

        if (ReadPos + ReadStepSize < ReadEnd)
          then ReadPos := ReadPos + ReadStepSize
          else fActive := false;

        In1^ := In1^ + (x1 * Volume * GateValue);
        In2^ := In2^ + (x1 * Volume * GateValue);
        inc(In1);
        Inc(In2);
      end;
    except

      // TODO:
      // NOTE: This section of code would sometimes trigger an access violation but I don't understand why.
      // The Sample loading is protected by a critical section so shouldn't be getting modified unexpectedly.
      on EAccessViolation do fActive := false;
    end;

    if Gate.Stage = gsOff then fActive := false;
  end;



  if (Active) and  (Sample.Properties.IsValid) and (Sample.Properties.ChannelCount = 2) then
  begin
    try
      for c1 := 0 to SampleFrames - 1 do
      begin
        GateValue := Gate.Step;

        r1 := ReadPos shr 10; //r1 := ReadPos div 1024;
        r2 := r1 + 1;
        f  := (ReadPos * OneOver1024) - r1;
        x1 := Sample.Ch1[r1] * (1 - f) + Sample.Ch1[r2] * f;
        x2 := Sample.Ch2[r1] * (1 - f) + Sample.Ch2[r2] * f;

        if (ReadPos + ReadStepSize < ReadEnd)
          then ReadPos := ReadPos + ReadStepSize
          else fActive  := false;

        In1^ := In1^ + (x1 * Volume * GateValue);
        In2^ := In2^ + (x2 * Volume * GateValue);
        inc(In1);
        Inc(In2);
      end;
    except
      // NOTE: This section of code would sometimes trigger an access violation but I don't understand why.
      // The Sample loading is protected by a critical section so shouldn't be getting modified unexpectedly.
      on EAccessViolation do fActive := false;
    end;

    if Gate.Stage = gsOff then fActive := false;
  end;




end;



end.
