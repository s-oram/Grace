unit soFadeOutRamp;

interface

uses
  VamLib.MoreTypes;

type
  TFadeOutRamp = record
  private
    fBlockSize : integer;
    fIsActive: boolean;
    fGain : single;
    fSampleRate: integer;
    RampStepSize : single;
    fFadeOutTime: integer;
    procedure SetBlockSize(const Value: integer);
  public
    fIsRelease : boolean;
    Buffer : array of single;

    procedure Trigger;
    procedure Release;
    procedure Kill;

    procedure Process(const SampleFrames : integer);

    property FadeOutTime : integer read fFadeOutTime write fFadeOutTime; // fade out time is in milliseconds.

    property BlockSize : integer read fBlockSize write SetBlockSize;
    property SampleRate : integer read fSampleRate write fSampleRate;
    property IsActive   : boolean read fIsActive;
    property IsRelease  : boolean read fIsRelease;
  end;


implementation

{ TFadeOutRamp }

procedure TFadeOutRamp.Trigger;
var
  c1: Integer;
begin
  fGain := 1;
  fIsActive := true;
  fIsRelease := false;
  RampStepSize := 0;
  for c1 := 0 to fBlockSize-1 do
  begin
    Buffer[c1] := 1;
  end;
end;

procedure TFadeOutRamp.Release;
begin
  if fIsActive then
  begin
    fIsRelease := true;
    if FadeOutTime > 0
      then RampStepSize := (1 / fSampleRate) * (1000 / FadeOutTime)
      else RampStepSize := 1;
    if RampStepSize > 1 then RampStepSize := 1;
  end;
end;

procedure TFadeOutRamp.SetBlockSize(const Value: integer);
begin
  fBlockSize := Value;
  SetLength(self.Buffer, Value);
end;

procedure TFadeOutRamp.Kill;
begin
  fIsActive := false;
  fIsRelease := false;
end;

procedure TFadeOutRamp.Process(const SampleFrames: integer);
var
  c1: Integer;
begin
  assert(fIsActive);
  assert(fIsRelease);

  for c1 := 0 to SampleFrames-1 do
  begin
    if fGain > 0 then
    begin
      Buffer[c1] := fGain;
      fGain := fGain - RampStepSize;
    end else
    begin
      Buffer[c1] := 0;
      fGain := 0;
      fIsActive := false;
      fIsRelease := false;
    end;
  end;

end;






end.
