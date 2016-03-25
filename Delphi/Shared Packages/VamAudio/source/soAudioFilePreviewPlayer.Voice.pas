unit soAudioFilePreviewPlayer.Voice;

interface

uses
  soFadeOutRamp,
  VamDsp.Interpolation,
  VamLib.MoreTypes, eeSampleFloat;

type
  TSamplePreviewVoice = class
  private
    fSampleRate: integer;
    fBlockSize: integer;
    fIsActive: boolean;
    fSampleData : TSampleFloat;
  private
    GainBuffer : array[0..256] of single;
    FadeOutRamp : TFadeOutRamp;
    ReadPos : single;
    ReadStepSize : single;
    Ch1: PArrayOfSingle;
    Ch2: PArrayOfSingle;
    SampleLength : integer;
    procedure SetSampleRate(const Value: integer);
    procedure SetBlockSize(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessReplacing(OutBuffer1, OutBuffer2 : PDouble; const SampleFrames : integer); //inline;

    procedure Trigger(SampleData : TSampleFloat);
    procedure FastRelease;
    procedure Kill;

    property SampleRate : integer read fSampleRate write SetSampleRate;
    property BlockSize : integer read fBlockSize write SetBlockSize;

    property IsActive : boolean read fIsActive;
  end;


implementation

uses
  Math,
  //SampleOscUtils,
  AudioIO;

{ TSamplePreviewVoice }

constructor TSamplePreviewVoice.Create;
begin
  FadeOutRamp.FadeOutTime := 5;
end;

destructor TSamplePreviewVoice.Destroy;
begin

  inherited;
end;

procedure TSamplePreviewVoice.Trigger(SampleData: TSampleFloat);
begin
  assert(self.SampleRate > 0);

  fSampleData := SampleData;
  if fSampleData.Properties.IsValid then
  begin
    fIsActive := true;
    FadeOutRamp.Trigger;

    if SampleData.Properties.ChannelCount = 1 then
    begin
      Ch1 := SampleData.Ch1Pointer;
      Ch2 := SampleData.Ch1Pointer;
    end else
    if SampleData.Properties.ChannelCount = 2 then
    begin
      Ch1 := SampleData.Ch1Pointer;
      Ch2 := SampleData.Ch2Pointer;
    end else
    begin
      exit; //========================>> exit >>=======================>>
    end;

    SampleLength := SampleData.Properties.SampleFrames;
    ReadStepSize := SampleData.Properties.SampleRate / self.SampleRate;
    ReadPos := 0;
  end;
end;

procedure TSamplePreviewVoice.FastRelease;
begin
  FadeOutRamp.Release;
end;

procedure TSamplePreviewVoice.Kill;
begin
  fIsActive := false;
  FadeOutRamp.Kill;
end;

procedure TSamplePreviewVoice.ProcessReplacing(OutBuffer1, OutBuffer2: PDouble; const SampleFrames: integer);
var
  c1: Integer;
  ReadIndex : cardinal;
  ReadFrac : single;
  Out1, Out2 : single;
  y0, y1, y2, y3 : single;
  FadeOutGain : PSingle;

begin
  assert(fIsActive);

  if (FadeOutRamp.IsRelease)
    then FadeOutRamp.Process(SampleFrames);
  FadeOutGain := @FadeOutRamp.Buffer[0];

  for c1 := 0 to SampleFrames-1 do
  begin
    //TODO:HIGH read the sample position here and output the sample preview.


    // === optimal linear interpolation if the signal is oversampled. We should
    // try that next. (oversampling, that is.)
    ReadIndex := floor(ReadPos);
    ReadFrac := ReadPos - ReadIndex;
    //ReadValuesFromSample_LinearInterpolation(sd, ReadIndex, ReadFrac, Out1, Out2);


    //=== straight linear interpolation =====
    y0 := self.Ch1^[ReadIndex];
    y1 := self.Ch1^[ReadIndex + 1];
    y2 := self.Ch1^[ReadIndex + 2];
    y3 := self.Ch1^[ReadIndex + 3];
    //Out1 := Linear(ReadFrac, y0, y1);
    //Out1 := CosineInterpolation(ReadFrac, y0, y1);
    Out1 := HermiteInterpolation(ReadFrac, y0, y1, y2, y3);
    //Out1 := HermiteInterpolation(ReadFrac, y0, y1, y2, y3);

    y0 := self.Ch2^[ReadIndex];
    y1 := self.Ch2^[ReadIndex + 1];
    y2 := self.Ch2^[ReadIndex + 2];
    y3 := self.Ch2^[ReadIndex + 3];
    //Out2 := Linear(ReadFrac, y0, y1);
    //Out2 := CosineInterpolation(ReadFrac, y0, y1);
    Out2 := HermiteInterpolation(ReadFrac, y0, y1, y2, y3);
    //Out2 := HermiteInterpolation(ReadFrac, y0, y1, y2, y3);

    //====================================================

    //FadeOutGain := FadeOutRamp.Buffer[c1];

    OutBuffer1^ := Out1 * FadeOutGain^;
    OutBuffer2^ := Out2 * FadeOutGain^;
    inc(FadeOutGain);
    //OutBuffer2^ := Out2 * FadeOutRampR.PBuffer[0];


    inc(OutBuffer1);
    inc(OutBuffer2);

    if ReadPos + ReadStepSize <= SampleLength-4
      then ReadPos := ReadPos + ReadStepSize
      else fIsActive := false;
  end;

  if not FadeOutRamp.IsActive then
  begin
    fIsActive := false;
  end;

end;

procedure TSamplePreviewVoice.SetBlockSize(const Value: integer);
begin
  fBlockSize := Value;
  FadeOutRamp.BlockSize := Value;
end;

procedure TSamplePreviewVoice.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  FadeOutRamp.SampleRate := Value;
end;

end.
