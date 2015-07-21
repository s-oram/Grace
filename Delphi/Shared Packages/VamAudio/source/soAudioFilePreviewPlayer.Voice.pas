unit soAudioFilePreviewPlayer.Voice;

interface

uses
  VamDsp.Interpolation,
  VamLib.MoreTypes, eeSampleFloat;

type
  TSamplePreviewVoice = class
  private
    fSampleRate: integer;
    fBlockSize: integer;
    fGain: single;
    fIsActive: boolean;
    fSampleData : TSampleFloat;
  private
    ReadPos : single;
    ReadStepSize : single;
    Ch1: PArrayOfSingle;
    Ch2: PArrayOfSingle;
    SampleLength : integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Process(In1,In2:PSingle; Sampleframes:integer); inline;
    procedure ProcessReplacing(OutBuffer1, OutBuffer2 : PDouble; const SampleFrames : integer); inline;

    procedure Trigger(SampleData : TSampleFloat);
    procedure Kill;

    property Gain : single read fGain write fGain;
    property SampleRate : integer read fSampleRate write fSampleRate;
    property BlockSize : integer read fBlockSize write fBlockSize;

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

end;

destructor TSamplePreviewVoice.Destroy;
begin

  inherited;
end;

procedure TSamplePreviewVoice.Kill;
begin
  fIsActive := false;
end;

procedure TSamplePreviewVoice.Trigger(SampleData: TSampleFloat);
begin
  assert(self.SampleRate > 0);

  fSampleData := SampleData;
  if fSampleData.Properties.IsValid then
  begin
    fIsActive := true;

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

procedure TSamplePreviewVoice.Process(In1, In2: PSingle; Sampleframes: integer);
var
  c1: Integer;
  ReadIndex : cardinal;
  ReadFrac : single;
  Out1, Out2 : single;
  y0, y1, y2, y3 : single;
begin
  assert(fIsActive);

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

    In1^ := In1^ + Out1 * self.fGain;
    In2^ := In2^ + Out2 * self.fGain;
    inc(In1);
    inc(In2);

    ReadPos := ReadPos + ReadStepSize;
    if ReadPos >= SampleLength then
    begin
      fIsActive := false;
      exit; //==============================================>> exit >>=====================>>
    end;
  end;
end;



procedure TSamplePreviewVoice.ProcessReplacing(OutBuffer1, OutBuffer2: PDouble; const SampleFrames: integer);
var
  c1: Integer;
  ReadIndex : cardinal;
  ReadFrac : single;
  Out1, Out2 : single;
  y0, y1, y2, y3 : single;
begin
  assert(fIsActive);

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

    OutBuffer1^ := Out1 * self.fGain;
    OutBuffer2^ := Out2 * self.fGain;
    inc(OutBuffer1);
    inc(OutBuffer2);

    ReadPos := ReadPos + ReadStepSize;
    if ReadPos >= SampleLength then
    begin
      fIsActive := false;
      exit; //==============================================>> exit >>=====================>>
    end;
  end;
end;

end.
