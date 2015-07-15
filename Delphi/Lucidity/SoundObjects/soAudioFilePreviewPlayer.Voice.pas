unit soAudioFilePreviewPlayer.Voice;

interface

uses
  VamLib.MoreTypes, eeSampleFloat;

type
  TVoiceSampleData = class
  private
    fSample:TSampleFloat;
    fSampleID : cardinal;
    fIsLoadingSample: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadSampleData(aSampleFileName : string; aSampleID : cardinal);

    property Sample : TSampleFloat read fSample;

    property IsLoadingSample : boolean read fIsLoadingSample;
    property SampleID : cardinal read fSampleID;
  end;

  TSamplePreviewVoice = class
  private
    fSampleRate: integer;
    fMaxBlockSize: integer;
    fGain: single;
    fIsActive: boolean;
    fSampleData : TVoiceSampleData;
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

    procedure Trigger(SampleData : TVoiceSampleData);
    procedure Kill;

    property Gain : single read fGain;
    property SampleRate : integer read fSampleRate write fSampleRate;
    property MaxBlockSize : integer read fMaxBlockSize write fMaxBlockSize;

    property IsActive : boolean read fIsActive;
  end;


implementation

uses
  AudioIO;

{ TVoiceSampleData }

constructor TVoiceSampleData.Create;
begin
  fSampleID := 0;
  fSample := TSampleFloat.Create;
  fIsLoadingSample := false;
end;

destructor TVoiceSampleData.Destroy;
begin
  fSample.Free;
  inherited;
end;

procedure TVoiceSampleData.LoadSampleData(aSampleFileName: string; aSampleID : cardinal);
begin
  fSampleID := aSampleID;
  fIsLoadingSample := true;

  //========================================================================
  // TODO:HIGH: Sample loading should be completed in a thread.
  Sample.LoadFromFile(aSampleFileName);
  fIsLoadingSample := false;
  //========================================================================
end;

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

procedure TSamplePreviewVoice.Trigger(SampleData: TVoiceSampleData);
begin
  //fIsActive := true;

  fSampleData := SampleData;
  if SampleData.Sample.Properties.IsValid then
  begin
    if SampleData.Sample.Properties.ChannelCount = 1 then
    begin
      Ch1 := SampleData.Sample.Ch1Pointer;
      Ch2 := SampleData.Sample.Ch1Pointer;
    end else
    if SampleData.Sample.Properties.ChannelCount = 2 then
    begin
      Ch1 := SampleData.Sample.Ch1Pointer;
      Ch2 := SampleData.Sample.Ch2Pointer;
    end else
    begin
      exit; //========================>> exit >>=======================>>
    end;

    SampleLength := SampleData.Sample.Properties.SampleFrames;
    ReadStepSize := SampleData.Sample.Properties.SampleRate / self.SampleRate;
    ReadPos := 0;
  end;






end;

procedure TSamplePreviewVoice.Process(In1, In2: PSingle; Sampleframes: integer);
var
  c1: Integer;
begin
  assert(fIsActive);

  for c1 := 0 to SampleFrames-1 do
  begin
    //TODO:HIGH read the sample position here and output the sample preview.
  end;
end;



end.
