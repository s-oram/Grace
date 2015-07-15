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
  fIsActive := true;
end;

procedure TSamplePreviewVoice.Process(In1, In2: PSingle; Sampleframes: integer);
begin

end;



end.
