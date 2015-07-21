unit soAudioFilePreviewPlayer;

interface

uses
  Math,
  eeSampleFloat,
  VamLib.OneShotTimer,
  VamLib.MoreTypes,
  VamDsp.Interpolation,
  soAudioFilePreviewPlayer.Voice;

const
  kPreviewVoiceCount = 4;
type
  PPreviewSampleProperties = ^TPreviewSampleProperties;
  TPreviewSampleProperties = record
    IsValid        :boolean;
    ChannelCount   :integer;
    SampleFrames   :integer;
    SampleRate     :integer;
    SourceBitDepth :integer;
  end;

  TAudioFilePreviewPlayer = class
  private
    fSampleRate : integer;
    fBlockSize : integer;
    fVolume     : single;
    fSampleInfo : TPreviewSampleProperties;
    procedure SetVolume(const Value: single);
    function GetSampleInfo: PPreviewSampleProperties;
  protected
    SampleData : TSampleFloat;
    TimerID : cardinal;
    NextSampleToLoad : string;
    IsPreviewTriggerRequired : boolean;
    Voice : TSamplePreviewVoice;
    IsLoadingSample : boolean;
    procedure DoSampleLoad;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Trigger(aFileName:string; Delay:integer);
    procedure Stop;
    procedure Kill;

    procedure Process(In1,In2:PSingle; Sampleframes:integer); //inline;

    procedure UpdateConfig(const aSampleRate, aBlockSize : integer);

    property SampleRate:integer read fSampleRate;
    property BlockSize : integer read fBlockSize;
    property Volume    :single  read fVolume     write SetVolume;     //range 0..1

    property SampleInfo :PPreviewSampleProperties read GetSampleInfo;
  end;

implementation

uses
  VamLib.Threads,
  eeCustomSample, AudioIO, SysUtils;





{ TAudioFilePreviewPlayer }

constructor TAudioFilePreviewPlayer.Create;
begin
  Voice := TSamplePreviewVoice.Create;
  SampleData := TSampleFloat.Create;
  IsPreviewTriggerRequired := false;
  IsLoadingSample := false;

  TimerID := 0;

  Volume     := 1;
  fSampleRate := 44100;
  fBlockSize := 0;

  SampleInfo^.IsValid        := false;
  SampleInfo^.ChannelCount   := 0;
  SampleInfo^.SampleFrames   := 0;
  SampleInfo^.SampleRate     := 0;
  SampleInfo^.SourceBitDepth := 0;
end;

destructor TAudioFilePreviewPlayer.Destroy;
begin
  SampleData.Free;
  Voice.Free;
  inherited;
end;

procedure TAudioFilePreviewPlayer.UpdateConfig(const aSampleRate, aBlockSize: integer);
begin
  Voice.BlockSize := aBlockSize;
  Voice.SampleRate := aSampleRate;

  fSampleRate := aSampleRate;
  fBlockSize := aBlockSize;
end;

procedure TAudioFilePreviewPlayer.SetVolume(const Value: single);
begin
  fVolume := Value;
  Voice.Gain := Value;
end;

function TAudioFilePreviewPlayer.GetSampleInfo: PPreviewSampleProperties;
begin
  result := @fSampleInfo;
end;

// procedure Trigger(aFileName: string; Delay: integer);
// Delay is in milliseconds.
procedure TAudioFilePreviewPlayer.Trigger(aFileName: string; Delay: integer);
var
  Info:TAudioFileInfo;
  DoTriggerSamplePreview : TProc;
begin
  //Get the file info.
  GetAudioFileInfoEx(aFileName, Info);
  if (Info.IsValid) and (Info.IsSupported) then
  begin
    SampleInfo^.IsValid        := true;
    SampleInfo^.ChannelCount   := Info.Channels;
    SampleInfo^.SampleFrames   := Info.SampleFrames;
    SampleInfo^.SampleRate     := Info.SampleRate;
    SampleInfo^.SourceBitDepth := Info.BitDepth;
  end else
  begin
    SampleInfo^.IsValid        := false;
    SampleInfo^.ChannelCount   := 0;
    SampleInfo^.SampleFrames   := 0;
    SampleInfo^.SampleRate     := 0;
    SampleInfo^.SourceBitDepth := 0;
  end;


  DoTriggerSamplePreview := procedure begin
    if (Info.IsValid) and (Info.IsSupported) then
    begin
      // NOTE: We are in the GUI thread here. Don't do any slow operations,
      // otherwise the GUI will be blocked.

      NextSampleToLoad := aFileName;
      IsPreviewTriggerRequired := true;
    end;
  end;

  TimerID := SetTimeout(DoTriggerSamplePreview, TimerID, Delay);
end;

procedure TAudioFilePreviewPlayer.Stop;
begin
  ClearTimeout(TimerID);
  IsPreviewTriggerRequired := false;
  Voice.Kill;
end;

procedure TAudioFilePreviewPlayer.Kill;
begin
  ClearTimeout(TimerID);
  IsPreviewTriggerRequired := false;
  Voice.Kill;
end;

procedure TAudioFilePreviewPlayer.DoSampleLoad;
begin
  RunTask(procedure begin
    SampleData.LoadFromFile(self.NextSampleToLoad);
  end,
  procedure begin
    if SampleData.Properties.IsValid then Voice.Trigger(SampleData);
    IsLoadingSample := false;
  end);
end;

procedure TAudioFilePreviewPlayer.Process(In1, In2: PSingle; Sampleframes: integer);
begin
  if (IsPreviewTriggerRequired) and (IsLoadingSample = false) then
  begin
    IsPreviewTriggerRequired := false;
    IsLoadingSample := true;
    Voice.Kill;
    DoSampleLoad;
  end;

  if Voice.IsActive then
  begin
    Voice.Process(In1, In2, SampleFrames);
  end;
end;

end.
