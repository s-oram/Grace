unit soAudioFilePreviewPlayer;

interface

uses
  eeSampleFloat,
  eeSampleInt,
  eeSimpleGate,
  VamLib.OneShotTimer,
  VamLib.MoreTypes,
  soAudioFilePreviewPlayer.Voice,
  eeAudioFilePreviewPlayerVoice;

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
    fVolume     : single;
    fSampleInfo : TPreviewSampleProperties;
    procedure SetSampleRate(const Value: integer);
    procedure SetVolume(const Value: single);
    function GetSampleInfo: PPreviewSampleProperties;
    procedure SetMaxBlockSize(const Value: integer);
  protected
    CurrentSampleID : cardinal;
    SampleData : TVoiceSampleData;
    TimerID : cardinal;
    NextSampleToLoad : string;
    IsPreviewTriggerRequired : boolean;
    Voice : TSamplePreviewVoice;
    Voices:array[0..kPreviewVoiceCount-1] of TAudioFilePreviewPlayerVoice;
    function FindFreeVoice:TAudioFilePreviewPlayerVoice;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Trigger(aFileName:string; Delay:integer);
    procedure Stop;
    procedure Kill;

    procedure Process(In1,In2:PSingle; Sampleframes:integer); inline;

    property SampleRate:integer read fSampleRate write SetSampleRate;
    property MaxBlockSize : integer write SetMaxBlockSize;
    property Volume    :single  read fVolume     write SetVolume;     //range 0..1

    property SampleInfo :PPreviewSampleProperties read GetSampleInfo;
  end;

implementation

uses
  eeCustomSample, AudioIO, SysUtils;





{ TAudioFilePreviewPlayer }

constructor TAudioFilePreviewPlayer.Create;
var
  c1: Integer;
begin
  Voice := TSamplePreviewVoice.Create;
  SampleData := TVoiceSampleData.Create;
  CurrentSampleID := 0;
  IsPreviewTriggerRequired := false;

  TimerID := 0;

  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    Voices[c1] := TAudioFilePreviewPlayerVoice.Create;
  end;

  Volume     := 1;
  SampleRate := 44100;

  SampleInfo^.IsValid        := false;
  SampleInfo^.ChannelCount   := 0;
  SampleInfo^.SampleFrames   := 0;
  SampleInfo^.SampleRate     := 0;
  SampleInfo^.SourceBitDepth := 0;
end;

destructor TAudioFilePreviewPlayer.Destroy;
var
  c1:integer;
begin
  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    FreeAndNil(Voices[c1]);
  end;

  SampleData.Free;
  Voice.Free;

  inherited;
end;

procedure TAudioFilePreviewPlayer.SetMaxBlockSize(const Value: integer);
begin
  Voice.MaxBlockSize := value;
end;

procedure TAudioFilePreviewPlayer.SetSampleRate(const Value: integer);
var
  c1:integer;
begin
  fSampleRate := Value;

  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    Voices[c1].SampleRate := Value;
  end;

end;

procedure TAudioFilePreviewPlayer.SetVolume(const Value: single);
var
  c1:integer;
begin
  fVolume := Value;
  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    Voices[c1].Volume := Value;
  end;
end;

function TAudioFilePreviewPlayer.FindFreeVoice: TAudioFilePreviewPlayerVoice;
var
  c1:integer;
begin
  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    if Voices[c1].Active = false then
    begin
      result := Voices[c1];
      exit; //======================>>>
    end;
  end;

  //No free voice found so exit.
  result := nil;

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
  aVoice:TAudioFilePreviewPlayerVoice;
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

      // self.NextSampleToLoad := aFileName;
      // Load the next sample here.
      {
      aVoice := FindFreeVoice;
      if aVoice <> nil then
      begin
        aVoice.Trigger(aFileName);
      end;
      }
      NextSampleToLoad := aFileName;
      IsPreviewTriggerRequired := true;

      // TODO:HIGH: Need to protect against overflow actions here.
      inc(CurrentSampleID);
    end;
  end;

  TimerID := SetTimeout(DoTriggerSamplePreview, TimerID, Delay);



  {
  // Load the sample via the sample loader class, which loads the sample in another thread.
  if (Info.IsValid) and (Info.IsSupported) then
  begin
    // non-threaded version
    aVoice := FindFreeVoice;
    if aVoice <> nil then
    begin
      aVoice.Trigger(aFileName);
    end;
  end;
  }
end;

procedure TAudioFilePreviewPlayer.Stop;
var
  c1:integer;
begin
  ClearTimeout(TimerID);
  IsPreviewTriggerRequired := false;
  Voice.Kill;

  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    if Voices[c1].Active then Voices[c1].Stop;
  end;
end;

procedure TAudioFilePreviewPlayer.Kill;
var
  c1:integer;
begin
  ClearTimeout(TimerID);
  IsPreviewTriggerRequired := false;
  Voice.Kill;

  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    Voices[c1].Kill;
  end;
end;

procedure TAudioFilePreviewPlayer.Process(In1, In2: PSingle; Sampleframes: integer);
var
  c1:integer;
begin
  if (IsPreviewTriggerRequired) and (SampleData.IsLoadingSample) then
  begin
    if CurrentSampleID <> SampleData.SampleID then
    begin
      SampleData.LoadSampleData(self.NextSampleToLoad, self.CurrentSampleID);
    end else
    begin
      IsPreviewTriggerRequired := false;
      Voice.Trigger(SampleData);
    end;
  end;

  if Voice.IsActive then
  begin
    Voice.Process(In1, In2, SampleFrames);
  end;




  {
  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    if Voices[c1].Active then
    begin
      Voices[c1].Process(In1, In2, SampleFrames);
    end;
  end;
  }
end;

end.
