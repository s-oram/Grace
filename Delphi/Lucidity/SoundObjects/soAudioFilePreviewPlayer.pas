unit soAudioFilePreviewPlayer;

interface

uses
  {$IFNDEF VER230}
  eeSampleLoader,
  {$ENDIF}
  eeSampleFloat,
  eeSampleInt,
  eeSimpleGate,
  VamLib.MoreTypes, eeAudioFilePreviewPlayerVoice;



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
  protected
    Voices:array[0..kPreviewVoiceCount-1] of TAudioFilePreviewPlayerVoice;
    {$IFNDEF VER230}
    SampleLoader:TSampleLoader;
    {$ENDIF}


    function FindFreeVoice:TAudioFilePreviewPlayerVoice;

    {$IFNDEF VER230}
    procedure HandleOnLoadSample(Sender:TObject; var Data:TLoadSampleResult);
    {$ENDIF}
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Trigger(aFileName:string; Delay:integer);
    procedure Stop;
    procedure Kill;

    procedure Process(In1,In2:PSingle; Sampleframes:integer); inline;

    property SampleRate:integer read fSampleRate write SetSampleRate;
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
  {$IFNDEF VER230}
  SampleLoader := TSampleLoader.Create;
  SampleLoader.OnLoadSample := HandleOnLoadSample;
  {$ENDIF}

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
  {$IFNDEF VER230}
  SampleLoader.Free;
  {$ENDIF}

  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    FreeAndNil(Voices[c1]);
  end;

  inherited;
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
  {$IFDEF VER230}aVoice:TAudioFilePreviewPlayerVoice;{$ENDIF}
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

  // Load the sample via the sample loader class, which loads the sample in another thread.
  if (Info.IsValid) and (Info.IsSupported) then
  begin
    {$IFDEF VER230}
      // non-threaded version
      aVoice := FindFreeVoice;
      if aVoice <> nil then
      begin
        aVoice.Trigger(aFileName);
      end;
    {$ELSE}
      // threaded version..
      SampleLoader.LoadSample(aFileName, dfInt, Delay, 0);
    {$ENDIF}
  end;
end;

{$IFNDEF VER230}
procedure TAudioFilePreviewPlayer.HandleOnLoadSample(Sender: TObject; var Data: TLoadSampleResult);
var
  aVoice:TAudioFilePreviewPlayerVoice;
begin
  if Data.IsSuccess then
  begin
    aVoice := FindFreeVoice;
    if aVoice <> nil then
    begin
      aVoice.Trigger(Data.Sample as TSampleInt);

      // IMPORTANT: Data.Sample needs to be set to 'nil' because the object has been passed to
      // the voice class. The voice class will free the sample when it is finished using it.
      Data.Sample := nil;
    end;
  end;
end;
{$ENDIF}




procedure TAudioFilePreviewPlayer.Stop;
var
  c1:integer;
begin
  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    if Voices[c1].Active then Voices[c1].Stop;
  end;
end;

procedure TAudioFilePreviewPlayer.Kill;
var
  c1:integer;
begin
  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    Voices[c1].Kill;
  end;
end;

procedure TAudioFilePreviewPlayer.Process(In1, In2: PSingle; Sampleframes: integer);
var
  c1:integer;
begin
  for c1 := 0 to kPreviewVoiceCount - 1 do
  begin
    if Voices[c1].Active then
    begin
      Voices[c1].Process(In1, In2, SampleFrames);
    end;
  end;
end;

end.
