unit soAudioFilePreviewPlayer;

interface

uses
  Math,
  eeSampleFloat,
  VamLib.OneShotTimer,
  VamLib.MoreTypes,
  VamDsp.Interpolation,
  VamAudio.RealTimeDecimationFilter,
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
    fOverSampleFactor: integer;
    procedure SetVolume(const Value: single);
    function GetSampleInfo: PPreviewSampleProperties;
  protected
    // TODO:MED At the moment we are only using one voice to preview samples. The preview player clicks
    // if samples are triggered quickly in succession. It would be better to use multiple voices to
    // allow samples a small amount of time to fade out.
    Voice : TSamplePreviewVoice;

    OutBuffer1, OutBuffer2 : array of double;
    DownSampler1, DownSampler2 : TDecimationFilter2x6thOrder;

    SampleData : TSampleFloat;
    TimerID : cardinal;
    NextSampleToLoad : string;
    IsPreviewTriggerRequired : boolean;

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

    property OverSampleFactor : integer read fOverSampleFactor;
    property SampleRate : integer read fSampleRate;
    property BlockSize  : integer read fBlockSize;
    property Volume     : single  read fVolume  write SetVolume;     //range 0..1

    property SampleInfo :PPreviewSampleProperties read GetSampleInfo;
  end;

implementation

uses
  r8bsrcEx,
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

  DownSampler1 := TDecimationFilter2x6thOrder.Create;
  DownSampler2 := TDecimationFilter2x6thOrder.Create;
end;

destructor TAudioFilePreviewPlayer.Destroy;
begin
  SampleData.Free;
  Voice.Free;

  SetLength(OutBuffer1, 0);
  SetLength(OutBuffer2, 0);

  DownSampler1.Free;
  DownSampler2.Free;

  inherited;
end;

procedure TAudioFilePreviewPlayer.UpdateConfig(const aSampleRate, aBlockSize : integer);
begin
  // Oversample factor is locked to 2.
  fOverSampleFactor := 2;

  fSampleRate := aSampleRate;
  fBlockSize := aBlockSize;

  Voice.BlockSize  := aBlockSize  * OverSampleFactor;
  Voice.SampleRate := aSampleRate * OverSampleFactor;

  SetLength(OutBuffer1, aBlockSize * OverSampleFactor);
  SetLength(OutBuffer2, aBlockSize * OverSampleFactor);
end;

procedure TAudioFilePreviewPlayer.SetVolume(const Value: single);
begin
  fVolume := Value;
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
      Voice.FastRelease;
      NextSampleToLoad := aFileName;
      IsPreviewTriggerRequired := true;
    end;
  end;

  TimerID := SetTimeout(DoTriggerSamplePreview, TimerID, Delay);
end;

procedure TAudioFilePreviewPlayer.DoSampleLoad;
begin
  RunTask(procedure begin
    SampleData.LoadFromFile(self.NextSampleToLoad);
  end,
  procedure begin
    DownSampler1.Reset;
    DownSampler2.Reset;
    if SampleData.Properties.IsValid then Voice.Trigger(SampleData);
    IsLoadingSample := false;
  end);
end;

procedure TAudioFilePreviewPlayer.Stop;
begin
  ClearTimeout(TimerID);
  IsPreviewTriggerRequired := false;
  Voice.FastRelease;
end;

procedure TAudioFilePreviewPlayer.Kill;
begin
  ClearTimeout(TimerID);
  IsPreviewTriggerRequired := false;
  Voice.Kill;
end;

procedure TAudioFilePreviewPlayer.Process(In1, In2: PSingle; Sampleframes: integer);
var
  OverSampleFrames : integer;
  c1: Integer;
  OutFrames : integer;
  pOut1 : PDouble;
  pOut2 : PDouble;
begin
  if Voice.IsActive then
  begin
    pOut1 := @OutBuffer1[0];
    pOut2 := @OutBuffer2[0];

    OverSampleFrames :=  SampleFrames * OverSampleFactor;

    Voice.ProcessReplacing(pOut1, pOut2, OverSampleFrames);

    DownSampler1.ProcessDouble(pOut1, pOut1, OverSampleFrames, OutFrames);
    assert(OutFrames = SampleFrames);

    DownSampler2.ProcessDouble(pOut2, pOut2, OverSampleFrames, OutFrames);
    assert(OutFrames = SampleFrames);

    for c1 := 0 to SampleFrames-1 do
    begin
      // NOTE: Clip the preview buffer.
      // One user is finding a bug somwhere which causes a
      // momentary volume spike in MuLab. I'm not sure if
      // this will fix it.
      if OutBuffer1[c1] > 1  then OutBuffer1[c1] := 1;
      if OutBuffer1[c1] < -1 then OutBuffer1[c1] := -1;

      if OutBuffer2[c1] > 1  then OutBuffer2[c1] := 1;
      if OutBuffer2[c1] < -1 then OutBuffer2[c1] := -1;

      In1^ := In1^ + OutBuffer1[c1] * Volume;
      In2^ := In2^ + OutBuffer2[c1] * Volume;

      inc(In1);
      inc(In2);
    end;
  end else
  begin
    if (IsPreviewTriggerRequired) and (IsLoadingSample = false) then
    begin
      IsPreviewTriggerRequired := false;
      IsLoadingSample := true;
      DoSampleLoad;
    end;
  end;
end;

end.
