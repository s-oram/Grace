unit soAudioFilePreviewPlayerV2;

interface

uses
  SysUtils,
  Math,
  eeSampleFloat,
  VamLib.Types,
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
    function GetIsActive: boolean;
  protected
    Voice : TSamplePreviewVoice;
    FCurrentSampleFileName : string;
    SampleData : TSampleFloat;
    SampleDataLock : TFixedCriticalSection;

    OutBuffer1, OutBuffer2 : array of double;
    DownSampler1, DownSampler2 : TDecimationFilter2x6thOrder;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure UpdateSampleFileInfo(const FileName : string); overload;
    procedure UpdateSampleFileInfo(const Src : TSampleFloat); overload;
    procedure LoadSample(const FileName : string);
    procedure LoadFromSampleData(const Src : TSampleFloat);
    procedure Trigger;
    procedure Stop;
    procedure Kill;

    procedure Process(In1,In2:PSingle; Sampleframes:integer); //inline;

    procedure UpdateConfig(const aSampleRate, aBlockSize : integer);

    property OverSampleFactor : integer read fOverSampleFactor;
    property SampleRate : integer read fSampleRate;
    property BlockSize  : integer read fBlockSize;
    property Volume     : single  read fVolume  write SetVolume;     //range 0..1

    property SampleInfo :PPreviewSampleProperties read GetSampleInfo;

    property CurrentPreviewSample : string read FCurrentSampleFileName;
    property IsActive : boolean read GetIsActive;
  end;

  EPreviewPlayerException = class(Exception);

implementation

uses
  r8bsrcEx,
  VamLib.Threads,
  eeSampleFloatFunctions,
  eeCustomSample, AudioIO;



{ TAudioFilePreviewPlayer }

constructor TAudioFilePreviewPlayer.Create;
begin
  FCurrentSampleFileName := '';
  SampleData := TSampleFloat.Create;

  SampleDataLock := TFixedCriticalSection.Create;

  Voice := TSamplePreviewVoice.Create;

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
  SampleDataLock.Free;

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

function TAudioFilePreviewPlayer.GetIsActive: boolean;
begin
  result := Voice.IsActive;
end;

function TAudioFilePreviewPlayer.GetSampleInfo: PPreviewSampleProperties;
begin
  result := @fSampleInfo;
end;

procedure TAudioFilePreviewPlayer.UpdateSampleFileInfo(const Src: TSampleFloat);
begin
  SampleInfo^.IsValid        := Src.Properties.IsValid;
  SampleInfo^.ChannelCount   := Src.Properties.ChannelCount;
  SampleInfo^.SampleFrames   := Src.Properties.SampleFrames;
  SampleInfo^.SampleRate     := Src.Properties.SampleRate;
  SampleInfo^.SourceBitDepth := Src.Properties.SourceBitDepth;
end;

procedure TAudioFilePreviewPlayer.UpdateSampleFileInfo(const FileName: string);
var
  Info:TAudioFileInfo;
begin
  //Get the file info.
  GetAudioFileInfoEx(FileName, Info);
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
end;

procedure TAudioFilePreviewPlayer.LoadFromSampleData(const Src: TSampleFloat);
begin
  SampleDataLock.Enter;
  try
    assert(Src.Properties.IsValid);
    if IsActive then raise EPreviewPlayerException.Create('Preview player is currently active.');
    AssignFrom(SampleData, Src);
  finally
    SampleDataLock.Leave;
  end;
end;

procedure TAudioFilePreviewPlayer.LoadSample(const FileName: string);
begin
  SampleDataLock.Enter;
  try
    if IsActive then raise EPreviewPlayerException.Create('Preview player is currently active.');

    if FileName <> FCurrentSampleFileName then
    begin
      FCurrentSampleFileName := FileName;
      SampleData.LoadFromFile(FileName);
    end;
  finally
    SampleDataLock.Leave;
  end;
end;

procedure TAudioFilePreviewPlayer.Trigger;
begin
  SampleDataLock.Enter;
  try
    if SampleData.Properties.IsValid then Voice.Trigger(SampleData);
    DownSampler1.Reset;
    DownSampler2.Reset;
  finally
    SampleDataLock.Leave;
  end;
end;

procedure TAudioFilePreviewPlayer.Stop;
begin
  Voice.FastRelease;
end;

procedure TAudioFilePreviewPlayer.Kill;
begin
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
      In1^ := In1^ + OutBuffer1[c1] * Volume;
      In2^ := In2^ + OutBuffer2[c1] * Volume;

      inc(In1);
      inc(In2);
    end;
  end;
end;

end.
