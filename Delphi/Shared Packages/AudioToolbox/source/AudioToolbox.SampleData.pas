unit AudioToolbox.SampleData;

interface

uses
  Types,
  VamLib.MoreTypes;

type
  TChannelConfig = (ccNotAssigned, ccMono, ccStereo);

  // TODO:MED - this definition should be moved to VamLib.MoreTypes.
  PSingleDynArray = ^TSingleDynArray;

  TSampleData = class
  private
    FChA, FChB : TSingleDynArray;
    FSampleFrames: integer;
    FSampleRate: integer;
    FChannelConfig: TChannelConfig;
    function GetData(Channel: integer): PSingle;
    function GetCh(const Index: Integer): PSingleDynArray;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignFrom(const Source : TSampleData);

    procedure Clear;

    procedure LoadFromFile(const FileName : string);
    procedure SaveToFile(const FileName : string);

    procedure GetSampleMem(const aChannelConfig:TChannelConfig; const aSampleFrames : integer);

    property Data[Channel : integer] : PSingle read GetData; // Channel = 0 for left, 1 for right.

    property ChA : PSingleDynArray index 0 read GetCh;
    property ChB : PSingleDynArray index 1 read GetCh;

    property ChannelConfig : TChannelConfig read FChannelConfig;
    property SampleFrames : integer read FSampleFrames;
    property SampleRate   : integer read FSampleRate write FSampleRate;
  end;

implementation

uses
  SysUtils,
  AudioIO;

{ TSampleData }

constructor TSampleData.Create;
begin
  Clear;
end;

destructor TSampleData.Destroy;
begin
  SetLength(FChA, 0);
  SetLength(FChB, 0);
  inherited;
end;

procedure TSampleData.AssignFrom(const Source: TSampleData);
var
  In1, In2, Out1, Out2 : PSingle;
  c1: Integer;
begin
  self.GetSampleMem(Source.ChannelConfig, Source.SampleFrames);
  self.SampleRate := Source.SampleRate;

  if Source.ChannelConfig = ccMono then
  begin
    In1 := Source.Data[0];
    Out1 := Self.Data[0];

    for c1 := 0 to Source.SampleFrames-1 do
    begin
      Out1^ := In1^;
      inc(Out1);
      inc(In1);
    end;
  end;

  if Source.ChannelConfig = ccStereo then
  begin
    In1 := Source.Data[0];
    In2 := Source.Data[1];
    Out1 := Self.Data[0];
    Out2 := Self.Data[1];

    for c1 := 0 to Source.SampleFrames-1 do
    begin
      Out1^ := In1^;
      Out2^ := In2^;
      inc(Out1);
      inc(Out2);
      inc(In1);
      inc(In2);
    end;
  end;
end;

procedure TSampleData.Clear;
begin
  FChannelConfig := ccNotAssigned;
  FSampleRate := 0;
  FSampleFrames := 0;
  SetLength(FChA, 0);
  SetLength(FChB, 0);
end;

function TSampleData.GetCh(const Index: Integer): PSingleDynArray;
begin
  case Index of
    0: result := @FChA;
    1: result := @FChB;
  else
    raise Exception.Create('Unexpected channel index.');
  end;
end;

function TSampleData.GetData(Channel: integer): PSingle;
begin
  assert( (Channel = 0) or (Channel = 1) );
  assert( ChannelConfig <> ccNotAssigned );
  assert( SampleFrames > 0 );

  case ChannelConfig of
    ccNotAssigned: result := nil;
    ccMono:        result := @FChA[0]; // Always return the first channel regardless.
    ccStereo:
    begin
      if Channel = 0
        then result := @FChA[0]
        else result := @FChB[0]
    end
  else
    raise Exception.Create('Unexpected channel config.');
  end;

end;

procedure TSampleData.GetSampleMem(const aChannelConfig : TChannelConfig; const aSampleFrames: integer);
begin
  case aChannelConfig of
    ccNotAssigned:
    begin
      SetLength(FChA, 0);
      SetLength(FChB, 0);
    end;

    ccMono:
    begin
      SetLength(FChA, aSampleFrames);
      SetLength(FChB, 0);
    end;

    ccStereo:
    begin
      SetLength(FChA, aSampleFrames);
      SetLength(FChB, aSampleFrames);
    end
  else
    raise Exception.Create('Channel config not supported.');
  end;

  FChannelConfig := aChannelConfig;
  FSampleFrames  := aSampleFrames;
end;

procedure TSampleData.LoadFromFile(const FileName: string);
var
  Info:TAudioFileInfo;
  LoadPar :  TAudioFileLoadParameters;
begin
  if not IsSupportedAudioFileFormat(FileName, false) then raise Exception.Create('Unsupported audio format.');
  GetAudioFileInfoEx(FileName, Info);

  if (not Info.IsValid) and (not Info.IsSupported) then
  begin
    Clear;
    raise Exception.Create('File not supported.');
  end;


  case Info.Channels of
    1: self.GetSampleMem(ccMono, Info.SampleFrames);
    2: self.GetSampleMem(ccStereo, Info.SampleFrames);
  else
    raise Exception.Create('Channel configuration not supported.');
  end;

  self.FSampleRate := Info.SampleRate;


  LoadPar.FileName := FileName;
  LoadPar.Ch1 := self.GetData(0);
  LoadPar.Ch2 := self.GetData(1);
  LoadPar.ChannelCount := Info.Channels;
  LoadPar.DstDataType := TDataType.sdFloat;

  if not LoadAudioFromFile(LoadPar) then
  begin
    Clear;
    raise Exception.Create('Something went wrong while loading.');
  end;

end;

procedure TSampleData.SaveToFile(const FileName: string);
var
  SaveInfo:TAudioFileSaveInfo;
begin
  SaveInfo.SrcDataType     := TDataType.sdFloat;
  SaveInfo.SrcSampleRate   := self.SampleRate;
  SaveInfo.SrcSampleFrames := self.SampleFrames;

  case self.ChannelConfig of
    ccMono:
    begin
      SaveInfo.SrcChannelCount := 1;
      SaveInfo.SrcCh1          := self.GetData(0);
      SaveInfo.SrcCh2          := nil;
    end;
    ccStereo:
    begin
      SaveInfo.SrcChannelCount := 2;
      SaveInfo.SrcCh1          := self.GetData(0);
      SaveInfo.SrcCh2          := self.GetData(1);
    end
  else
    raise Exception.Create('Unexpected channel configuration value.');
  end;

  SaveInfo.DstSampleRate := SampleRate;
  SaveInfo.DstBitDepth   := 16;
  SaveInfo.DstFileFormat := TAudioFileFormat.afWave;

  SaveAudioToFile(FileName, SaveInfo);
end;

end.
