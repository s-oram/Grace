unit eeSampleInt;

interface

uses
  MoreTypes, eeCustomSample, eeSampleIntF;

const
  OneOver32767 = 1 / 32767; //One over size(SmallInt)

type
  TSampleInt = class(TCustomSample, ISample)
  private
    fCh1: TArrayOfSmallInt;
    fCh2: TArrayOfSmallInt;
  protected
    function GetSampleMem(ChannelCount, SampleFrames:integer):boolean; override;


  public
    constructor Create; override;
	  destructor Destroy; override;

    function Init(Channels, SampleFrames, SampleRate, SourceBitDepth:integer):boolean; override;
    procedure Clear; override;

    function LoadFromFile(FileName:string):boolean; override;
    function SaveToFile(FileName:string):boolean; override;

    property Ch1:TArrayOfSmallInt read fCh1 write fCh1;
    property Ch2:TArrayOfSmallInt read fCh2 write fCh2;

    property Properties;
  end;

implementation

uses
  AudioIO, SysUtils;

{ TSampleInt }

constructor TSampleInt.Create;
begin
  Clear; //Init's everything.
end;

destructor TSampleInt.Destroy;
begin
  SetLength(fCh1, 0);
  SetLength(fCh2, 0);
  inherited;
end;

function TSampleInt.GetSampleMem(ChannelCount, SampleFrames: integer): boolean;
begin
  assert((ChannelCount = 1) or (ChannelCount = 2), 'Invalid channel count.');
  assert(SampleFrames >= 0);

  try
    if ChannelCount = 1 then
    begin
      SetLength(fCh1, SampleFrames);
      SetLength(fCh2, 0);
      fProperties.Ch1 := @fCh1[0];
      fProperties.Ch2 := @fCh1[0];
    end;

    if ChannelCount = 2 then
    begin
      SetLength(fCh1, SampleFrames);
      SetLength(fCh2, SampleFrames);
      fProperties.Ch1 := @fCh1[0];
      fProperties.Ch2 := @fCh2[0];
    end;

    //If we've made it thus far with no expceptions, assume memory has been initialised correctly.
    result := true;
  except

    //SetLength will raise an EOutOfMemory exception if memory could not be assigned.
    on EOutOfMemory do
    begin
      SetLength(fCh1, 0);
      SetLength(fCh2, 0);
      fProperties.Ch1 := nil;
      fProperties.Ch2 := nil;
      result := false;
      exit; //===================================>>
    end;

    // HACK: This routine has raised access violations, but AFAICT it shouldn't... grrr :/
    // response, so handle anyway.
    on EAccessViolation do
    begin
      fProperties.Ch1 := nil;
      fProperties.Ch2 := nil;
      result := false;
      exit; //===================================>>
    end;
  end;
end;

function TSampleInt.Init(Channels, SampleFrames, SampleRate, SourceBitDepth:integer):boolean;
begin
  if GetSampleMem(Channels, SampleFrames) = false then
  begin
    Clear;
    result := false;
  end else
  begin
    fProperties.IsValid        := true;
    fProperties.ChannelCount   := Channels;
    fProperties.SampleFrames   := SampleFrames;
    fProperties.SampleRate     := SampleRate;
    fProperties.SourceBitDepth := SourceBitDepth;
    result := true;
  end;
end;

procedure TSampleInt.Clear;
begin
  SetLength(fCh1, 0);
  SetLength(fCh2, 0);

  fProperties.IsValid        := false;
  fProperties.SampleFrames   := 0;
  fProperties.ChannelCount   := 0;
  fProperties.SampleRate     := 0;
  fProperties.SourceBitDepth := 0;
  fProperties.Ch1            := nil;
  fProperties.Ch2            := nil;
end;



function TSampleInt.LoadFromFile(FileName: string): boolean;
var
  Info:TAudioFileInfo;
  LoadResult:boolean;
begin
  GetAudioFileInfoEx(FileName, Info);

  if (Info.IsValid = false) or (Info.IsSupported = false) then
  begin
    Clear;
    result := false;
    exit; //=============>>===================>>=================>>
  end;

  if (Info.IsValid) and (Info.IsSupported) then
  begin
    //Before anything, try to get memory for sample data.
    if GetSampleMem(Info.Channels, Info.SampleFrames) = false then
    begin
      Clear;
      result := false;
      exit; //=============>>===================>>=================>>
    end;

    LoadResult := false;

    fProperties.SampleFrames   := Info.SampleFrames;
    fProperties.ChannelCount   := Info.Channels;
    fProperties.SampleRate     := Info.SampleRate;
    fProperties.SourceBitDepth := Info.BitDepth;

    if Info.Channels = 1  then
    begin
      LoadResult := LoadMono_Int(FileName, @fCh1[0]);
      if LoadResult = false then Clear;
    end;

    if Info.Channels = 2  then
    begin
      LoadResult := LoadStereo_Int(FileName, @fCh1[0], @fCh2[0]);
      if LoadResult = false then Clear;
    end;

    if (Info.Channels <> 1) and (Info.Channels <> 2) then
    begin
      Clear;
      LoadResult := false;
    end;

    fProperties.IsValid := LoadResult;
    result := LoadResult;
    exit; //=============>>===================>>=================>>
  end;


  //The function should never make it here.
  result := false;
end;

function TSampleInt.SaveToFile(FileName: string): boolean;
begin
  //TODO: .....
  // will need to have a saveParameters thingy to hold information
  // on targer samplerate/format/bitdepth/channels etc. 
  assert(false, 'TODO');
  result := false;
end;

end.
