unit eeSampleFloat;


interface

uses
  VamLib.MoreTypes, eeCustomSample, eeSampleIntF;

type
  PSampleFloat = ^TSampleFloat;
  TSampleFloat = class(TCustomSample, ISample)
  private
    fCh1: TArrayOfSingle;
    fCh2: TArrayOfSingle;
    fLastErrorMessage: string;
  protected
    function GetSampleMem(ChannelCount, SampleFrames:integer):boolean; override;
  public
    constructor Create; override;
	  destructor Destroy; override;

    function Init(Channels, SampleFrames, SampleRate, SourceBitDepth:integer):boolean; override;
    procedure Clear; override;

    function LoadFromFile(FileName:string):boolean; override;
    function SaveToFile(FileName:string):boolean; override;

    property Ch1:TArrayOfSingle read fCh1 write fCh1;
    property Ch2:TArrayOfSingle read fCh2 write fCh2;

    property LastErrorMessage : string read fLastErrorMessage;

    property Properties;
  end;

implementation

uses
  SysUtils, AudioIO;

{ TSampleFloat }

constructor TSampleFloat.Create;
begin
  Clear;
end;

destructor TSampleFloat.Destroy;
begin
  SetLength(fCh1, 0);
  SetLength(fCh2, 0);
  inherited;
end;

function TSampleFloat.GetSampleMem(ChannelCount, SampleFrames: integer): boolean;
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
    on  EOutOfMemory do
    begin
      SetLength(fCh1, 0);
      SetLength(fCh2, 0);
      fProperties.Ch1 := nil;
      fProperties.Ch2 := nil;
      result := false;
      exit; //===================================>>
    end;

    on  ERangeError do
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

function TSampleFloat.Init(Channels, SampleFrames, SampleRate, SourceBitDepth:integer):boolean;
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

procedure TSampleFloat.Clear;
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



function TSampleFloat.LoadFromFile(FileName: string): boolean;
var
  Info:TAudioFileInfo;
  LoadResult:boolean;
begin
  fLastErrorMessage := '';

  try
    GetAudioFileInfoEx(FileName, Info);

    if (Info.IsValid = false) or (Info.IsSupported = false) then
    begin
      Clear;
      fLastErrorMessage := Info.ErrorMessage;
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
        fLastErrorMessage := 'Out of memory.';
        exit; //=============>>===================>>=================>>
      end;

      LoadResult := false;

      fProperties.SampleFrames   := Info.SampleFrames;
      fProperties.ChannelCount   := Info.Channels;
      fProperties.SampleRate     := Info.SampleRate;
      fProperties.SourceBitDepth := Info.BitDepth;

      if Info.Channels = 1  then
      begin
        LoadResult := LoadMono(FileName, @fCh1[0]);
        if LoadResult = false then Clear;
      end;

      if Info.Channels = 2  then
      begin
        LoadResult := LoadStereo(FileName, @fCh1[0], @fCh2[0]);
        if LoadResult = false then Clear;
      end;

      if (Info.Channels <> 1) and (Info.Channels <> 2) then
      begin
        Clear;
        LoadResult := false;
        fLastErrorMessage := 'Unsupported channel count.';
      end;

      fProperties.IsValid := LoadResult;
      result := LoadResult;
      exit; //=============>>===================>>=================>>
    end;
  except
    on E: EAudioIOException do
    begin
      fLastErrorMessage := E.Message;
    end;
    else raise;
  end;


  //The function should never make it here.
  result := false;
end;

function TSampleFloat.SaveToFile(FileName: string): boolean;
var
  SaveInfo : TAudioFileSaveInfo;
begin
   SaveInfo.SrcChannelCount := Properties.ChannelCount;
   SaveInfo.SrcSampleRate   := Properties.SampleRate;
   SaveInfo.SrcSampleFrames := Properties.SampleFrames;
   SaveInfo.SrcDataType     := sdFloat;
   SaveInfo.SrcCh1          := Properties.Ch1;
   SaveInfo.SrcCh2          := Properties.Ch2;

   SaveInfo.DstFileFormat := afWave;
   SaveInfo.DstBitDepth   := 16;
   SaveInfo.DstSampleRate := 44100;

   result := SaveAudioToFile(FileName, SaveInfo);
end;



end.



