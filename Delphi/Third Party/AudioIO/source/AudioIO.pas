{
  TODO: When loading a sample, need to have stuff to limit the maximum memory size of a sample.

  TODO: The methods should be extended so they can optionally catch and hide exceptions or allow
  them to bubble up to the using code.
  When catching exceptions, there should be a global 'LastError' and 'GetLastError()' function
  to allow applications to report failure to load errors back to the user.

}

unit AudioIO;

interface

uses
  SysUtils, VamLib.MoreTypes;

const
  // Some memory size constants useful for checking whether a audio file is too
  // large to load into memory.
  kByte = 1;
  kKilobyte = kByte * 1024;
  kMegaByte = kKiloByte * 1024;
  kGigaByte = kMegaByte * 1024;

type
  PSingle = VamLib.MoreTypes.PSingle;

  TAudioFileFormat = (afUnknown, afWave, afAiff, afSnd);

  TAudioFileInfo = record
    IsValid        : boolean; //Is this audio file a valid file? ie, sample rate value is correct etc.
    IsSupported    : boolean; //Can the audio file be loaded? Not all format files are supported.
    SampleRate     : integer;
    Channels       : integer; //TODO: Rename to ChannelCount.
    SampleFrames   : integer;
    BitDepth       : integer;
    FileFormat     : TAudioFileFormat;
    FileFormatEx   : string;  //Additional info about the file format.
    ErrorMessage   : string;  // if the file is invalid or not supported, ErrorMessage explains why.
  end;

  //sdFloat = Single type, range -1..1. (4 Bytes per sample)
  //sdInt   = SmallInt type, range -32768..32767. (2 Bytes per sample)
  TDataType = (sdFloat, sdInt);

  TAudioFileSaveInfo = record
    SrcChannelCount :integer;
    SrcSampleRate   :integer;
    SrcSampleFrames :integer;
    SrcDataType     :TDataType;
    SrcCh1          :Pointer;
    SrcCh2          :Pointer;

    DstFileFormat   :TAudioFileFormat; //ATM only support for afWave.
    DstBitDepth     :integer; //16 or 32
    DstSampleRate   :integer; //Must been same as SrcSampleRate or audio will be resampled.
  end;

  TDstChannelCount = (dcSameAsSource, dcMono, dcStereo);

  // TODO: TAudioFileLoadParameters.ChannelCount is a bit confusing when using as an application developer.
  // Currently it must be set to the file ChannelCount. This information is in the sample file and shouldn't
  // need to be provided by the application developer.
  TAudioFileLoadParameters = record
    FileName     :string;
    Ch1          :Pointer;
    Ch2          :Pointer;   //The second pointer can be nil if loading a mono file.
    ChannelCount :integer;   //must be set to the actual channel count of the file being loaded.
    DstDataType  :TDataType;
    //DstChannelCount : TDstChannelCount; //TODO: Implement handling for this field.
  end;

  EAudioIOException = class(Exception);




//====================================================================================================================
//   Public Methods
//====================================================================================================================
// TODO: Consider using two IsSupported() functions.
// - IsSupportedAudioFile_QuickCheck()
// - IsSupportedAudioFile_DetailedCheck()
// An alternative consideration could be:
// - IsSupportedAudioFileFormat(). Checks file name extension only to see if it
//   is an audio file.
// - IsValidAudioFile(). A detailed check to see if the file can be loaded
//   a couple checks for file corruption. It's more convenient to find out
//   a file is corrupted before attempting to load it.
function IsSupportedAudioFileFormat(const FileName:string; const QuickCheck : boolean = false):boolean; overload;
function IsSupportedAudioFileFormat(const FileName:string; out ErrorMessage:string; const QuickCheck : boolean = false):boolean; overload;

function AudioFileFormatToStr(aFileFormat:TAudioFileFormat):string;
procedure GetAudioFileInfoEx(FileName:String; var Info:TAudioFileInfo);

function LoadAudioFromFile(const LoadParameters:TAudioFileLoadParameters):boolean; overload;
function LoadAudioFromFile(const LoadParameters:TAudioFileLoadParameters; out ErrorMessage:string):boolean; overload;
function SaveAudioToFile(const FileName:string; const SaveInfo:TAudioFileSaveInfo):boolean;

//Memory required for sample data. Result in bytes.
function CalcSampleDataSize(const SampleFrames, ChannelCount : integer; const SampleFormat:TDataType):cardinal;

function ReadLoopPoints(const FileName : string; out LoopStart, LoopEnd : integer):boolean;
//====================================================================================================================







//====================================================================================================================
//   Private Methods    TODO: These methods should be made private. (Maybe move to a different unit?)
//====================================================================================================================
function LoadMono(FileName:string; LeftData:PSingle):boolean;
function LoadStereo(FileName:string; LeftData,RightData:PSingle):boolean;

procedure SaveMono(FileName:string; LeftData:PSingle; SampleRate, SampleFrames, BitDepth:integer);
procedure SaveStereo(FileName:string; LeftData, RightData:PSingle; SampleRate, SampleFrames, BitDepth:integer);

function LoadMono_Int(FileName:string; LeftData:PSmallInt):boolean;
function LoadStereo_Int(FileName:string; LeftData,RightData:PSmallInt):boolean;
//====================================================================================================================



implementation

uses
  AudioIO_Wave, AudioIO_Aiff, AudioIO_Snd,
  AudioIO_WindowedSincResampler,
  AudioIO_Resampler_r8brain;


function IsFileUsingSupportedFileNameExtension(const FileName : string; out ErrorMessage:string):boolean;
var
  Ext : string;
  IsSupportedExtension : boolean;
begin
  //=== Check file name extension ===
  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  IsSupportedExtension := false;
  if Ext = '.wav'  then IsSupportedExtension := true;
  if Ext = '.aif'  then IsSupportedExtension := true;
  if Ext = '.aiff' then IsSupportedExtension := true;
  if Ext = '.snd'  then IsSupportedExtension := true;

  if IsSupportedExtension
    then result := true
    else result := false;
end;

function IsFileLoadable(const FileName : string; out ErrorMessage:string):boolean;
var
  IsError : boolean;
  Info:TAudioFileInfo;
begin
  ErrorMessage := '';
  IsError := false;

  try
    GetAudioFileInfoEx(FileName, Info);
    if not((Info.IsValid) and (Info.IsSupported)) then
    begin
      ErrorMessage := Info.ErrorMessage;
      IsError := true;
    end;
  except
    on E: Exception do
    begin
      ErrorMessage := 'Exception Error ' + E.Message;
      IsError := true;
    end;
  end;

  if IsError
    then result := false
    else result := true;
end;


function IsSupportedAudioFileFormat(const FileName:string; const QuickCheck : boolean = false):boolean; overload;
var
  errMsg : string;
begin
  result := IsSupportedAudioFileFormat(FileName, errMsg, QuickCheck);
end;

function IsSupportedAudioFileFormat(const FileName:string; out ErrorMessage:string; const QuickCheck : boolean = false):boolean;
var
  err : string;
begin
  if FileExists(FileName) = false then
  begin
    ErrorMessage := 'File could not be located';
    result := false;
    exit; //================>>exit>>==================>>
  end;


  // TODO: This method isn't readable enough IMHO.
  if (QuickCheck) then
  begin
    //== QUICK CHECK ==
    if IsFileUsingSupportedFileNameExtension(FileName, err) then
    begin
      ErrorMessage := '';
      result := true;
      exit; //================>>exit>>==================>>
    end else
    begin
      ErrorMessage := err;
      result := false;
      exit; //================>>exit>>==================>>
    end;

  end else
  begin
    //== SLOW CHECK ==
    if IsFileUsingSupportedFileNameExtension(FileName, err) then
    begin
      if IsFileLoadable(FileName, err) then
      begin
        ErrorMessage := '';
        result := true;
        exit; //================>>exit>>==================>>
      end else
      begin
        ErrorMessage := err;
        result := false;
        exit; //================>>exit>>==================>>
      end;
    end else
    begin
      ErrorMessage := err;
      result := false;
      exit; //================>>exit>>==================>>
    end;
  end;


  //== the method should exit before it makes it this far ===
  raise Exception.Create('Something is wrong. Execution should never make it this far.');
end;

function AudioFileFormatToStr(aFileFormat:TAudioFileFormat):string;
begin
  case aFileFormat of
    afUnknown: result := 'Unknown';
    afWave:    result := 'Wave';
    afAiff:    result := 'Aiff';
    afSnd:     result := 'Snd';
  else
    result := 'Error'
  end;
end;

function CalcSampleDataSize(const SampleFrames, ChannelCount : integer; const SampleFormat:TDataType):cardinal;
begin
  case SampleFormat of
    sdFloat: result := SampleFrames * ChannelCount * SizeOf(Single);
    sdInt:   result := SampleFrames * ChannelCount * SizeOf(SmallInt);
  else
    raise Exception.Create('Unknown SampleFormat.');
  end;
end;


procedure GetAudioFileInfoEx(FileName:String; var Info:TAudioFileInfo);
var
  Ext:string;
  WaveInfo:TWaveInfo;
  AiffInfo:TAiffInfo;
  SndInfo:TSndInfo;
begin
  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  //reset some values...
  Info.IsValid      := false;
  Info.IsSupported  := false;
  Info.ErrorMessage := '';

  if FileExists(FileName) = false then
  begin
    Info.ErrorMessage := 'Audio file not found.';
    exit; //===========================================>> exit >>============>>
  end;

  try
    if Ext = '.wav' then
    begin
      Info.IsSupported := true;

      WaveInfo := GetWaveFileInfo(FileName);

      Info.FileFormat       := afWave;
      Info.IsValid          := WaveInfo.IsValid;
      Info.IsSupported      := WaveInfo.IsSupported;
      Info.SampleRate       := WaveInfo.SampleRate;
      Info.Channels         := WaveInfo.Channels;
      Info.SampleFrames     := WaveInfo.SampleFrames;
      Info.BitDepth         := WaveInfo.Bitdepth;
      Info.FileFormatEx     := WaveInfo.FormatString;
      Info.ErrorMessage     := WaveInfo.ErrorMessage;
    end;


    if (Ext = '.aif') or (Ext = '.aiff') then
    begin
      Info.IsSupported := true;

      AiffInfo := GetAiffFileInfo(FileName);

      Info.FileFormat    := afAiff;
      Info.IsValid       := AiffInfo.IsValid;
      Info.IsSupported   := AiffInfo.IsSupported;
      Info.SampleRate    := round(Extended(AiffInfo.SampleRate));
      Info.Channels      := AiffInfo.Channels;
      Info.SampleFrames  := AiffInfo.SampleFrames;
      Info.BitDepth      := AiffInfo.Bitdepth;
      Info.FileFormatEx  := '';
    end;


    if Ext = '.snd' then
    begin
      Info.IsSupported := true;

      SndInfo := GetSndFileInfo(FileName);

      Info.FileFormat    := afSnd;
      Info.IsValid       := SndInfo.IsValid;
      Info.IsSupported   := SndInfo.IsSupported;
      Info.SampleRate    := SndInfo.SampleRate;
      Info.Channels      := SndInfo.Channels;
      Info.SampleFrames  := SndInfo.SampleFrames;
      Info.BitDepth      := SndInfo.Bitdepth;
      Info.FileFormatEx  := '';
    end;


    if (Info.IsValid) and (Info.IsSupported) then
    begin
      //Perform some data safe guard checks.
      if (Info.Channels   <= 0) or (Info.Channels   > 2)      then
      begin
        Info.IsValid := false;
        Info.ErrorMessage := 'Sample contains an unsupported number of channels. (Channels = ' + IntToStr(Info.Channels) + ')';
      end;

      if (Info.SampleRate <= 0) or (Info.SampleRate > 192000) then
      begin
        Info.IsValid := false;
        Info.ErrorMessage := 'Samplerate is unsupported. (SampleRate = ' + IntToStr(Info.SampleRate) + ')';
      end;

      if (Info.BitDepth   <= 0) or (Info.BitDepth   > 64)     then
      begin
        Info.IsValid := false;
        Info.ErrorMessage := 'Bitdepth is unsupported. (Bitdepth = ' + IntToStr(Info.BitDepth) + ')';
      end;

      if (Info.SampleFrames <= 0) then
      begin
        Info.IsValid := false;
        Info.ErrorMessage := 'Audio file has 0 sampleframes.';
      end;
    end;

  except
    on E: EAudioIOException do
    begin
      Info.IsValid      := false;
      Info.ErrorMessage := E.Message;
    end;
    else
      raise;
  end;
end;


function LoadMono(FileName:string; LeftData:PSingle):boolean;
var
  Ext:string;
begin
  result := false;

  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  if Ext = '.aif'  then result := LoadAiffFileMono(FileName, LeftData);
  if Ext = '.aiff' then result := LoadAiffFileMono(FileName, LeftData);
  if Ext = '.snd'  then result := LoadSndFileMono(FileName, LeftData);
  if Ext = '.wav'  then result := LoadWaveFileMono(FileName, LeftData);


end;

function LoadStereo(FileName:string; LeftData,RightData:PSingle):boolean;
var
  Ext:string;
begin
  result := false;

  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  if Ext = '.aif'  then result := LoadAiffFileStereo(FileName, LeftData, RightData);
  if Ext = '.aiff' then result := LoadAiffFileStereo(FileName, LeftData, RightData);
  if Ext = '.snd'  then result := LoadSndFileStereo(FileName, LeftData, RightData);
  if Ext = '.wav'  then result := LoadWaveFileStereo(FileName, LeftData, RightData);


end;

function LoadMono_Int(FileName:string; LeftData:PSmallInt):boolean;
var
  Ext:string;
begin
  result := false;

  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  if Ext = '.aif'  then result := LoadAiffFileMono_Int(FileName, LeftData);
  if Ext = '.aiff' then result := LoadAiffFileMono_Int(FileName, LeftData);
  if Ext = '.snd'  then result := LoadSndFileMono_Int(FileName, LeftData);
  if Ext = '.wav'  then result := LoadWaveFileMono_Int(FileName, LeftData);


end;

function LoadStereo_Int(FileName:string; LeftData,RightData:PSmallInt):boolean;
var
  Ext:string;
begin
  result := false;

  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  if Ext = '.aif'  then result := LoadAiffFileStereo_Int(FileName, LeftData, RightData);
  if Ext = '.aiff' then result := LoadAiffFileStereo_Int(FileName, LeftData, RightData);
  if Ext = '.snd'  then result := LoadSndFileStereo_Int(FileName, LeftData, RightData);
  if Ext = '.wav'  then result := LoadWaveFileStereo_Int(FileName, LeftData, RightData);


end;


procedure SaveMono(FileName:string; LeftData:PSingle; SampleRate, SampleFrames, BitDepth:integer);
var
  Ext:string;
begin
  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  if Ext = '.wav' then SaveWaveFileMono(FileName, LeftData, SampleFrames, SampleRate, BitDepth);

  if (Ext = '.aif') or (Ext = '.aiff') then raise Exception.Create('Aiff File saving not implemented yet.');

  if Ext = '.snd' then raise Exception.Create('SND File saving not implemented yet.');

end;

procedure SaveStereo(FileName:string; LeftData, RightData:PSingle; SampleRate, SampleFrames, BitDepth:integer);
var
  Ext:string;
begin
  Ext := ExtractFileExt(FileName);
  Ext := Lowercase(Ext);

  if Ext = '.wav' then SaveWaveFileStereo(FileName, LeftData, RightData, SampleFrames, SampleRate, BitDepth);

  if (Ext = '.aif') or (Ext = '.aiff') then raise Exception.Create('Aiff File saving not implemented yet.');

  if Ext = '.snd' then raise Exception.Create('SND File saving not implemented yet.');

end;



function SaveAudioToFile(const FileName:string; const SaveInfo:TAudioFileSaveInfo):boolean;
var
  //Resampler : TWindowedSincResampler;
  Resampler : Tr8bResampler;

  DstSampleFrames:integer;
  TempL_Float,TempR_Float: TArrayOfSingle;
  LData_Float, RData_Float:PSingle;
  SaveResult:boolean;
  sb, db   : PSingle;
begin
  SaveResult := false;

  if SaveInfo.SrcDataType <> sdFloat then raise Exception.Create('Source data type is not float.');
  if SaveInfo.SrcSampleRate = 0      then raise Exception.Create('Source samplerate is 0.');
  if SaveInfo.DstSampleRate = 0      then raise Exception.Create('Destination samplerate is 0.');
  if (SaveInfo.SrcChannelCount <> 1) and (SaveInfo.SrcChannelCount <> 2) then raise Exception.Create('Channel count not supported.');
  if SaveInfo.DstFileFormat <> TAudioFileFormat.afWave then raise Exception.Create('Destination format not supported.');

  //Resampler := TWindowedSincResampler.Create;
  Resampler := Tr8bResampler.Create;
  try
    try
      if SaveInfo.SrcSampleRate <> SaveInfo.DstSampleRate then
      begin
        DstSampleFrames := round(SaveInfo.DstSampleRate / SaveInfo.SrcSampleRate * SaveInfo.SrcSampleFrames);

        SetLength(TempL_Float, DstSampleFrames);
        SetLength(TempR_Float, DstSampleFrames);

        if SaveInfo.SrcChannelCount = 1 then
        begin
          sb := SaveInfo.SrcCh1;
          db := @TempL_Float[0];
          Resampler.Resample(sb, db, SaveInfo.SrcSampleFrames, DstSampleFrames);
        end else
        begin
          sb := SaveInfo.SrcCh1;
          db := @TempL_Float[0];
          Resampler.Resample(sb, db, SaveInfo.SrcSampleFrames, DstSampleFrames);

          sb := SaveInfo.SrcCh2;
          db := @TempR_Float[0];
          Resampler.Resample(sb, db, SaveInfo.SrcSampleFrames, DstSampleFrames);
        end;
      end else
      begin
        DstSampleFrames := SaveInfo.SrcSampleFrames;
      end;

      //-------------------------------------------------
      // Save the file....
      //-------------------------------------------------

      // -- 1 channel, float -----
      if (SaveInfo.SrcChannelCount = 1) and (SaveInfo.SrcDataType = sdFloat) then
      begin
        if SaveInfo.SrcSampleRate <> SaveInfo.DstSampleRate then
        begin
          LData_Float := @TempL_Float[0]
        end else
        begin
          LData_Float := SaveInfo.SrcCh1;
        end;

        SaveResult := SaveWaveFileMono(FileName, LData_Float, DstSampleFrames, SaveInfo.DstSampleRate, SaveInfo.DstBitDepth);
      end;

      // -- 2 channel, float -----
      if (SaveInfo.SrcChannelCount = 2) and (SaveInfo.SrcDataType = sdFloat) then
      begin
        if SaveInfo.SrcSampleRate <> SaveInfo.DstSampleRate then
        begin
          LData_Float := @TempL_Float[0];
          RData_Float := @TempR_Float[0];
        end else
        begin
          LData_Float := SaveInfo.SrcCh1;
          RData_Float := SaveInfo.SrcCh2;
        end;

        SaveResult := SaveWaveFileStereo(FileName, LData_Float, RData_Float, DstSampleFrames, SaveInfo.DstSampleRate, SaveInfo.DstBitDepth);
      end;

    except
      on EOutOfMemory do SaveResult := false;
    end;
  finally
    if assigned(ReSampler)
      then Resampler.Free;
    SetLength(TempL_Float, 0);
    SetLength(TempR_Float, 0);
    result := SaveResult;
  end;

end;


function LoadAudioFromFile(const LoadParameters:TAudioFileLoadParameters):boolean; overload;
var
  Ext:string;
  FileFormat:TAudioFileFormat;
begin
  Ext := ExtractFileExt(LoadParameters.FileName);
  Ext := Lowercase(Ext);

  FileFormat := afUnknown;
  if Ext = '.aif'  then FileFormat := afAiff;
  if Ext = '.aiff' then FileFormat := afAiff;
  if Ext = '.snd'  then FileFormat := afSnd;
  if Ext = '.wav'  then FileFormat := afWave;

  if (FileFormat = afUnknown) then
  begin
    result := false;
    exit; //============================>>
  end;

  if (LoadParameters.DstDataType = sdFloat) and (LoadParameters.ChannelCount = 1)  then
  begin
    case FileFormat of
      afWave: result := LoadWaveFileMono(LoadParameters.FileName, LoadParameters.Ch1);
      afAiff: result := LoadAiffFileMono(LoadParameters.FileName, LoadParameters.Ch1);
      afSnd:  result := LoadSndFileMono(LoadParameters.FileName, LoadParameters.Ch1);
    else
      result := false;
    end;
  end else
  if (LoadParameters.DstDataType = sdFloat) and (LoadParameters.ChannelCount = 2) then
  begin
    case FileFormat of
      afWave: result := LoadWaveFileStereo(LoadParameters.FileName, LoadParameters.Ch1, LoadParameters.Ch2);
      afAiff: result := LoadAiffFileStereo(LoadParameters.FileName, LoadParameters.Ch1, LoadParameters.Ch2);
      afSnd:  result := LoadSndFileStereo(LoadParameters.FileName, LoadParameters.Ch1, LoadParameters.Ch2);
    else
      result := false;
    end;
  end else
  if (LoadParameters.DstDataType = sdInt) and (LoadParameters.ChannelCount = 1) then
  begin
    case FileFormat of
      afWave: result := LoadWaveFileMono_Int(LoadParameters.FileName, LoadParameters.Ch1);
      afAiff: result := LoadAiffFileMono_Int(LoadParameters.FileName, LoadParameters.Ch1);
      afSnd:  result := LoadSndFileMono_Int(LoadParameters.FileName, LoadParameters.Ch1);
    else
      result := false;
    end;
  end else
  if (LoadParameters.DstDataType = sdInt) and (LoadParameters.ChannelCount = 2) then
  begin
    case FileFormat of
      afWave: result := LoadWaveFileStereo_Int(LoadParameters.FileName, LoadParameters.Ch1, LoadParameters.Ch2);
      afAiff: result := LoadAiffFileStereo_Int(LoadParameters.FileName, LoadParameters.Ch1, LoadParameters.Ch2);
      afSnd:  result := LoadSndFileStereo_Int(LoadParameters.FileName, LoadParameters.Ch1, LoadParameters.Ch2);
    else
      result := false;
    end;
  end else
  begin
    result := false;
    exit; //============================>>
  end;
end;

function LoadAudioFromFile(const LoadParameters:TAudioFileLoadParameters; out ErrorMessage:string):boolean;
var
  LoadResult : boolean;
begin
  result := false;

  try
    LoadResult := LoadAudioFromFile(LoadParameters);
    result := LoadResult;
  except
    on E: EAudioIOException do
    begin
      ErrorMessage := E.Message;
    end;
    else raise;
  end;

end;

function ReadLoopPoints(const FileName : string; out LoopStart, LoopEnd : integer):boolean;
var
  Info:TAudioFileInfo;
begin
  GetAudioFileInfoEx(FileName, Info);

  if (Info.IsValid) and (Info.IsSupported) then
  begin
    case Info.FileFormat of
      afWave:    result := WaveFile_ReadLoopPoints(FileName, LoopStart, LoopEnd);
      afAiff:    result := false; //TODO:HIGH
      afSnd:     result := false; //TODO:HIGH
      afUnknown: result := false;
    else
      raise Exception.Create('Unhandled file type.');
    end;
  end else
  begin
    result := false;
  end;

  if result = false then
  begin
    LoopStart := -1;
    LoopEnd   := -1;
  end;

end;





end.

