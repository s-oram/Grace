{
  Audio File Format Specifications
  http://www-mmsp.ece.mcgill.ca/documents/AudioFormats/WAVE/WAVE.html

  There are lots of Wave format "compression" codes
  http://www.recordingblogs.com/sa/tabid/88/Default.aspx?topic=Format+chunk+%28of+a+Wave+file%29

  MSDN : Waveformat Extensible Structure
  http://msdn.microsoft.com/en-us/library/windows/desktop/dd757714%28v=vs.85%29.aspx

  Samples of various format wave files.
  http://www-mmsp.ece.mcgill.ca/documents/AudioFormats/WAVE/Samples.html
}

unit AudioIO_Wave;

interface

uses
  AudioIO;

type
  TWaveInfo = record
    IsValid      :boolean;  //a flag to indicate the file is valid. ie has correct chunks and sizes...
    IsSupported  :boolean;
    FormatString :string; //Human readable format info.
    SampleFrames :integer;
    SampleRate   :integer;
    Channels     :integer;
    Bitdepth     :integer;
    BlockAlign   :integer;
    Format       :integer;
    ErrorMessage   : string; // if the file is invalid or not supported, ErrorMessage explains why.

    //Used with WAVE_FORMAT_EXTENSIBLE
    ValidBitsPerSample : integer;
    ChannelMask        : integer;
    SubFormat          : integer;
  end;

//----  HighLevel functions   ------------------------------------------------------------------

function IsWaveFileFormatSupported(FileName:string):boolean;

function GetWaveFileInfo(FileName:string):TWaveInfo;

function SaveWavefileStereo(FileName:string; Left, Right:PSingle; SampleFrames:integer; SampleRate, BitDepth:integer):boolean;
function SaveWavefileMono(FileName:string; Left:PSingle; SampleFrames:integer; SampleRate, BitDepth:integer):boolean;

function LoadWaveFileStereo(FileName:string; Left,Right:PSingle):boolean;
function LoadWaveFileMono(FileName:string; Left:PSingle):boolean;

function LoadWaveFileMono_Int(FileName:string; Left:PSmallInt):boolean;
function LoadWaveFileStereo_Int(FileName:string; Left, Right:PSmallInt):boolean;

implementation

uses
  SysUtils, Classes, Math, AudioIO_WinFile,
  AudioIO_WaveLowLevel;

const
  WAVE_FORMAT_PCM         = $1;
  WAVE_FORMAT_IEEE_FLOAT  = $3;
  WAVE_FORMAT_ALAW        = $6;
  WAVE_FORMAT_MULAW       = $7;
  WAVE_FORMAT_EXTENSIBLE  = $fffe;

  // NOTE: WAVE_FORMAT_OGG_VORBIS
  // I don't know the correct name of this format. While searching for
  // details I found this. WAVE_FORMAT_OGG_VORBIS will do for now.
  // http://music.columbia.edu/pipermail/linux-audio-user/2003-October/007279.html
  // http://www.recordingblogs.com/sa/tabid/88/Default.aspx?topic=Format+chunk+%28of+a+Wave+file%29
  WAVE_FORMAT_OGG_VORBIS  = $674f;


  // There are lots of Wave format "compression" codes
  // http://www.recordingblogs.com/sa/tabid/88/Default.aspx?topic=Format+chunk+%28of+a+Wave+file%29






type
  DWORD = LongWord;

  TChannelPointers = record
    Count : integer;
    Ch1   : PSingle;
    Ch2   : PSingle;
  end;

  TChunkDescriptor = packed record
    ckID   : array[0..3] of AnsiChar;
    ckSize : dword;
  end;


  // The Format chunk specifies the format of the data. There are 3 variants of
  // the Format chunk for sampled data. These differ in the extensions to the
  // basic Formant chunk.

  TFmtChunk = packed record
    FormatTag          : word;
    Channels           : word;
    SamplesPerSec      : dword;
    AverageBytesPerSec : dword;
    BlockAlign         : word;
    BitsPerSample      : word;
  end;

  TFmtChunkEx1 = packed record
    FormatTag          : word;
    Channels           : word;
    SamplesPerSec      : dword;
    AverageBytesPerSec : dword;
    BlockAlign         : word;
    BitsPerSample      : word;
    cbSize             : word; //Size of the extension (0 or 22)
  end;

  TFmtChunkEx2 = packed record
    FormatTag          : word;
    Channels           : word;
    SamplesPerSec      : dword;
    AverageBytesPerSec : dword;
    BlockAlign         : word;
    BitsPerSample      : word;
    cbSize             : word; //Size of the extension (0 or 22)
    ValidBitsPerSample : word; //number of valid bits.
    ChannelMask        : dword; //speaker position mask.
    case integer of
      0: (GUID      : array[0..15] of byte); // GUID, including the data sub format code
      1: (SubFormat : word);
    end;


procedure InterleaveAndScaleData(SampleInfo:TWaveInfo; SourceSampleData:TChannelPointers; DestSampleData:Pointer);
var
  c1:integer;
  ch1,ch2:PSingle;
  pb:PByte;
  sv:single;
  Smp:SmallInt;
  PSmp:PSmallInt;
  OutPSingle:PSingle;
begin
  //---- Single channel, 8 bit --------------------------------------------------
  if (SampleInfo.Channels = 1) and (SampleInfo.Bitdepth = 8) then
  begin
    ch1 := SourceSampleData.Ch1;
    pb := DestSampleData;

    for c1 := 0 to SampleInfo.SampleFrames - 1 do
    begin
      pb^ := round(Ch1^ * ((1 shl (SampleInfo.Bitdepth - 1)) - 1));
      inc(Ch1);
      inc(pb);
    end;
  end;


  //---- Single channel, 16 bit --------------------------------------------------
  if (SampleInfo.Channels = 1) and (SampleInfo.Bitdepth = 16) then
  begin
    ch1  := SourceSampleData.Ch1;
    PSmp := DestSampleData;

    for c1 := 0 to SampleInfo.SampleFrames - 1 do
    begin
      sv := Ch1^;
      if sv > 1 then sv := 1;
      if sv <-1 then sv := -1;
      Smp := floor(sv * 32765);
      pSmp^ := Smp;
      inc(Ch1);
      inc(pSmp);
    end;
  end;

  //---- Single channel, 32 bit --------------------------------------------------
  if (SampleInfo.Channels = 1) and (SampleInfo.Bitdepth = 32) then
  begin
    ch1        := SourceSampleData.Ch1;
    OutPSingle := DestSampleData;
    for c1 := 0 to SampleInfo.SampleFrames - 1 do
    begin
      OutPSingle^ := ch1^;
      inc(Ch1);
      inc(OutPSingle);
    end;
  end;


  //---- 2 channels, 8 bit --------------------------------------------------

  if (SampleInfo.Channels = 2) and (SampleInfo.Bitdepth = 8) then
  begin
    ch1 := SourceSampleData.Ch1;
    ch2 := SourceSampleData.Ch2;
    pb := DestSampleData;

    for c1 := 0 to SampleInfo.SampleFrames - 1 do
    begin
      pb^ := round(Ch1^ * ((1 shl (SampleInfo.Bitdepth - 1)) - 1));
      inc(Ch1);
      inc(pb);

      pb^ := round(Ch2^ * ((1 shl (SampleInfo.Bitdepth - 1)) - 1));
      inc(Ch2);
      inc(pb);
    end;
  end;


  //---- 2 channels, 16 bit --------------------------------------------------

  if (SampleInfo.Channels = 2) and (SampleInfo.Bitdepth = 16) then
  begin
    ch1  := SourceSampleData.Ch1;
    ch2  := SourceSampleData.Ch2;
    PSmp := DestSampleData;

    for c1 := 0 to SampleInfo.SampleFrames - 1 do
    begin
      sv := Ch1^;
      if sv > 1 then sv := 1;
      if sv <-1 then sv := -1;
      Smp := floor(sv * 32765);
      pSmp^ := Smp;
      inc(Ch1);
      inc(pSmp);

      sv := Ch2^;
      if sv > 1 then sv := 1;
      if sv <-1 then sv := -1;
      Smp := floor(sv * 32765);
      pSmp^ := Smp;
      inc(Ch2);
      inc(pSmp);
    end;
  end;

  //---- 2 channels, 32 bit --------------------------------------------------
  if (SampleInfo.Channels = 2) and (SampleInfo.Bitdepth = 32) then
  begin
    ch1        := SourceSampleData.Ch1;
    ch2        := SourceSampleData.Ch2;
    OutPSingle := DestSampleData;
    for c1 := 0 to SampleInfo.SampleFrames - 1 do
    begin
      OutPSingle^ := ch1^;
      inc(Ch1);
      inc(OutPSingle);

      OutPSingle^ := ch2^;
      inc(Ch2);
      inc(OutPSingle);
    end;
  end;
end;



function BasicWaveFileValidation(WaveFile:TWinFile):boolean;
var
  s:ansistring;
begin
  //Reset the wave file position to read head info.
  WaveFile.Position := 0;

  SetLength(s,4);
  WaveFile.Read(s[1],4);
  //WaveFile.Seek(4,soFromCurrent);
  WaveFile.Position := WaveFile.Position + 4;

  if s <> 'RIFF' then
  begin
    result := false;
    exit; //====================>>
  end;

  WaveFile.Read(s[1],4);
  if s <> 'WAVE' then
  begin
    result := false;
    exit; //====================>>
  end;

  result := true;
end;

function LocateFmtChunk(WaveFile:TWinFile):integer;
var
  ChunkDescriptor : TChunkDescriptor;
begin
  //LocateFmtChunk will change the WaveFile.Position value to the
  //start of the FmtChunkData. The FmtChunk 'fmt' label and chunksize
  //will be skipped. The function will return the size of the 'fmt' chunk.


  //Reset the wave file position to read the first chunk label.
  WaveFile.Position := 12;
  WaveFile.Read(ChunkDescriptor, SizeOf(ChunkDescriptor));

  while ChunkDescriptor.ckID <> 'fmt ' do
  begin
    WaveFile.Position := WaveFile.Position + ChunkDescriptor.ckSize;
    if WaveFile.Position >= WaveFile.Size-1 then raise EAudioIOException.Create('The fmt chunk could not be located.');
    WaveFile.Read(ChunkDescriptor, SizeOf(ChunkDescriptor));
  end;

  assert(ChunkDescriptor.ckID = 'fmt ');
  result := ChunkDescriptor.ckSize;
end;

function LocateDataChunk(WaveFile:TWinFile):integer;
var
  ReadIndex : integer;
  ChunkDescriptor : TChunkDescriptor;
begin
  // TODO: It would be better to write a generic LocateChunk() method that could be used to find any chunk.



  //Reset the wave file position to read the first chunk label.
  WaveFile.Position := 12;
  WaveFile.Read(ChunkDescriptor, SizeOf(ChunkDescriptor));

  while (ChunkDescriptor.ckID <> 'data') and (WaveFile.Position < WaveFile.Size-1) do
  begin
    WaveFile.Position := WaveFile.Position + ChunkDescriptor.ckSize;
    WaveFile.Read(ChunkDescriptor, SizeOf(ChunkDescriptor));
  end;

  if ChunkDescriptor.ckID = 'data' then
  begin
    // Chunk found.
    result := ChunkDescriptor.ckSize;
    exit; //=============>>exit>>===========>>
  end;


  //do a slow and detailed check..
  ReadIndex := 12;
  WaveFile.Position := ReadIndex;
  WaveFile.Read(ChunkDescriptor, SizeOf(ChunkDescriptor));
  while (ChunkDescriptor.ckID <> 'data') and (WaveFile.Position < WaveFile.Size-1) do
  begin
    inc(ReadIndex);
    WaveFile.Position := ReadIndex;
    WaveFile.Read(ChunkDescriptor, SizeOf(ChunkDescriptor));
  end;

  if ChunkDescriptor.ckID = 'data' then
  begin
    // Chunk found.
    result := ChunkDescriptor.ckSize;
    exit; //=============>>exit>>===========>>
  end;

  // If execution makes it this far, the data chunk has not been found...
  raise EAudioIOException.Create('The data chunk could not be located.');


  // NOTE: The detailed check above searches through the entire file looking
  // for the data chunk byte by byte. It doesn't step through the file using
  // the riff chunk offsets. As such this is a forgiving implementation that
  // could allow files which should be labelled as corrupt to propagate.
end;



function GetWaveFileInfo(FileName:string):TWaveInfo;
var
  WaveFile:TWinFile;
  ChunkSize:DWORD;
  FmtChunk    : TFmtChunk;
  FmtChunkEx2 : TFmtChunkEx2;
begin
  //Zero all result data.
  FillChar(result,SizeOf(result),0);
  result.ErrorMessage := '';

  try
    WaveFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result.IsValid     := false;
    result.IsSupported := false;
    result.ErrorMessage := 'could not open wave file.';
    exit; //================================================================>
  end;

  try
    if BasicWaveFileValidation(WaveFile) = false then
    begin
      result.IsValid := false;
      result.ErrorMessage := 'File appears to be corrupt or not a wave format file.';
      exit; //=======================================================>
    end else
    begin
      //Probably need to do some more checks to ensure the file is valid.
      result.IsValid := true;
    end;

    //Get info from the 'fmt' Chunk.
    LocateFmtChunk(WaveFile);
    WaveFile.Read(FmtChunk,SizeOf(FmtChunk));

    result.Format     := FmtChunk.FormatTag;
    result.Channels   := FmtChunk.Channels;
    result.SampleRate := FmtChunk.SamplesPerSec;
    result.BlockAlign := FmtChunk.BlockAlign;
    result.Bitdepth   := FmtChunk.BitsPerSample;
    result.ValidBitsPerSample := 0;
    result.ChannelMask        := 0;
    result.SubFormat          := 0;


    case FmtChunk.FormatTag of
      WAVE_FORMAT_PCM:
      begin
        result.FormatString := 'Wave PCM';
        result.IsSupported  := true;

        ChunkSize           := LocateDataChunk(WaveFile);
        result.SampleFrames := ChunkSize div FmtChunk.BlockAlign;
      end;

      WAVE_FORMAT_IEEE_FLOAT:
      begin
        result.FormatString := 'Wave IEEE Float';
        result.IsSupported  := true;

        ChunkSize           := LocateDataChunk(WaveFile);
        result.SampleFrames := ChunkSize div FmtChunk.BlockAlign;
      end;

      WAVE_FORMAT_ALAW:
      begin
        result.FormatString := 'Wave A-Law';
        result.IsSupported  := false;
        result.ErrorMessage := 'Wav file is using A-Law.'
      end;

      WAVE_FORMAT_MULAW:
      begin
        result.FormatString := 'Wave Mu-Law';
        result.IsSupported  := false;
        result.ErrorMessage := 'Wav file is using Mu-Law.'
      end;

      WAVE_FORMAT_EXTENSIBLE:
      begin
        result.FormatString := 'Wave Extensible : Unknown';
        result.IsSupported  := false;
        result.ErrorMessage := 'Wav file is of an unknow Wave Extensible type.';

        LocateFmtChunk(WaveFile);
        WaveFile.Read(FmtChunkEx2,SizeOf(FmtChunkEx2));

        if (FmtChunkEx2.SubFormat = WAVE_FORMAT_PCM) then
        begin
          ChunkSize           := LocateDataChunk(WaveFile);

          result.FormatString       := 'Wave Extensible : PCM';
          result.SampleFrames       := ChunkSize div FmtChunk.BlockAlign;
          result.ValidBitsPerSample := FmtChunkEx2.ValidBitsPerSample;
          result.SubFormat          := WAVE_FORMAT_PCM;
          result.IsSupported        := true;
          result.ErrorMessage := '';
        end;

        if (FmtChunkEx2.SubFormat = WAVE_FORMAT_IEEE_FLOAT) then
        begin
          ChunkSize           := LocateDataChunk(WaveFile);

          result.FormatString := 'Wave Extensible : IEEE Float';
          result.SampleFrames       := ChunkSize div FmtChunk.BlockAlign;
          result.ValidBitsPerSample := FmtChunkEx2.ValidBitsPerSample;
          result.SubFormat          := WAVE_FORMAT_IEEE_FLOAT;
          result.IsSupported  := true;
          result.ErrorMessage := '';
        end;
      end;

      WAVE_FORMAT_OGG_VORBIS:
      begin
        result.FormatString := 'Wave Ogg Vorbis';
        result.IsSupported  := false;
        result.ErrorMessage := 'Wav file is using Ogg Vorbis compression.'
      end;

    end;

  finally
    WaveFile.Free;
  end;
end;


function WriteWaveFile(FileName:string; SampleInfo:TWaveInfo; SampleData:Pointer; DataSize:integer):boolean;
var
  WaveFile:TWinFile;
  s:ansistring;
  //ChunkSize:DWORD;
  ChunkSize:integer;
  FmtChunk:TFmtChunk;
  BytesWritten:integer;
  FormatTag:integer;
  ChunkDescriptor : TChunkDescriptor;

begin
  //assume the wave file write fails.
  result := false;

  try
    WaveFile := TWinFile.Create(FileName, kGenericWrite, kNotShared, kCreateAlways);
  except
    //Wave file couldn't be opened/created
    raise;
    result := false;
    exit; //========================================================>
  end;

  try
    //Write the wave file header.
    s := 'RIFF    WAVE';
    BytesWritten := WaveFile.Write(s[1],12);
    if BytesWritten <> 12 then raise Exception.Create('Error writing data to file.');

    //Copy some data into the Fmt chunk record.
    if SampleInfo.Bitdepth = 32
      then FormatTag := WAVE_FORMAT_IEEE_FLOAT
      else FormatTag := WAVE_FORMAT_PCM;

    FmtChunk.FormatTag          := FormatTag;
    FmtChunk.Channels           := SampleInfo.Channels;
    FmtChunk.BlockAlign         := SampleInfo.BlockAlign;
    FmtChunk.BitsPerSample      := SampleInfo.Bitdepth;
    FmtChunk.SamplesPerSec      := SampleInfo.SampleRate;
    FmtChunk.AverageBytesPerSec := SampleInfo.SampleRate * SampleInfo.BlockAlign;

    //Write the 'fmt' chunk.
    ChunkDescriptor.ckID := 'fmt ';
    ChunkDescriptor.ckSize := SizeOf(FmtChunk);
    if (ChunkDescriptor.ckSize <> 16) and (ChunkDescriptor.ckSize <> 18) and (ChunkDescriptor.ckSize <> 40) then
    begin
      //Something has gone horribly wrong. The Format chunck is only allowed to be 16, 18 or 40 bytes.
      raise EAudioIOException.Create('ERROR: Unexpected format chunk size (error 462). Please contact support.');
    end;

    BytesWritten := WaveFile.Write(ChunkDescriptor, SizeOf(ChunkDescriptor));
    if BytesWritten <> SizeOf(ChunkDescriptor) then raise EAudioIOException.Create('Error writing data to file.');

    ChunkSize := SizeOf(FmtChunk);
    BytesWritten := WaveFile.Write(FmtChunk,ChunkSize);
    if BytesWritten <> ChunkSize then raise EAudioIOException.Create('Error writing data to file.');


    //Write the 'data' chunk
    ChunkDescriptor.ckID := 'data';
    ChunkDescriptor.ckSize := DataSize;

    BytesWritten := WaveFile.Write(ChunkDescriptor, SizeOf(ChunkDescriptor));
    if BytesWritten <> SizeOf(ChunkDescriptor) then raise EAudioIOException.Create('Error writing data to file.');

    BytesWritten := WaveFile.Write(SampleData^,DataSize);
    if BytesWritten <> DataSize then raise EAudioIOException.Create('Error writing data to file.');
  finally
    WaveFile.Free;
  end;


end;



function SaveWavefileStereo(FileName:string; Left, Right:PSingle; SampleFrames:integer; SampleRate, BitDepth:integer):boolean;
var
  SampleInfo:TWaveInfo;
  SampleData:Pointer;
  Channels:integer;
  ChannelPointers:TChannelPointers;
  DataSize:integer;
begin
  if (BitDepth <> 8) and (BitDepth <> 16) and (BitDepth <> 32)
    then raise EAudioIOException.Create('Bitdepth not supported.');

  if SampleFrames = 0
    then raise EAudioIOException.Create('SampleFrames is zero.');

   if SampleRate = 0
     then raise EAudioIOException.Create('SampleRate is zero.');

  //----------------------------------------------------------------------------
  //  Get basic sample info.
  //----------------------------------------------------------------------------
  Channels := 2;  //looks superfluous but just makes the Sample info code below a little neater.

  SampleInfo.Channels     := Channels;
  SampleInfo.SampleRate   := SampleRate;
  SampleInfo.SampleFrames := SampleFrames;
  SampleInfo.Bitdepth     := Bitdepth;
  SampleInfo.BlockAlign   := Channels * (BitDepth div 8);

  //----------------------------------------------------------------------------------------------
  // Need to scale from the source 32 bit float sample data to the target PCM (integer) bit depth.
  // Multichannel audio also needs to be interleaved.
  //-----------------------------------------------------------------------------------------------
  DataSize := SampleInfo.BlockAlign * SampleInfo.SampleFrames;

  try
    GetMem(SampleData,DataSize);
  except
    on EOutOfMemory do
    begin
      result := false;
      exit; //================================================================================>
    end;
  end;

  try
    ChannelPointers.Ch1 := Left;
    ChannelPointers.Ch2 := Right;
    InterleaveAndScaleData(SampleInfo,ChannelPointers,SampleData);

    //  SampleInfo and SampleData now need to be written to the dest wavefile.
    WriteWaveFile(FileName, SampleInfo,SampleData,DataSize);

    //If no exceptions have been raise to this point, assume everything worked.
    result := true;

  finally
    //Something went wrong, so free the temp buffer and exit.
    FreeMem(SampleData,DataSize);
  end;


end;

function SaveWavefileMono(FileName:string; Left:PSingle; SampleFrames:integer; SampleRate, BitDepth:integer):boolean;
var
  SampleInfo:TWaveInfo;
  SampleData:Pointer;
  Channels:integer;
  ChannelPointers:TChannelPointers;
  DataSize:integer;
begin
  if (BitDepth <> 8) and (BitDepth <> 16) and (BitDepth <> 32)
    then raise EAudioIOException.Create('Bitdepth not supported.');

  if SampleFrames = 0
    then raise EAudioIOException.Create('SampleFrames is zero.');

   if SampleRate = 0
     then raise EAudioIOException.Create('SampleRate is zero.');


  //----------------------------------------------------------------------------
  //  Get basic sample info.
  //----------------------------------------------------------------------------
  Channels := 1;  //looks superfluous but just makes the Sample info code below a little neater.

  SampleInfo.Channels     := Channels;
  SampleInfo.SampleRate   := SampleRate;
  SampleInfo.SampleFrames := SampleFrames;
  SampleInfo.Bitdepth     := Bitdepth;
  SampleInfo.BlockAlign   := Channels * (BitDepth div 8);

  //----------------------------------------------------------------------------------------------
  // Need to scale from the source 32 bit float sample data to the target PCM (integer) bit depth.
  // Multichannel audio also needs to be interleaved.
  //-----------------------------------------------------------------------------------------------
  DataSize := SampleInfo.BlockAlign * SampleInfo.SampleFrames;

  try
    GetMem(SampleData,DataSize);
  except
    on EOutOfMemory do
    begin
      result := false;
      exit; //================================================================================>
    end;
  end;

  try
    ChannelPointers.Ch1 := Left;
    InterleaveAndScaleData(SampleInfo,ChannelPointers,SampleData);

    //  SampleInfo and SampleData now need to be written to the dest wavefile.
    WriteWaveFile(FileName, SampleInfo,SampleData,DataSize);

    //If no exceptions have been raise to this point, assume everything worked.
    result := true;

  finally
    //Something went wrong, so free the temp buffer and exit.
    FreeMem(SampleData,DataSize);
  end;

end;



function LoadWaveFileStereo(FileName:string; Left,Right:PSingle):boolean;
var
  Info:TWaveInfo;
  WaveFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;
  SampleFrames:integer;
begin
  result := false;

  Info := GetWaveFileInfo(FileName);

  if Info.Channels <> 2 then raise EAudioIOException.Create('Wavefile isn''t stereo.');

  try
    WaveFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    exit; //================================================================>
  end;

  try
    ChunkSize := LocateDataChunk(WaveFile);

    try
      GetMem(SampleData,ChunkSize);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      WaveFile.Read(SampleData^,ChunkSize);

      SampleFrames := Info.SampleFrames;

      try
        if (Info.Format = WAVE_FORMAT_PCM) or ((Info.Format = WAVE_FORMAT_EXTENSIBLE) and (Info.SubFormat = WAVE_FORMAT_PCM)) then
        begin
          case Info.Bitdepth of
          8:  Unpack8BitPCM_Stereo(SampleData, Left, Right, SampleFrames);
          //12: ; //TODO:
          16: Unpack16BitPCM_Stereo(SampleData, Left, Right, SampleFrames);
          24: Unpack24BitPCM_Stereo(SampleData, Left, Right, SampleFrames);
          32: Unpack32BitPCM_Stereo(SampleData, Left, Right, SampleFrames);
          else
            raise EAudioIOException.Create('ERROR: Bitdepth not supported.');
          end;
        end else
        if (Info.Format = WAVE_FORMAT_IEEE_FLOAT) or (((Info.Format = WAVE_FORMAT_EXTENSIBLE) and (Info.SubFormat = WAVE_FORMAT_IEEE_FLOAT))) then
        begin
          case Info.Bitdepth of
          32: Unpack32BitIEEEFloat_Stereo(SampleData, Left, Right, SampleFrames);
          //64: ; //TODO:
          else
            raise EAudioIOException.Create('ERROR: Bitdepth not supported.');
          end;
        end else
        begin
          // NOTE: Execution should never reach this point. If the file can't be loaded, the wave file loading
          // code should have figured this out earlier.
          raise EAudioIOException.Create('ERROR: Wavefile can not be loaded (error 760). Please contact support.');
        end;
      except
        // HACK: Sometimes the Unpack methods raise an access violation. I don't quite understand why.
        // The memory for the data has been acquired, so AFAICT it should be fine. :/
        on EAccessViolation do
        begin
          result := false;
          exit; //================================================================================>
        end;
      end;

      //Made it so far with no exceptions, assume file loaded successfully.
      result := true;

    finally
      FreeMem(SampleData,ChunkSize);;
    end;

  finally
    WaveFile.Free;
  end;

end;

function LoadWaveFileMono(FileName:string; Left:PSingle):boolean;
var
  Info:TWaveInfo;
  WaveFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;
  SampleFrames:integer;
begin
  result := false;

  Info := GetWaveFileInfo(FileName);

  if Info.Channels <> 1 then raise EAudioIOException.Create('Wavefile isn''t stereo.');

  try
    WaveFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit; //================================================================================>
  end;

  try
    ChunkSize := LocateDataChunk(WaveFile);

    try
      GetMem(SampleData,ChunkSize);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      WaveFile.Read(SampleData^,ChunkSize);

      SampleFrames := Info.SampleFrames;
      try
        if (Info.Format = WAVE_FORMAT_PCM) or ((Info.Format = WAVE_FORMAT_EXTENSIBLE) and (Info.SubFormat = WAVE_FORMAT_PCM)) then
        begin
          case Info.Bitdepth of
          8:  Unpack8BitPCM_Mono(SampleData, Left, SampleFrames);
          16: Unpack16BitPCM_Mono(SampleData, Left, SampleFrames);
          24: Unpack24BitPCM_Mono(SampleData, Left, SampleFrames);
          32: Unpack32BitPCM_Mono(SampleData, Left, SampleFrames);
          else
            raise EAudioIOException.Create('ERROR: Bitdepth not supported.');
          end;
        end else
        if (Info.Format = WAVE_FORMAT_IEEE_FLOAT) or (((Info.Format = WAVE_FORMAT_EXTENSIBLE) and (Info.SubFormat = WAVE_FORMAT_IEEE_FLOAT))) then
        begin
          case Info.Bitdepth of
          32: Unpack32BitIEEEFloat_Mono(SampleData, Left, SampleFrames);
          else
            raise EAudioIOException.Create('ERROR: Bitdepth not supported.');
          end;
        end else
        begin
          // NOTE: Execution should never reach this point. If the file can't be loaded, the wave file loading
          // code should have figured this out earlier.
          raise EAudioIOException.Create('ERROR: Wavefile can not be loaded (error 760). Please contact support.');
        end;
      except
        // HACK: Sometimes the Unpack methods raise an access violation. I don't quite understand why.
        // The memory for the data has been acquired, so AFAICT it should be fine. :/
        on EAccessViolation do
        begin
          result := false;
          exit; //================================================================================>
        end;
      end;

      //Made it so far with no exceptions, assume file loaded successfully.
      result := true;
    finally
      FreeMem(SampleData, ChunkSize);;
    end;
  finally
    WaveFile.Free;
  end;

end;

function LoadWaveFileMono_Int(FileName:string; Left:PSmallInt):boolean;
var
  Info:TWaveInfo;
  WaveFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;
  SampleFrames:integer;
begin
  Info := GetWaveFileInfo(FileName);

  if Info.Channels <> 1 then raise EAudioIOException.Create('Wavefile isn''t stereo.');

  try
    WaveFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit; //================================================================================>
  end;

  try
    ChunkSize := LocateDataChunk(WaveFile);


    try
      GetMem(SampleData,ChunkSize);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      WaveFile.Read(SampleData^,ChunkSize);

      SampleFrames := Info.SampleFrames;
      try
        if Info.Format = WAVE_FORMAT_PCM then
        begin
          case Info.Bitdepth of
          8:  Unpack8BitPCM_Mono_Int(SampleData, Left, SampleFrames);
          16: Unpack16BitPCM_Mono_Int(SampleData, Left, SampleFrames);
          24: Unpack24BitPCM_Mono_Int(SampleData, Left, SampleFrames);
          32: Unpack32BitPCM_Mono_Int(SampleData, Left, SampleFrames);
          end;
        end;

        if Info.Format = WAVE_FORMAT_IEEE_FLOAT then
        begin
          case Info.Bitdepth of
          32: Unpack32BitIEEEFloat_Mono_Int(SampleData, Left, SampleFrames);
          end;
        end;
      except
        // HACK: Sometimes the Unpack methods raise an access violation. I don't quite understand why.
        // The memory for the data has been acquired, so AFAICT it should be fine. :/
        on EAccessViolation do
        begin
          result := false;
          exit; //================================================================================>
        end;
      end;

      //Made it so far with no exceptions, assume file loaded successfully.
      result := true;

    finally
      FreeMem(SampleData,ChunkSize);;
    end;

  finally
    WaveFile.Free;
  end;
  
end;

function LoadWaveFileStereo_Int(FileName:string; Left, Right:PSmallInt):boolean;
var
  Info:TWaveInfo;
  WaveFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;
  SampleFrames:integer;
begin
  Info := GetWaveFileInfo(FileName);

  if Info.Channels <> 2 then raise EAudioIOException.Create('Wavefile isn''t stereo.');

  try
    WaveFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit; //================================================================================>
  end;

  try
    ChunkSize := LocateDataChunk(WaveFile);

    try
      GetMem(SampleData,ChunkSize);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      WaveFile.Read(SampleData^,ChunkSize);

      SampleFrames := Info.SampleFrames;

      try
        if Info.Format = WAVE_FORMAT_PCM then
        begin
          case Info.Bitdepth of
          8:  Unpack8BitPCM_Stereo_Int(SampleData, Left, Right, SampleFrames);
          16: Unpack16BitPCM_Stereo_Int(SampleData, Left, Right, SampleFrames);
          24: Unpack24BitPCM_Stereo_Int(SampleData, Left, Right, SampleFrames);
          32: Unpack32BitPCM_Stereo_Int(SampleData, Left, Right, SampleFrames);
          end;
        end;

        if Info.Format = WAVE_FORMAT_IEEE_FLOAT then
        begin
          case Info.Bitdepth of
          32: Unpack32BitIEEEFloat_Stereo_Int(SampleData, Left, Right, SampleFrames);
          end;
        end;
      except
        // HACK: Sometimes the Unpack methods raise an access violation. I don't quite understand why.
        // The memory for the data has been acquired, so AFAICT it should be fine. :/ 
        on EAccessViolation do
        begin
          result := false;
          exit; //================================================================================>
        end;
      end;

      //Made it so far with no exceptions, assume file loaded successfully.
      result := true;

    finally
      FreeMem(SampleData,ChunkSize);;
    end;
  finally
    WaveFile.Free;
  end;

end;

function IsWaveFileFormatSupported(FileName:string):boolean;
var
  Info:TWaveInfo;
begin
  Info := GetWaveFileInfo(FileName);

  if (Info.Format = WAVE_FORMAT_PCM) then
  begin
    result := true;
    exit; //============================================>
  end;

  if (Info.Format = WAVE_FORMAT_IEEE_FLOAT) then
  begin
    result := true;
    exit; //============================================>
  end;

  result := false;

end;




end.
