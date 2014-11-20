unit AudioIO_Aiff;

interface

uses
  AudioIO, AudioIO_ExtendedX87;

type
  TAiffInfo = record
    IsValid      :boolean;  //a flag to indicate the file is valid. ie has correct chunks and sizes...
    IsSupported  :boolean;
    SampleFrames :integer;
    Channels     :integer;
    Bitdepth     :integer;
    SampleRate   :TExtendedX87; //Must be 10bytes.
    ErrorMessage   : string; // if the file is invalid or not supported, ErrorMessage explains why.
  end;

//----  HighLevel functions   ------------------------------------------------------------------
function GetAiffFileInfo(FileName:string):TAiffInfo;

function LoadAiffFileMono(FileName:string; Left:PSingle):boolean;
function LoadAiffFileStereo(FileName:string; Left,Right:PSingle):boolean;

function LoadAiffFileMono_Int(FileName:string; Left:PSmallInt):boolean;
function LoadAiffFileStereo_Int(FileName:string; Left,Right:PSmallInt):boolean;

implementation

uses
  AudioIO_WinFile, SysUtils, Classes, AudioIO_EndianConversion, AudioIO_AiffLowLevel;

type
  TCommChunk = packed record
    Channels     : smallInt;
    SampleFrames : LongWord;
    BitDepth     : smallInt;
    SampleRate   : TExtendedX87;  //Must be 10bytes.
  end;



function CheckIfAiffFile(AiffFile:TWinFile):boolean;
var
  s:ansistring;
begin
  //result := false;

  //Reset the wave file position to read head info.
  AiffFile.Position := 0;

  SetLength(s,4);
  AiffFile.Read(s[1],4);
  AiffFile.Seek(4,soCurrent);

  if s <> 'FORM' then
  begin
    result := false;
    exit;
  end;

  AiffFile.Read(s[1],4);
  if s <> 'AIFF' then
  begin
    result := false;
    exit;
  end;

  result := true;
end;

function LocateCommChunk(AiffFile:TWinFile):integer;
var
  s:ansistring;
  ChunkSize:LongInt;
begin
  //result := -1;

  AiffFile.Position := 12;


  SetLength(s,4);
  AiffFile.Read(s[1],4);

  while s <> 'COMM' do
  begin
    AiffFile.Read(ChunkSize, SizeOf(ChunkSize));
    ChunkSize := ByteSwap(ChunkSize);
    AiffFile.Seek(ChunkSize,soCurrent);

    AiffFile.Read(s[1],4);
    if AiffFile.Position >= AiffFile.Size-1 then
    begin
      raise Exception.Create('The COMM chunk could not be located.');
      exit;  //End of file reached.
    end;
  end;

  AiffFile.Read(ChunkSize, SizeOf(ChunkSize));

  result := ByteSwap(ChunkSize)
end;

function LocateSsndChunk(AiffFile:TWinFile):integer;
var
  s:ansistring;
  ChunkSize:LongInt;
begin
  // NOTE: I don't think this is the correct way to find the SSND chunk.
  // It might work, but it looks like a hack to me.
  AiffFile.Position := 12;

  SetLength(s,4);
  AiffFile.Read(s[1],4);

  while s <> 'SSND' do
  begin
    AiffFile.Read(ChunkSize, SizeOf(ChunkSize));
    ChunkSize := ByteSwap(ChunkSize);
    AiffFile.Seek(ChunkSize,soCurrent);

    AiffFile.Read(s[1],4);
    if AiffFile.Position >= AiffFile.Size-1 then
    begin
      //End of file reached...
      result := -1;
      exit; //==================>>exit>>===========>>
    end;
  end;

  AiffFile.Read(ChunkSize, SizeOf(ChunkSize));
  result := ByteSwap(ChunkSize);
end;



function GetAiffFileInfo(FileName:string):TAiffInfo;
var
  AiffFile:TWinFile;
  //ChunkSize:LongInt;
  CommChunk:TCommChunk;
  Size:integer;
  ChunkSize:integer;
begin
  result.IsValid     := false;
  result.IsSupported := false;
  result.ErrorMessage := '';

  //Zero all result data.
  FillChar(result,SizeOf(result),0);

  try
    AiffFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result.IsValid     := false;
    result.IsSupported := false;
    result.ErrorMessage := 'Could not open file for reading.';
    raise;
  end;


  try

    if CheckIfAiffFile(AiffFile) = false then
    begin
      result.IsValid := false;
      result.ErrorMessage := 'File appears to be corrupt or not a valid AIF format file.';
      exit; //=======================================================>
    end else
    begin
      //Probably need to do some more checks to ensure the file is valid.
      result.IsValid := true;
    end;

    //Get info from the 'COMM' Chunk.
    ChunkSize := LocateCOMMChunk(AiffFile);
    if ChunkSize < 18 then raise Exception.Create('Aiff Comm Chunk size is invalid.');

    size := SizeOf(CommChunk);
    AiffFile.Read(CommChunk,Size);

    result.SampleFrames := ByteSwap(CommChunk.SampleFrames);
    result.Channels     := ByteSwap(CommChunk.Channels);
    result.Bitdepth     := ByteSwap(CommChunk.BitDepth);

    //NOTE: Something is going wrong with the byte swap operation in cubase.
    // The same aif file loads correctly in EnergyXT 1.4. I don't know why cubase
    // is causing this to go sour.
    result.SampleRate   := ByteSwap_TExtendedX87(CommChunk.SampleRate);

    result.IsSupported := true;
  finally
    AiffFile.Free;
  end;

end;



function LoadAiffFileStereo(FileName:string; Left,Right:PSingle):boolean;
var
  Info:TAiffInfo;
  AiffFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;

  Offset:cardinal;
  BlockSize:cardinal;
begin
  result := false;

  Info := GetAiffFileInfo(FileName);

  if Info.Channels <> 2 then raise Exception.Create('Aiff file isn''t stereo.');

  try
    AiffFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit; //==================>>exit>>===========>>
  end;

  try
    ChunkSize := LocateSsndChunk(AiffFile);
    if ChunkSize = -1 then
    begin
      //SsndChunk couldn't be found.
      result := false;
      exit; //==================>>exit>>===========>>
    end;


    AiffFile.Read(Offset,SizeOf(Offset));
    Offset := ByteSwap(Offset);

    AiffFile.Read(BlockSize,SizeOf(Offset));
    BlockSize := ByteSwap(BlockSize);

    if (Offset <> 0) or (BlockSize <> 0) then
    begin
      raise Exception.Create('File format not supported.');
    end;


    try
      GetMem(SampleData,ChunkSize-8);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      AiffFile.Read(SampleData^,ChunkSize-8);
      //ByteSwap(SampleData,ChunkSize-8);  //Need to swap to little endian format.
      try
        case Info.Bitdepth of
        8:  Unpack8BitPCM_Stereo(SampleData, Left, Right, Info.SampleFrames);
        16: Unpack16BitPCM_Stereo(SampleData, Left, Right, Info.SampleFrames);
        24: Unpack24BitPCM_Stereo(SampleData, Left, Right, Info.SampleFrames);
        32: Unpack32BitPCM_Stereo(SampleData, Left, Right, Info.SampleFrames);
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
      FreeMem(SampleData,ChunkSize-8);;
    end;

  finally
    AiffFile.Free;
  end;

end;


function LoadAiffFileMono(FileName:string; Left:PSingle):boolean;
var
  Info:TAiffInfo;
  AiffFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;

  Offset:cardinal;
  BlockSize:cardinal;
begin
  result := false;

  Info := GetAiffFileInfo(FileName);

  if Info.Channels <> 1 then raise Exception.Create('Aiff file isn''t stereo.');

  try
    AiffFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit;
  end;

  try
    ChunkSize := LocateSsndChunk(AiffFile);

    AiffFile.Read(Offset,SizeOf(Offset));
    Offset := ByteSwap(Offset);

    AiffFile.Read(BlockSize,SizeOf(Offset));
    BlockSize := ByteSwap(BlockSize);

    if (Offset <> 0) or (BlockSize <> 0) then
    begin
      raise Exception.Create('File format not supported.');
    end;

    try
      GetMem(SampleData,ChunkSize-8);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      AiffFile.Read(SampleData^,ChunkSize-8);

      try
        case Info.Bitdepth of
        8:  Unpack8BitPCM_Mono(SampleData, Left, Info.SampleFrames);
        16: Unpack16BitPCM_Mono(SampleData, Left, Info.SampleFrames);
        24: Unpack24BitPCM_Mono(SampleData, Left, Info.SampleFrames);
        32: Unpack32BitPCM_Mono(SampleData, Left, Info.SampleFrames);
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
      FreeMem(SampleData,ChunkSize-8);;
    end;
  finally
    AiffFile.Free;
  end;

end;

function LoadAiffFileMono_Int(FileName:string; Left:PSmallInt):boolean;
var
  Info:TAiffInfo;
  AiffFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;

  Offset:cardinal;
  BlockSize:cardinal;
begin
  result := false;

  Info := GetAiffFileInfo(FileName);

  if Info.Channels <> 1 then raise Exception.Create('Aiff file isn''t stereo.');

  try
    AiffFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit;
  end;

  try
    ChunkSize := LocateSsndChunk(AiffFile);

    AiffFile.Read(Offset,SizeOf(Offset));
    Offset := ByteSwap(Offset);

    AiffFile.Read(BlockSize,SizeOf(Offset));
    BlockSize := ByteSwap(BlockSize);

    if (Offset <> 0) or (BlockSize <> 0) then
    begin
      raise Exception.Create('File format not supported.');
    end;

    try
      GetMem(SampleData,ChunkSize-8);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      AiffFile.Read(SampleData^,ChunkSize-8);

      try
        case Info.Bitdepth of
        8:  Unpack8BitPCM_Mono_Int(SampleData, Left, Info.SampleFrames);
        16: Unpack16BitPCM_Mono_Int(SampleData, Left, Info.SampleFrames);
        24: Unpack24BitPCM_Mono_Int(SampleData, Left, Info.SampleFrames);
        32: Unpack32BitPCM_Mono_Int(SampleData, Left, Info.SampleFrames);
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
      FreeMem(SampleData,ChunkSize-8);;
    end;

  finally
    AiffFile.Free;
  end;

end;

function LoadAiffFileStereo_Int(FileName:string; Left,Right:PSmallInt):boolean;
var
  Info:TAiffInfo;
  AiffFile:TWinFile;
  SampleData:Pointer;
  ChunkSize:integer;

  Offset:cardinal;
  BlockSize:cardinal;
begin
  result := false;

  Info := GetAiffFileInfo(FileName);

  if Info.Channels <> 2 then raise Exception.Create('Aiff file isn''t stereo.');

  try
    AiffFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit;
  end;

  try
    ChunkSize := LocateSsndChunk(AiffFile);

    AiffFile.Read(Offset,SizeOf(Offset));
    Offset := ByteSwap(Offset);

    AiffFile.Read(BlockSize,SizeOf(Offset));
    BlockSize := ByteSwap(BlockSize);

    if (Offset <> 0) or (BlockSize <> 0) then
    begin
      raise Exception.Create('File format not supported.');
    end;


    try
      GetMem(SampleData,ChunkSize-8);
    except
      on EOutOfMemory do
      begin
        result := false;
        exit; //================================================================================>
      end;
    end;

    try
      AiffFile.Read(SampleData^,ChunkSize-8);

      try
        case Info.Bitdepth of
        8:  Unpack8BitPCM_Stereo_Int(SampleData, Left, Right, Info.SampleFrames);
        16: Unpack16BitPCM_Stereo_Int(SampleData, Left, Right, Info.SampleFrames);
        24: Unpack24BitPCM_Stereo_Int(SampleData, Left, Right, Info.SampleFrames);
        32: Unpack32BitPCM_Stereo_Int(SampleData, Left, Right, Info.SampleFrames);
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
      FreeMem(SampleData,ChunkSize-8);;
    end;

  finally
    AiffFile.Free;
  end;

end;

initialization
  assert(sizeOf(TExtendedX87) = 10);



end.
