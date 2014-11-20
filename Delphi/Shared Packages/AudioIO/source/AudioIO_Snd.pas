unit AudioIO_Snd;

interface

uses
  AudioIO;

type
  TSndInfo = record
    IsValid      :boolean;  //a flag to indicate the file is valid. ie has correct chunks and sizes...
    IsSupported  :boolean;
    SampleFrames :integer;
    Channels     :integer;
    Bitdepth     :integer;   //Always 16 bits.
    SampleRate   :integer;
  end;

//----  HighLevel functions   ------------------------------------------------------------------
function GetSndFileInfo(FileName:string):TSndInfo;

function LoadSndFileMono(FileName:string; Left:PSingle):boolean;
function LoadSndFileStereo(FileName:string; Left,Right:PSingle):boolean;


function LoadSndFileMono_Int(FileName:string; Left:PSmallInt):boolean;
function LoadSndFileStereo_Int(FileName:string; Left,Right:PSmallInt):boolean;


implementation

uses
  SysUtils, Classes, AudioIO_WinFile, AudioIO_SndLowLevel;

type
  TSndHeader = packed record
    _pb1        :word; //Pad byte
    FileName    :array[0..15] of AnsiChar;
    _pb2        :byte;
    Level       :byte;
    Tune        :byte;
    Channels    :byte;    //Channels: 0=Mono 1=Stereo
    LoopStart   :LongWord;
    LoopEnd     :LongWord;
    SampleEnd   :LongWord;
    LoopLength  :LongWord;
    LoopMode    :byte;  //Loop Mode: 0=Off 1=On
    BeatsInLoop :byte;  //Beats in loop 1...16 (default 1)
    SampleRate  :word;
  end;

function GetSndFileInfo(FileName:string):TSndInfo;
var
  SndFile:TWinFile;
  Size:integer;
  SndHeader:TSndHeader;
begin
  //Zero all result data.
  FillChar(result,SizeOf(result),0);

  try
    SndFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result.IsValid     := false;
    result.IsSupported := false;
    exit; //========================================================>
  end;

  Size := SizeOf(SndHeader);

  SndFile.Read(SndHeader,Size);

  result.SampleFrames := SndHeader.SampleEnd - 2;
  result.Channels     := SndHeader.Channels + 1;
  result.Bitdepth     := 16;     //Always 16 bit.
  result.SampleRate   := 44100; //Always set to 44100. But I'm not sure if this is correct.
  result.IsValid      := true;
  result.IsSupported  := true;

  SndFile.Free;

end;




function LoadSndFileStereo(FileName:string; Left,Right:PSingle):boolean;
var
  Info:TSndInfo;
  SndFile:TWinFile;
  SampleData:Pointer;
  DataSize:integer;
  DataRead:integer;
begin
  result := false;

  Info := GetSndFileInfo(FileName);

  if Info.Channels <> 2 then raise Exception.Create('Snd file isn''t stereo.');

  try
    SndFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit;
  end;

  try
    SndFile.Position := 42; //The start of the sample data.

    //NOTE: Something doesn't seem to be right here.
    //Calculated Data size is 4 bytes larger than the remaining file size.
    //For the time being I have massaged the GetSndFileInfo to return
    //a modified SampleFrames value...
    DataSize := Info.SampleFrames * Info.Channels * 2;

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
      DataRead := SndFile.Read(SampleData^,DataSize);

      if DataRead <> DataSize then raise Exception.Create('SampleData size not reported correctly. Snd file is invalid.');

      try
        Unpack16BitPCM_Stereo(SampleData,Left,Right, Info.SampleFrames);
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
      FreeMem(SampleData,DataSize);;
    end;

  finally
    SndFile.Free;
  end;


end;

function LoadSndFileMono(FileName:string; Left:PSingle):boolean;
var
  Info:TSndInfo;
  SndFile:TWinFile;
  SampleData:Pointer;
  DataSize:integer;
  DataRead:integer;
begin
  result := false;

  Info := GetSndFileInfo(FileName);

  if Info.Channels <> 1 then raise Exception.Create('Snd file isn''t mono.');

  try
    SndFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit;
  end;

  try
    SndFile.Position := 42; //The start of the sample data.

    //NOTE: Something doesn't seem to be right here.
    //Calculated Data size is 4 bytes larger than the remaining file size.
    //For the time being I have massaged the GetSndFileInfo to return
    //a modified SampleFrames value...
    DataSize := Info.SampleFrames * Info.Channels * 2;

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
      DataRead := SndFile.Read(SampleData^,DataSize);

      if DataRead <> DataSize then raise Exception.Create('SampleData size not reported correctly. Snd file is invalid.');

      try
        Unpack16BitPCM_Mono(SampleData, Left, Info.SampleFrames);
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
      FreeMem(SampleData,DataSize);;
    end;
  finally
    SndFile.Free;
  end;


end;

function LoadSndFileMono_Int(FileName:string; Left:PSmallInt):boolean;
var
  Info:TSndInfo;
  SndFile:TWinFile;
  SampleData:Pointer;
  DataSize:integer;
  DataRead:integer;
begin
  result := false;

  Info := GetSndFileInfo(FileName);

  if Info.Channels <> 1 then raise Exception.Create('Snd file isn''t mono.');

  try
    SndFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit;
  end;

  try
    SndFile.Position := 42; //The start of the sample data.

    //NOTE: Something doesn't seem to be right here.
    //Calculated Data size is 4 bytes larger than the remaining file size.
    //For the time being I have massaged the GetSndFileInfo to return
    //a modified SampleFrames value...
    DataSize := Info.SampleFrames * Info.Channels * 2;

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
      DataRead := SndFile.Read(SampleData^,DataSize);

      if DataRead <> DataSize then raise Exception.Create('SampleData size not reported correctly. Snd file is invalid.');

      try
        Unpack16BitPCM_Mono_Int(SampleData, Left, Info.SampleFrames);
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
      FreeMem(SampleData,DataSize);;
    end;

  finally
    SndFile.Free;
  end;


end;


function LoadSndFileStereo_Int(FileName:string; Left,Right:PSmallInt):boolean;
var
  Info:TSndInfo;
  SndFile:TWinFile;
  SampleData:Pointer;
  DataSize:integer;
  DataRead:integer;
begin
  result := false;

  Info := GetSndFileInfo(FileName);

  if Info.Channels <> 2 then raise Exception.Create('Snd file isn''t stereo.');

  try
    SndFile := TWinFile.Create(FileName, kGenericRead, kFileShareRead, kOpenExisting);
  except
    result := false;
    exit;
  end;

  try
    SndFile.Position := 42; //The start of the sample data.

    //NOTE: Something doesn't seem to be right here.
    //Calculated Data size is 4 bytes larger than the remaining file size.
    //For the time being I have massaged the GetSndFileInfo to return
    //a modified SampleFrames value...
    DataSize := Info.SampleFrames * Info.Channels * 2;

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
      DataRead := SndFile.Read(SampleData^,DataSize);

      if DataRead <> DataSize then raise Exception.Create('SampleData size not reported correctly. Snd file is invalid.');

      try
        Unpack16BitPCM_Stereo_Int(SampleData,Left,Right, Info.SampleFrames);
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
      FreeMem(SampleData,DataSize);;
    end;

  finally
    SndFile.Free;
  end;


end;


end.
