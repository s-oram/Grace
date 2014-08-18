unit AudioIO_Mp3;

interface

uses
  AudioIO;

type
  TMp3FileInfo = record
    IsValid        : boolean; //Is this audio file a valid file? ie, sample rate value is correct etc.
    IsSupported    : boolean; //Can the audio file be loaded? Not all format files are supported.
    SampleRate     : integer;
    Channels       : integer;
    SampleFrames   : integer;
    //BitDepth       : integer;
    FileFormatEx   : string;  //Additional info about the file format.
    ErrorMessage   : string;  // if the file is invalid or not supported, ErrorMessage explains why.
  end;

function GetMp3FileInfo(FileName:string):TMp3FileInfo;

function LoadMp3FileStereo(FileName:string; Left,Right:PSingle):boolean;
function LoadMp3FileMono(FileName:string; Left:PSingle):boolean;

implementation

uses
  ACS_MemFloat,
  ACS_smpeg;

function GetMp3FileInfo(FileName:string):TMp3FileInfo;
var
  Mp3In : TMP3In;
begin
  Mp3In := TMp3In.Create(nil);
  try
    Mp3In.FileName := FileName;
    Mp3In.Init;
    if Mp3In.HasAudio then
    begin
      result.IsValid := true;
      result.IsSupported := true;

      result.SampleRate := Mp3In.SampleRate;
      result.Channels   := Mp3In.Channels;
      result.SampleFrames := Mp3In.TotalSamples;
    end else
    begin
      result.IsValid     := false;
      result.IsSupported := false;
    end;
  finally
    Mp3In.Free;
  end;
end;


function LoadMp3FileStereo(FileName:string; Left,Right:PSingle):boolean;
var
  Mp3In : TMP3In;
  MemFloatOut : TMemFloatOut;
  c1: Integer;
begin
  mp3In := TMp3In.Create(nil);
  memFloatOut := TMemFloatOut.Create(nil);
  try
    try
      mp3In.FileName := FileName;
      MemFloatOut.Input := mp3In;
      MemFloatOut.BlockingRun;
      for c1 := 0 to MemFloatOut.SampleFrames-1 do
      begin
        Left^  := MemFloatOut.SampleData[0,c1];
        Right^ := MemFloatOut.SampleData[1,c1];
        inc(Left);
        inc(Right);
      end;
      result := true;
    except
      result := false;
    end;
  finally
    mp3In.free;
    MemFloatOut.Free;
  end;
end;

function LoadMp3FileMono(FileName:string; Left:PSingle):boolean;
var
  Mp3In : TMP3In;
  MemFloatOut : TMemFloatOut;
  c1: Integer;
begin
  mp3In := TMp3In.Create(nil);
  memFloatOut := TMemFloatOut.Create(nil);
  try
    try
      mp3In.FileName := FileName;
      MemFloatOut.Input := mp3In;
      MemFloatOut.BlockingRun;
      for c1 := 0 to MemFloatOut.SampleFrames-1 do
      begin
        Left^  := MemFloatOut.SampleData[0,c1];
        inc(Left);
      end;
      result := true;
    except
      result := false;
    end;
  finally
    mp3In.free;
    MemFloatOut.Free;
  end;
end;

end.
