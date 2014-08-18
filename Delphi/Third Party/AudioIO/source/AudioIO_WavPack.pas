unit AudioIO_WavPack;

interface


// TODO:HIGH
// WavPack support depends on a "wavpackdll.dll" being present on the system.
// It would be good if AudioIO could test if the WavPackDLL is present and
// dynamically enable wavpack support if it is present and of the correct version.

uses
  AudioIO;

type
  TWavPackFileInfo = record
    IsValid        : boolean; //Is this audio file a valid file? ie, sample rate value is correct etc.
    IsSupported    : boolean; //Can the audio file be loaded? Not all format files are supported.
    SampleRate     : integer;
    Channels       : integer;
    SampleFrames   : integer;
    FileFormatEx   : string;  //Additional info about the file format.
    ErrorMessage   : string;  // if the file is invalid or not supported, ErrorMessage explains why.
  end;

function GetWavPackFileInfo(FileName:string):TWavPackFileInfo;

function LoadWavPackFileStereo(FileName:string; Left,Right:PSingle):boolean;
function LoadWavPackFileMono(FileName:string; Left:PSingle):boolean;

implementation

uses
  ACS_MemFloat,
  ACS_WavPack;

function GetWavPackFileInfo(FileName:string):TWavPackFileInfo;
var
  WavPackIn : TWVIn;
begin
  WavPackIn := TWVIn.Create(nil);
  try
    WavPackIn.FileName := FileName;
    WavPackIn.Init;

    result.IsValid := true;
    result.IsSupported := true;

    result.SampleRate := WavPackIn.SampleRate;
    result.Channels   := WavPackIn.Channels;
    result.SampleFrames := WavPackIn.TotalSamples;

    //TODO:HIGH
    // test what happens when loading an invalid file.
  finally
    WavPackIn.Free;
  end;
end;


function LoadWavPackFileStereo(FileName:string; Left,Right:PSingle):boolean;
var
  WavPackIn : TWVIn;
  MemFloatOut : TMemFloatOut;
  c1: Integer;
begin
  WavPackIn := TWVIn.Create(nil);
  memFloatOut := TMemFloatOut.Create(nil);
  try
    try
      WavPackIn.FileName := FileName;
      MemFloatOut.Input := WavPackIn;
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
    WavPackIn.free;
    MemFloatOut.Free;
  end;
end;

function LoadWavPackFileMono(FileName:string; Left:PSingle):boolean;
var
  WavPackIn : TWVIn;
  MemFloatOut : TMemFloatOut;
  c1: Integer;
begin
  WavPackIn := TWVIn.Create(nil);
  memFloatOut := TMemFloatOut.Create(nil);
  try
    try
      WavPackIn.FileName := FileName;
      MemFloatOut.Input := WavPackIn;
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
    WavPackIn.free;
    MemFloatOut.Free;
  end;
end;

end.
