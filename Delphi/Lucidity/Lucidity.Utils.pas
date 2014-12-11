unit Lucidity.Utils;

interface

{$INCLUDE Defines.inc}

uses
  Controls,
  Windows,
  Classes;

function IsLucidityProgramFile(const FileName : string): boolean;
function IsSupportedProgramFormat(const FileName : string): boolean;
function IsSupportedAudioFormat(const FileName : string): boolean;

procedure SendMsg_StartProfiling;
procedure SendMsg_StopProfiling;

procedure LogMemoryUsage(const LogTag : string = '');
procedure LogStackTrace;


// This method has been added to allow Lucidity to load a patch from a directory
// using MIDI program change commands.
function FindLucidityProgramUsingIndex(const Dir : string; const ProgramIndex : integer):string;

function GetPluginBuildInfo:string;


function GetComponentHandle(c : TComponent):HWND;

implementation

uses
  eeVstExtra,
  {$IFDEF MadExcept}MadStackTrace,{$ENDIF}
  {$IFDEF Logging}VamLib.Logging,{$ENDIF}
  VamLib.Utils,
  uFindFiles,
  AudioIO,
  SysUtils;

procedure SendMsg_StartProfiling;
begin
  OutputDebugString('SAMPLING ON');
end;

procedure SendMsg_StopProfiling;
begin
  OutputDebugString('SAMPLING OFF');
end;



function IsLucidityProgramFile(const FileName : string): boolean;
var
  ext : string;
begin
  ext := ExtractFileExt(FileName);

  if SameText(ext, '.lpg')
    then result := true
    else result := false;
end;

function IsSupportedProgramFormat(const FileName : string): boolean;
var
  ext : string;
begin
  ext := ExtractFileExt(FileName);

  if SameText(ext, '.sfz') then exit(true);
  if SameText(ext, '.lpg') then exit(true);

  result := false;
end;


function IsSupportedAudioFormat(const FileName : string): boolean;
begin
  result := IsSupportedAudioFileFormat(Filename, true);
end;


var
  GlobalMemUsageTag : cardinal;

procedure LogMemoryUsage(const LogTag : string = '');
var
  MemUsage : single;
  s : string;
  Tag : string;
begin
  Tag := 'Tag ' + IntToStr(GlobalMemUsageTag);
  inc(GlobalMemUsageTag);

  MemUsage := BytesToMegaBytes(MemoryUsed);
  s := FloatToStr(MemUsage);

  // TODO:HIGH - reimplement this in the logging class.
  //Log.Main.LogSingle('Mem Usage MB ' + Tag + ' ' + LogTag, MemUsage);
end;



procedure LogStackTrace;
begin
  // TODO:HIGH - reimplement this in the logging class.
  //Log.Main.LogText('Stack Trace', MadStackTrace.StackTrace);
end;


function FindLucidityProgramUsingIndex(const Dir : string; const ProgramIndex : integer):string;
var
  FileResults : TStringList;
  PrunedFileName : string;
  c1: Integer;
begin
  assert(false, 'TODO');
  FileResults := TStringList.Create;
  FileResults.Sorted := true;
  FileResults.CaseSensitive := false;
  AutoFree(@FileResults);

  FindOnlyFiles(Dir, FileResults,'*.lpg');


  for c1 := 0 to FileResults.Count-1 do
  begin
    //PrunedFileName := ExtractFileName(

    // TODO:HIGH finish this method.

    {
      - prune the file name
      - extract the first three charactors
      - convert to an integer.
      - see if it matches the program index, load.
    }
  end;

end;

function GetPluginBuildInfo:string;
var
  TargetPlatform : string;
  BuildType : string;
begin
  {$IF Defined(WIN32)}
     TargetPlatform := '32bit';
  {$ELSEIF Defined(WIN64)}
     TargetPlatform := '64bit';
  {$ELSE}
     TargetPlatform := 'unknown-platform';
  {$IFEND}

  {$IFDEF ReleaseBuild}
    BuildType := 'Release Build';
  {$ELSE}
    BuildType := 'Debug Build';
  {$ENDIF}

  result := 'Version ' + GetBuildInfoAsString + kChar.Space + BuildType + kChar.Space + TargetPlatform + EndOfLine;
end;

function GetComponentHandle(c : TComponent):HWND;
begin
  if (c is TWinControl)
    then result := (c as TWinControl).Handle
    else result := 0;
end;


initialization
  GlobalMemUsageTag := 0;

finalization

end.
