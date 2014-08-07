unit uLucidityExtra;

interface

{$INCLUDE Defines.inc}


function IsLucidityProgramFile(const FileName : string): boolean;
function IsSupportedProgramFormat(const FileName : string): boolean;
function IsSupportedAudioFormat(const FileName : string): boolean;

procedure SendMsg_StartProfiling;
procedure SendMsg_StopProfiling;

procedure SetUpLogging;

procedure LogMemoryUsage(const LogTag : string = '');


procedure LogStackTrace;

implementation

uses
  {$IFDEF MadExcept}
  MadStackTrace,
  {$ENDIF}
  VamLib.Utils,
  VamLib.LoggingProxy,
  SmartInspectLogging,
  Windows,
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


procedure SetUpLogging;
begin
  VamLib.LoggingProxy.Log.SetProxy(TSmartInspectProxy.Create);
  Log.LogMessage('Logging Proxy Initialised');
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

  LogMain.LogSingle('Mem Usage MB ' + Tag + ' ' + LogTag, MemUsage);
end;



procedure LogStackTrace;
begin
  LogMain.LogText('Stack Trace', MadStackTrace.StackTrace);
end;


initialization
  GlobalMemUsageTag := 0;

finalization

end.
