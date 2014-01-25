unit uLucidityExtra;

interface


function IsLucidityProgramFile(const FileName : string): boolean;
function IsSupportedProgramFormat(const FileName : string): boolean;
function IsSupportedAudioFormat(const FileName : string): boolean;



procedure SendMsg_StartProfiling;
procedure SendMsg_StopProfiling;

implementation

uses
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

  if SameText(ext, '.lpg')
    then result := true
    else result := false;

end;


function IsSupportedAudioFormat(const FileName : string): boolean;
begin
  result := IsSupportedAudioFileFormat(Filename, true);
end;

end.
