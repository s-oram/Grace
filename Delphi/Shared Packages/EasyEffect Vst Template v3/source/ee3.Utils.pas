unit ee3.Utils;

interface

procedure GetBuildVersion(var V1, V2, V3, V4: word);
function GetBuildVersionAsString: string;

function GetDLLFilename:string;
function GetDLLDirectory:string;

function GetHostApplicationFullPath: string;
function GetHostApplicationFilename: string;
function GetHostApplicationDirectory: string;

implementation

uses
  IOUtils,
  Classes,
  SysUtils,
  Windows;

procedure GetBuildVersion(var V1, V2, V3, V4: word);
var
  verblock:PVSFIXEDFILEINFO;
  versionMS,versionLS:cardinal;
  verlen:cardinal;
  rs:TResourceStream;
  m:TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    rs := TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      m.CopyFrom(rs,rs.Size);
    finally
      rs.Free;
    end;

    m.Position := 0;

    if VerQueryValue(m.Memory,'\',pointer(verblock),verlen) then
    begin
      VersionMS:=verblock.dwFileVersionMS;
      VersionLS:=verblock.dwFileVersionLS;
      V1 := versionMS shr 16;
      V2 := versionMS and $FFFF;
      V3 := VersionLS shr 16;
      V4 := VersionLS and $FFFF;
    end;

  finally
    m.Free;
  end;
end;

function GetBuildVersionAsString: string;
var
  V1, V2, V3, V4: word;
begin
  GetBuildVersion(V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);
end;

function GetDLLFilename:string;
{$IFDEF VER230}
var
  s:array[0..1500] of WideChar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilename(st);
  result:=st;
end;
{$ELSE}
var
  s:array[0..1500] of ansichar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilename(st);
  result:=st;
end;
{$ENDIF}

function GetDLLDirectory:string;
{$IFDEF VER230}
var
  s:array[0..1500] of WideChar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilepath(st);
  result:=st;
end;
{$ELSE}
var
  s:array[0..1500] of AnsiChar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilepath(st);
  result:=st;
end;
{$ENDIF}


function GetHostApplicationFullPath: string;
var
  s: array[0..1500] of WideChar;
begin
  GetModuleFilename(0, s, sizeof(s));
  result := StrPas(s);
end;

function GetHostApplicationFilename:string;
var
  s: array[0..1500] of WideChar;
  st: string;
begin
  GetModuleFilename(0, s, sizeof(s));
  st := StrPas(s);
  st := ExtractFilename(st);
  result := st;
end;

function GetHostApplicationDirectory:string;
var
  s: array[0..1500] of WideChar;
  st: string;
begin
  GetModuleFilename(0, s, sizeof(s));
  st := StrPas(s);
  st := ExtractFilePath(st);
  result := st;
end;

end.
