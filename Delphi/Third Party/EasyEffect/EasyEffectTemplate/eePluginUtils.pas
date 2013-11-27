unit eePluginUtils;

interface

procedure GetBuildVersion(var V1, V2, V3, V4: word);
function GetBuildVersionAsString: string;

function GetDLLFilename:string;
function GetDLLDirectory:string;
function GetHostApplicationFullPath: string;
function GetHostApplicationFilename: string;
function GetHostApplicationDirectory: string;

// WriteDataToFile() is useful for appending data to a simple text based log file.
procedure WriteDataToFile(const FileName: string; const Data:string; AppendData:boolean);

function IsDirectoryWriteable(const AName: string): Boolean;



implementation

uses
  IOUtils,
  Windows, Classes, SysUtils;

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
var
  s:array[0..1500] of Widechar;
  st:string;
begin
 GetModuleFilename(hinstance,s,sizeof(s));
 st := strpas(s);
 st := extractfilename(st);
 result:=st;
end;

function GetDLLDirectory:string;
var
  s:array[0..1500] of Widechar;
  st:string;
begin
 GetModuleFilename(hinstance,s,sizeof(s));
 st := strpas(s);
 st := extractfilepath(st);
 result:=st;
end;

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



procedure WriteDataToFile(const FileName: string; const Data:string; AppendData:boolean);
const
  LineBreak = string(#13#10);
var
  Flags : word;
  TextFile : TFileStream;
begin
  if (AppendData = false) or (FileExists(FileName) = false)
    then Flags := fmCreate
    else Flags := fmOpenReadWrite;

  TextFile := TFileStream.Create(FileName, Flags);

  try
    TextFile.Seek(0,TSeekOrigin.soEnd);
    TextFile.Write(Data[1], Length(Data) * SizeOf(Char));
    TextFile.Write(LineBreak[1], Length(LineBreak) * SizeOf(Char));
  finally
    TextFile.Free;
  end;
end;


function IsDirectoryWriteable(const AName: string): Boolean;
// Source: http://stackoverflow.com/a/3611484/395461
var
  FileName: String;
  H: THandle;
  ts: TTimeStamp;
  RandomValue : integer;
begin
  assert(DirectoryExists(aName));
  if DirectoryExists(aName) = false then raise Exception.Create('Direction doesn''t exist. (' + aName + ')');

  ts := DateTimeToTimeStamp(Now);
  RandomValue := random(High(integer));
  FileName := IncludeTrailingPathDelimiter(AName) + 'DirWriteCheck' + IntToStr(ts.Date) + IntToStr(ts.Time) + IntToStr(RandomValue) + '.tmp';
  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then CloseHandle(H);

end;




end.
