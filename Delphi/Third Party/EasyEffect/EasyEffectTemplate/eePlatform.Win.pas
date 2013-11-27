unit eePlatform.Win;

interface

function GetCommonAppDataDir:string;

implementation

uses
  WinApi.Windows,
  ShlObj;

function GetCommonAppDataDir:string;
var
  hwnd : Int64;
  CSIDL : integer;
  hToken : Int64;
  Flags  : cardinal;
  PPath   : PWideChar;
  TempPath : array[0..MAX_PATH] of WideChar;
begin
  hwnd := 0;
  CSIDL := CSIDL_COMMON_APPDATA;
  hToken := 0;
  Flags  := SHGFP_TYPE_CURRENT;
  PPath := @TempPath[0];
  if Succeeded(ShGetFolderPath(hwnd, CSIDL, hToken, flags, PPath)) then
  begin
    result := TempPath;
  end else
  begin
    result := '';
  end;
end;

end.
