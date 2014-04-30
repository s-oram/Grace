unit uGetWindowsInfo;

interface

uses
  Windows;

function eeGetWindowText(WindowHandle:hwnd):string;
function eeGetClassName(WindowHandle:hwnd):string;

function eeGetTempPath:string;

function eeGetWindowsVersion:string;

function eeGetArchitecture:string;  //x86 or x64 mainly...

function eeGetScreenSize:string;

implementation

uses
  SysUtils;

const
  // ==== System Metric Constants =======================
  // More constants are available:
  // http://msdn.microsoft.com/en-us/library/ms724385%28v=VS.85%29.aspx

  SM_CXSCREEN     = 0;    //The width of the screen of the primary display monitor, in pixels.
  SM_CYSCREEN     = 1;    //The height of the screen of the primary display monitor, in pixels.
  SM_CXFULLSCREEN = 16;   //The width of the client area for a full-screen window on the primary display monitor, in pixels.
  SM_CYFULLSCREEN = 17;   //The height of the client area for a full-screen window on the primary display monitor, in pixels.
  SM_SERVERR2     = 89;   //The build number if the system is Windows Server 2003 R2; otherwise, 0.
  SM_NETWORK      = 63;   //The least significant bit is set if a network is present; otherwise, it is cleared. The other bits are reserved for future use.
  SM_CMONITORS    = 80;   //The number of display monitors on a desktop.



  // ==== SystemInfo - wProcessorArchitecture ================
  //  http://msdn.microsoft.com/en-us/library/ms724958%28v=vs.85%29.aspx
  PROCESSOR_ARCHITECTURE_INTEL   = 0;      //x86
  PROCESSOR_ARCHITECTURE_IA64    = 6;      //Intel Itanium-based
  PROCESSOR_ARCHITECTURE_AMD64   = 9;      //x64 (AMD or Intel)
  PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;  //Unknown.


function eeGetWindowText(WindowHandle:hwnd):string;
const
  kMaxStringLength = 100;
var
  s1 : AnsiString;
  TempString : AnsiString;
  L : integer;
begin
  SetLength(s1,kMaxStringLength);
  L := GetWindowTextA(WindowHandle, @s1[1], kMaxStringLength);
  TempString := s1;
  SetLength(TempString, L);
  result := String(TempString);
end;

function eeGetClassName(WindowHandle:hwnd):string;
{$IFDEF VER230}
// NOTE: I'm not sure if this will work. need to test this function...
const
  kMaxStringLength = 100;
var
  s1:AnsiString;
  TempString : string;
  L:integer;
begin
  SetLength(s1,kMaxStringLength);
  L := GetClassNameA(WindowHandle, @s1[1], kMaxStringLength);
  TempString := string(s1);
  SetLength(s1, L);
  SetLength(TempString, L);
  result := TempString;
end;
{$ELSE}
const
  kMaxStringLength = 100;
var
  s1:AnsiString;
  L:integer;
  TempString:string;
begin
  SetLength(s1,kMaxStringLength);
  L := GetClassName(WindowHandle, @s1[1], kMaxStringLength);
  TempString := s1;
  SetLength(TempString, L);
  result := TempString;
end;
{$ENDIF}


function eeGetTempPath:string;
const
  kMaxPathLength = 255;
var
  s1 : AnsiString;
  TempString : AnsiString;
  L:integer;
begin
  SetLength(s1,kMaxPathLength);
  L := GetTempPathA(kMaxPathLength,@s1[1]);
  TempString := s1;
  SetLength(TempString, L);
  result := string(TempString);
end;


function eeGetWindowsVersion:string;
  const
    VER_NT_WORKSTATION       = 1;
    VER_NT_DOMAIN_CONTROLLER = 2;
    VER_NT_SERVER            = 3;
  type
    TOsVersionInfoEx = record
      dwOSVersionInfoSize : DWORD;
      dwMajorVersion      : DWORD;
      dwMinorVersion      : DWORD;
      dwBuildNumber       : DWORD;
      dwPlatformId        : DWORD;
      szCSDVersion        : array[0..127] of AnsiChar; { Maintenance AnsiString for PSS usage }
      wServicePackMajor   : WORD;
      wServicePackMinor   : WORD;
      wSuiteMask          : WORD;
      wProductType        : BYTE;
      wReserved           : BYTE;
    end;
var
  OsInfo:^OsVersionInfo;
  MinorVersion, MajorVersion:cardinal;
  OsInfoEx:TOsVersionInfoEx;
  OS:string;
  SystemInfo:TSystemInfo;
begin
  //When calling GetVersionEx, the method must be told the size
  //of the OsVersionInfo data structure, else it will fail.
  OsInfoEx.dwOSVersionInfoSize := SizeOf(TOsVersionInfoEx);
  OsInfo := @OsInfoEx;

  if GetVersionEx(OsInfo^) = false then
  begin
    result := 'error';
    exit; //==================================================>
  end;


  MajorVersion := OsInfo.dwMajorVersion;
  MinorVersion := OsInfo.dwMinorVersion;

  // NOTE: For information on interpereting the OsVersionInfo structure...
  //   http://msdn.microsoft.com/en-us/library/ms724834%28v=VS.85%29.aspx

  if (MajorVersion = 6) and (MinorVersion = 1) then
  begin
    if OsInfoEx.wProductType = VER_NT_WORKSTATION
      then OS := 'Windows 7'
      else OS := 'Windows Server 2008 R2';
  end else
  if (MajorVersion = 6) and (MinorVersion = 0) then
  begin
    if OsInfoEx.wProductType = VER_NT_WORKSTATION
      then OS := 'Windows Vista'
      else OS := 'Windows Server 2008';
  end else
  if (MajorVersion = 5) and (MinorVersion = 2) then
  begin
    // NOTE: It is possible to test for several different Windows Server OS types here.
    // But I don't think I'll need it at any point. Mainly just wanting to check for XP 64 bit.
    GetSystemInfo(SystemInfo);
    if (OsInfoEx.wProductType = VER_NT_WORKSTATION) and (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64)
      then OS := 'Windows XP Professional X64 Edition'
      else OS := 'Windows Server 2003 / Windows Home Server';
  end else
  if (MajorVersion = 5) and (MinorVersion = 1) then
  begin
    OS := 'Windows XP';
  end else
  if (MajorVersion = 5) and (MinorVersion = 0) then
  begin
    OS := 'Windows 2000';
  end else
  if (MajorVersion = 4) then
  begin
    OS := 'Win98/Win95/WinME';
  end else
  begin
    OS := Format('Unknown: Windows Version %s.%s', [IntToStr(MajorVersion), IntToStr(MinorVersion)]);
  end;

  result := OS;
end;


function eeGetArchitecture:string;
var
  SystemInfo:TSystemInfo;
begin
  GetSystemInfo(SystemInfo);
  case SystemInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL   : result := 'x86';
    PROCESSOR_ARCHITECTURE_IA64    : result := 'Intel Itanium';
    PROCESSOR_ARCHITECTURE_AMD64   : result := 'x64';
    PROCESSOR_ARCHITECTURE_UNKNOWN : result := 'Unknown';
  else
    result := 'error';
  end;
end;

function eeGetScreenSize:string;
var
  x, y:integer;
begin
  x := GetSystemMetrics(SM_CXSCREEN);
  y := GetSystemMetrics(SM_CYSCREEN);       
  result := IntToStr(x) + 'x' + IntToStr(y);
end;


end.
