unit VamLib.WinUtils;

interface

function IsCtrlKeyDown  : Boolean;
function IsShiftKeyDown : Boolean;
function IsAltKeyDown   : Boolean;

procedure SendDebugMesssage(const Msg: String);

function ShellExecuteErrorCodeToString(const ErrorCode : integer):string;

function ShowDirectoryInWindowsExplorer(const Dir : string; out ErrMsg : string):boolean;


// Execute or open a file according to its extension.
function ExecuteFile(const FileName : string; out ErrMsg : string):boolean; overload;
function ExecuteFile(const FileName : string):boolean; overload;

function ShowFileInWindowsExplorer(const FileName: string): boolean;


type
  // NOTE: TWindowsVersion related code was copied from the JCL library. (JclSysInfo.pas)
  TWindowsVersion =
   (wvUnknown, wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinME,
    wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4, wvWin2000, wvWinXP,
    wvWin2003, wvWinXP64, wvWin2003R2, wvWinVista, wvWinServer2008,
    wvWin7, wvWinServer2008R2);

  function GetWindowsVersion: TWindowsVersion;






implementation

uses
  ShlObj,
  SysUtils,
  VamLib.DebugString,
  WinApi.ShellApi,
  WinApi.Windows;

function IsCtrlKeyDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Control] And 128) <> 0) ;
end;

function IsShiftKeyDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Shift] and 128) <> 0) ;
end;

function IsAltKeyDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Menu] and 128) <> 0) ;
end;


procedure SendDebugMesssage(const Msg: String);
begin
  //OutputDebugString(PChar(Msg))
  DbWin__OutputDebugStringU(PWideChar(Msg));
end;

function ShellExecuteErrorCodeToString(const ErrorCode : integer):string;
begin
  case ErrorCode of
    0 :                      result := 'The operating system is out of memory or resources.';
    ERROR_FILE_NOT_FOUND :   result := 'The specified file was not found.';
    ERROR_PATH_NOT_FOUND :   result := 'The specified path was not found.';
    //SE_ERR_FNF :             result := 'The specified file was not found.';
    //SE_ERR_PNF :             result := 'The specified path was not found.';
    ERROR_BAD_FORMAT :       result := 'The .exe file is invalid (non-Win32 .exe or error in .exe image).';
    SE_ERR_ACCESSDENIED :    result := 'The operating system denied access to the specified file.';
    SE_ERR_ASSOCINCOMPLETE : result := 'The file name association is incomplete or invalid.';
    SE_ERR_DDEBUSY :         result := 'The DDE transaction could not be completed because other DDE transactions were being processed.';
    SE_ERR_DDEFAIL :         result := 'The DDE transaction failed.';
    SE_ERR_DDETIMEOUT :      result := 'The DDE transaction could not be completed because the request timed out.';
    SE_ERR_DLLNOTFOUND :     result := 'The specified DLL was not found.';
    SE_ERR_NOASSOC :         result := 'There is no application associated with the given file name extension. This error will also be returned if you attempt to print a file that is not printable.';
    SE_ERR_OOM :             result := 'There was not enough memory to complete the operation.';
    SE_ERR_SHARE :           result := 'A sharing violation occurred.';
  else
    result := 'Unexpected error code (' + IntToStr(ErrorCode) + ').';
  end;
end;

function ShowDirectoryInWindowsExplorer(const Dir : string; out ErrMsg : string):boolean;
var
  seResult : integer;
begin
  seResult := ShellExecute(0, PChar('explore'), PChar(Dir), nil, nil, SW_SHOWNORMAL);
  if seResult <= 32 then
  begin
    ErrMsg := ShellExecuteErrorCodeToString(seResult);
    result := false; // ShellExecute() ran with errors.
  end else
  begin
    ErrMsg := '';
    result := true;  // ShellExecute() ran without errors.
  end;
end;

function ExecuteFile(const FileName : string; out ErrMsg : string):boolean;
var
  seResult : integer;
begin
  seResult := ShellExecute(0, PChar('open'), PChar(FileName), nil, nil, SW_SHOWNORMAL);
  if seResult <= 32 then
  begin
    ErrMsg := ShellExecuteErrorCodeToString(seResult);
    result := false; // ShellExecute() ran with errors.
  end else
  begin
    ErrMsg := '';
    result := true;  // ShellExecute() ran without errors.
  end;
end;

function ExecuteFile(const FileName : string):boolean; overload;
var
  ErrMsg : string;
begin
  result := ExecuteFile(FileName, ErrMsg);
end;

// function OpenFolderAndSelectFile()
// Source:
// - http://stackoverflow.com/q/15300999/395461
// - http://stackoverflow.com/a/15301028/395461
function ShowFileInWindowsExplorer(const FileName: string): boolean;
var
  IIDL: PItemIDList;
begin
  result := false;
  IIDL := ILCreateFromPath(PChar(FileName));
  if IIDL <> nil then
    try
      result := SHOpenFolderAndSelectItems(IIDL, 0, nil, 0) = S_OK;
    finally
      ILFree(IIDL);
    end;
end;


var
  KernelVersionHi: DWORD;

function GetWindowsVersion: TWindowsVersion;
var
  TrimmedWin32CSDVersion: string;
  SystemInfo: TSystemInfo;
  OSVersionInfoEx: TOSVersionInfoEx;
const
  SM_SERVERR2 = 89;
begin
  Result := wvUnknown;
  TrimmedWin32CSDVersion := Trim(Win32CSDVersion);
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      case Win32MinorVersion of
        0..9:
          if (TrimmedWin32CSDVersion = 'B') or (TrimmedWin32CSDVersion = 'C') then
            Result := wvWin95OSR2
          else
            Result := wvWin95;
        10..89:
          // On Windows ME Win32MinorVersion can be 10 (indicating Windows 98
          // under certain circumstances (image name is setup.exe). Checking
          // the kernel version is one way of working around that.
          if KernelVersionHi = $0004005A then // 4.90.x.x
            Result := wvWinME
          else
          if (TrimmedWin32CSDVersion = 'A') or (TrimmedWin32CSDVersion = 'B') then
            Result := wvWin98SE
          else
            Result := wvWin98;
        90:
          Result := wvWinME;
      end;
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        3:
          case Win32MinorVersion of
            1:
              Result := wvWinNT31;
            5:
              Result := wvWinNT35;
            51:
              Result := wvWinNT351;
          end;
        4:
          Result := wvWinNT4;
        5:
          case Win32MinorVersion of
            0:
              Result := wvWin2000;
            1:
              Result := wvWinXP;
            2:
              begin
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                SystemInfo.dwOemId := 0;
                GetNativeSystemInfo(SystemInfo);
                if GetSystemMetrics(SM_SERVERR2) <> 0 then
                  Result := wvWin2003R2
                else
                if (SystemInfo.wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_INTEL) and
                  GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWinXP64
                else
                  Result := wvWin2003;
              end;
          end;
        6:
          case Win32MinorVersion of
            0:
              begin
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWinVista
                else
                  Result := wvWinServer2008;
              end;
            1:
              begin
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWin7
                else
                  Result := wvWinServer2008R2;
              end;
          end;
      end;
  end;
end;





end.
