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




end.
