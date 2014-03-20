{
  The Windows API is useful but a little annoying to use, so
  here are some wrappers around some functions.


}

unit WinShell;

interface


uses
  ShlObj, ShellApi, Windows;


function BrowseDialog(AppHandle: hwnd; const Title: string; const Flag: integer): string;

function BrowseDialogCallBack(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM):integer stdcall;

procedure ShellOpenFile(Filename:string);

procedure ShellSendMail(ToAddress, ReplyAddress, Subject, Body:string);

function GetRegistryData(RootKey: HKEY; Key, Value: string): variant;

function GetProgramFilesDir: string;

implementation

uses
  System.Win.Registry,
  SysUtils;

function BrowseDialog (AppHandle: hwnd; const Title: string; const Flag: integer): string;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of AnsiChar;
  {$IFDEF VER230}
  TempPath    : array[0..MAX_PATH] of WideChar;
  {$ELSE}
  TempPath    : array[0..MAX_PATH] of AnsiChar;
  {$ENDIF}

begin
  Result:='';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := AppHandle;
    pszDisplayName := @DisplayName;
    {$IFDEF VER230}
    lpszTitle := PWideChar(Title);
    {$ELSE}
    lpszTitle := PAnsiChar(Title);
    {$ENDIF}
    ulFlags := Flag;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;

function BrowseDialogCallBack
  (Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM):
  integer stdcall;
//var
  {
  wa, rect : TRect;
  dialogPT : TPoint;
  }
begin
  {
  //center in work area
  if uMsg = BFFM_INITIALIZED then
  begin
    wa := Screen.WorkAreaRect;
    GetWindowRect(Wnd, Rect);
    dialogPT.X := ((wa.Right-wa.Left) div 2) -
                  ((rect.Right-rect.Left) div 2);
    dialogPT.Y := ((wa.Bottom-wa.Top) div 2) -
                  ((rect.Bottom-rect.Top) div 2);
    MoveWindow(Wnd,
               dialogPT.X,
               dialogPT.Y,
               Rect.Right - Rect.Left,
               Rect.Bottom - Rect.Top,
               True);
  end;
  }
  Result := 0;

end; (*BrowseDialogCallBack*)

procedure ShellOpenFile(Filename:string);
var
  Handle:hwnd;
begin
  Handle := 0;
  {$IFDEF VER230}
  ShellExecuteA(Handle, 'open', PAnsiChar(AnsiString(Filename)), nil, nil, SW_SHOWNORMAL);
  {$ELSE}
  ShellExecute(Handle, 'open', PAnsiChar(Filename), nil, nil, SW_SHOWNORMAL);
  {$ENDIF}

end;



//Sends an email using the default mail client.
procedure ShellSendMail(ToAddress, ReplyAddress, Subject, Body:string);
var
  Mail:string;
  Handle:hwnd;
begin

  Mail := 'mailto:' + ToAddress;
  Mail := Mail + '?subject='  + Subject;
  Mail := Mail + '&Reply-To=' + ReplyAddress;
  Mail := Mail + '&body='     + Body;


  //em_mail := 'mailto:delphi.guide@about.com?subject=' +
  //  em_subject + '&body=' + em_body ;

  Handle := 0;
  {$IFDEF VER230}
  ShellExecuteA(Handle,'open', PAnsiChar(AnsiString(Mail)), nil, nil, SW_SHOWNORMAL)
  {$ELSE}
  ShellExecute(Handle,'open', PAnsiChar(Mail), nil, nil, SW_SHOWNORMAL)
  {$ENDIF}


end;

function GetProgramFilesDir: string;
begin
  Result := GetRegistryData(HKEY_LOCAL_MACHINE, '\Software\Microsoft\Windows\CurrentVersion', 'ProgramFilesDir');  // or 'ProgramFilesPath'
end;


function GetRegistryData(RootKey: HKEY; Key, Value: string): variant;
var
 Reg: TRegistry;
  RegDataType: TRegDataType;
  DataSize, Len: integer;
  s: AnsiString;

label cantread;

begin
 Reg := nil;
  try
    Reg := TRegistry.Create;
    Reg.RootKey := RootKey;
    if Reg.OpenKey(Key, false) then begin
     try
       RegDataType := Reg.GetDataType(Value);
       if (RegDataType = rdString) or (RegDataType = rdExpandString)
         then Result := Reg.ReadString(Value)
       else if RegDataType = rdInteger
         then Result := Reg.ReadInteger(Value)
       else if RegDataType = rdBinary then
       begin
         DataSize := Reg.GetDataSize(Value);
         if DataSize = -1 then goto cantread;
         SetLength(s, DataSize);
         Len := Reg.ReadBinaryData(Value, PAnsiChar(s)^, DataSize);
         if Len <> DataSize then goto cantread;
         Result := s;
       end else
       begin
         cantread:
         raise Exception.Create(SysErrorMessage(ERROR_CANTREAD));
       end;

      except
        s := ''; // Deallocates memory if allocated
        Reg.CloseKey;
        raise;
      end;
      Reg.CloseKey;
    end
    else
      raise Exception.Create(SysErrorMessage(GetLastError));
  except
    Reg.Free;
    raise;
  end;
  Reg.Free;
end;

{
function GetProgramFilesDir: TFileName;
begin
 Result := GetRegistryData(HKEY_LOCAL_MACHINE,
      '\Software\Microsoft\Windows\CurrentVersion',
      'ProgramFilesDir');
end;
}



end.

