{
  ShellFunctions provides some wrappers around operating system interaction...

}

unit eeShellFunctions;

interface

// BrowseURL(()
// Source: http://delphi.about.com/cs/adptips2004/a/bltip0504_4.htm
function BrowseURL(const URL:ansistring):boolean;

implementation

uses
   Windows, Registry, ShellAPI;

function BrowseURL(const URL:ansistring):boolean;
var
   Browser: ansistring;
begin
  Result := True;
  Browser := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    Access  := KEY_QUERY_VALUE;
    if OpenKey('\htmlfile\shell\open\command', False) then
      Browser := ReadString('') ;
    CloseKey;
  finally
    Free;
  end;

  if Browser = '' then
  begin
    Result := False;
    Exit;
  end;

  Browser := Copy(Browser, Pos('"', Browser) + 1, Length(Browser)) ;
  Browser := Copy(Browser, 1, Pos('"', Browser) - 1);


  {$IFDEF VER230}
  ShellExecuteA(0, 'open', PAnsiChar(Browser), PAnsiChar(URL), nil, SW_SHOW);
  {$ELSE}
  ShellExecute(0, 'open', PAnsiChar(Browser), PAnsiChar(URL), nil, SW_SHOW);
  {$ENDIF}


end;


end.
