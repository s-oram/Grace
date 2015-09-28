unit VamLib.Win.Links;

interface

function  GetLinkTarget(const LinkFileName : string) : string;

implementation

uses
  VamLib.Types,
  SysUtils, Windows, ShlObj, ActiveX;



// function GetLinkTarget()
// Source: http://delphi.about.com/od/windowsshellapi/l/aa072704a.htm
function  GetLinkTarget(const LinkFileName : string) : string;
var
   psl  : IShellLink;
   ppf  : IPersistFile;
   WidePath  : Array[0..MAX_PATH] of WideChar;
   Info      : Array[0..MAX_PATH] of Char;
   wfs       : TWin32FindData;
   lpMultiByteString : PAnsiChar;
   CoCreateResult : integer;
begin
  if FileExists(LinkFileName) = false
    then raise Exception.Create('File not found. "' + LinkFileName + '"');

  if UpperCase(ExtractFileExt(LinkFileName)) <> '.LNK'
    then raise Exception.Create('This is not a .LNK file.');

  // CoCreateInstance() docs.
  // https://msdn.microsoft.com/en-us/library/windows/desktop/ms686615%28v=vs.85%29.aspx
  CoCreateResult := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, psl);
  if CoCreateResult <> S_OK then
  begin
    case CoCreateResult of
      REGDB_E_CLASSNOTREG:   raise EVamLibException.Create('Class not registered. (REGDB_E_CLASSNOTREG)');
      CLASS_E_NOAGGREGATION: raise EVamLibException.Create('Class cannot be created as part of aggregate. (CLASS_E_NOAGGREGATION)');
      E_NOINTERFACE:         raise EVamLibException.Create('Interface not implemented. (E_NOINTERFACE)');
      E_POINTER:             raise EVamLibException.Create('ppv parameter is null. (E_POINTER)');
    else
      raise EVamLibException.Create('Unexpected CoCreateInstance() error.');
    end;
  end;

  if psl.QueryInterface(IPersistFile, ppf) = 0 then
  begin
    lpMultiByteString := PAnsiChar(AnsiString(LinkFileName));
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, lpMultiByteString, -1, @WidePath, MAX_PATH);
    ppf.Load(WidePath, STGM_READ);
    psl.GetPath(@info, MAX_PATH, wfs, SLGP_UNCPRIORITY);
    Result := info;
  end
  else
    Result := '';
end;

end.
