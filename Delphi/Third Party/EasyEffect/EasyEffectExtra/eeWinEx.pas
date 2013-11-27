unit eeWinEx;

interface

uses
  SysUtils, Windows, ShellApi, ShlObj, ActiveX;



function WindowsMessageIdToStr(MsgId:Cardinal):string;

// GetLinkTarget() finds the target of Windows links/shortcuts.
function  GetLinkTarget(const LinkFileName : string) : string;

function OpenFolderAndSelectFile(const FileName: string): boolean;
function ShowInWindowsExplorer(const FileName: string): boolean;

procedure ShellOpenFileWith(Filename:string; EditApp:string);


const
  OFASI_EDIT = $0001;
  OFASI_OPENDESKTOP = $0002;

{$IFDEF UNICODE}
function ILCreateFromPath(pszPath: PChar): PItemIDList stdcall; external shell32 name 'ILCreateFromPathW';
{$ELSE}
function ILCreateFromPath(pszPath: PChar): PItemIDList stdcall; external shell32 name 'ILCreateFromPathA';
{$ENDIF}
procedure ILFree(pidl: PItemIDList) stdcall; external shell32;
function SHOpenFolderAndSelectItems(pidlFolder: PItemIDList; cidl: Cardinal; apidl: pointer; dwFlags: DWORD): HRESULT; stdcall; external shell32;



implementation

uses
  eeStringConversion;


procedure ShellOpenFileWith(Filename:string; EditApp:string);
begin
  // NOTE: Add double quotes around the file name so the file loads in Image Line's Edison.
  // I'm not sure if this is required by Windows but it will do if it works.
  // Tested with:
  // - Wavosaur
  // - Edison
  FileName := '"' + FileName + '"';

  {$IFDEF VER230}
  ShellExecuteA(0,'open', PAnsiChar(AnsiString(EditApp)),PAnsiChar(AnsiString(Filename)), nil, SW_SHOWNORMAL);
  {$ELSE}
  ShellExecute(0,'open', PAnsiChar(AnsiString(EditApp)),PAnsiChar(AnsiString(Filename)), nil, SW_SHOWNORMAL);
  {$ENDIF}
end;



// function GetLinkTarget()
// Source: http://delphi.about.com/od/windowsshellapi/l/aa072704a.htm
function  GetLinkTarget(const LinkFileName : string) : string;
var
   psl  : IShellLink;
   ppf  : IPersistFile;
   r : HResult;
   WidePath  : Array[0..260] of WideChar;
   Info      : Array[0..MAX_PATH] of Char;
   wfs       : TWin32FindData;
   lpMultiByteString : PAnsiChar;
begin
  if FileExists(LinkFileName) = false
    then raise Exception.Create('File not found. "' + LinkFileName + '"');

  if UpperCase(ExtractFileExt(LinkFileName)) <> '.LNK'
    then raise Exception.Create('This is not a .LNK file.');

  r := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, psl);
  if (r <> S_OK) and (assigned(psl)) and (psl.QueryInterface(IPersistFile, ppf) = 0) then
  begin
    lpMultiByteString := ToPAnsiChar(LinkFileName);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, lpMultiByteString, -1, @WidePath, MAX_PATH);
    ppf.Load(WidePath, STGM_READ);
    psl.GetPath(@info, MAX_PATH, wfs, SLGP_UNCPRIORITY);
    Result := info;
  end
  else
    Result := '';
end;




// function OpenFolderAndSelectFile()
// Source:
// - http://stackoverflow.com/q/15300999/395461
// - http://stackoverflow.com/a/15301028/395461
function OpenFolderAndSelectFile(const FileName: string): boolean;
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

function ShowInWindowsExplorer(const FileName: string): boolean;
begin
  result := OpenFolderAndSelectFile(FileName);
end;


function WindowsMessageIdToStr(MsgId:Cardinal):string;
var
  s : string;
begin
  case MsgId of
    0: s := 'WM_NULL';
    1: s := 'WM_CREATE';
    2: s := 'WM_DESTROY';
    3: s := 'WM_MOVE';
    5: s := 'WM_SIZE';
    6: s := 'WM_ACTIVATE';
    7: s := 'WM_SETFOCUS';
    8: s := 'WM_KILLFOCUS';
    10: s := 'WM_ENABLE';
    11: s := 'WM_SETREDRAW';
    12: s := 'WM_SETTEXT';
    13: s := 'WM_GETTEXT';
    14: s := 'WM_GETTEXTLENGTH';
    15: s := 'WM_PAINT';
    16: s := 'WM_CLOSE';
    17: s := 'WM_QUERYENDSESSION';
    18: s := 'WM_QUIT';
    19: s := 'WM_QUERYOPEN';
    20: s := 'WM_ERASEBKGND';
    21: s := 'WM_SYSCOLORCHANGE';
    22: s := 'WM_ENDSESSION';
    24: s := 'WM_SHOWWINDOW';
    25: s := 'WM_CTLCOLOR';
    26: s := 'WM_WININICHANGE';
    27: s := 'WM_DEVMODECHANGE';
    28: s := 'WM_ACTIVATEAPP';
    29: s := 'WM_FONTCHANGE';
    30: s := 'WM_TIMECHANGE';
    31: s := 'WM_CANCELMODE';
    32: s := 'WM_SETCURSOR';
    33: s := 'WM_MOUSEACTIVATE';
    34: s := 'WM_CHILDACTIVATE';
    35: s := 'WM_QUEUESYNC';
    36: s := 'WM_GETMINMAXINFO';
    38: s := 'WM_PAINTICON';
    39: s := 'WM_ICONERASEBKGND';
    40: s := 'WM_NEXTDLGCTL';
    42: s := 'WM_SPOOLERSTATUS';
    43: s := 'WM_DRAWITEM';
    44: s := 'WM_MEASUREITEM';
    45: s := 'WM_DELETEITEM';
    46: s := 'WM_VKEYTOITEM';
    47: s := 'WM_CHARTOITEM';
    48: s := 'WM_SETFONT';
    49: s := 'WM_GETFONT';
    50: s := 'WM_SETHOTKEY';
    51: s := 'WM_GETHOTKEY';
    55: s := 'WM_QUERYDRAGICON';
    57: s := 'WM_COMPAREITEM';
    61: s := 'WM_GETOBJECT';
    65: s := 'WM_COMPACTING';
    68: s := 'WM_COMMNOTIFY';
    70: s := 'WM_WINDOWPOSCHANGING';
    71: s := 'WM_WINDOWPOSCHANGED';
    72: s := 'WM_POWER';
    74: s := 'WM_COPYDATA';
    75: s := 'WM_CANCELJOURNAL';
    78: s := 'WM_NOTIFY';
    80: s := 'WM_INPUTLANGCHANGEREQUEST';
    81: s := 'WM_INPUTLANGCHANGE';
    82: s := 'WM_TCARD';
    83: s := 'WM_HELP';
    84: s := 'WM_USERCHANGED';
    85: s := 'WM_NOTIFYFORMAT';
    123: s := 'WM_CONTEXTMENU';
    124: s := 'WM_STYLECHANGING';
    125: s := 'WM_STYLECHANGED';
    126: s := 'WM_DISPLAYCHANGE';
    127: s := 'WM_GETICON';
    128: s := 'WM_SETICON';
    129: s := 'WM_NCCREATE';
    130: s := 'WM_NCDESTROY';
    131: s := 'WM_NCCALCSIZE';
    132: s := 'WM_NCHITTEST';
    133: s := 'WM_NCPAINT';
    134: s := 'WM_NCACTIVATE';
    135: s := 'WM_GETDLGCODE';
    136: s := 'WM_SYNCPAINT';
    160: s := 'WM_NCMOUSEMOVE';
    161: s := 'WM_NCLBUTTONDOWN';
    162: s := 'WM_NCLBUTTONUP';
    163: s := 'WM_NCLBUTTONDBLCLK';
    164: s := 'WM_NCRBUTTONDOWN';
    165: s := 'WM_NCRBUTTONUP';
    166: s := 'WM_NCRBUTTONDBLCLK';
    167: s := 'WM_NCMBUTTONDOWN';
    168: s := 'WM_NCMBUTTONUP';
    169: s := 'WM_NCMBUTTONDBLCLK';
    171: s := 'WM_NCXBUTTONDOWN';
    172: s := 'WM_NCXBUTTONUP';
    173: s := 'WM_NCXBUTTONDBLCLK';
    255: s := 'WM_INPUT';

    // NOTE: My Windows message reference shows WM_KEYDOWN and WM_KEYFIRST as using the same id.
    // It seems like a mistage to me, but checking in Winapi.Messages.pas shows both messages
    // using the same IDs as well!! Perhaps the Delphi devs were using the same source as I was...
    // Source: http://wiki.winehq.org/List_Of_Windows_Messages
    //256: s := 'WM_KEYFIRST';
    //256: s := 'WM_KEYDOWN';
    256: s := 'WM_KEYDOWN / WM_KEYFIRST';

    257: s := 'WM_KEYUP';
    258: s := 'WM_CHAR';
    259: s := 'WM_DEADCHAR';
    260: s := 'WM_SYSKEYDOWN';
    261: s := 'WM_SYSKEYUP';
    262: s := 'WM_SYSCHAR';
    263: s := 'WM_SYSDEADCHAR';
    264: s := 'WM_KEYLAST';
    265: s := 'WM_WNT_CONVERTREQUESTEX';
    266: s := 'WM_CONVERTREQUEST';
    267: s := 'WM_CONVERTRESULT';
    268: s := 'WM_INTERIM';
    269: s := 'WM_IME_STARTCOMPOSITION';
    270: s := 'WM_IME_ENDCOMPOSITION';

    // NOTE: Some ambiguity here as well as to what message ID means what...
    //271: s := 'WM_IME_COMPOSITION';
    //271: s := 'WM_IME_KEYLAST';
    271: s := 'WM_IME_KEYLAST / WM_IME_COMPOSITION';

    272: s := 'WM_INITDIALOG';
    273: s := 'WM_COMMAND';
    274: s := 'WM_SYSCOMMAND';
    275: s := 'WM_TIMER';
    276: s := 'WM_HSCROLL';
    277: s := 'WM_VSCROLL';
    278: s := 'WM_INITMENU';
    279: s := 'WM_INITMENUPOPUP';
    280: s := 'WM_SYSTIMER';
    287: s := 'WM_MENUSELECT';
    288: s := 'WM_MENUCHAR';
    289: s := 'WM_ENTERIDLE';
    290: s := 'WM_MENURBUTTONUP';
    291: s := 'WM_MENUDRAG';
    292: s := 'WM_MENUGETOBJECT';
    293: s := 'WM_UNINITMENUPOPUP';
    294: s := 'WM_MENUCOMMAND';
    295: s := 'WM_CHANGEUISTATE';
    296: s := 'WM_UPDATEUISTATE';
    297: s := 'WM_QUERYUISTATE';
    306: s := 'WM_CTLCOLORMSGBOX';
    307: s := 'WM_CTLCOLOREDIT';
    308: s := 'WM_CTLCOLORLISTBOX';
    309: s := 'WM_CTLCOLORBTN';
    310: s := 'WM_CTLCOLORDLG';
    311: s := 'WM_CTLCOLORSCROLLBAR';
    312: s := 'WM_CTLCOLORSTATIC';

    //512: s := 'WM_MOUSEFIRST';
    //512: s := 'WM_MOUSEMOVE';
    512: s := 'WM_MOUSEMOVE / WM_MOUSEFIRST';


    513: s := 'WM_LBUTTONDOWN';
    514: s := 'WM_LBUTTONUP';
    515: s := 'WM_LBUTTONDBLCLK';
    516: s := 'WM_RBUTTONDOWN';
    517: s := 'WM_RBUTTONUP';
    518: s := 'WM_RBUTTONDBLCLK';
    519: s := 'WM_MBUTTONDOWN';
    520: s := 'WM_MBUTTONUP';

    //521: s := 'WM_MBUTTONDBLCLK';
    //521: s := 'WM_MOUSELAST';
    521: s := 'WM_MBUTTONDBLCLK / WM_MOUSELAST';

    522: s := 'WM_MOUSEWHEEL';
    523: s := 'WM_XBUTTONDOWN';
    524: s := 'WM_XBUTTONUP';
    525: s := 'WM_XBUTTONDBLCLK';
    528: s := 'WM_PARENTNOTIFY';
    529: s := 'WM_ENTERMENULOOP';
    530: s := 'WM_EXITMENULOOP';
    531: s := 'WM_NEXTMENU';
    532: s := 'WM_SIZING';
    533: s := 'WM_CAPTURECHANGED';
    534: s := 'WM_MOVING';
    536: s := 'WM_POWERBROADCAST';
    537: s := 'WM_DEVICECHANGE';
    544: s := 'WM_MDICREATE';
    545: s := 'WM_MDIDESTROY';
    546: s := 'WM_MDIACTIVATE';
    547: s := 'WM_MDIRESTORE';
    548: s := 'WM_MDINEXT';
    549: s := 'WM_MDIMAXIMIZE';
    550: s := 'WM_MDITILE';
    551: s := 'WM_MDICASCADE';
    552: s := 'WM_MDIICONARRANGE';
    553: s := 'WM_MDIGETACTIVE';
    560: s := 'WM_MDISETMENU';
    561: s := 'WM_ENTERSIZEMOVE';
    562: s := 'WM_EXITSIZEMOVE';
    563: s := 'WM_DROPFILES';
    564: s := 'WM_MDIREFRESHMENU';
    640: s := 'WM_IME_REPORT';
    641: s := 'WM_IME_SETCONTEXT';
    642: s := 'WM_IME_NOTIFY';
    643: s := 'WM_IME_CONTROL';
    644: s := 'WM_IME_COMPOSITIONFULL';
    645: s := 'WM_IME_SELECT';
    646: s := 'WM_IME_CHAR';
    648: s := 'WM_IME_REQUEST';
    656: s := 'WM_IMEKEYDOWN / WM_IME_KEYDOWN';
    657: s := 'WM_IMEKEYUP / WM_IME_KEYUP';
    672: s := 'WM_NCMOUSEHOVER';
    673: s := 'WM_MOUSEHOVER';
    674: s := 'WM_NCMOUSELEAVE';
    675: s := 'WM_MOUSELEAVE';
    768: s := 'WM_CUT';
    769: s := 'WM_COPY';
    770: s := 'WM_PASTE';
    771: s := 'WM_CLEAR';
    772: s := 'WM_UNDO';
    773: s := 'WM_RENDERFORMAT';
    774: s := 'WM_RENDERALLFORMATS';
    775: s := 'WM_DESTROYCLIPBOARD';
    776: s := 'WM_DRAWCLIPBOARD';
    777: s := 'WM_PAINTCLIPBOARD';
    778: s := 'WM_VSCROLLCLIPBOARD';
    779: s := 'WM_SIZECLIPBOARD';
    780: s := 'WM_ASKCBFORMATNAME';
    781: s := 'WM_CHANGECBCHAIN';
    782: s := 'WM_HSCROLLCLIPBOARD';
    783: s := 'WM_QUERYNEWPALETTE';
    784: s := 'WM_PALETTEISCHANGING';
    785: s := 'WM_PALETTECHANGED';
    786: s := 'WM_HOTKEY';
    791: s := 'WM_PRINT';
    792: s := 'WM_PRINTCLIENT';
    793: s := 'WM_APPCOMMAND';
    856: s := 'WM_HANDHELDFIRST';
    863: s := 'WM_HANDHELDLAST';
    864: s := 'WM_AFXFIRST';
    895: s := 'WM_AFXLAST';
    896: s := 'WM_PENWINFIRST';
    897: s := 'WM_RCRESULT';
    898: s := 'WM_HOOKRCRESULT';
    899: s := 'WM_GLOBALRCCHANGE / WM_PENMISCINFO';
    900: s := 'WM_SKB';
    901: s := 'WM_HEDITCTL / WM_PENCTL';
    902: s := 'WM_PENMISC';
    903: s := 'WM_CTLINIT';
    904: s := 'WM_PENEVENT';
    911: s := 'WM_PENWINLAST';
    1190: s := 'WM_CAP_SET_MCI_DEVICEW';
    1191: s := 'WM_CAP_GET_MCI_DEVICEW';
    1204: s := 'WM_CAP_PAL_OPENW';
    1205: s := 'WM_CAP_PAL_SAVEW';
    8192: s := 'OCMBASE';
    8217: s := 'OCM_CTLCOLOR';
    8235: s := 'OCM_DRAWITEM';
    8236: s := 'OCM_MEASUREITEM';
    8237: s := 'OCM_DELETEITEM';
    8238: s := 'OCM_VKEYTOITEM';
    8239: s := 'OCM_CHARTOITEM';
    8249: s := 'OCM_COMPAREITEM';
    8270: s := 'OCM_NOTIFY';
    8465: s := 'OCM_COMMAND';
    8468: s := 'OCM_HSCROLL';
    8469: s := 'OCM_VSCROLL';
    8498: s := 'OCM_CTLCOLORMSGBOX';
    8499: s := 'OCM_CTLCOLOREDIT';
    8500: s := 'OCM_CTLCOLORLISTBOX';
    8501: s := 'OCM_CTLCOLORBTN';
    8502: s := 'OCM_CTLCOLORDLG';
    8503: s := 'OCM_CTLCOLORSCROLLBAR';
    8504: s := 'OCM_CTLCOLORSTATIC';
    8720: s := 'OCM_PARENTNOTIFY';
    32768: s := 'WM_APP';
    52429: s := 'WM_RASDIALEVENT';
  else
    s := 'Unknown';
  end;

  result := s;
end;

end.
