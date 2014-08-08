unit XPLAT.Dialogs;

interface

uses
  Vcl.Dialogs,
  System.Classes;

{$SCOPEDENUMS ON}

type
  TxpMode = (WinXP, WinVista);

  TxpFileOpenDialog = class
  private
    FFileName: string;
    FFilterIndex: Integer;
    FDefaultExt: string;
    FFilter: string;
    FInitialDir: string;
    FTitle: string;
  protected
    xpMode   : TxpMode;
    WinXP    : TOpenDialog;
    WinVista : TFileOpenDialog;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Execute : boolean;

    property Title: string        read FTitle       write FTitle;
    property DefaultExt: string   read FDefaultExt  write FDefaultExt;  //example: 'exe' or 'txt'
    property FileName: string     read FFileName    write FFileName;
    // Similar to TOpenDialog.
    // Format: <first displayed name>|<first file extension>|<second displayed name>|<second file extension>|...|<n-th displayed name>|<n-th file extension>
    // Example:
    //    Filter := 'Applications|*.EXE|Text files|*.TXT';
    //    Filter := 'Pascal files|*.PAS;*.DPK;*.DPR';
    property Filter: string       read FFilter      write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex default 1;
    property InitialDir: string   read FInitialDir  write FInitialDir;
  end;

  TxpBrowserSelectDialog = class
  private
    Owner : TComponent;
    xpMode   : TxpMode;
    WinVista : TFileOpenDialog;
    fFileName : string;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Execute : boolean;
    function FileName : string;
  end;

implementation

uses
  SysUtils,
  JclSysInfo,
  Vcl.FileCtrl,
  VamLib.Utils;


function GetXPMode:TxpMode;
var
  WinVer : TWindowsVersion;
begin
  WinVer := GetWindowsVersion;
  case WinVer of
    wvWin95:           result := TxpMode.WinXP;
    wvWin95OSR2:       result := TxpMode.WinXP;
    wvWin98:           result := TxpMode.WinXP;
    wvWin98SE:         result := TxpMode.WinXP;
    wvWinME:           result := TxpMode.WinXP;
    wvWinNT31:         result := TxpMode.WinXP;
    wvWinNT35:         result := TxpMode.WinXP;
    wvWinNT351:        result := TxpMode.WinXP;
    wvWinNT4:          result := TxpMode.WinXP;
    wvWin2000:         result := TxpMode.WinXP;
    wvWinXP:           result := TxpMode.WinXP;
    wvWin2003:         result := TxpMode.WinXP;
    wvWinXP64:         result := TxpMode.WinXP;
    wvWin2003R2:       result := TxpMode.WinXP;
    wvWinVista:        result := TxpMode.WinVista;
    wvWinServer2008:   result := TxpMode.WinVista;
    wvWin7:            result := TxpMode.WinVista;
    wvWinServer2008R2: result := TxpMode.WinVista;
    wvUnknown:         result := TxpMode.WinVista;
  else
    //TODO:HIGH need to check for Windows 8.
    result := TxpMode.WinVista;
  end;
end;


{ TxpFileOpenDialog }

constructor TxpFileOpenDialog.Create(AOwner: TComponent);
begin
  xpMode := GetxpMode;
  //xpMode := TxpMode.WinXP;

  case xpMode of
    TxpMode.WinXP:    WinXP    := TOpenDialog.Create(AOwner);
    TxpMode.WinVista: WinVista := TFileOpenDialog.Create(AOwner);
  else
    raise Exception.Create('Unexpected type.');
  end;

  FFileName   := '';
  FDefaultExt := '';
  FFilter     := '';
  FInitialDir := '';
  FDefaultExt := '';
  FTitle      := 'Open File';
end;

destructor TxpFileOpenDialog.Destroy;
begin
  if assigned(WinXP)    then WinXP.Free;
  if assigned(WinVista) then WinVista.Free;

  inherited;
end;

function TxpFileOpenDialog.Execute: boolean;
var
  FileTypesList : TStringList;
  c1: Integer;
  Index : integer;
  ft : TFileTypeItem;
  FilterTypesProcessed : string;
begin
  case xpMode of
    TxpMode.WinXP:
    begin
      //======= Setup File Tpes ========================
      FilterTypesProcessed := '';
      FileTypesList := TStringList.Create;
      try
        ExplodeString('|', fFilter, FileTypesList);
        for c1 := 0 to FileTypesList.Count div 2 - 1 do
        begin
          Index := c1 * 2;
          FilterTypesProcessed := FilterTypesProcessed + FileTypesList[Index] + ' (' + FileTypesList[Index+1] + ')';
          FilterTypesProcessed := FilterTypesProcessed + '|' + FileTypesList[Index+1];
          if c1+1 <= FileTypesList.Count div 2 - 1
            then FilterTypesProcessed := FilterTypesProcessed + '|';
        end;
      finally
        FileTypesList.Free;
      end;
      //================================================
      WinXP.Filter     := FilterTypesProcessed;
      WinXP.FileName   := FFileName;
      WinXP.DefaultExt := FDefaultExt;
      WinXP.InitialDir := FInitialDir;
      WinXP.DefaultExt := FDefaultExt;
      WinXP.Title      := FTitle;

      if WinXP.Execute then
      begin
        FFileName := WinXP.FileName;
        result := true;
      end else
      begin
        FFileName := '';
        result := false;
      end;
    end;

    TxpMode.WinVista:
    begin
      //======= Setup File Tpes ========================
      WinVista.FileTypes.Clear;
      FileTypesList := TStringList.Create;
      try
        ExplodeString('|', fFilter, FileTypesList);
        for c1 := 0 to FileTypesList.Count div 2 - 1 do
        begin
          Index := c1 * 2;
          ft := WinVista.FileTypes.Add;
          ft.DisplayName := FileTypesList[Index];
          ft.FileMask    := FileTypesList[Index+1];
        end;
      finally
        FileTypesList.Free;
      end;
      //================================================
      WinVista.DefaultFolder    := FInitialDir;
      WinVista.DefaultExtension := FDefaultExt;
      WinVista.FileTypeIndex    := FFilterIndex;
      WinVista.FileName         := FFileName;
      WinVista.Title            := FTitle;

      if WinVista.Execute then
      begin
        FFileName := WinVista.FileName;
        result := true;
      end else
      begin
        FFileName := '';
        result := false;
      end;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

{ TxpBrowserSelectDialog }

constructor TxpBrowserSelectDialog.Create(AOwner: TComponent);
begin
  xpMode := GetxpMode;

  Owner := AOwner;
  fFileName := '';
end;

destructor TxpBrowserSelectDialog.Destroy;
begin
  if assigned(WinVista) then WinVista.Free;

  inherited;
end;

function TxpBrowserSelectDialog.Execute: boolean;
var
  //== WinXP ==
  Root, Directory : string;
  //== WinVista ==
begin
  case xpMode of
    TxpMode.WinXP:
    begin
       // Using SelectDirectory()
       // http://stackoverflow.com/a/7422937/395461
       if SelectDirectory('Select a directory', Root, Directory, [sdNewUI]) then
       begin
         fFileName := Directory;
         result := true;
       end else
       begin
         fFileName := '';
         result := false;
       end;
    end;

    TxpMode.WinVista:
    begin
      // Using TFileOpenDialog to select folders:
      // http://stackoverflow.com/a/7422764/395461
      if not assigned(WinVista)
        then WinVista := TFileOpenDialog.Create(Owner);
      WinVista.Options := [fdoPickFolders];
      if WinVista.Execute then
      begin
        fFileName := WinVista.FileName;
        result := true;
      end else
      begin
        fFileName := '';
        result := false;
      end;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

function TxpBrowserSelectDialog.FileName: string;
begin
  result := fFileName;
end;

end.
