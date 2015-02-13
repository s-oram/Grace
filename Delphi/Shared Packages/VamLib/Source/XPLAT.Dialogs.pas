unit XPLAT.Dialogs;

interface

uses
  Windows,
  Vcl.Dialogs,
  System.Classes;

{$SCOPEDENUMS ON}

{.$DEFINE ForceWinXPMode}

type
  TxpMode = (WinXP, WinVista);

  TxpFileSaveDialog = class
  private
    FFileName: string;
    FFilterIndex: Integer;
    FDefaultExt: string;
    FTitle: string;
    FFilter: string;
    FInitialDir: string;
  protected
    FOwner : TComponent;
    xpMode   : TxpMode;
    WinXP    : TSaveDialog;
    WinVista : TFileSaveDialog;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Execute(ParentWnd: HWND):boolean;

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

  TxpFileOpenDialog = class
  private
    FFileName: string;
    FFilterIndex: Integer;
    FDefaultExt: string;
    FFilter: string;
    FInitialDir: string;
    FTitle: string;
  protected
    FOwner : TComponent;
    xpMode   : TxpMode;
    WinXP    : TOpenDialog;
    WinVista : TFileOpenDialog;
    procedure EventHandle_WinVistaSelectionChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Execute(ParentWnd: HWND):boolean;

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

  TxpDirectorySelectDialog = class
  private
    FOwner : TComponent;
    xpMode   : TxpMode;
    WinVista : TFileOpenDialog;
    fDirectoryFilePath : string;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Execute(const ParentWnd: HWND; const InitialDirectory : string = ''):boolean;

    function FileName : string; deprecated; //use DirectoryFilePath().
    function DirectoryFilePath : string;
  end;


  TxpInputBox = class   // TODO:MED replace usage of this with input dialog.
  private
    //Owner : TComponent;
    //xpMode   : TxpMode;
    fInitialValue: string;
    fPrompt: string;
    fCaption: string;
    fResultText: string;
  protected
    FOwner : TComponent;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function Execute(ParentWnd: HWND):boolean;

    property ResultText : string read fResultText;

    property Caption : string read fCaption write fCaption;
    property Prompt  : string read fPrompt  write fPrompt;
    property InitialValue : string read fInitialValue write fInitialValue;
  end;



// NOTE: All dialogs require ParentWnd to be a non-nil value to open correctly in some
// VST hosts. If ParentWnd is zero, the dialog can open behind the main host window,
// effectively giving the appearence of a frozen host.
// As far as I know, AOwner isn't required and can be nil.

type
  TDirectorySelectResult = record
    IsOk : boolean; // returns true if user selected a directory. False if user canceled.
    Dir  : string;  // directory selected  by user.
  end;
function DirectorySelectDialog(const ParentWnd : HWND; const InitialDirectory : string = ''):TDirectorySelectResult;

implementation

uses
  Controls,
  System.UITypes,
  SysUtils,
  Vcl.FileCtrl,
  VamLib.WinUtils,
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
    //TODO:MED need to check for Windows 8.
    result := TxpMode.WinVista;
  end;
end;


{ TxpFileOpenDialog }

constructor TxpFileOpenDialog.Create(AOwner: TComponent);
begin
  {$IFDEF ForceWinXPMode}
    xpMode := TxpMode.WinXP;
  {$ELSE}
    xpMode := GetxpMode;
  {$ENDIF}

  FOwner := AOwner;

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

procedure TxpFileOpenDialog.EventHandle_WinVistaSelectionChange(Sender: TObject);
var
  x : string;
  Dialog : TFileOpenDialog;
begin
  Dialog := (Sender as TFileOpenDialog);
  x :=  Dialog.FileName;
  if FileExists(x) then
  begin
    // TODO:MED Enable the dialog open button here.
  end else
  begin
   // TODO:MED disable the dialog open button here.
  end;
end;

function TxpFileOpenDialog.Execute(ParentWnd: HWND):boolean;
var
  FileTypesList : TStringList;
  c1: Integer;
  Index : integer;
  ft : TFileTypeItem;
  FilterTypesProcessed : string;
begin
  if ParentWnd = 0 then raise Exception.Create('ParentWnd cannot be 0.');

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
      WinXP.Options    := [TOpenOption.ofFileMustExist];

      if WinXP.Execute(ParentWnd) then
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
      WinVista.OnSelectionChange := self.EventHandle_WinVistaSelectionChange;
      WinVista.Options := [TFileDialogOption.fdoFileMustExist];
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

      if WinVista.Execute(ParentWnd) then
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

constructor TxpDirectorySelectDialog.Create(AOwner: TComponent);
begin
  {$IFDEF ForceWinXPMode}
    xpMode := TxpMode.WinXP;
  {$ELSE}
    xpMode := GetxpMode;
  {$ENDIF}

  FOwner := AOwner;
  fDirectoryFilePath:= '';
end;

destructor TxpDirectorySelectDialog.Destroy;
begin
  if assigned(WinVista) then WinVista.Free;

  inherited;
end;

function TxpDirectorySelectDialog.DirectoryFilePath: string;
begin
  result := fDirectoryFilePath;
end;

function TxpDirectorySelectDialog.Execute(const ParentWnd: HWND; const InitialDirectory : string = ''):boolean;
var
  //== WinXP ==
  Root, Directory : string;
  //== WinVista ==
begin
  fDirectoryFilePath := '';
  if ParentWnd = 0 then raise Exception.Create('ParentWnd cannot be 0.');

  case xpMode of
    TxpMode.WinXP:
    begin
      Root := InitialDirectory;

      // Using SelectDirectory()
      // http://stackoverflow.com/a/7422937/395461
      if SelectDirectory('Select a directory', Root, Directory, [sdNewUI]) then
      begin
        fDirectoryFilePath := Directory;
        result := true;
      end else
      begin
        fDirectoryFilePath := '';
        result := false;
      end;
    end;

    TxpMode.WinVista:
    begin
      // Using TFileOpenDialog to select folders:
      // http://stackoverflow.com/a/7422764/395461
      if not assigned(WinVista) then WinVista := TFileOpenDialog.Create(FOwner);

      WinVista.DefaultFolder := InitialDirectory;
      WinVista.Options := [fdoPickFolders];

      if WinVista.Execute(ParentWnd) then
      begin
        fDirectoryFilePath := WinVista.FileName;
        result := true;
      end else
      begin
        fDirectoryFilePath := '';
        result := false;
      end;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

function TxpDirectorySelectDialog.FileName: string;
begin
  result := fDirectoryFilePath;
end;

{ TxpFileSaveDialog }

constructor TxpFileSaveDialog.Create(AOwner: TComponent);
begin
  {$IFDEF ForceWinXPMode}
    xpMode := TxpMode.WinXP;
  {$ELSE}
    xpMode := GetxpMode;
  {$ENDIF}

  FOwner := AOwner;

  FFileName   := '';
  FDefaultExt := '';
  FFilter     := '';
  FInitialDir := '';
  FDefaultExt := '';
  FTitle      := 'Save';
end;

destructor TxpFileSaveDialog.Destroy;
begin
  if assigned(WinXP)    then WinXP.Free;
  if assigned(WinVista) then WinVista.Free;
  inherited;
end;

function TxpFileSaveDialog.Execute(ParentWnd: HWND): boolean;
var
  FileTypesList : TStringList;
  c1: Integer;
  Index : integer;
  ft : TFileTypeItem;
  FilterTypesProcessed : string;
begin
  if ParentWnd = 0 then raise Exception.Create('ParentWnd cannot be 0.');

  case xpMode of
    TxpMode.WinXP:
    begin
      if not assigned(WinXP)
        then WinXP := TSaveDialog.Create(FOwner);

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

      // ParentWnd is ignored here.
      // http://stackoverflow.com/a/14502932/395461
      if WinXP.Execute(ParentWnd) then
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
      if not assigned(WinVista)
        then WinVista := TFileSaveDialog.Create(FOwner);

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

      if WinVista.Execute(ParentWnd) then
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

{ TxpInputBox }

constructor TxpInputBox.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  fResultText := '';
end;

destructor TxpInputBox.Destroy;
begin

  inherited;
end;

function TxpInputBox.Execute(ParentWnd: HWND):boolean;
var
  s: string;
begin
  if ParentWnd = 0 then raise Exception.Create('ParentWnd cannot be 0.');

  s := InitialValue;
  if InputQuery(Caption, Prompt, s) then
  begin
    fResultText := s;
    result := true;
  end else
  begin
    fResultText := '';
    result := false;
  end;

end;

function DirectorySelectDialog(const ParentWnd : HWND; const InitialDirectory : string = ''):TDirectorySelectResult;
var
  Dialog : TxpDirectorySelectDialog;
begin
  Dialog := TxpDirectorySelectDialog.Create(nil);
  try
    if Dialog.Execute(ParentWnd, InitialDirectory) then
    begin
      result.IsOk := true;
      result.Dir  := Dialog.DirectoryFilePath;
    end else
    begin
      result.IsOk := false;
      result.Dir  := '';
    end;
  finally
    Dialog.Free;
  end;
end;

end.
