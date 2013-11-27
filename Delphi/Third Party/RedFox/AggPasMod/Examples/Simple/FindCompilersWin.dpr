program FindCompilersWin;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}
{$I- }

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,
  Windows,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas';

type
  TSourceKey = record
    Key, Val: string[99];
  end;

const
  CFlipY = True;

  CAppl = 'AggPas';
  CFull = 'AggPas 2.4 RM3 vector graphics library';

  CAggPaths = 'src;src\ctrl;src\platform\win;src\util;src\svg;gpc;expat-wrap';
  CIncPaths = 'src';
  COutPaths = '_debug';

  CDelphiConfig = '-CG -B -H- -W-';
  CFpcConfig = '-Mdelphi -Twin32 -WG -Sg -Se3 -CX -XX -Xs -B -Op3 -v0i';

  CMax = 20;
  CMaxDemos = 100;

  CKeyMax = 99;

var
  GLock, GImage: Boolean;

  GFound, GNumDemos: Cardinal;

  GsearchResults: array [0..CMax - 1] of ShortString;

  GDemos: array [0..CMaxDemos - 1] of string[99];

  GKeyArray: array [0..CKeyMax - 1] of TSourceKey;
  GKeyCount: Cardinal;
  GKeyLastX: Cardinal;
  GKeyScanX: ShortString;

type
  TAggApplication = class;
  TDialog = class;

  TAggFuncAction = function(Appl: TAggApplication; Sender: TDialog): Boolean;

  PUserAction = ^TUserAction;
  TUserAction = record
    Func: TAggFuncAction;
    Ctrl: TAggControlRadioBox;
  end;

  TUserChoice = record
    Ctrl: TAggControlCheckBox;
    Attr: ShortString;
  end;

  TDlgStatus = (dsNone, dsDefine, dsReady, dsWaitingInput, dsRunning);

  TDialog = class
  private
    FAppl: TAggApplication;
    FInfo: PAnsiChar;
    FText: PAnsiChar;
    FTxX, FTxY: Double;
    FAloc, FSize: Cardinal;
    FClri, FClrt: TAggColor;

    FStatus: TDlgStatus;

    FActions: array [0..4] of TUserAction;
    FChoices: array [0..25] of TUserChoice;

    FNumActions, FNumChoices: Cardinal;

    FCurAction: PUserAction;

    FWaiting: TAggFuncAction;
  public
    constructor Create(Appl: TAggApplication; Info: PAnsiChar;
      Clr: PAggColor = nil);
    destructor Destroy; override;

    procedure SetWaiting(Act: TAggFuncAction);

    procedure AddAction(Name: PAnsiChar; Act: TAggFuncAction; X1, Y1, X2, Y2: Double);
    procedure AddChoice(Name, Attr: PAnsiChar; X, Y: Double;
      Status: Boolean = False);

    procedure ChangeText(Text: PAnsiChar; X, Y: Double; Clr: PAggColor = nil);
    procedure AppendText(Text: PAnsiChar);

    function AddControls: Boolean;
    procedure SetNextStatus(Status: TDlgStatus = dsNone);

    function FindCurAction: Boolean;
    function CallCurAction: Boolean;
    procedure CallWaiting;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FDlgWelcome, FDlgSetDrives, FDlgSearching: TDialog;
    FDlgNotFound, FDlgFoundSome: TDialog;

    FCurDlg: TDialog;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;

    FThread: THandle;
    FApplID: LongWord;
    FDoQuit: Boolean;
    FShLast, FDoShow: ShortString;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure DrawText(X, Y: Double; Msg: PAnsiChar; Clr: PAggColor = nil);

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnControlChange; override;
    procedure OnIdle; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;

function NextKey(var Val: ShortString): Boolean;
begin
  Result := False;

  while GKeyLastx < GKeyCount do
  begin
    Inc(GKeyLastx);

    if CompString(GKeyArray[GKeyLastx - 1].Key) = GKeyScanX then
    begin
      Val := GKeyArray[GKeyLastx - 1].Val;

      Result := True;

      Break;
    end;
  end;
end;

function FirstKey(Key: ShortString; var Val: ShortString): Boolean;
begin
  GKeyLastx := 0;
  GKeyScanx := CompString(Key);

  Result := NextKey(Val);
end;

procedure LoadKeys(Buff: PAnsiChar; Size: Integer);
type
  TScan = (sExpectLp, sLoadKey, sLoadVal, sNextLn, sExpectCrLf);

var
  Scan    : TScan;
  Key, Val: ShortString;

  procedure AddKey;
  begin
    if GKeyCount < CKeyMax then
    begin
      GKeyArray[GKeyCount].Key := Key;
      GKeyArray[GKeyCount].Val := Val;

      Inc(GKeyCount);
    end;

    Key := '';
    Val := '';
  end;

begin
  GKeyCount := 0;

  Scan := sExpectLp;
  Key := '';
  Val := '';

  while Size > 0 do
  begin
    case Scan of
      sExpectLp:
        case Buff^ of
          '{':
            Scan := sLoadKey;
        else
          Break;
        end;

      sLoadKey:
        case Buff^ of
          #13, #10:
            Break;

          ':':
            Scan := sLoadVal;

          '}':
            begin
              AddKey;

              Scan := sNextLn;
            end;

        else
          Key := Key + Buff^;
        end;

      sLoadVal:
        case Buff^ of
          #13, #10:
            Break;

          '}':
            begin
              AddKey;

              Scan := sNextLn;
            end;

        else
          Val := Val + Buff^;
        end;

      sNextLn:
        case Buff^ of
          #13, #10:
            Scan := sExpectCrLf;

          ' ':
          else
            Break;
        end;

      sExpectCrLf:
        case Buff^ of
          '{':
            Scan := sLoadKey;

          #13, #10:
          else
            Break;
        end;
    end;

    Dec(Size);
    Inc(Buff);
  end;
end;


{ TDialog }

constructor TDialog.Create(Appl: TAggApplication; Info: PAnsiChar;
  Clr: PAggColor = nil);
begin
  FClri.Black;
  FClrt.Black;

  FAppl := Appl;
  FInfo := Info;
  FText := nil;
  FTxX := 0;
  FTxY := 0;
  FAloc := 0;
  FSize := 0;

  if Clr <> nil then
    FClri := Clr^;

  FStatus := dsDefine;

  FNumActions := 0;
  FNumChoices := 0;

  FCurAction := nil;
  FWaiting := nil;
end;

destructor TDialog.Destroy;
var
  I: Cardinal;
begin
  if FText <> nil then
    AggFreeMem(Pointer(FText), FAloc);

  if FNumActions > 0 then
    for I := 0 to FNumActions - 1 do
      FActions[I].Ctrl.Free;

  if FNumChoices > 0 then
    for I := 0 to FNumChoices - 1 do
      FChoices[I].Ctrl.Free;
  inherited;
end;

procedure TDialog.SetWaiting(Act: TAggFuncAction);
begin
  FWaiting := @Act;
end;

procedure TDialog.AddAction(Name: PAnsiChar; Act: TAggFuncAction; X1, Y1, X2,
  Y2: Double);
begin
  case FStatus of
    dsDefine, dsReady:
      if FNumActions < 5 then
      begin
        FActions[FNumActions].Ctrl := TAggControlRadioBox.Create(X1, Y1, X2,
          Y2, not CFlipY);
        FActions[FNumActions].Ctrl.AddItem(name);

        FActions[FNumActions].Func := @Act;

        Inc(FNumActions);

        SetNextStatus(dsReady);
      end;
  end;
end;

procedure TDialog.AddChoice(Name, Attr: PAnsiChar; X, Y: Double;
  Status: Boolean = False);
begin
  case FStatus of
    dsDefine, dsReady:
      if FNumChoices < 26 then
      begin
        FChoices[FNumChoices].Ctrl := TAggControlCheckBox.Create(X, Y, Name,
          not CFlipY);
        FChoices[FNumChoices].Ctrl.Status := Status;

        FChoices[FNumChoices].Attr := StrPas(Attr) + #0;

        Inc(FNumChoices);
      end;
  end;
end;

procedure TDialog.ChangeText(Text: PAnsiChar; X, Y: Double;
  Clr: PAggColor = nil);
begin
  if StrLen(Text) + 1 > FAloc then
  begin
    AggFreeMem(Pointer(FText), FAloc);

    FAloc := StrLen(Text) + 1;

    AggGetMem(Pointer(FText), FAloc);
  end;

  Move(Text[0], FText^, StrLen(Text) + 1);

  FSize := StrLen(Text);
  FTxX := X;
  FTxY := Y;

  if Clr <> nil then
    FClrt := Clr^;
end;

procedure TDialog.AppendText(Text: PAnsiChar);
var
  NewText: PAnsiChar;
  NewAloc: Cardinal;
begin
  if StrLen(Text) + FSize + 1 > FAloc then
  begin
    NewAloc := StrLen(Text) + FSize + 1;

    AggGetMem(Pointer(NewText), NewAloc);

    Move(FText^, NewText^, FSize);

    AggFreeMem(Pointer(FText), FAloc);

    FAloc := NewAloc;
    FText := NewText;
  end;

  Move(Text[0], PAnsiChar(PtrComp(FText) + FSize)^, StrLen(Text) + 1);

  Inc(FSize, StrLen(Text));
end;

function TDialog.AddControls: Boolean;
var
  I: Cardinal;
begin
  Result := False;

  case FStatus of
    dsReady:
      begin
        FAppl.ControlContainer.Clear;

        if FNumActions > 0 then
          for I := 0 to FNumActions - 1 do
            FAppl.AddControl(FActions[I].Ctrl);

        if FNumChoices > 0 then
          for I := 0 to FNumChoices - 1 do
            FAppl.AddControl(FChoices[I].Ctrl);

        SetNextStatus;

        Result := True;
      end;
  end;
end;

procedure TDialog.SetNextStatus(Status: TDlgStatus = dsNone);
begin
  if Status <> dsNone then
    FStatus := Status
  else
    case FStatus of
      dsDefine:
        FStatus := dsReady;

      dsReady:
        FStatus := dsWaitingInput;

      dsWaitingInput:
        FStatus := dsRunning;
    end;
end;

function TDialog.FindCurAction;
var
  I: Cardinal;
begin
  Result := False;

  case FStatus of
    dsWaitingInput:
      if FNumActions > 0 then
        for I := 0 to FNumActions - 1 do
          if FActions[I].Ctrl.GetCurrentItem = 0 then
          begin
            FCurAction := @FActions[I];

            Result := True;

            Exit;
          end;
  end;
end;

// result of true means, that this was the last call
function TDialog.CallCurAction;
begin
  Result := False;

  case FStatus of
    dsRunning:
      if FCurAction <> nil then
        Result := FCurAction.Func(FAppl, @Self);
  end;
end;

procedure TDialog.CallWaiting;
begin
  if @FWaiting <> nil then
    FWaiting(FAppl, @Self);
end;

procedure CreateDelphi(BatchFile, CompPath, Project: ShortString);
var
  Command: AnsiString;
  Suffix, FilePath, FileName, FileExt: ShortString;
  Df: Text;
begin
  // Compose the units path string
  SpreadName(CompPath, FilePath, FileName, FileExt);

  Command := Dir_str(FilePath);

  SpreadName(Command, FilePath, Suffix, FileExt);

  Suffix := FilePath + 'lib';

  // Compose the command string
  Command := '"' + CompPath + 'dcc32.exe" ';
  Command := Command + '-U"' + Suffix + '";';
  Command := Command + CAggPaths + ' ';
  Command := Command + '-I' + CIncPaths + ' ';
  Command := Command + '-N' + COutPaths + ' ';
  Command := Command + CDelphiConfig + ' ';
  Command := Command + Project;

  // Create the file
  AssignFile(Df, BatchFile);
  Rewrite(Df);
  Writeln(Df, Command);
  Close(Df);
end;

procedure CreateFpc(BatchFile, CompPath, Project: ShortString);
var
  Command: AnsiString;
  Suffix, FilePath, FileName, FileExt: ShortString;
  Df: Text;
begin
  // Compose the units path string
  SpreadName(CompPath, FilePath, FileName, FileExt);

  Command := Dir_str(FilePath);

  SpreadName(Command, FilePath, Suffix, FileExt);

  Command := Dir_str(FilePath);

  SpreadName(Command, FilePath, FileName, FileExt);

  Suffix := FilePath + 'units\' + Suffix;

  // Compose the command string
  Command := '"' + CompPath + 'ppc386.exe" ';
  Command := Command + '-FD"' + Suffix + '" ';
  Command := Command + '-Fu' + CAggPaths + ' ';
  Command := Command + '-Fi' + CIncPaths + ' ';
  Command := Command + '-FU' + COutPaths + ' ';
  Command := Command + CFpcConfig + ' ';
  Command := Command + Project;

  // Create the file
  AssignFile(Df, BatchFile);
  Rewrite(Df);
  Writeln(Df, Command);
  Close(Df);
end;

procedure CreateBatchFiles(Project: ShortString; var Del, Fpc: Cardinal);
var
  I, DelCnt, FpcCnt: Cardinal;

  Batch, BatchPath, CompPath, FilePath, CompName, FileName,
    FileExt: ShortString;

  Df: Text;
begin
  SpreadName(ParamStr(0), BatchPath, FileName, FileExt);

  DelCnt := 1;
  FpcCnt := 1;

  for I := 0 to GFound - 1 do
  begin
    SpreadName(GSearchResults[I], CompPath, CompName, FileExt);
    SpreadName(Project, FilePath, FileName, FileExt);

    if CompString(CompName) = CompString('dcc32') then
    begin
      // Make batch for Delphi
      if DelCnt = 1 then
        Batch := ''
      else
        Str(DelCnt, Batch);

      Batch := 'delphi' + Batch + '-' + FileName;
      Batch := FoldName(BatchPath, Batch, '*.bat');

      CreateDelphi(Batch, CompPath, Project);

      // Make file
      if DelCnt = 1 then
        FileExt := ''
      else
        Str(DelCnt, FileExt);

      FileExt := 'delphi' + FileExt + '_make_all';
      FileName := FoldName(BatchPath, FileExt, '*.bat');

      AssignFile(Df, FileName);

      if Del = 0 then
        Rewrite(Df)
      else
        Append(Df);

      FileExt := 'call "' + Batch + '"';

      Writeln(Df, FileExt);
      Close(Df);

      Inc(DelCnt);
    end
    else
    begin
      // Make batch for FreePascal
      if FpcCnt = 1 then
        Batch := ''
      else
        Str(FpcCnt, Batch);

      Batch := 'fpc' + Batch + '-' + FileName;
      Batch := FoldName(BatchPath, Batch, '*.bat');

      CreateFpc(Batch, CompPath, Project);

      // Make file
      if FpcCnt = 1 then
        FileExt := ''
      else
        Str(FpcCnt, FileExt);

      FileExt := 'fpc' + FileExt + '_make_all';
      FileName := FoldName(BatchPath, FileExt, '*.bat');

      AssignFile(Df, FileName);

      if Fpc = 0 then
        Rewrite(Df)
      else
        Append(Df);

      FileExt := 'call "' + Batch + '"';

      Writeln(Df, FileExt);
      Close(Df);

      Inc(FpcCnt);
    end;
  end;

  Inc(Del, DelCnt - 1);
  Inc(Fpc, FpcCnt - 1);
end;

function ActionConfigure(Appl: TAggApplication;
  Sender: TDialog): Boolean;
var
  I: Cardinal;

  Text: ShortString;
  Rgba: TAggColor;

  Del, Fpc: Cardinal;
begin
  Rgba.FromRgbaDouble(0, 0.5, 0);

  Appl.FDlgSearching.ChangeText('Creating appropriate batch files ...', 10,
    320, @Rgba);
  Appl.ForceRedraw;

  // Setup the final text
  Rgba.FromRgbaDouble(0, 0.5, 0);

  Appl.FDlgFoundSome.ChangeText('', 10, 385, @Rgba);

  for I := 0 to GFound - 1 do
  begin
    Str(I + 1, Text);

    Text := '(' + Text + ')  ' + GSearchResults[I] + #13#0;

    Appl.FDlgFoundSome.AppendText(@Text[1]);
  end;

  // Create the batch files
  if GNumDemos > 0 then
  begin
    Appl.FDlgFoundSome.AppendText
      (#13 + 'Appropriate batch files for compiling the ' + CAppl +
      ' demos were created'#13 +
      'in the directory, from which this helper utility was run.');

    Del := 0;
    Fpc := 0;

    for I := 0 to GNumDemos - 1 do
      CreateBatchFiles(GDemos[I], Del, Fpc);

    if Del > 0 then
      Appl.FDlgFoundSome.AppendText
        (#13#13 + 'Note: For the Delphi compiler, which was found on your '
        + 'system, helper utility assumes, that the system libraries needed '
        + 'for successful compilation are located in the parallel directory'
        + '"..\lib" of the particular Delphi compiler path.');

    if Fpc > 0 then
      Appl.FDlgFoundSome.AppendText
        (#13#13 + 'Note: For the Free Pascal compiler, which was found on your '
        + 'system, helper utility assumes, that the system libraries needed '
        + 'for successful compilation are located in the parallel directory'
        + '"..\units\i386-win32" of the particular Free Pascal compiler path.');

  end
  else
    Appl.FDlgFoundSome.AppendText(#13 + 'NO batch files for compiling the '
      + CAppl + ' demos were created in the directory, from which this '
      + 'helper utility was run, because no *.dpr projects were found.');

  // Refresh
  Appl.ForceRedraw;
end;

function ActionSetDrives(Appl: TAggApplication;
  Sender: TDialog): Boolean;
var
  Letter, Path, Drive: ShortString;
  DriveType, I, Count: Cardinal;
begin
  // Scan for drives in the system
  Letter := 'C';
  Count := 0;

  for I := 1 to 24 do
  begin
    Path := Letter + ':\'#0;
    Drive := '';

    DriveType := GetDriveType(@Path[1]);

    case DriveType of
      DRIVE_FIXED:
        Drive := 'fixed harddrive';
      DRIVE_REMOVABLE:
        Drive := 'removable drive';
      DRIVE_REMOTE:
        Drive := 'network or remote drive';
      DRIVE_CDROM:
        Drive := 'CD-ROM drive';
      DRIVE_RAMDISK:
        Drive := 'RAM disk';
    end;

    if Drive <> '' then
    begin
      Drive := '  ' + StrPas(PAnsiChar(Path[1])) + ' (' + Drive + ')' + #0;

      Appl.FDlgSetDrives.AddChoice(@Drive[1], @Path[1], 30,
        360 - Count * 30, Count = 0);

      Inc(Count);
    end;

    Inc(Byte(Letter[1]));
  end;

  Appl.FCurDlg := Appl.FDlgSetDrives;

  // OK Done
  Result := True;
end;

function ActionWhileSearch(Appl: TAggApplication;
  Sender: TDialog): Boolean;
var
  Text: ShortString;
  Rgba: TAggColor;
begin
  while GLock do;

  GLock := True;

  if Appl.FShLast <> Appl.FDoShow then
  begin
    Str(GFound, Text);

    Text := '  ' + Appl.FDoShow + #13#13 + 'Compilers found: ' + Text + #0;

    // rgba.FromRgbaDouble(0 ,0 ,0.5 );

    Appl.FDlgSearching.ChangeText(@Text[1], 10, 320);
    Appl.ForceRedraw;

    Appl.FShLast := Appl.FDoShow;
  end;

  GLock := False;
end;

function ProcessFile(FileName: ShortString): Boolean;
begin
  if GFound < CMax then
  begin
    GSearchResults[GFound] := FileName;

    Inc(GFound);
  end;
end;

function ScanFiles(Files: ShortString; Appl: TAggApplication): Boolean;
var
  SR : TSearchRec;
  Err: Integer;
  Find, FilePath, FileName, FileExt: ShortString;
begin
  Result := False;

  { Scan dirs and go further }
  SpreadName(Files, FilePath, FileName, FileExt);

  while GLock do;

  GLock := True;

  Appl.FDoShow := FilePath;

  GLock := False;

  Err := SysUtils.FindFirst(Str_dir(FilePath) + '*', FaDirectory, SR);

  while Err = 0 do
  begin
    if Appl.FDoQuit then
    begin
      SysUtils.FindClose(SR);

      Exit;
    end;

    if (SR.Name <> '.') and (SR.Name <> '..') and
      (SR.Attr and FaDirectory = FaDirectory) then
    begin
      SpreadName(Files, FilePath, FileName, FileExt);

      if not ScanFiles(FoldName(Str_dir(FilePath) + SR.Name + '\', FileName,
        FileExt), Appl) then
        Exit;
    end;

    Err := SysUtils.FindNext(SR);
  end;

  SysUtils.FindClose(SR);

  { Scan files for Delphi compiler }
  Find := FoldName(FilePath, 'dcc32', '*.exe');

  Err := SysUtils.FindFirst(Find, FaArchive, SR);

  while Err = 0 do
  begin
    if Appl.FDoQuit then
    begin
      SysUtils.FindClose(SR);

      Exit;
    end;

    ProcessFile(FoldName(Files, SR.Name, SR.Name));

    Err := SysUtils.FindNext(SR);
  end;

  SysUtils.FindClose(SR);

  { Scan files for FPC compiler }
  Find := FoldName(FilePath, 'ppc386', '*.exe');

  Err := SysUtils.FindFirst(Find, FaArchive, SR);

  while Err = 0 do
  begin
    if Appl.FDoQuit then
    begin
      SysUtils.FindClose(SR);

      Exit;
    end;

    ProcessFile(FoldName(Files, SR.Name, SR.Name));

    Err := SysUtils.FindNext(SR);
  end;

  SysUtils.FindClose(SR);

  { OK }
  ScanFiles := True;
end;

procedure FnSearch(Appl: TAggApplication);
var
  I: Cardinal;

begin
  Appl.FShLast := '';
  Appl.FDoShow := '';

  GFound := 0;

  // OK, Go through selected drives and issue search
  Appl.FDlgSearching.SetWaiting(@ActionWhileSearch);

  if Appl.FDlgSetDrives.FNumChoices > 0 then
    for I := 0 to Appl.FDlgSetDrives.FNumChoices - 1 do
      if Appl.FDlgSetDrives.FChoices[I].Ctrl.Status then
        if not ScanFiles(Appl.FDlgSetDrives.FChoices[I].Attr, Appl) then
          Break;

  Appl.FDlgSearching.SetWaiting(nil);

  // Were we forced to quit ?
  if Appl.FDoQuit then
    NoP;

  // Depending on the search result activate the next user dialog
  if GFound > 0 then
  begin
    ActionConfigure(Appl, nil);

    Appl.FCurDlg := Appl.FDlgFoundSome;

  end
  else
    Appl.FCurDlg := Appl.FDlgNotFound;
end;

function ThSearch(Parameter: Pointer): Integer;
begin
  { Synchronize }
  while TAggApplication(Parameter).FThread = 0 do;

  { Call Thread }
  FnSearch(Parameter);

  { Exit }
  TAggApplication(Parameter).FThread := 0;
  TAggApplication(Parameter).FApplID := 0;

  { Done }
  EndThread(0);
end;

function ActionBeginSearch(Appl: TAggApplication;
  Sender: TDialog): Boolean;
var
  I: Cardinal;
begin
  Result := False;

  // Check, if we have drives to search
  if Appl.FDlgSetDrives.FNumChoices > 0 then
    for I := 0 to Appl.FDlgSetDrives.FNumChoices - 1 do
      if Appl.FDlgSetDrives.FChoices[I].Ctrl.Status then
      begin
        Result := True;

        Break;
      end;

  if not Result then
  begin
    Appl.FDlgSetDrives.FActions[0].Ctrl.SetCurrentItem(-1);
    Appl.FDlgSetDrives.SetNextStatus(dsWaitingInput);
    Appl.ForceRedraw;

    Exit;
  end;

  // Go on to search dialog
  Appl.FCurDlg := Appl.FDlgSearching;

  // Start Up the search thread
  Appl.FThread := BeginThread(nil, 65536, ThSearch, Appl, 0, Appl.FApplID);
end;

function ActionStopSearch(Appl: TAggApplication;
  Sender: TDialog): Boolean;
begin
  Appl.FDoQuit := True;
end;

function ActionExit(Appl: TAggApplication; Sender: TDialog): Boolean;
begin
  Appl.Quit;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  Rgba: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  FCurDlg := nil;

  FThread := 0;
  FApplID := 0;
  FDoQuit := False;
  FShLast := '';
  FDoShow := '';

  // Welcome dialog
  FDlgWelcome := TDialog.Create(Self, 'Welcome to the ' + CFull + '.'#13 + ''#13 +
    'This helper utility will scan your system to search'#13 +
    'for all available Object Pascal compilers.'#13 + ''#13 +
    'It will also create appropriate batch files with current'#13 +
    'paths and options needed to compile properly all'#13 + 'the ' + CAppl +
    ' demos.'#13 + ''#13 +
    'Currently Delphi and Free Pascal compilers are supported.');

  FDlgWelcome.AddAction('Continue', @ActionSetDrives, 480, 15, 580, 45);

  // Set drives to search on dialog
  FDlgSetDrives := TDialog.Create(@Self,
    'Please select, on which drives of your system should'#13 +
    'this helper utility perform search for Object Pascal compilers:');

  FDlgSetDrives.AddAction('Continue', @ActionBeginSearch, 480,
    15, 580, 45);

  // Wait, searching dialog
  FDlgSearching := TDialog.Create(@Self, 'Please wait ...'#13 + ''#13 +
    'Helper utility is searching for Object Pascal compilers'#13 +
    'on the drives, you have selected.');

  FDlgSearching.AddAction('Stop searching', @ActionStopSearch, 440, 15,
    580, 45);

  // Found nothing dialog
  Rgba.FromRgbaInteger(255, 0, 0);

  FDlgNotFound := TDialog.Create(@Self,
    'I am sorry, but NO Object Pascal compilers were found'#13 +
    'on your system.'#13 + ''#13 + 'Please install Delphi or FreePascal'#13 +
    'and then rerun this utility.'#13#13 + 'http://www.borland.com'#13#13 +
    '- or - '#13#13 + 'http://www.freepascal.org', @Rgba);

  FDlgNotFound.AddAction('Exit', @ActionExit, 500, 15, 580, 45);

  // Compilers found dialog
  Rgba.FromRgbaDouble(0, 0.5, 0);

  FDlgFoundSome := TDialog.Create(@Self,
    'FolLowing Object Pascal compilers were found your system:', @Rgba);

  FDlgFoundSome.AddAction('Exit', @ActionExit, 500, 15, 580, 45);
end;

destructor TAggApplication.Destroy;
begin
  while FThread <> 0 do
    FDoQuit := True;

  inherited;

  FScanLine.Free;
  FRasterizer.Free;

  FDlgWelcome.Free;
  FDlgSetDrives.Free;
  FDlgSearching.Free;
  FDlgNotFound.Free;
  FDlgFoundSome.Free;
end;

procedure TAggApplication.DrawText(X, Y: Double; Msg: PAnsiChar;
  Clr: PAggColor = nil);
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Txt : TAggGsvText;
  Pt: TAggConvStroke;
begin
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      Txt := TAggGsvText.Create;
      Txt.SetSize(9.5);
      Txt.LineSpace := 10;

      Pt := TAggConvStroke.Create(Txt);
      Pt.Width := 1.2;

      Txt.SetStartPoint(X, Y);
      Txt.SetText(Msg);

      if Clr <> nil then
        RenScan.SetColor(Clr)
      else
        RenScan.SetColor(CRgba8Black);

      FRasterizer.AddPath(Pt);
      RenderScanLines(FRasterizer, FScanLine, RenScan);

      Txt.Free;
      Pt.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnInit;
var
  SR : TSearchRec;
  Err: Integer;

  Find, FilePath, FileName, FileExt: ShortString;

  Cf: file;
  Bf: Pointer;
  Sz: Integer;

  Target, Get: ShortString;

begin
  WaitMode := False;

  // Load the list of current projects
  GNumDemos := 0;

  SpreadName(ParamStr(0), FilePath, FileName, FileExt);

  Find := FoldName(FilePath, '*', '*.dpr');
  Err := SysUtils.FindFirst(Find, FaArchive, SR);

  while Err = 0 do
  begin
    // Load keys from the source file
    GKeyCount := 0;

    Get := FoldName(FilePath, SR.Name, SR.Name);

    AssignFile(Cf, SR.Name);
    Reset(Cf, 1);

    if IOResult = 0 then
    begin
      Sz := System.FileSize(Cf);

      if AggGetMem(Bf, Sz) then
      begin
        Blockread(Cf, Bf^, Sz);
        LoadKeys(Bf, Sz);
        AggFreeMem(Bf, Sz);
      end;

      Close(Cf);
    end;

    Target := 'win';

    FirstKey('target', Target);

    // Add To List
    if (CompString(Target) <> CompString('win')) or FirstKey('skip', Get) then

    else if GNumDemos < CMaxDemos then
    begin
      GDemos[GNumDemos] := FoldName('', SR.Name, SR.Name);

      Inc(GNumDemos);
    end;

    Err := SysUtils.FindNext(SR);
  end;

  SysUtils.FindClose(SR);
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  I, Plus: Cardinal;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Render Dialog
      if FCurDlg <> nil then
        case FCurDlg.FStatus of
          dsWaitingInput, dsRunning:
            begin
              // Render logo if has one
              Plus := 0;

              if (FCurDlg = FDlgWelcome) and GImage then
              begin
                RendererBase.CopyFrom(RenderingBufferImage[1], nil, 6, 330);

                Plus := RenderingBufferImage[1].Height + 20;
              end;

              // Render base text
              DrawText(10, 420 - Plus, FCurDlg.FInfo, @FCurDlg.FClri);

              // Render dynamic text
              if FCurDlg.FText <> nil then
                DrawText(FCurDlg.FTxX, FCurDlg.FTxY,
                  PAnsiChar(FCurDlg.FText), @FCurDlg.FClrt);

              // Render choices
              if FCurDlg.FNumChoices > 0 then
                for I := 0 to FCurDlg.FNumChoices - 1 do
                  RenderControl(FRasterizer, FScanLine, RenScan,
                    FCurDlg.FChoices[I].Ctrl);

              // Render actions
              if FCurDlg.FNumActions > 0 then
                for I := 0 to FCurDlg.FNumActions - 1 do
                  RenderControl(FRasterizer, FScanLine, RenScan,
                    FCurDlg.FActions[I].Ctrl);
            end;
        end;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnControlChange;
begin
  if FCurDlg <> nil then
    case FCurDlg.FStatus of
      dsWaitingInput:
        if FCurDlg.FindCurAction then
          FCurDlg.SetNextStatus;
    end;
end;

procedure TAggApplication.OnIdle;
begin
  if FCurDlg = nil then
  begin
    FCurDlg := FDlgWelcome;

    if FCurDlg.FStatus <> dsReady then
      FCurDlg := nil;
  end
  else
    case FCurDlg.FStatus of
      dsReady:
        if FCurDlg.AddControls then
          ForceRedraw;

      dsWaitingInput:
        FCurDlg.CallWaiting;

      dsRunning:
        if FCurDlg.CallCurAction then
          NoP;
    end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This is just an AggPas library helper utility which has '
      + 'nothing to do with demonstrating any of graphical possibilities of '
      + 'AGG.'#13#13
      + 'Author of this pascal port (Milano) recomends to proceed with this '
      + 'utility on your system right after unpacking the archive, because '
      + 'it will scan your computer for all available Object Pascal compilers '
      + 'and it will create the up-to-date working batch files for fompiling '
      + 'the library demos.'#13#13
      + 'In the welcome screen of this utility, there is a logo for the AGG '
      + 'library, which was designed and proposed by Milano. It has the '
      + 'meaning of spiral primitive upon the interactive polygon control, '
      + 'which should mean in "translation" that "With AGG the possibilities '
      + 'are endless (the spiral) and custom adjustments are easy possible. '
      + '(interactive polygon)".'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  GLock := False;
  GImage := False;

  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := CAppl + ' Startup utility (F1-Help)';

    if LoadImage(1, 'aggpas_logo') then
      GImage := True;

    if Init(600, 450, []) then
      Run;
  finally
    Free;
  end;
end.
