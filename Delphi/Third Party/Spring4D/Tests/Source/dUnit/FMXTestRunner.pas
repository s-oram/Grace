unit FMXTestRunner;

interface

{$IFDEF MSWINDOWS}
  {$DEFINE DESKTOP}
{$ENDIF}
{$IFDEF MACOS}
  {$IFNDEF IOS}
    {$DEFINE DESKTOP}
  {$ENDIF}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.Variants,
  System.UITypes,
  System.Classes,
  System.Actions,
  System.Rtti,
  System.IOUtils,
  System.IniFiles,
  FMX.Forms,
  FMX.Types,
  FMX.Controls,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ActnList,
  FMX.Layouts,
  FMX.Memo,
  FMX.TabControl,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Effects,
  TestFramework,
  DUnitConsts;

type
  TRunnerExitBehavior = (
    rxbContinue,
    rxbPause,
    rxbHaltOnFailures);

  TFMXTestRunner = class(TForm, ITestListener, ITestListenerX)
    Header: TToolBar;
    HeaderLabel: TLabel;
    ActionList1: TActionList;
    RunTests: TAction;
    StopTests: TAction;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Memo: TMemo;
    TabItem2: TTabItem;
    ToolBar1: TToolBar;
    cmdRun: TButton;
    cmdSettings: TButton;
    cmdBack: TButton;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    chkBreakOnFailures: TListBoxItem;
    chkFailIfNoChecksExecuted: TListBoxItem;
    chkFailIfMemoryLeaked: TListBoxItem;
    ChangeTab2: TChangeTabAction;
    ChangeTab1: TChangeTabAction;
    chkIngnoreLeaksInSetupTeardown: TListBoxItem;
    TestTree: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    Panel1: TPanel;
    OverflowMenu: TListBox;
    mnuSelectAll: TListBoxItem;
    mnuUnselectAll: TListBoxItem;
    mnuRunSelected: TListBoxItem;
    tmrPopup: TTimer;
    ShadowEffect1: TShadowEffect;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    StyleBookAndoird: TStyleBook;
    StyleBookIOS7: TStyleBook;
    StyleBookWindows: TStyleBook;
    StyleBookIOS6: TStyleBook;
    procedure RunTestsExecute(Sender: TObject);
    procedure ListBoxItemCheckInvert(Sender: TObject);
    procedure ChangeTabUpdate(Sender: TObject);
    procedure RunTestsUpdate(Sender: TObject);
    procedure ChangeTab2Update(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrPopupTimer(Sender: TObject);
    procedure TestTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TestTreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuUnselectAllClick(Sender: TObject);
    procedure TestTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure mnuRunSelectedClick(Sender: TObject);
  strict private
    FMenuPos: TPointF;
  private const
    SIniSection = 'FMXTestRunner Config';
    SStylePass = 'ItemPass';
    SStyleFail = 'ItemFail';
    SStyleError = 'ItemError';
  protected
    class var Suite: ITest;
    procedure SetChecked(const Parent : TTreeViewItem; Value : Boolean;
      Recursive : Boolean = false);
    procedure HideOverflowMenu;
    function TestToItem(const Test : ITest) : TTreeViewItem; inline;
    procedure SetTreeItemStyle(Item : TTreeViewItem; const StyleName : string);
  protected
    startTime: TDateTime;
    endTime: TDateTime;
    runTime: TDateTime;
    FRunning: Boolean;
    FTestResult: TTestResult;
    FErrorCount: Integer;
    FFailureCount: Integer;
    FIniName: string;

    procedure Setup(const Base : ITest);
    procedure SetupTree;
    procedure RunTheTest(aTest: ITest);
    procedure EnableUI(Enable: Boolean);
    procedure ClearResult;
    procedure ClearFailureMessage;
  public
    // implement the ITestListener interface
    procedure AddSuccess(Test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function ShouldRunTest(Test: ITest): Boolean; virtual;
    procedure StartSuite(Suite: ITest); virtual;
    procedure EndSuite(Suite: ITest); virtual;
    procedure StartTest(Test: ITest); virtual;
    procedure EndTest(Test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(Test: ITest; const Msg: string);
    procedure Warning(Test: ITest; const Msg: string);
    function Report(r: TTestResult): string;
    procedure AfterConstruction; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single;
      Y: Single); override;

    property testResult: TTestResult read FTestResult Write FTestResult;

    class procedure RunTest(Suite: ITest;
      exitBehavior: TRunnerExitBehavior = rxbContinue); overload;
    class procedure RunRegisteredTests(
      exitBehavior : TRunnerExitBehavior = rxbContinue);
  protected
    function AddSingularOrPlural(const SingularWithoutS: string; const Value: Integer): string; virtual;
    procedure AddStatus(const aStatusStyle, aStatusText: string; aStatus: TTestFailure); virtual;
    function PrintErrors(r: TTestResult): string; virtual;
    function PrintFailures(r: TTestResult): string; virtual;
    function PrintHeader(r: TTestResult): string; virtual;
    function PrintFailureItems(r: TTestResult): string; virtual;
    function PrintErrorItems(r: TTestResult): string; virtual;
    function TruncateString(s: string; len: Integer): string; virtual;
    procedure Write(const s: string);
    procedure Writeln(const s: string = '');
    procedure FillTestTree(const RootNode: TTreeViewItem; const aTest: ITest);
    function PrintStatusItem(const print: string; const index: Integer; const status: TTestFailure): string; virtual;
  end;

  { : This type defines what the RunTest and RunRegisteredTests methods will do when
    testing has ended.
    @enum rxbContinue Just return the TestResult.
    @enum rxbPause    Pause with a ReadLn before returnng the TestResult.
    @enum rxbHaltOnFailures   Halt the program if errors or failures occurred, setting
    the program exit code to FailureCount+ErrorCount;
    behave like rxbContinue if all tests suceeded.
    @seeAlso <See Unit="TextTestRunner" Routine="RunTest">
    @seeAlso <See Unit="TextTestRunner" Routine="RunRegisteredTests">
  }

  { : Run the given Test suite }
procedure RunTest(Suite: ITest;
  exitBehavior: TRunnerExitBehavior = rxbContinue); overload;
procedure RunRegisteredTests(
  exitBehavior : TRunnerExitBehavior = rxbContinue); overload;

implementation

uses
  System.Math;

{$R *.fmx}

type
  TListBoxItemHelper = class helper for TListBoxItem
  private
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
  public
    property Checked: Boolean read GetChecked Write SetChecked;
  end;

  TTestTreeItem = class(TTreeViewItem)
  private
    FData: TValue;
  protected
    procedure SetData(const Value: TValue); override;
    function GetData: TValue; override;
  end;

  { TFMXTestRunner }

procedure TFMXTestRunner.AddSuccess(Test: ITest);
begin
  if (IsTestMethod(Test)) then
    SetTreeItemStyle(TestToItem(Test), SStylePass);
end;

procedure TFMXTestRunner.AfterConstruction;
{$IFDEF DESKTOP}
var
  LHeight : Single;
{$ENDIF}
begin
  inherited;
  ListBoxItem2.Text:='dUnit FireMonkey test runner'#$D#$A +
    'Usage:'#$D#$A +
    '  Touch and hold any test tree item to show popup menu'#$D#$A +
    '  Test settings is saved before any test are started after touching the start button.'#$D#$A +
    '  Note that test settings is not persisted on iOS Device but is on Andorid or iOS Simulator';

  FIniName := 'dUnit.ini';

  // On some platforms, design items could flick before the tree is recreated
  // items
  TreeViewItem1 := nil;
  TreeViewItem2 := nil;
  TreeViewItem3 := nil;
  TreeViewItem4 := nil;
  TestTree.Clear;

{$IF Defined(MSWINDOWS)}
  FIniName := ExtractFilePath(ParamStr(0)) + FIniName;
  StyleBook := StyleBookWindows;
{$ELSEIF Defined(MACOS) AND Defined(DESKTOP)}
  // Deployment would delete our INI, rather place it under documents
  FIniName := System.IOUtils.TPath.Combine(
    System.IOUtils.TPath.GetDocumentsPath, FIniName);
  // MAC OS shares the same as Windows both default styles appear to be the same
  StyleBook := StyleBookWindows;
{$ELSEIF Defined(ANDROID)}
  // Note that this requires Read/Write External Storage permissions
  // Write permissions option is enough, reading will be available as well
  //
  // Use shared path since private one will be cleared upon application
  // reinstallation - oops this one as well
  // FIniName := TPath.Combine(TPath.GetSharedDocumentsPath, FIniName);
  // Use the global SDCard path
  FIniName := System.IOUtils.TPath.Combine('/storage/emulated/0', FIniName);
  StyleBook := StyleBookAndoird;
{$ELSEIF Defined(IOS)}
  FIniName := System.IOUtils.TPath.Combine(
    System.IOUtils.TPath.GetDocumentsPath, FIniName);
  if (TOSVersion.Check(7)) then
    StyleBook := StyleBookIOS7
  else
    StyleBook := StyleBookIOS6;
{$ENDIF}
{$IFDEF DESKTOP}
  // There is some layouting differences on desktop
  LHeight := OverflowMenu.Height;
  // Realings content based on style
  OverflowMenu.ApplyStyleLookup;
  // Fix height to designed
  OverflowMenu.Height := OverflowMenu.Height + (LHeight - OverflowMenu.ClientHeight);
{$ENDIF}
end;

procedure TFMXTestRunner.ChangeTab2Update(Sender: TObject);
var
  Action: TChangeTabAction;
begin
  Action := Sender as TChangeTabAction;
  Action.Enabled := Action.Supported and (not FRunning) and
    Assigned(Action.Tab) and
  // Assigned(Action.Tab.TabControl) and
    (TabControl1.TabIndex <> Action.Tab.Index);
end;

procedure TFMXTestRunner.ChangeTabUpdate(Sender: TObject);
var
  Action: TChangeTabAction;
begin
  Action := Sender as TChangeTabAction;
  Action.Enabled := Action.Supported and (not FRunning) and
    Assigned(Action.Tab) and
    { Assigned(Action.Tab.TabControl) and }
    (TabControl1.TabIndex <> Action.Tab.Index);
  Action.Visible := Action.Enabled;
end;

procedure TFMXTestRunner.ClearFailureMessage;
begin
  Memo.Lines.Clear;
end;

procedure TFMXTestRunner.ClearResult;
begin
  if FTestResult <> nil then
  begin
    FTestResult.Free;
    FTestResult := nil;
  end;
  ClearFailureMessage;
end;

procedure TFMXTestRunner.AddError(error: TTestFailure);
begin
  AddStatus(SStyleError, 'E', error);
end;

procedure TFMXTestRunner.AddFailure(failure: TTestFailure);
begin
  AddStatus(SStyleFail, 'F', failure);
end;

{ : Prints failures to the standard output }
function TFMXTestRunner.Report(r: TTestResult): string;
begin
  Result := PrintHeader(r) + PrintErrors(r) + PrintFailures(r);
end;

{ : Prints the errors to the standard output }
function TFMXTestRunner.PrintErrors(r: TTestResult): string;
begin
  Result := '';
  if (r.errorCount <> 0) then
  begin
    Result := Result + AddSingularOrPlural('error', r.FailureCount) + sLineBreak;
    Result := Result + PrintErrorItems(r);
    Result := Result + sLineBreak
  end
end;

function TFMXTestRunner.PrintFailureItems(r: TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  Result := '';
  for i := 0 to r.FailureCount - 1 do
  begin
    failure := r.Failures[i];
    Result := PrintStatusItem(Result, i, failure);
  end;
end;

function TFMXTestRunner.PrintErrorItems(r: TTestResult): string;
var
  i: Integer;
  error: TTestFailure;
begin
  Result := '';
  for i := 0 to r.errorCount - 1 do
  begin
    error := r.Errors[i];
    Result := PrintStatusItem(Result, i, error);
  end;
end;

{ : Prints failures to the standard output }
function TFMXTestRunner.PrintFailures(r: TTestResult): string;
begin
  Result := '';
  if (r.FailureCount <> 0) then
  begin
    Result := Result + AddSingularOrPlural('failure', r.FailureCount) + sLineBreak;
    Result := Result + PrintFailureItems(r);
    Result := Result + sLineBreak
  end
end;

{ : Prints the ClassName of the Report }
function TFMXTestRunner.PrintHeader(r: TTestResult): string;
begin
  Result := '';
  if r.wasSuccessful then
  begin
    Result := Result + sLineBreak;
    Result := Result + Format('OK: %d tests' + sLineBreak, [r.runCount]);
  end
  else
  begin
    Result := Result + sLineBreak;
    Result := Result + 'FAILURES!!!' + sLineBreak;
    Result := Result + 'Test Results:' + sLineBreak;
    Result := Result + Format('Run:      %8d' + sLineBreak + 'Failures: %8d' +
      sLineBreak + 'Errors:   %8d' + sLineBreak, [r.runCount, r.FailureCount,
      r.errorCount]);
  end
end;

procedure TFMXTestRunner.StartTest(Test: ITest);
begin
  Self.Write('.');
  Application.ProcessMessages;
end;

procedure TFMXTestRunner.EndTest(Test: ITest);
begin
  Application.ProcessMessages;
end;

procedure TFMXTestRunner.FillTestTree(const RootNode: TTreeViewItem;
  const aTest: ITest);
var
  TestTests: IInterfaceList;
  i: Integer;
  NewNode: TTreeViewItem;
  NewTest: ITest;
begin
  if aTest = nil then
    Exit;

  NewNode := TTestTreeItem.Create(Self);
  if (RootNode = nil) then
  begin
    NewNode.Parent := TestTree;
  end
  else
  begin
    NewNode.Parent := RootNode;
  end;
  NewNode.data := TValue.From(aTest);
  NewNode.Text := aTest.name;
  NewNode.IsChecked := aTest.Enabled;
  aTest.GUIObject := NewNode;

  TestTests := aTest.Tests;
  if (TestTests.Count > 0) then
  begin
    if (RootNode <> nil) then
      RootNode.Expand;
    for i := 0 to TestTests.Count - 1 do
    begin
      NewTest := TestTests[i] as ITest;
      FillTestTree(NewNode, NewTest);
    end;
  end
  else if (RootNode <> nil) then
    RootNode.Collapse;
  if (not aTest.Enabled) then
    NewNode.Collapse;
end;

procedure TFMXTestRunner.FormShow(Sender: TObject);
begin
  SetupTree;
end;

procedure TFMXTestRunner.HideOverflowMenu;
begin
  OverflowMenu.Visible := False;
end;

procedure TFMXTestRunner.ListBoxItemCheckInvert(Sender: TObject);
var
  Item: TListBoxItem;
begin
  Item := (Sender as TListBoxItem);
  if (Item.Enabled) then
    Item.Checked := not Item.Checked;
end;

procedure TFMXTestRunner.mnuRunSelectedClick(Sender: TObject);
var
  Suite : ITest;
begin
  HideOverflowMenu;
  Suite := TestTree.Selected.data.AsInterface as ITest;
  Setup(Suite);
  // Enable currently selected test even if it was previously disabled,
  // if complete suite is ran, it will be set back to false during setup
  Suite.Enabled := True;
  RunTheTest(Suite);
end;

procedure TFMXTestRunner.mnuSelectAllClick(Sender: TObject);
begin
  SetChecked(TestTree.Selected, True, True);
  HideOverflowMenu;
end;

procedure TFMXTestRunner.mnuUnselectAllClick(Sender: TObject);
begin
  SetChecked(TestTree.Selected, False, True);
  HideOverflowMenu;
end;

procedure TFMXTestRunner.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Obj: IControl;
begin
  if (OverflowMenu.Visible) then
  begin
    Obj := IControl(ObjectAtPoint(ClientToScreen(PointF(X, Y))));
    if (not Assigned(Obj)) then
      HideOverflowMenu
    else if (TObject(Obj) = OverflowMenu) then
      // NOP
    else
      HideOverflowMenu;
  end;

  inherited;
end;

function TFMXTestRunner.TruncateString(s: string; len: Integer): string;
begin
  if Length(s) > len then
    Result := copy(s, 1, Max(1, len-3)) + '...'
  else
    Result := s
end;

procedure TFMXTestRunner.TestingStarts;
begin
  Self.Writeln;
  Self.Writeln('DUnit / Testing ');
  startTime := now;
end;

function TFMXTestRunner.TestToItem(const Test: ITest): TTreeViewItem;
begin
  Result := Test.GUIObject as TTreeViewItem;
  Assert(Result <> nil);
end;

procedure TFMXTestRunner.TestTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Item: TTreeViewItem;
begin
  if (not (Button in [TMouseButton.mbLeft, TMouseButton.mbRight])) then
    Exit;

  // In any case, restart the timer
  tmrPopup.Enabled := False;
  FMenuPos := PointF(X, Y + TabControl1.Position.Y);
  if (Button = TMouseButton.mbRight) then
  begin
    Item := TestTree.ItemByPoint(X, Y);
    if (Item <> nil) then
    begin
      TestTree.Selected := Item;
      tmrPopupTimer(nil);
    end
    else
      HideOverflowMenu;
  end
  else
  begin
    tmrPopup.Enabled := True;
    HideOverflowMenu;
  end;
end;

procedure TFMXTestRunner.TestTreeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  d: Single;
begin
  if (not tmrPopup.Enabled) then
   Exit;

  d := PointF(X, Y + TabControl1.Position.Y).Distance(FMenuPos);
  if (d > 10) then
    tmrPopup.Enabled := false;
end;

procedure TFMXTestRunner.TestTreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  tmrPopup.Enabled := False;
end;

procedure TFMXTestRunner.tmrPopupTimer(Sender: TObject);
begin
  tmrPopup.Enabled := False;
  if (TestTree.Selected = nil) then
    Exit;
  OverflowMenu.Position.Point := FMenuPos;
  OverflowMenu.Visible := True;
  OverflowMenu.SetFocus;
end;

procedure TFMXTestRunner.TestingEnds(testResult: TTestResult);
var
  h, m, s, l: Word;
begin
  endTime := now;
  runTime := endTime - startTime;
  Self.Writeln;
  DecodeTime(runTime, h, m, s, l);
  Self.Writeln(Format('Time: %d:%2.2d:%2.2d.%d', [h, m, s, l]));
  Self.Writeln(Report(testResult));
  Self.Writeln;
end;

class procedure TFMXTestRunner.RunTest(Suite: ITest;
  exitBehavior: TRunnerExitBehavior = rxbContinue);
var
  Form: TFMXTestRunner;
begin
  Application.Initialize;
  Application.CreateForm(TFMXTestRunner, Form);
  TFMXTestRunner.Suite := Suite;
  Application.Run;
  TFMXTestRunner.Suite := nil;
end;

procedure TFMXTestRunner.RunTestsExecute(Sender: TObject);
{$IFDEF DESKTOP}
var
  Ini: TMemIniFile;
{$ENDIF}
begin
  if Suite = nil then
    Exit;

  Setup(nil);
  try
{$IFDEF DESKTOP}
    Ini := TMemIniFile.Create(FIniName);
    try
      Ini.WriteInteger(SIniSection, 'Left', Left);
      Ini.WriteInteger(SIniSection, 'Top', Top);
      Ini.WriteInteger(SIniSection, 'Width', Width);
      Ini.WriteInteger(SIniSection, 'Height', Height);
      Suite.SaveConfiguration(Ini, sTests);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
{$ELSE}
    Suite.SaveConfiguration(FIniName, False, True);
{$ENDIF}
  except
    // Consume the exception
    // Android: Write External Storage permission is not set
    on EFileStreamError do
      ; // NOP
    else
      raise;
  end;
  RunTheTest(Suite);
end;

procedure TFMXTestRunner.RunTestsUpdate(Sender: TObject);
begin
  RunTests.Enabled := not FRunning;
  RunTests.Visible := TabControl1.TabIndex = 0;
end;

procedure TFMXTestRunner.RunTheTest(aTest: ITest);
begin
  if aTest = nil then
    Exit;
  if FRunning then
  begin
    // warning: we're reentering this method if FRunning is true
    assert(FTestResult <> nil);
    FTestResult.Stop;
    Exit;
  end;

  FRunning := True;
  try
    StopTests.Enabled := True;

    // CopyMessageToClipboardAction.Enabled := False;

    EnableUI(False);
    // AutoSaveConfiguration;
    ClearResult;
    testResult := TTestResult.Create;
    try
      testResult.addListener(Self);
      testResult.BreakOnFailures := chkBreakOnFailures.Checked;
      testResult.FailsIfNoChecksExecuted := chkFailIfNoChecksExecuted.Checked;
      testResult.FailsIfMemoryLeaked := chkFailIfMemoryLeaked.Checked;
      testResult.IgnoresMemoryLeakInSetUpTearDown :=
        chkIngnoreLeaksInSetupTeardown.Checked;
      aTest.Run(testResult);
    finally
      FErrorCount := testResult.errorCount;
      FFailureCount := testResult.FailureCount;
{$IFNDEF NEXTGEN}
      testResult.Free;
{$ENDIF}
      testResult := nil;
    end;
  finally
    FRunning := False;
    EnableUI(True);
  end;
end;

class procedure TFMXTestRunner.RunRegisteredTests(
  exitBehavior : TRunnerExitBehavior = rxbContinue);
begin
  RunTest(registeredTests, exitBehavior);
end;

procedure RunTest(Suite: ITest;
  exitBehavior: TRunnerExitBehavior = rxbContinue);
begin
  TFMXTestRunner.RunTest(Suite, exitBehavior);
end;

procedure RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue);
begin
  TFMXTestRunner.RunRegisteredTests(exitBehavior);
end;

procedure TFMXTestRunner.AddStatus(const aStatusStyle, aStatusText: string; aStatus: TTestFailure);
var
  Item: TTreeViewItem;
begin
  Item := TestToItem(aStatus.FailedTest);
  Item.StyleLookup := aStatusStyle;
  SetTreeItemStyle(Item.ParentItem, aStatusStyle);
  Self.Write(aStatusText);
end;

procedure TFMXTestRunner.Status(Test: ITest; const Msg: string);
begin
  Self.Writeln(Format('%s: %s', [Test.name, Msg]));
end;

procedure TFMXTestRunner.Warning(Test: ITest; const Msg: string);
begin
  Self.Writeln(Format('%s: %s', [Test.name, Msg]));
end;

procedure TFMXTestRunner.Write(const s: string);
begin
  if (Memo.Lines.Count = 0) then
    Writeln(s)
  else
    Memo.Lines[Memo.Lines.Count - 1] := Memo.Lines[Memo.Lines.Count - 1] + s;
end;

procedure TFMXTestRunner.Writeln(const s: string);
begin
  Memo.Lines.Add(s);
end;

procedure TFMXTestRunner.SetChecked(const Parent: TTreeViewItem; Value,
  Recursive: Boolean);
var i : Integer;
begin
  if (Parent = nil) then
    Exit;

  Parent.IsChecked := Value;
  for i := 0 to Parent.Count - 1 do
  begin
    if (Recursive) then
      SetChecked(Parent[i], Value, Recursive)
    else
      Parent[i].IsChecked := Value;
  end;
end;

procedure TFMXTestRunner.SetTreeItemStyle(Item: TTreeViewItem;
  const StyleName: string);
begin
  while Item <> nil do
  begin
    if (Item.StyleLookup = '') then
      Item.StyleLookup := StyleName // If style is empty, apply any style
    else if (StyleName = SStyleError) then
      Item.StyleLookup := StyleName // Highest priority
    else if ((StyleName = SStyleFail) and (Item.StyleLookup = SStylePass)) then
      Item.StyleLookup := StyleName; // Fail may replace only Pass

    Item := Item.ParentItem;
  end;
end;

procedure TFMXTestRunner.Setup(const Base : ITest);
  procedure TraverseItems(const Item: TTreeViewItem);
  var
    Test: ITest;
    i: Integer;
  begin
    Test := Item.data.AsInterface as ITest;
    Test.Enabled := Item.IsChecked;
    Item.StyleLookup := '';
    for i := 0 to Item.Count - 1 do TraverseItems(Item[i]);
  end;
begin
  if (Base <> nil) then
    TraverseItems(TestToItem(Base))
  else
    TraverseItems(TestTree.Items[0]);
end;

procedure TFMXTestRunner.SetupTree;
{$IFDEF DESKTOP}
var
  Ini: TMemIniFile;
{$ENDIF}
begin
  if (Suite = nil) then
    Exit;

  TestTree.BeginUpdate;
  try
    TestTree.Clear;
    try
  {$IFDEF DESKTOP}
      Ini := TMemIniFile.Create(FIniName);
      try
        SetBounds(
          Ini.ReadInteger(SIniSection, 'Left', Left),
          Ini.ReadInteger(SIniSection, 'Top', Top),
          Ini.ReadInteger(SIniSection, 'Width', Width),
          Ini.ReadInteger(SIniSection, 'Height', Height));
        Suite.LoadConfiguration(Ini, sTests);
      finally
        Ini.Free;
      end;
  {$ELSE}
      Suite.LoadConfiguration(FIniName, False, True);
  {$ENDIF}
    except
      // Consume the exception
      // Android: Read External Storage permission is not set
      on EFileStreamError do
        ; // NOP
      else
        raise;
    end;
    FillTestTree(nil, Suite);
  finally
    TestTree.EndUpdate;
  end;
end;

function TFMXTestRunner.ShouldRunTest(Test: ITest): Boolean;
begin
  Result := Test.Enabled;
end;

procedure TFMXTestRunner.EnableUI(Enable: Boolean);
begin

end;

procedure TFMXTestRunner.EndSuite(Suite: ITest);
begin

end;

function TFMXTestRunner.PrintStatusItem(const print: string; const index: Integer; const status: TTestFailure): string;
begin
  Result := print + Format('%3d) %s: %s' + sLineBreak + '     at %s' +
    sLineBreak + '      "%s"', [index + 1, status.failedTest.name,
    status.thrownExceptionName, status.LocationInfo,
    status.thrownExceptionMessage]) + sLineBreak;
end;


function TFMXTestRunner.AddSingularOrPlural(const SingularWithoutS: string; const Value: Integer): string;
begin
  if (Value = 1) then
	Result := Format('There was %d %s:', [Value, SingularWithoutS])
  else
    Result := Format('There were %d %ss:', [Value, SingularWithoutS]);
end;

procedure TFMXTestRunner.StartSuite(Suite: ITest);
begin

end;

{ TListBoxItemHelper }

function TListBoxItemHelper.GetChecked: Boolean;
begin
  Result := Self.ItemData.Accessory = TListBoxItemData.TAccessory.aCheckmark;
end;

procedure TListBoxItemHelper.SetChecked(const Value: Boolean);
begin
  if (Value) then
    Self.ItemData.Accessory := TListBoxItemData.TAccessory.aCheckmark
  else
    Self.ItemData.Accessory := TListBoxItemData.TAccessory.aNone;
end;

{ TTestTreeItem }

function TTestTreeItem.GetData: TValue;
begin
  Result := FData;
end;

procedure TTestTreeItem.SetData(const Value: TValue);
begin
  FData := Value;
end;

end.
