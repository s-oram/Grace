unit uMainForm;

interface



uses
  Temp_PluginPar,
  Lucidity.FirstRun,
  fontenumTest,
  VamLib.Threads,
  //OtlParallel,
  MyWorker,
  VamLib.WinHook,
  eeFileBrowserAddon,
  wmfintf,
  ACS_MemFloat,
  ACS_Wave,
  ACS_smpeg,
  SfzParser,
  NativeXML,
  VamLib.Utils,
  VamLib.Debouncer,
  VamLib.GuiUtils,
  eeOscPhaseCounter,
  RedFoxImageBuffer,
  VamLib.UniqueID,
  VamLib.ZeroObject,
  VamLib.Collections.Lists,
  VamLib.MultiEvent,
  VamLib.Animation,
  AudioIO,
  eeSampleFloat, {VamSampleDisplayBackBuffer,} {VamSamplePeakBuffer,}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl, VamWinControl,
  VamSampleDisplay, RedFoxContainer, Vcl.StdCtrls, VamLabel, VamKnob,
  VamModSelector, VamCompoundNumericKnob, VamNumericKnob,
  LucidityGui.DropBoxSelector, VamShortMessageOverlay, Vcl.ExtCtrls,
  VamSliderSwitch, VamMiniLevelMeter, VamCustomTreeView, VamTreeView,
  VamTextBox, VamTabPanel, VamTabs, VamScrollBox, VamScrollBar, VamPanel,
  VamMultiLineTextBox, VamMemo, VamImage, VamDiv, VamButton,
  RedFoxGraphicControl, VamGraphicControl, VamArrows, VamXYPad, VamSlider,
  VamStatusLed, VamSamplerKeys, VamSampleMap,
  VamModularJack, LucidityGui.VectorSequence,
  VamCompoundLabel;

type
  TWinEventCallback = reference to procedure( hWinEventHook : NativeUInt; dwEvent:dword; handle : hwnd; idObject, idChild : Long; dwEventThread, dwmsEventTime : dword) stdcall;

  TMyTestObject = class(TRefCountedZeroObject)
  public
    destructor Destroy; override;
  end;

  TFancyUpdater = class;


  TForm1 = class(TForm)
    RedFoxContainer1: TRedFoxContainer;
    VamPanel2: TVamPanel;
    VamTextBox1: TVamTextBox;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    procedure VamKnob1KnobPosChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure VamTextBox1Click(Sender: TObject);
  private
    Updater : TFancyUpdater;
    FileBrowserAddon : TFileBrowserAddon;

    MotherShip : TMotherShip;
    OscPhase : TOscPhaseCounter;
    StepSize : TOscPhaseCounter;

    ThrottleID_VSTParChange : TUniqueID;

    BackBuffer: TRedFoxImageBuffer;

    ID : TUniqueID;
    KnobValue : single;
    TimeReference : TDateTime;
    Token : TDebounceToken;
    KnobTT : TThrottleToken;

    WindowsEventHook : TWindowsEventHook;

    procedure UpdateLabel;
    procedure WinEventHandler(Sender : TObject; Event, hwnd, idObject, idChild, EventThread, EventTime : cardinal);
  public
    procedure UpdateMemo;
  published
    procedure MyTestHandler2(Sender : TObject);
    procedure WinEventProcCallbackObject( hWinEventHook : NativeUInt; dwEvent:dword; handle : hwnd; idObject, idChild : Long; dwEventThread, dwmsEventTime : dword); stdcall;
  end;


  TFancyUpdater = class(TCustomMotile)
  private
    fForm: TForm1;
  protected
    procedure Task; override;
  public
    property Form : TForm1 read fForm write fForm;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VamLib.DuckType,
  _DirectSound,
  VamGuiControlInterfaces,
  RedFoxColor,
  Generics.Collections,
  DateUtils,
  InWindowDialog,
  InWindowDialog.MessageDialog,
  InWindowDialog.InputDialog;

type
  TProcDictionary = TDictionary<integer, TDateTime>;

var
  GlobalDict : TProcDictionary;

type
  TKnobHack = class(TVamKnob)
  private
  public
  end;

const
    MM_MAX_NUMAXES = 16;
type
  PDesignVector = ^TDesignVector;
  TDesignVector = packed record
    dvReserved: DWORD;
    dvNumAxes: DWORD;
    dvValues: array[0..MM_MAX_NUMAXES-1] of Longint;
  end;

//function AddFontMemResourceEx(p1: Pointer; p2: DWORD; p3: PDesignVector; p4: LPDWORD): THandle; external 'gdi32.dll' name 'AddFontMemResourceEx'; stdcall;
//function RemoveFontMemResourceEx(p1: THandle): BOOL; external 'gdi32.dll' name 'RemoveFontMemResourceEx'; stdcall;

procedure LoadFontFromRes(FontName: PWideChar);
var
  ResHandle: HRSRC;
  ResSize, NbFontAdded: Cardinal;
  ResAddr: HGLOBAL;
begin
  ResHandle := FindResource(HINSTANCE, FontName, RT_FONT);

  {
  if ResHandle = 0 then
    RaiseLastOSError;
  ResAddr := LoadResource(HINSTANCE, ResHandle);
  if ResAddr = 0 then
    RaiseLastOSError;
  ResSize := SizeOfResource(, ResHandle);
  if ResSize = 0 then
    RaiseLastOSError;
  if 0 = AddFontMemResourceEx(Pointer(ResAddr), ResSize, nil, @NbFontAdded) then
    RaiseLastOSError;
  }
end;



procedure DrawKnob_ModEditOverlay(Sender: TObject);
  procedure CalcStartSweep(const Angle1, Angle2 : single; out Start, Sweep : single);
  begin
    Start := (Angle2+90) / 360 * 2 * pi;
    Sweep := (Angle1+90) / 360 * 2 * pi;
  end;
const
  kMinAngle = 30;
  kMaxAngle = 300;
  kArcSpan  = 300;
var
  Knob : TKnobHack;
  MidX, MidY : single;
  Angle1, Angle2 : single;
  s1, s2 : single;
begin
  Knob := TKnobHack(Sender as TVamKnob);

  MidX := Knob.Width  * 0.5;
  MidY := Knob.Height * 0.5;

  Knob.BackBuffer.BufferInterface.LineWidth := Knob.ModLineWidth;
  Knob.BackBuffer.BufferInterface.LineColor := GetAggColor(clAqua, 128);

  Angle1 := kMinAngle;
  Angle2 := kMinAngle + kArcSpan;

  CalcStartSweep(Angle1, Angle2, s1, s2);
  Knob.BackBuffer.BufferInterface.Arc(MidX, MidY, Knob.ModLineDist, Knob.ModLineDist, s1, s2);
end;


//==============================================================================



procedure WinEventProcCallback( hWinEventHook : NativeUInt; dwEvent:dword; handle : hwnd; idObject, idChild : Long; dwEventThread, dwmsEventTime : dword); stdcall;
begin
  //LogMain.LogMessage('WinEvent ' + IntToStr(dwEvent));
end;

procedure TForm1.WinEventHandler(Sender: TObject; Event, hwnd, idObject, idChild, EventThread, EventTime: cardinal);
begin
  //LogMain.LogMessage('WinEvent2xObj ' + IntToStr(Event));
end;

procedure TForm1.WinEventProcCallbackObject(hWinEventHook: NativeUInt; dwEvent: dword; handle: hwnd; idObject, idChild: Long; dwEventThread, dwmsEventTime: dword);
begin

end;



procedure TForm1.FormCreate(Sender: TObject);
const
  kDebugStr = '$11223344';
  kRed = '$FFFF0000';
var
  x : integer;
  rc : TRedFoxColor;
  xs : string;
  ResHandle: HRSRC;
  ResSize, NbFontAdded: Cardinal;
  ResAddr: HGLOBAL;
  Dir : string;
  fn : string;
begin
  Dir := ExtractFilePath(Application.ExeName);
  fn := IncludeTrailingPathDelimiter(Dir) + 'resources\westwood.ttf';
  if FileExists(fn) then
  begin
    AddFontResource(pWideChar(fn)) ;
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0) ;
  end else
  begin
    //raise Exception.Create('file does not exist.');
  end;


  Updater := TFancyUpdater.Create;

  ThrottleID_VSTParChange.Init;

  xs := 'James Brown';

  Delete(xs, Length(xs), 1);

  WindowsEventHook := TWindowsEventHook.Create(EVENT_SYSTEM_FOREGROUND, EVENT_SYSTEM_FOREGROUND);
  WindowsEventHook.OnWinEvent := WinEventHandler;



  self.Scaled := false;
  //Self.ScaleBy(Screen.PixelsPerInch, 96);

  VamTextBox1.Font.Size := 8;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  Dir : string;
  fn : string;
begin
  Dir := ExtractFilePath(Application.ExeName);
  fn := IncludeTrailingPathDelimiter(Dir) + 'resources\westwood.ttf';
  if FileExists(fn) then
  begin
    RemoveFontResource(PWideChar(fn));
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0) ;
  end else
  begin
    //raise Exception.Create('file does not exist.');
  end;



  Updater.Free;

  WindowsEventHook.Free;

  BackBuffer.Free;
  FileBrowserAddon.Free;
end;

procedure SlowAction(x : TObject); cdecl;
begin

end;



procedure Bang;
begin
  ShowMessage('beep');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Updater.Stop;
  Updater.Form := self;
  Updater.Run;

  //Worker.Run;

  //if assigned(a) then a.CancelInvocation;
  //a := AsyncCall(@SlowAction, [self]);
  //a.ForceDifferentThread;
  //TAsyncCalls.Invoke(SlowAction);
  //a := AsyncCall(@SlowAction, 10);
  //a.ForceDifferentThread;
  //a.Sync;
  //a.ForceDifferentThread; // Do not execute in the main thread because this will
                          // change LocalAyncVclCall into a blocking LocalVclCall
  // do something
  //a.Sync; The Compiler will call this for us in the Interface._Release method
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  fn : string;
  FontName : string;
  fontNames : TStringList;
begin
  fn :=  'C:\ProgramData\One Small Clue\Grace\Resources\LiberationSans-Bold.ttf';
  InstallFont(fn);

  fn :=  'C:\ProgramData\One Small Clue\Grace\Resources\LiberationSans-Regular.ttf';
  InstallFont(fn);


  Memo1.Clear;

  FontNames := TStringList.Create;

  CollectFonts(Fontnames);

  Memo1.Lines.AddStrings(FontNames);

  FontNames.Free;
end;

procedure TForm1.UpdateLabel;
begin

end;

procedure TForm1.UpdateMemo;
begin

end;




procedure TForm1.VamKnob1KnobPosChanged(Sender: TObject);
var
  ReferenceTime : TDateTime;
begin

end;


procedure TForm1.VamTextBox1Click(Sender: TObject);
begin
  InWindow_ShowMessage(self, 'Bang');
end;

procedure TForm1.MyTestHandler2(Sender: TObject);
begin
  showmessage('I''m the king!');
end;



{ TMyTestObject }

destructor TMyTestObject.Destroy;
begin

  inherited;
end;

{ TFancyUpdater }

procedure TFancyUpdater.Task;
var
  c1: Integer;
begin
  inherited;

end;

initialization
  ReportMemoryLeaksOnShutDown := True;


finalization


end.
