unit uMainForm;

interface



uses
  VamLib.Threads,
  //OtlParallel,
  MyWorker,
  AsyncCalls,
  VamLib.WinHook,
  SynCommons,
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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    procedure VamKnob1KnobPosChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Updater : TFancyUpdater;
    a: IAsyncCall;
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
  VamLib.Throttler,
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
begin
  Updater := TFancyUpdater.Create;

  ThrottleID_VSTParChange.Init;

  xs := 'James Brown';

  Delete(xs, Length(xs), 1);

  WindowsEventHook := TWindowsEventHook.Create(EVENT_SYSTEM_FOREGROUND, EVENT_SYSTEM_FOREGROUND);
  WindowsEventHook.OnWinEvent := WinEventHandler;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Updater.Free;

  WindowsEventHook.Free;

  BackBuffer.Free;
  FileBrowserAddon.Free;
end;

procedure SlowAction(x : TObject); cdecl;
const
  ID = 'TEST';
var
  c1: Integer;

  procedure Start;
  begin
    (x as TForm1).Edit4.Text := Id + ' Working...';
  end;
  procedure Finished;
  begin
    (x as TForm1).Edit4.Text := Id + ' Done';
  end;
  procedure UpdateText;
  begin
    (x as TForm1).Edit4.Text := Id + ' Working... ' + IntToStr(c1);
  end;

begin
  LocalAsyncVclCall(@Start);
  for c1 := 0 to 10 do
  begin
    sleep(250);
    LocalAsyncVclCall(@UpdateText);
  end;
  LocalVclCall(@Finished); // blocking
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
  MissingSamples, SearchPaths : TStringList;
begin
  MissingSamples := TStringList.Create;
  SearchPaths    := TStringList.Create;
  try
    MissingSamples.Add('D:\Audio\Data\Samples\Drum Machines\Casio MT-100\00.wav');
    MissingSamples.Add('D:\Audio\Data\Samples\Drum Machines\Casio MT-100\01.wav');
    MissingSamples.Add('02.wav');
    SearchPaths.Add('D:\Audio\Data\Poise Drum Kits 2');
    SearchPaths.Add('D:\Audio\Data\Samples (Instruments)');
    InWindow_SampleFinderDialog(self, MissingSamples, SearchPaths, nil);
  finally
    MissingSamples.Free;
    SearchPaths.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Task : TThreadProcedure;
  Fin  : TThreadProcedure;
begin
  Task := procedure
  var
    c1: Integer;
  begin
    TThread.Queue(nil, procedure
    begin
      Edit4.Text := 'jb is dead';
    end);
    Sleep(2000);
  end;

  Fin := procedure
  begin
    Edit4.Text := 'Go home';
  end;

  Async(Task).Await(Fin);
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

  for c1 := 0 to 100 do
  begin
    if IsCanceled then exit;
    Sleep(250);
    if IsCanceled then exit;

    self.Synchronize(procedure
    begin
      Form.Edit4.Text := IntToStr(c1);
    end);
  end;


end;

initialization
  ReportMemoryLeaksOnShutDown := True;


finalization


end.
