unit uMainForm;

interface



uses
  VamLib.Threads,
  VamLib.WinHook,
  SfzParser,
  VamLib.Utils,
  VamLib.Debouncer,
  VamLib.GuiUtils,
  RedFoxImageBuffer,
  VamLib.UniqueID,
  VamLib.ZeroObject,
  VamLib.Collections.Lists,
  VamLib.MultiEvent,
  VamLib.Animation,
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
  TForm1 = class(TForm)
    RedFoxContainer1: TRedFoxContainer;
    VamPanel2: TVamPanel;
    VamTextBox1: TVamTextBox;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  published
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
  DateUtils;

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

procedure TForm1.FormCreate(Sender: TObject);
begin
  //
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //
end;




initialization
  ReportMemoryLeaksOnShutDown := True;


finalization


end.
