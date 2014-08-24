unit uMainForm;

interface



uses
  wmfintf,
  ACS_MemFloat,
  ACS_Wave,
  ACS_smpeg,
  SfzParser,
  NativeXML,
  Lucidity.Sfz,
  VamLib.Utils,
  VamLib.Debouncer,
  VamLib.GuiUtils,
  eeOscPhaseCounter,
  RedFoxImageBuffer,
  VamLib.HighSpeedTimer,
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
  TMyTestObject = class(TRefCountedZeroObject)
  public
    destructor Destroy; override;
  end;


  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    RedFoxContainer1: TRedFoxContainer;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    VamPanel1: TVamPanel;
    VamKnob1: TVamKnob;
    VamLabel1: TVamLabel;
    VamLabel2: TVamLabel;
    VamTextBox1: TVamTextBox;
    Filter2KeyTrackKnob: TVamCompoundNumericKnob;
    Edit1: TEdit;
    FileOpenDialog1: TFileOpenDialog;
    VamTextBox2: TVamTextBox;
    procedure VamKnob1KnobPosChanged(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    MotherShip : TMotherShip;
    OscPhase : TOscPhaseCounter;
    StepSize : TOscPhaseCounter;

    ThrottleID_VSTParChange : TUniqueID;

    BackBuffer: TRedFoxImageBuffer;

    ID : TUniqueID;
    KnobValue : single;
    TimeReference : TDateTime;
    Timer : THighSpeedTimer;
    Token : TDebounceToken;
    KnobTT : TThrottleToken;
    procedure UpdateLabel;

    procedure HandleTimerEvent(Sender : TObject);
  public
    procedure UpdateMemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  _DirectSound,
  VamGuiControlInterfaces,
  RedFoxColor,
  VamLib.Throttler,
  //eeEnumHelper,
  Generics.Collections,
  VamLib.Threads,
  DateUtils,
  ACS_MAD;

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

procedure TForm1.FormCreate(Sender: TObject);
const
  kDebugStr = '$11223344';
  kRed = '$FFFF0000';
var
  x : integer;
  rc : TRedFoxColor;
  xs : string;
begin
  ThrottleID_VSTParChange.Init;

  Timer := THighSpeedTimer.Create;
  Timer.Interval := 300;
  Timer.OnTimer := self.HandleTimerEvent;
  Timer.Enabled := false;

  xs := 'James Brown';

  Delete(xs, Length(xs), 1);

  VamLabel1.Text := 'James Brown Went To The Shop To Eat Some Pizza';

  VamTextBox1.AutoTrimText := true;
  VamTextBox1.TextPadding.Left := 8;
  VamTextBox1.TextPadding.Right := 24;

  VamTextBox1.Text := 'James Brown Went To The Shop To Eat Some Pizza';

  VamKnob1.KnobMode := TKnobMode.ModEdit;

  VamTextBox2.CornerRadius[0] := 0;
  VamTextBox2.CornerRadius[1] := 0;
  VamTextBox2.CornerRadius[2] := 0;
  VamTextBox2.CornerRadius[3] := 5;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer.Free;
  BackBuffer.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
// c:\test1\test2\
var
  Mp3In : TMP3In;
  wavOut : TWaveOut;
  MemFloatOut : TMemFloatOut;
begin
  mp3In := TMp3In.Create(nil);
  wavOut := TWaveOut.Create(nil);
  memFloatOut := TMemFloatOut.Create(nil);

  try
    if FileOpenDialog1.Execute then
    begin
      mp3In.FileName := FileOpenDialog1.FileName;

      //wavOut.Input := mp3In;
      //wavOut.FileName := 'D:\A WAve Convert Test.wav';
      //WavOut.BlockingRun;

      MemFloatOut.Input := mp3In;
      MemFloatOut.BlockingRun;
      //MemFloatOut.SampleData[0,0] := 1;
    end;
  finally
    mp3In.free;
    WavOut.Free;
    MemFloatOut.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  VamLabel1.Text := 'Message A';

  Sleep(2000);
  VamLabel1.Text := 'Message B';

  Sleep(2000);
  VamLabel1.Text := 'Message C';

  Sleep(2000);
  VamLabel1.Text := 'Message D';
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

procedure TForm1.HandleTimerEvent(Sender: TObject);
begin
  VamLabel2.Text :=  IntToStr(Random(4000));
end;





{ TMyTestObject }

destructor TMyTestObject.Destroy;
begin

  inherited;
end;

initialization
  ReportMemoryLeaksOnShutDown := True;


finalization


end.
