unit uMainForm;

interface

uses
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
  RedFoxColor,
  VamLib.Throttler,
  //eeEnumHelper,
  Generics.Collections,
  VamLib.Threads,
  DateUtils;

type
  TProcDictionary = TDictionary<integer, TDateTime>;

var
  GlobalDict : TProcDictionary;

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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer.Free;
  BackBuffer.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  OpenDialog : TFileOpenDialog;
  LoopStart, LoopEnd : integer;
begin
  OpenDialog := TFileOpenDialog.Create(nil);

  if OpenDialog.Execute then
  begin
    ReadLoopPoints(OpenDialog.FileName, LoopStart, LoopEnd);
    VamLabel1.Text := IntToStr(LoopStart) + ' - ' + IntToStr(LoopEnd);
  end;

  OpenDialog.Free;
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
  //
  {
  Throttle(ThrottleID_VSTParChange, 1000,
  procedure
  begin
    VamLabel1.Text := FloatToStr(VamKnob1.Pos);
  end);
  }


  //Debounce(10, ReferenceTime);


  {
  VamLib.GuiUtils.Debounce(Token, TDebounceEdge.deBoth, 500,
  procedure
  begin
    VamLabel1.Text := FloatToStr(VamKnob1.Pos);
  end);
  }

  VamLib.GuiUtils.Throttle(KnobTT, 100,
  procedure
  begin
    VamLabel1.Text := FloatToStr(VamKnob1.Pos);
  end);
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
