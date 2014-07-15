unit uMainForm;

interface

uses
  eeOscPhaseCounter,
  RedFoxImageBuffer,
  VamLib.HighSpeedTimer,
  VamLib.UniqueID,
  VamLib.ZeroObject,
  VamLib.Collections.Lists,
  VamLib.MultiEvent,
  VamLib.Debouncer,
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
  VamLib.Throttler,
  //eeEnumHelper,
  Generics.Collections,
  VamLib.Threads,
  VamLib.Utils,
  DateUtils;

type
  TProcDictionary = TDictionary<integer, TDateTime>;

var
  GlobalDict : TProcDictionary;


procedure Wait(lNumberOfSeconds : Longint);
const
 _SECOND = 10000000;
var
 lBusy : LongInt;
 hTimer : LongInt;
 liDueTime : LARGE_INTEGER;

begin
  // Waitable Timers in Delphi.
  // http://delphi32.blogspot.com.au/2006/03/using-waitable-timer-in-delphi.html
  // http://www.adp-gmbh.ch/win/misc/timer.html

  hTimer := CreateWaitableTimer(nil, True, nil);
  if hTimer = 0 then
   Exit;
  liDueTime.QuadPart := -10000000 * lNumberOfSeconds;
  SetWaitableTimer(hTimer, TLargeInteger(liDueTime), 0, nil, nil, False);

  repeat
    lBusy := MsgWaitForMultipleObjects(1, hTimer, False,
            INFINITE, QS_ALLINPUT);
      Application.ProcessMessages;
   Until lBusy = WAIT_OBJECT_0;

    // Close the handles when you are done with them.
   CloseHandle(hTimer);

End;


//==============================================================================

procedure TForm1.FormCreate(Sender: TObject);
var
  x : integer;
begin
  ThrottleID_VSTParChange.Init;

  Timer := THighSpeedTimer.Create;
  Timer.Interval := 300;
  Timer.OnTimer := self.HandleTimerEvent;
  //Timer.Enabled := true;

  BackBuffer := TRedFoxImageBuffer.Create;


  //x := round(BackBuffer.TextWidth('110CLP.WAV'));
  //x := round(BackBuffer.TextWidth('A', self.Font));
  BackBuffer.UpdateFont(Font);
  x := round(BackBuffer.TextWidth('110CLP.WAV'));
  VamLabel1.Text := IntToStr(x);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer.Free;
  BackBuffer.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //
  //VamPanel1.BeginUpdate;

  VamLabel1.Text := 'Message A';

  Wait(1);
  VamLabel1.Text := 'Message B';

  Wait(1);
  VamLabel1.Text := 'Message C';

  Wait(1);
  VamLabel1.Text := 'Message D';
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  //
  //VamPanel1.EndUpdate;
  VamLabel1.Text := 'Message Z';
end;


procedure TForm1.UpdateLabel;
begin

end;

procedure TForm1.UpdateMemo;
begin

end;




procedure TForm1.VamKnob1KnobPosChanged(Sender: TObject);
begin
  //

  Throttle(ThrottleID_VSTParChange, 1000,
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
