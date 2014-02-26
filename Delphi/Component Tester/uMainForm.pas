unit uMainForm;

interface

uses
  eeOscPhaseCounterV2,
  VamLib.HighSpeedTimer,
  VamLib.UniqueID,
  VamLib.ZeroObject,
  VamLib.Collections.Lists,
  VamLib.MultiEvent,
  VamLib.Debouncer,
  VamLib.Animation,
  AudioIO,
  eeSampleFloat, VamSampleDisplayBackBuffer, VamSamplePeakBuffer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl, VamWinControl,
  VamSampleDisplay, RedFoxContainer, Vcl.StdCtrls, VamLabel, VamKnob,
  VamModSelector, VamCompoundNumericKnob, VamNumericKnob,
  LucidityGui.DropBoxSelector, VamShortMessageOverlay, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    RedFoxContainer1: TRedFoxContainer;
    Knob1: TVamNumericKnob;
    Knob2: TVamNumericKnob;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    VamShortMessageOverlay1: TVamShortMessageOverlay;
    Button5: TButton;
    VamKnob1: TVamKnob;
    VamKnob2: TVamKnob;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure HandleTimerEvent(Sender: TObject);
    procedure VamKnob1KnobPosChanged(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    OscPhase : TOscPhaseCounter;
    StepSize : TOscPhaseCounter;

    ID : TUniqueID;
    KnobValue : single;
    TimeReference : TDateTime;
    Timer : THighSpeedTimer;
    procedure UpdateLabel;

  public
    procedure UpdateMemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VamLib.Throttler,
  eeEnumHelper,
  Generics.Collections,
  VamLib.Threads,
  VamLib.Utils,
  DateUtils;

type
  TProcDictionary = TDictionary<integer, TDateTime>;

var
  GlobalDict : TProcDictionary;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ID.Init;

  Memo1.Clear;


  Timer := THighSpeedTimer.Create;
  Timer.OnTimer := self.HandleTimerEvent;
  Timer.UseMainThreadForTimerEvent := true;

  OscPhase := 0.99;
  StepSize := 0.0001;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer.Free;
end;

procedure TForm1.HandleTimerEvent(Sender: TObject);
var
  ms : Int64;
  Overflow : boolean;
  Index : cardinal;
  Frac  : single;
  s : string;
begin
  OscPhase.IncBy(StepSize);

  OscPhase.GetIndex2048(Index, Frac);


  s := IntToStr(Index) + ' ' + FloatToStr(Frac);
  Memo1.Lines.Add(s);
  Memo1.Invalidate;


  {
  if Overflow
    then Memo1.Lines.Add(IntToStr(round(single(OscPhase) * 100)) + ' OVERFLOW')
    else Memo1.Lines.Add(IntToStr(round(single(OscPhase) * 100)));
  Memo1.Invalidate;
  }
end;



procedure TForm1.Button1Click(Sender: TObject);
begin
  TimeReference := Now;
  Timer.Interval := 300;
  Timer.Enabled := true;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Timer.Enabled := false;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  Timer.Interval := 200;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Timer.Interval := 1000;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Timer.Interval := 750;
end;

procedure TForm1.UpdateLabel;
begin

end;

procedure TForm1.UpdateMemo;
begin
  {
  Memo1.Clear;
  for c1 := 0 to FooList.Count-1 do
  begin
    Memo1.Lines.Add(FooList[c1].Text);
  end;
  }
end;




procedure TForm1.VamKnob1KnobPosChanged(Sender: TObject);
begin
  VamKnob2.Pos := VamKnob1.Pos;
end;



initialization
  GlobalDict := TProcDictionary.Create(100);

finalization
  GlobalDict.Free;

end.
