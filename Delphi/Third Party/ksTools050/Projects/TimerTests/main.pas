unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ksTimers;

type
  TTimerData = array[0..99] of TDateTime;

type
  TForm1 = class(TForm)
    btnRun: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    cmbInterval: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTimer1: TTimer;
    FTimer2: TksTimer;
    FTimer1Data: TTimerData;
    FTimer1Count: Integer;
    FTimer2Data: TTimerData;
    FTimer2Count: Integer;
    FRunning: Boolean;
    procedure TimerFired(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnRunClick(Sender: TObject);
var
  Interval: Integer;

begin
  btnRun.Enabled:= False;
  FRunning:= False;
  Interval:= StrToInt(cmbInterval.Text);
  FTimer1.Enabled:= False;
  FTimer1.Interval:= Interval;
  FTimer1Count:= 0;
  FTimer2.Enabled:= False;
  FTimer2.DueTime:= Interval;
  FTimer2.Period:= Interval;
//  FTimer2.Period:= 0;
  FTimer2Count:= 0;
  FTimer1.OnTimer:= TimerFired;
  FTimer2.OnTimer:= TimerFired;
  FRunning:= True;
  FTimer1.Enabled:= True;
  FTimer2.Enabled:= True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTimer1:= TTimer.Create(Self);
  FTimer1.Enabled:= False;
  FTimer2:= TksTimer.Create(Self);
end;

procedure TForm1.TimerFired(Sender: TObject);
var
  I: Integer;
  Average: Integer;

begin
  if not FRunning then begin
    FTimer1.Enabled:= False;
    FTimer2.Enabled:= False;

    Memo1.Lines.Clear;
    for I:= 0 to FTimer1Count - 1 do
      Memo1.Lines.Add(Format('%.2d: %s',
        [I, FormatDateTime('hh:nn:ss:zzz', FTimer1Data[I])]));
    Memo1.Lines.Add('-----------------------');
    if FTimer1Count > 1 then begin
      Average:= Round((FTimer1Data[FTimer1Count - 1] - FTimer1Data[0])
                       * 24 * 60 * 60 * 1000 / FTimer1Count);
      Memo1.Lines.Add(Format('Average %d ms', [Average]));
    end;

    Memo2.Lines.Clear;
    for I:= 0 to FTimer2Count - 1 do
      Memo2.Lines.Add(Format('%.2d: %s',
        [I, FormatDateTime('hh:nn:ss:zzz', FTimer2Data[I])]));
    Memo2.Lines.Add('-----------------------');
    if FTimer2Count > 1 then begin
      Average:= Round((FTimer2Data[FTimer2Count - 1] - FTimer2Data[0])
                       * 24 * 60 * 60 * 1000 / FTimer2Count);
      Memo2.Lines.Add(Format('Average %d ms', [Average]));
    end;

    btnRun.Enabled:= True;
  end
  else if Sender = FTimer1 then begin
    FTimer1Data[FTimer1Count]:= Now;
    Inc(FTimer1Count);
    FRunning:= FTimer1Count < 100;
  end
  else begin
    FTimer2Data[FTimer2Count]:= Now;
    Inc(FTimer2Count);
    FRunning:= FTimer2Count < 100;
  end;
end;

end.
