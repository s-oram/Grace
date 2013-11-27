unit eePerformanceCounterForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TPerformanceCounterForm = class(TForm)
    LastTimeDisplay: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MaxTimeDisplay: TEdit;
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Timer1: TTimer;
    procedure OnUpdateDisplay(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    fLastTime: single;
    fMaxTime: single;

  public
    property LastTime:single read fLastTime write fLastTime;
    property MaxTime:single  read fMaxTime  write fMaxTime;
  end;


implementation

{$R *.dfm}

procedure TPerformanceCounterForm.FormCreate(Sender: TObject);
begin
  LastTime := 0;
  MaxTime  := 0;
end;

procedure TPerformanceCounterForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TPerformanceCounterForm.OnUpdateDisplay(Sender: TObject);
var
  x:string;
  Format:TFloatFormat;
begin
  Format := ffFixed;

  x := FloatToStrF(fLastTime, Format, 6,6);
  if LastTimeDisplay.Text <> x then LastTimeDisplay.Text := x;


  x := FloatToStrF(fMaxTime, Format, 6,6);
  if MaxTimeDisplay.Text <> x then MaxTimeDisplay.Text := x;  
end;

procedure TPerformanceCounterForm.Button1Click(Sender: TObject);
begin
  LastTime := 0;
  MaxTime  := 0;
  OnUpdateDisplay(nil);
end;



end.
