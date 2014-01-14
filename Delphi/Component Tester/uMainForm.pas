unit uMainForm;

interface

uses
  VamLib.Debouncer,
  AudioIO,
  eeSampleFloat, VamSampleDisplayBackBuffer, VamSamplePeakBuffer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl, VamWinControl,
  VamSampleDisplay, RedFoxContainer, Vcl.StdCtrls, VamLabel, VamKnob,
  VamModSelector;

type
  TForm1 = class(TForm)
    Button1: TButton;
    RedFoxContainer1: TRedFoxContainer;
    FileOpenDialog1: TFileOpenDialog;
    Knob1: TVamKnob;
    Label1: TVamLabel;
    VamModSelector1: TVamModSelector;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Knob1Changed(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateLabel;
  public
    Debouncer : TDebouncer;

    Sample : TSampleFloat;
    Peakbuffer : IPeakBuffer;
    ImageBuffer : ISampleImageBuffer;

    a : integer;
    b : cardinal;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VamLib.Threads,
  VamLib.Utils;



procedure TForm1.FormCreate(Sender: TObject);
begin
  Sample := TSampleFloat.Create;
  PeakBuffer := TPeakbuffer.Create;
  ImageBuffer := TSampleImageBuffer.Create;


  a := 50;
  b := 2000000000000;

  //ShowMessage(IntToStr(a + Integer(b)));

  Debouncer := TDebouncer.Create;
  Debouncer.DebounceTime := 100;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Debouncer.Free;
  Sample.Free;
  PeakBuffer := nil;
  ImageBuffer := nil;
end;


procedure TForm1.Knob1Changed(Sender: TObject);
begin
  Debouncer.Debounce(UpdateLabel);
end;

procedure TForm1.UpdateLabel;
var
  Task : TProc;
  CallBack : TProc;
  x : string;
begin
  x := FloatToStr(Knob1.Pos * 100);

  Task := procedure
  begin
    Sleep(1500);
  end;

  CallBack := procedure
  begin
    //Label1.Text := FloatToStr(Knob1.Pos * 100);
    Label1.Text := x;
  end;

  RunTask(Task, CallBack);
end;

end.
