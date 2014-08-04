(*
 ACS Sound Recorder demo main unit
 Copyright (c) Andrei Borovsky
 You can contact me at aborovsky@mtu-net.ru
 Make sure you've selected an appropriate recording
 source before starting recording.
*)
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_Wave, ACS_Audio, ComCtrls, Spin,
  ExtCtrls, ACS_Indicator;

type
  TForm1 = class(TForm)
    AudioIn1: TAudioIn;
    WaveOut1: TWaveOut;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    ProgressBar1: TProgressBar;
    Button2: TButton;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    GroupBox2: TGroupBox;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    GroupBox3: TGroupBox;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    ProgressBar2: TProgressBar;
    SoundIndicator1: TSoundIndicator;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure WaveOut1Progress(Sender: TComponent);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    if RadioButton1.Checked then
    AudioIn1.InSampleRate := 11025
    else if RadioButton2.Checked then
    AudioIn1.InSampleRate := 22050
    else AudioIn1.InSampleRate := 44100;

    if RadioButton4.Checked then
    AudioIn1.InBitsPerSample := 8
    else AudioIn1.InBitsPerSample := 16;

    if RadioButton6.Checked then
    AudioIn1.InChannels := 1
    else AudioIn1.InChannels := 2;

    AudioIn1.RecTime := SpinEdit1.Value;

    WaveOut1.FileName := SaveDialog1.FileName;
    WaveOut1.Run;
    Button1.Enabled := False;
  end;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

procedure TForm1.WaveOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WaveOut1.Progress;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  WaveOut1.Stop;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Vals : array[0..31] of Double;
begin
  SoundIndicator1.GetValues(Vals);
  ProgressBar2.Position := Round(Vals[0]);
end;

end.
