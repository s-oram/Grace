(*
 ACS Audio Mixer demo main unit
 Copyright (c) Andrei Borovsky
 You can contact me at aborovsky@mtu-net.ru
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_Wave, ACS_AudioMix;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    WaveIn2: TWaveIn;
    AudioMixer1: TAudioMixer;
    WaveOut1: TWaveOut;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
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
  if OpenDialog1.Execute then
  WaveIn1.FileName := OpenDialog1.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  WaveIn2.FileName := OpenDialog1.FileName;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  AudioMixer1.Mode := amMix;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  AudioMixer1.Mode := amConcatenate;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    WaveOut1.FileName := SaveDialog1.FileName;
    WaveOut1.Run;
    Button1.Enabled := False;
    Button2.Enabled := False;
    Button3.Enabled := False;
  end;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
end;

end.
