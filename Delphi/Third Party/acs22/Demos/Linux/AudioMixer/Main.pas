(*
 ACS Audio Mixer demo main unit
 (c) 2002 Andrei Borovsky, all rights reserved
 You can contact me at borovsky@yandex.ru
 This program takes in two wav files and
 generates a file where two audio streams
 are mixed together.
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, ACS_Classes, ACS_Wave, ACS_AudioMix;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    WaveIn2: TWaveIn;
    AudioMixer1: TAudioMixer;
    WaveOut1: TWaveOut;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrackBar1.Position := AudioMixer1.Volume1;
  TrackBar2.Position := AudioMixer1.Volume2;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  AudioMixer1.Volume1 := TrackBar1.Position;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  AudioMixer1.Volume2 := TrackBar2.Position;
end;


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

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    if (WaveIn1.Valid and WaveIn2.Valid) = False then Exit;
    WaveOut1.FileName := SaveDialog1.FileName;
    WaveOut1.Run;
    Button3.Enabled := False;
  end;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button3.Enabled := True;
end;

end.
