(*
 ACS CD Ripper demo main unit
 Copyright (c) Andrei Borovsky
 You can contact me at aborovsky@mtu-net.ru
 This demo uses LAME to convert data from
 CD tracks to MP3 files.
 You will need libmp3lame.so library to run this demo.
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, ACS_Classes, ACS_CDROM, ACS_LAME;

type
  TForm1 = class(TForm)
    MP3Out1: TMP3Out;
    CDIn1: TCDIn;
    SpinEdit1: TSpinEdit;
    ProgressBar1: TProgressBar;
    Button1: TButton;
    Label1: TLabel;
    procedure SpinEdit1Enter(Sender: TObject);
    procedure MP3Out1Progress(Sender: TComponent);
    procedure Button1Click(Sender: TObject);
    procedure MP3Out1Done(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.SpinEdit1Enter(Sender: TObject);
begin
  if CDIn1.TracksCount > 0 then
  SpinEdit1.Max := CDIn1.TracksCount;
end;

procedure TForm1.MP3Out1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := MP3Out1.Progress;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  track : Integer;
begin
  track := SpinEdit1.Value;
  CDIn1.StartTrack := track;
  CDIn1.EndTrack := track;
  MP3Out1.FileName := Format('Track-%d.mp3', [track]);
  MP3Out1.Id3TagTrack := IntToStr(track);
  MP3Out1.Run;
  Button1.Enabled := False;
end;

procedure TForm1.MP3Out1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

end.
