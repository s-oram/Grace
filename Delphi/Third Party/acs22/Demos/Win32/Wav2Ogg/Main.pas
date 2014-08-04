(*
 ACS Wav to Ogg file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at aborovsky@mtu-net.ru
 You will need Ogg Vorbis codec libraries to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Vorbis, ACS_Wave, Spin;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    VorbisOut1: TVorbisOut;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure VorbisOut1Progress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
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
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    if SaveDialog1.Execute then
    begin
      VorbisOut1.FileName := SaveDialog1.FileName;
      VorbisOut1.Compression := SpinEdit1.Value/100;
      VorbisOut1.Run;
      Button1.Enabled := False;
    end;
  end;
end;

procedure TForm1.VorbisOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

procedure TForm1.VorbisOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := VorbisOut1.Progress;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  VorbisOut1.Delay := 16 - TrackBar1.Position*5;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  VorbisOut1.Delay := 16 - TrackBar1.Position*5;
end;

end.
