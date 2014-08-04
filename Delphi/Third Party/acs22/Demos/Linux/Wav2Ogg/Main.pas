unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, ACS_Classes, ACS_Wave, ACS_Vorbis;

type
  TForm1 = class(TForm)
    VorbisOut1: TVorbisOut;
    WaveIn1: TWaveIn;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure VorbisOut1Progress(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    if SaveDialog1.Execute then
    begin
      VorbisOut1.FileName := SaveDialog1.FileName;
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

end.
