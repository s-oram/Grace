unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ACS_Classes, ACS_WinMedia, ACS_smpeg, StdCtrls,
  ACS_Wave, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    MP3In1: TMP3In;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    WaveOut1: TWaveOut;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure WaveOut1Progress(Sender: TComponent);
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
  begin
    MP3In1.FileName := OpenDialog1.FileName;
    SaveDialog1.FileName := ChangeFileExt(MP3In1.FileName, '.wav');
    if SaveDialog1.Execute then
    begin
      WaveOut1.FileName := SaveDialog1.FileName;
      Button1.Enabled := False;
      WaveOut1.Run;
    end;  
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

end.
