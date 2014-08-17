unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ACS_Classes, ACS_WinMedia, ACS_smpeg, StdCtrls,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    WMAOut1: TWMAOut;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    Edit1: TEdit;
    Label1: TLabel;
    MP3In1: TMP3In;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure WMAOut1Done(Sender: TComponent);
    procedure WMAOut1Progress(Sender: TComponent);
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
    WMAOut1.FileName := ChangeFileExt(MP3In1.FileName, '.wma');
    WMAOut1.Id3v2Tags := MP3In1.Id3v2Tags;
    WMAOut1.DesiredBitrate := StrToInt(Edit1.Text);
    Button1.Enabled := False;
    WMAOut1.Run;
  end;
end;

procedure TForm1.WMAOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

procedure TForm1.WMAOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WMAOut1.Progress;
end;

end.
