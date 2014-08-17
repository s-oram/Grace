unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_WinMedia, ACS_Misc, ACS_smpeg, ExtCtrls;

type
  TForm5 = class(TForm)
    MP3In1: TMP3In;
    AudioPlayList1: TAudioPlayList;
    WMStreamedOut1: TWMStreamedOut;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel3: TPanel;
    Label5: TLabel;
    Edit1: TEdit;
    Label6: TLabel;
    Edit2: TEdit;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WMStreamedOut1Done(Sender: TComponent);
    procedure AudioPlayList1PlayItemChanged(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.AudioPlayList1PlayItemChanged(Sender: TComponent);
begin
  Label2.Caption := MP3In1.Id3v2Tags.Artist;
  Label3.Caption := MP3In1.Id3v2Tags.Title;
  Label4.Caption := MP3In1.Id3v2Tags.Album;
end;

procedure TForm5.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    MP3In1.FileName := OpenDialog1.FileName;
    WMStreamedOut1.Port := StrToInt(Edit1.Text);
    WMStreamedOut1.DesiredBitrate := StrToInt(Edit2.Text);
    WMStreamedOut1.Run;
    Button1.Enabled := False;
  end;
end;

procedure TForm5.Button2Click(Sender: TObject);
begin
  WMStreamedOut1.Stop;
  Button1.Enabled := True;
end;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WMStreamedOut1.Stop;
end;

procedure TForm5.WMStreamedOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Label2.Caption := '';
  Label3.Caption := '';
  Label4.Caption := '';
end;

end.
