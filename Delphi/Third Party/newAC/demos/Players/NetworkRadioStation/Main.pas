(*
  Network broadcasting demo.
  Written by Andrei Borovsky, anb@symmetrica.net
  This demo broadcasts audio in streaming WMA format using the port specified.
  You can listen broadcasts with any player capable of reading WMA streams.
  For example in Windows Media Player use command File>>Open URL...
  enter the URL in the form http://broadcasting_host:selected_port
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Wave, StdCtrls, ComCtrls, ACS_Vorbis,
  ACS_FLAC, ACS_smpeg, ExtCtrls, ACS_DXAudio, ACS_WinMedia;

type
  TForm1 = class(TForm)
    PlayButton: TButton;
    ProgressBar1: TProgressBar;
    VorbisIn1: TVorbisIn;
    StopButton: TButton;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    WaveIn1: TWaveIn;
    StatusBar1: TStatusBar;
    FLACIn1: TFLACIn;
    MP3In1: TMP3In;
    Panel1: TPanel;
    WMStreamedOut1: TWMStreamedOut;
    WMIn1: TWMIn;
    Edit1: TEdit;
    Label3: TLabel;
    Memo1: TMemo;
    Edit2: TEdit;
    Label4: TLabel;
    procedure PlayButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AudioOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WMStreamedOut1Done(Sender: TComponent);
    procedure WMStreamedOut1Progress(Sender: TComponent);
    procedure WMStreamedOut1ClientConnected(Sender: TComponent);
    procedure WMStreamedOut1ClientDisconnected(Sender: TComponent);
  private
    { Private declarations }
     FI : TAuFileIn;
     procedure PlayItem;
     procedure FillLog;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.PlayButtonClick(Sender: TObject);
begin
  WMStreamedOut1.Port := StrToInt(Edit1.Text);
  WMStreamedOut1.DesiredBitrate := StrToInt(Edit2.Text);
  PlayItem;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  WMStreamedOut1.Stop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 // AudioOut1.Delay := 10;
end;

procedure TForm1.PlayItem;
var
  Ext, Fmt : String;
  Sec, Min : Integer;
begin
  if OpenDialog1.Execute then
  begin
    Ext := SysUtils.ExtractFileExt(OpenDialog1.FileName);
    Ext := AnsiLowerCase(Ext);
    FI := nil;
    if Ext = '.mp3' then FI := MP3In1;
    if Ext = '.ogg' then FI := VorbisIn1;
    if Ext = '.wav' then FI := WaveIn1;
    if Ext = '.flac' then FI := FLACIn1;
    if Ext = '.wma' then FI := WMIn1;
    if FI = nil then
    begin
      StatusBar1.Panels[0].Text := 'Unknown file extension';
      Exit;
    end;
    FI.FileName := OpenDialog1.FileName;
    FI.Loop := CheckBox1.Checked;
    if not FI.Valid then Exit;
    StatusBar1.Panels[0].Text := 'Playing: ' + ExtractFileName(OpenDialog1.FileName);
    WMStreamedOut1.Input := FI;
    PlayButton.Enabled := False;
    WMStreamedOut1.Run;
    Sec := FI.TotalTime;
    Min := Sec div 60;
    Sec := Sec - Min*60;
    if Sec < 10 then Fmt := '%d:0%d'
    else Fmt := '%d:%d';
    Label2.Caption := Format(Fmt, [Min, Sec]);
  end;
end;


procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
  PlayButton.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WMStreamedOut1.Stop(False);
end;

procedure TForm1.WMStreamedOut1Done(Sender: TComponent);
begin
  PlayButton.Enabled := True;
  StatusBar1.Panels[0].Text := '';
  ProgressBar1.Position := 0;
end;

procedure TForm1.WMStreamedOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WMStreamedOut1.Progress;
end;

procedure TForm1.FillLog;
var
  i : Integer;
  S :String;
begin
  Memo1.Clear;
  for i := 0 to WMStreamedOut1.ConnectionsCount - 1 do
  begin
    S := WMStreamedOut1.Connections[i].IP;
    Memo1.Lines.Add(Format('Address: %s', [S]));
  end;
end;

procedure TForm1.WMStreamedOut1ClientConnected(Sender: TComponent);
begin
  FillLog;
end;

procedure TForm1.WMStreamedOut1ClientDisconnected(Sender: TComponent);
begin
  FillLog;
end;

end.
