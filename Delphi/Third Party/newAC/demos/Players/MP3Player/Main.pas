(*
 NewAC MP3 player demo.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  ACS_smpeg, Buttons, ACS_DXAudio, ACS_WinMedia, AuASIO, ACS_Converters,
  ACS_Misc;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    MP3In1: TMP3In;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    ForwardButton: TButton;
    BackwardButton: TButton;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    DXAudioOut1: TDXAudioOut;
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AudioOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure ForwardButtonClick(Sender: TObject);
    procedure BackwardButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    FS : TFileStream;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  Secs : String;
  FN : String;
begin
  if OpenDialog1.Execute then
  begin
    MP3In1.FileName := OpenDialog1.FileName;
    if not MP3In1.Valid then
    begin
      StatusBar1.Panels[0].Text := 'MP3 file is not valid.';
      Exit;
    end;
    BitBtn1.Enabled := False;
    StatusBar1.Panels[0].Text := MP3In1.FileName;
    Label3.Caption :=  MP3In1.Id3v2Tags.Artist;
    Label9.Caption :=  MP3In1.Id3v2Tags.Album;
    Label11.Caption :=  MP3In1.Id3v2Tags.Title;
    Label13.Caption :=  MP3In1.Id3v2Tags.Year;
    Label1.Caption := IntToStr(MP3In1.BitRate div 1000) + ' kbps';
    Label4.Caption := IntToStr(MP3In1.SampleRate) + ' Hz';
    if MP3In1.Channels = 1 then Label8.Caption := 'Mono'
    else Label8.Caption := 'Stereo';
    Secs := IntToStr(MP3In1.TotalTime mod 60);
    if MP3In1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [MP3In1.TotalTime div 60, Secs]);
    MP3In1.Loop := CheckBox1.Checked;
    DXAudioOut1.Run;
  end;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := DXAudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   ProgressBar1.Position := 0;
   StatusBar1.Panels[0].Text := DXAudioOut1.ExceptionMessage;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then
  DXAudioOut1.Pause
  else
  DXAudioOut1.Resume;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
end;

procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
end;

procedure TForm1.ForwardButtonClick(Sender: TObject);
begin
  DXAudioOut1.Jump(100);
end;

procedure TForm1.BackwardButtonClick(Sender: TObject);
begin
  DXAudioOut1.Jump(-100);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  MP3In1.Loop := CheckBox1.Checked;
end;

end.
