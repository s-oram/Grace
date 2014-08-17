(*
 NewAC Windows Media player demo.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  Buttons, ACS_WinMedia, ACS_DXAudio;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
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
    WMIn1: TWMIn;
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
    WMIn1.FileName := OpenDialog1.FileName;
    StatusBar1.Panels[0].Text := WMIn1.FileName;
    if not WMIn1.HasAudio then
      raise Exception.Create('Could not find an audio stream in this file.');
    if WMIn1.IsProtected then
      raise Exception.Create('DRM-protected files are not supported.');
    BitBtn1.Enabled := False;
    WMIn1.FormatSelected := wmfDefault; // Play audio in default format
    DXAudioOut1.Run;
    Label3.Caption :=  WMIn1.Id3v2Tags.Artist;
    Label9.Caption :=  WMIn1.Id3v2Tags.Album;
    Label11.Caption :=  WMIn1.Id3v2Tags.Title;
    Label13.Caption :=  WMIn1.Id3v2Tags.Year;
    Label1.Caption := IntToStr(WMIn1.BitRate div 1000) + ' kbps';
    Label4.Caption := IntToStr(WMIn1.SampleRate) + ' Hz';
    if WMIn1.Channels = 1 then
      Label8.Caption := 'Mono'
    else
    if WMIn1.Channels = 2 then
      Label8.Caption := 'Stereo'
    else
    if WMIn1.Channels = 6 then
      Label8.Caption := '5.1 channels'
    else
    if WMIn1.Channels = 8 then
      Label8.Caption := '7.1 channels';
    Secs := IntToStr(WMIn1.TotalTime mod 60);
    if WMIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [WMIn1.TotalTime div 60, Secs]);
    WMIn1.Loop := CheckBox1.Checked;
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
  WMIn1.Loop := CheckBox1.Checked;
end;

end.
