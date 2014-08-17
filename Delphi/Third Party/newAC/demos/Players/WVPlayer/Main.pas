(*
 NewAC WavPack player demo.
 To run this demo you will need the WavPackDll.dll library.
 See ACS documentation on where to find the library.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  Buttons, ACS_WavPack, ACS_DXAudio;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    CheckBox1: TCheckBox;
    WVIn1: TWVIn;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label1: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    DXAudioOut1: TDXAudioOut;
    Label4: TLabel;
    Label6: TLabel;
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure DXAudioOut1ThreadException(Sender: TComponent);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
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

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := DXAudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   SpeedButton1.Enabled := True;
   ProgressBar1.Position := 0;
   StatusBar1.Panels[0].Text := DXAudioOut1.ExceptionMessage;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  WVIn1.Loop := CheckBox1.Checked;
end;

procedure TForm1.DXAudioOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := DXAudioOut1.ExceptionMessage;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  Secs : String;
begin
  if OpenDialog1.Execute then
  begin
    WVIn1.FileName := OpenDialog1.FileName;
    SpeedButton1.Enabled := False;
    StatusBar1.Panels[0].Text := WVIn1.FileName;
    DXAudioOut1.Run;
    Label1.Caption := IntToStr(WVIn1.SampleRate) + ' Hz';
    if WVIn1.Hybrid then
        Label4.Caption := 'Hybrid'
    else
        Label4.Caption := 'Single file';
    if WVIn1.Lossless then
        Label6.Caption := 'Lossless'
    else
        Label6.Caption := 'Lossy';

    if WVIn1.Channels = 1 then Label17.Caption := 'Mono'
    else Label17.Caption := 'Stereo';
    Secs := IntToStr(WVIn1.TotalTime mod 60);
    if WVIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label16.Caption := Format('%d:%s', [WVIn1.TotalTime div 60, Secs]);
    Label14.Caption := IntToStr(WVIn1.BitsPerSample) + '-bit samples';
    Label3.Caption := WVIn1.APEv2Tags.Artist;
    Label9.Caption := WVIn1.APEv2Tags.Album;
    Label11.Caption := WVIn1.APEv2Tags.Title;
    Label13.Caption := WVIn1.APEv2Tags.Year;
    WVIn1.Loop := CheckBox1.Checked;
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then
  DXAudioOut1.Pause
  else
  DXAudioOut1.Resume;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  DXAudioOut1.Jump(100);
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  DXAudioOut1.Jump(-100);
end;

end.
