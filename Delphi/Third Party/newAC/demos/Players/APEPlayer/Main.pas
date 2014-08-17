(*
 NewAC APE (Monkey's Auido) player demo.
 To run this demo you will need the MACDll.dll library.
 See ACS documentation on where to find the library.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  Buttons, ACS_MAC, ACS_DXAudio;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    ForwardButton: TButton;
    BackwardButton: TButton;
    CheckBox1: TCheckBox;
    MACIn1: TMACIn;
    DXAudioOut1: TDXAudioOut;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ForwardButtonClick(Sender: TObject);
    procedure BackwardButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure DXAudioOut1ThreadException(Sender: TComponent);
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
    MACIn1.FileName := OpenDialog1.FileName;
    BitBtn1.Enabled := False;
    StatusBar1.Panels[0].Text := MACIn1.FileName;
    Label4.Caption := IntToStr(MACIn1.SampleRate) + ' Hz';
    if MACIn1.Channels = 1 then Label8.Caption := 'Mono'
    else Label8.Caption := 'Stereo';
    Secs := IntToStr(MACIn1.TotalTime mod 60);
    if MACIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [MACIn1.TotalTime div 60, Secs]);
    MACIn1.Loop := CheckBox1.Checked;
    Label1.Caption := MACIn1.APEv2Tags.Artist;
    Label2.Caption := MACIn1.APEv2Tags.Album;
    Label3.Caption := MACIn1.APEv2Tags.Title;
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
  MACIn1.Loop := CheckBox1.Checked;
end;

procedure TForm1.DXAudioOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := DXAudioOut1.ExceptionMessage;
end;

end.
