(*
 ACS Audio Player demo main unit
 (c) 2002 Andrei Borovsky, all rights reserved
 You can contact me at aborovsky@mtu-net.ru
 This audio player is capable of playing
 .wav, .ogg, .mp3, and .flac files.
 You will need libogg.so, libvorbis.so and
 libvorbisfile.so libraries to play .ogg files
 and libsmpeg.so library to play .mp3 files.

 This Demo shows how play series of files with
 two audio output components (with playlists) and also
 how to handle exceptions that may rise
 in an output component.
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  ACS_Classes, ACS_Audio, ACS_Wave, QStdCtrls, QComCtrls, ACS_Misc,
  ACS_Vorbis, QTypes, QExtCtrls, PlayListUnit, ACS_FLAC;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    AudioOut1: TAudioOut;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    VorbisIn1: TVorbisIn;
    MPEGIn1: TMPEGIn;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    ProgressBar1: TProgressBar;
    AudioOut2: TAudioOut;
    Button6: TButton;
    Button7: TButton;
    StatusBar1: TStatusBar;
    FLACIn1: TFLACIn;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AudioOut1Done(Sender: TComponent);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure AudioOut1ThreadException(Sender: TComponent; E: Exception);
    procedure AudioOut2Done(Sender: TComponent);
  private
    { Private declarations }
    CurrentItem : Integer;
    PLPlaying : Boolean;
    procedure PlayPlayList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  AudioOut1.Delay := 0;
  AudioOut2.Delay := 0;
  AudioOut1.Input := WaveIn1;
  TrackBar1.Position := AudioOut1.Volume;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Ext  : String;
  FI : TACSFileIn;
begin
  StatusBar1.Panels[0].Text := '';
  if OpenDialog1.Execute then
  begin
    Ext := ExtractFileExt(OpenDialog1.FileName);
    if Ext = '.wav' then
    FI := WaveIn1 else
    if Ext = '.ogg' then
    FI := VorbisIn1 else
    if Ext = '.mp3' then
    FI := MPEGIn1 else
    if Ext = '.flac' then
    FI := FLACIn1 else
    Exit;
    AudioOut1.Input := FI;
    FI.FileName := OpenDialog1.FileName;
    if not FI.Valid then Exit;
    FI.Loop := CheckBox1.Checked;
    Label1.Caption := ExtractFileName(OpenDialog1.FileName);
    AudioOut1.Run;
    Button1.Enabled := False;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if PLPlaying then
  CurrentItem := PLForm.ListBox1.Items.Count;
  AudioOut1.Stop;
  AudioOut2.Stop;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  AudioOut1.Volume := TrackBar1.Position;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AudioOut1.Input as TACSFileIn;
  FI.Loop := CheckBox1.Checked;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AudioOut1.Input as TACSFileIn;
  FI.Jump(-10);
end;


procedure TForm1.Button5Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AudioOut1.Input as TACSFileIn;
  FI.Jump(10);
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AudioOut1.Input as TACSFileIn;
  FI.Jump(-100);
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
  if PLPlaying then
  begin
    PlayPlayList;
  end else
  begin
    Button1.Enabled := True;
    Button7.Enabled := True;
  end;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
var
  AOut : TAudioOut;
begin
// Don't use TLabel here
  AOut := Sender as TAudioOut;
  ProgressBar1.Position := AOut.Progress;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
  if PLForm.ListBox1.Items.Count > 0 then
  begin
    CurrentItem := 0;
    Self.PLPlaying := True;
    PlayPlayList;
    Button1.Enabled := False;
    Button7.Enabled := False;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  PLForm.ShowModal;
end;

procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  E: Exception);
var
  AO : TACSOutput;
begin
 AO := Sender as TACSOutput;
 AO.Input.Reset;
 StatusBar1.Panels[0].Text := E.Message;
 Button1.Enabled := True;
 Button7.Enabled := True;
end;

procedure TForm1.PlayPlayList;
var
  FN, Ext  : String;
  FI : TACSFileIn;
begin
  if CurrentItem >= PLForm.ListBox1.Items.Count then
  begin
    Button1.Enabled := True;
    Button7.Enabled := True;
    PLPlaying := False;
    Exit;
  end;
  FN := PLForm.ListBox1.Items.Strings[CurrentItem];
  Ext := ExtractFileExt(FN);
  if Ext = '.wav' then
  FI := WaveIn1 else
  if Ext = '.ogg' then
  FI := VorbisIn1 else
  if Ext = '.mp3' then
  FI := MPEGIn1 else
  if Ext = '.flac' then
  FI := FLACIn1 else
  Exit;
  if Odd(CurrentItem) then AudioOut2.Input := FI
  else AudioOut1.Input := FI;
  FI.FileName := FN;
  if not FI.Valid then Exit;
  Label1.Caption := ExtractFileName(OpenDialog1.FileName);
  if Odd(CurrentItem) then AudioOut2.Run
  else AudioOut1.Run;
  Label1.Caption := ExtractFileName(FN);
  Inc(CurrentItem);
end;

procedure TForm1.AudioOut2Done(Sender: TComponent);
begin
  if PLPlaying then
  begin
    PlayPlayList;
  end else
  begin
    Button1.Enabled := True;
    Button7.Enabled := True;
  end;
end;

end.
