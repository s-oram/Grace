(*
 ACS LibAO Player demo main unit
 (c) 2002 Andrei Borovsky, all rights reserved
 You can contact me at aborovsky@mtu-net.ru
 This audio player is capable of playing
 .wav, .ogg, and .mp3 files.
 You will need libogg.so, libvorbis.so and
 libvorbisfile.so libraries to play .ogg files
 and libsmpeg.so library to play .mp3 files.
 And of course, you need a libao.so library
 to play this demo.
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  ACS_Classes, ACS_Audio, ACS_Wave, QStdCtrls, QComCtrls, ACS_Misc,
  ACS_Vorbis, QTypes, QExtCtrls;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
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
    ComboBox1: TComboBox;
    Label2: TLabel;
    AOLive1: TAOLive;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure AOLive1Done(Sender: TComponent);
    procedure AOLive1Progress(Sender: TComponent);
    procedure ComboBox1Select(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  AOLive1.Input := WaveIn1;
  TrackBar1.Position := AOLive1.Volume;
  ComboBox1.Items.Clear;
  for i:= 1 to AOLive1.Drivers.Count -1 do
  if AOLive1.IsDevicePlayable(AOLive1.Drivers.Strings[i]) then
  ComboBox1.Items.Add(AOLive1.Drivers.Strings[i]);
  ComboBox1.Text := AOLive1.DefaultDriver;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Ext  : String;
  FI : TACSFileIn;
begin
  if OpenDialog1.Execute then
  begin
    Ext := ExtractFileExt(OpenDialog1.FileName);
    if Ext = '.wav' then
    FI := WaveIn1 else
    if Ext = '.ogg' then
    FI := VorbisIn1 else
    if Ext = '.mp3' then
    FI := MPEGIn1 else
    Exit;
    AOLive1.Input := FI;
    FI.FileName := OpenDialog1.FileName;
    if not FI.Valid then Exit;
    FI.Loop := CheckBox1.Checked;
    Label1.Caption := ExtractFileName(OpenDialog1.FileName);
    AOLive1.Run;
    Button1.Enabled := False;
    ComboBox1.Enabled := False;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AOLive1.Stop;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  AOLive1.Volume := TrackBar1.Position;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AOLive1.Input as TACSFileIn;
  FI.Loop := CheckBox1.Checked;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AOLive1.Input as TACSFileIn;
  FI.Jump(-10);
end;


procedure TForm1.Button5Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AOLive1.Input as TACSFileIn;
  FI.Jump(10);
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  FI := AOLive1.Input as TACSFileIn;
  FI.Jump(-100);
end;

procedure TForm1.AOLive1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  ComboBox1.Enabled := True;
end;

procedure TForm1.AOLive1Progress(Sender: TComponent);
var
  AOut : TAOLive;
begin
// Don't use TLabel here
  AOut := Sender as TAOLive;
  ProgressBar1.Position := AOut.Progress;
end;
procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  AOLive1.Driver := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
end;


end.
