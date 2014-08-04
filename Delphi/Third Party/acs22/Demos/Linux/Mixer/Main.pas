(*
   ACS Mixer demo
   Copyright (c) Andrei Borovsky, all rights reserved.
   You can contact me at aborovsky@mtu-net.ru. 
*)
unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QExtCtrls, Libc, ACS_Mixer;

type
  TForm1 = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton1: TRadioButton;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    TrackBar9: TTrackBar;
    TrackBar10: TTrackBar;
    TrackBar11: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    TrackBar12: TTrackBar;
    Label11: TLabel;
    TrackBar16: TTrackBar;
    Mixer1: TMixer;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure TrackBar7Change(Sender: TObject);
    procedure TrackBar8Change(Sender: TObject);
    procedure TrackBar9Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure TrackBar10Change(Sender: TObject);
    procedure TrackBar11Change(Sender: TObject);
    procedure TrackBar12Change(Sender: TObject);
    procedure TrackBar16Change(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.TrackBar1Change(Sender: TObject);
var
vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcVolume];
  vol.Left:=100-TrackBar1.Position;
  Mixer1.Level[mcVolume] := vol;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  vol : TMixerLevel;
begin
  Label2.Caption := Mixer1.MixerName;
  vol := Mixer1.Level[mcVolume];
  TrackBar1.Position := 100 - vol.Left;
  TrackBar2.Position := 100 - vol.Right;
  vol := Mixer1.Level[mcPCM];
  TrackBar6.Position := 100 - vol.Left;
  TrackBar7.Position := 100 - vol.Right;
  vol := Mixer1.Level[mcBass];
  TrackBar8.Position := 100 - vol.Main;
  vol := Mixer1.Level[mcTreble];
  TrackBar9.Position := 100 - vol.Main;
  vol := Mixer1.Level[mcLine];
  TrackBar3.Position := 100 - vol.Left;
  TrackBar4.Position := 100 - vol.Right;
  vol := Mixer1.Level[mcCD];
  TrackBar5.Position := 100 - vol.Left;
  TrackBar10.Position := 100 - vol.Right;
  vol := Mixer1.Level[mcMic];
  TrackBar11.Position := 100 - vol.Main;
  vol := Mixer1.Level[mcRecLev];
  TrackBar12.Position := 100 - vol.Left;
  TrackBar16.Position := 100 - vol.Right;
  case Mixer1.RecordSource of
    mcMic : RadioButton3.Checked := True;
    mcCD :  RadioButton2.Checked := True;
    mcLine: RadioButton1.Checked := True;
  end;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
var
vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcVolume];
  vol.Right:=100-TrackBar2.Position;
  Mixer1.Level[mcVolume] := vol;
end;

procedure TForm1.TrackBar6Change(Sender: TObject);
var
vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcPCM];
  vol.Left:=100-TrackBar6.Position;
  Mixer1.Level[mcPCM] := vol;
end;

procedure TForm1.TrackBar7Change(Sender: TObject);
var
vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcPCM];
  vol.Right:=100-TrackBar7.Position;
  Mixer1.Level[mcPCM] := vol;
end;

procedure TForm1.TrackBar8Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcBass];
  vol.Left:=100-TrackBar8.Position;
  Mixer1.Level[mcBass] := vol;
end;

procedure TForm1.TrackBar9Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcTreble];
  vol.Left:=100-TrackBar9.Position;
  Mixer1.Level[mcTreble] := vol;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcLine];
  vol.Left:=100-TrackBar3.Position;
  Mixer1.Level[mcLine] := vol;
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcLine];
  vol.Right:=100-TrackBar4.Position;
  Mixer1.Level[mcLine] := vol;
end;

procedure TForm1.TrackBar5Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcCD];
  vol.Left:=100-TrackBar5.Position;
  Mixer1.Level[mcCD] := vol;
end;

procedure TForm1.TrackBar10Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcCD];
  vol.Right:=100-TrackBar10.Position;
  Mixer1.Level[mcCD] := vol;
end;

procedure TForm1.TrackBar11Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcMic];
  vol.Left:=100-TrackBar11.Position;
  Mixer1.Level[mcMic] := vol;
end;

procedure TForm1.TrackBar12Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcRecLev];
  vol.Left:=100-TrackBar12.Position;
  Mixer1.Level[mcRecLev] := vol;
end;

procedure TForm1.TrackBar16Change(Sender: TObject);
var
  vol : TMixerLevel;
begin
  vol := Mixer1.Level[mcRecLev];
  vol.Right:=100-TrackBar16.Position;
  Mixer1.Level[mcRecLev] := vol;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Mixer1.RecordSource := mcLine;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  Mixer1.RecordSource := mcCD;
end;

procedure TForm1.RadioButton3Click(Sender: TObject);
begin
  Mixer1.RecordSource := mcMic;
end;

end.
