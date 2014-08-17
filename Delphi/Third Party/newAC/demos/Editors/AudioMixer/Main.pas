(*
 NewAC Audio Mixer demo main unit
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_Wave, ACS_AudioMix, ACS_DXAudio,
  ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    WaveIn2: TWaveIn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    TrackBar2: TTrackBar;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Button4: TButton;
    DXAudioOut1: TDXAudioOut;
    WaveOut1: TWaveOut;
    AudioMixer1: TAudioMixer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure Button4Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  WaveIn1.FileName := OpenDialog1.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  WaveIn2.FileName := OpenDialog1.FileName;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Button1.Enabled := False;
  Button2.Enabled := False;
  Button3.Enabled := False;
  if RadioGroup1.ItemIndex = 0 then
    AudioMixer1.Mode := amMix
  else
    AudioMixer1.Mode := amConcatenate;
  if RadioGroup2.ItemIndex = 0 then
  begin
    if SaveDialog1.Execute then
    begin
      WaveOut1.FileName := SaveDialog1.FileName;
      WaveOut1.Run;
    end else
    begin
    Button1.Enabled := True;
    Button2.Enabled := True;
    Button3.Enabled := True;
   end;
  end
  else
   DXAudioOut1.Run;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then DXAudioOut1.Stop;
  if WaveOut1.Status = tosPlaying then WaveOut1.Stop;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  AudioMixer1.Volume2 := TrackBar2.Position
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  AudioMixer1.Volume1 := TrackBar1.Position
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrackBar1.Position := amMaxVolume;
  TrackBar2.Position := amMaxVolume;
end;

end.
