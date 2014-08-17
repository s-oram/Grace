(*
 NewAC Real Time Audio Mixer demo main unit
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
    RadioGroup2: TRadioGroup;
    Button4: TButton;
    DXAudioOut1: TDXAudioOut;
    WaveOut1: TWaveOut;
    RealTimeMixer1: TRealTimeMixer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure Button4Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  begin
    RealTimeMixer1.Input1 := nil; // dereferece the source
    WaveIn1.FileName := OpenDialog1.FileName; // assign a new file
    RealTimeMixer1.Input1 := WaveIn1; // reassign the source
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    RealTimeMixer1.Input2 := nil; // dereferece the source
    WaveIn2.FileName := OpenDialog1.FileName; // assign a new file
    RealTimeMixer1.Input2 := WaveIn2; // reassign the source
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Button3.Enabled := False;
  if RadioGroup2.ItemIndex = 0 then
  begin
    if SaveDialog1.Execute then
    begin
      WaveOut1.FileName := SaveDialog1.FileName;
      WaveOut1.Run;
    end else
    begin
    Button3.Enabled := True;
   end;
  end
  else
   DXAudioOut1.Run;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button3.Enabled := True;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then DXAudioOut1.Stop;
  if WaveOut1.Status = tosPlaying then WaveOut1.Stop;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  RealTimeMixer1.Volume2 := TrackBar2.Position
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  RealTimeMixer1.Volume1 := TrackBar1.Position
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TrackBar1.Position := amMaxVolume;
  TrackBar2.Position := amMaxVolume;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if DXAudioOut1.Status = tosPlaying then DXAudioOut1.Stop;
  if WaveOut1.Status = tosPlaying then WaveOut1.Stop;
end;

end.
