(*
  ACS Sound Recording Demo.
  (c) 2002 Andrei Borovsky, all rights reserved
  You can contact me at aborovsky@mtu-net.ru
  This program records sound from your soundcard
  and saves it in a .wav or .flac file.
  Use your mixer software
  to set the recording source and volume.
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QTypes, QExtCtrls, QStdCtrls, ACS_Classes, ACS_Wave, ACS_Audio, QComCtrls,
  ACS_Indicator, ACS_FLAC;

type
  TForm1 = class(TForm)
    AudioIn1: TAudioIn;
    WaveOut1: TWaveOut;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    SaveDialog1: TSaveDialog;
    ProgressBar1: TProgressBar;
    FLACOut1: TFLACOut;
    SoundIndicator1: TSoundIndicator;
    ProgressBar2: TProgressBar;
    Timer1: TTimer;
    ComboBox1: TComboBox;
    Label4: TLabel;
    ComboBox2: TComboBox;
    Label5: TLabel;
    ComboBox3: TComboBox;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure Button2Click(Sender: TObject);
    procedure WaveOut1Progress(Sender: TComponent);
    procedure SaveDialog1FilterChange(Sender: TObject; NewIndex: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FO : TACSFileOut;
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Ext : String;
begin
  if SaveDialog1.Execute then
  begin
    case ComboBox1.ItemIndex of
      0 : AudioIn1.InSampleRate := 8000;
      1 : AudioIn1.InSampleRate := 11025;
      2 : AudioIn1.InSampleRate := 16000;
      3 : AudioIn1.InSampleRate := 22050;
      4 : AudioIn1.InSampleRate := 44100;
    end;
    if ComboBox2.ItemIndex = 0 then AudioIn1.InChannels:= 1
    else AudioIn1.InChannels:= 2;
    if ComboBox3.ItemIndex = 0 then AudioIn1.InBitsPerSample := 8
    else AudioIn1.InBitsPerSample:= 16;
    AudioIn1.RecTime := StrToInt(Edit1.Text);
    Ext := ExtractFileName(SaveDialog1.FileName);
    if Ext = '.wav' then
    FO := WaveOut1
    else
    FO := FLACOut1;
    FO.FileName := SaveDialog1.FileName;
    FO.Run;
    Button1.Enabled := False;
  end;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if FO <> nil then FO.Stop;
end;

procedure TForm1.WaveOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := FO.Progress;
  //Label2.Caption := IntToStr(WaveOut1.TimeElapsed);
end;

procedure TForm1.SaveDialog1FilterChange(Sender: TObject;
  NewIndex: Integer);
begin
  case NewIndex of
    0 : SaveDialog1.DefaultExt := 'wav';
    1 : SaveDialog1.DefaultExt := 'flac';
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Vals : array[0..31] of Double;
begin
  SoundIndicator1.GetValues(Vals);
  ProgressBar2.Position := Round(Vals[0]);
  Label2.Caption := IntToStr(Round(AudioIn1.Position /((AudioIn1.BitsPerSample shr 3)*AudioIn1.Channels)/AudioIn1.SampleRate));
end;

end.
