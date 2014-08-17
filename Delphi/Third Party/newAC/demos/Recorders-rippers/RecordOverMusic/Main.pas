unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ACS_AudioMix, ACS_Classes, ACS_Wave, ACS_DXAudio,
  ComCtrls, NewACIndicators;

type
  TForm15 = class(TForm)
    DXAudioIn1: TDXAudioIn;
    WaveOut1: TWaveOut;
    WaveIn1: TWaveIn;
    RealTimeMixer1: TRealTimeMixer;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    StatusBar1: TStatusBar;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    FastGainIndicator1: TFastGainIndicator;
    FastGainIndicator2: TFastGainIndicator;
    SaveDialog1: TSaveDialog;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure GainIndicator1GainData(Sender: TComponent);
    procedure GainIndicator2GainData(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form15: TForm15;

implementation

{$R *.dfm}

procedure TForm15.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    if WaveIn1.Valid then
    begin
      DXAudioIn1.InBitsPerSample := WaveIn1.BitsPerSample;
      DXAudioIn1.InChannels := WaveIn1.Channels;
      DXAudioIn1.InSampleRate := WaveIn1.SampleRate;
      RealTimeMixer1.OutBitsPerSample := WaveIn1.BitsPerSample;
      RealTimeMixer1.OutChannels := WaveIn1.Channels;
      RealTimeMixer1.OutSampleRate := WaveIn1.SampleRate;
      if SaveDialog1.Execute then
      begin
        Button1.Enabled := False;
        WaveOut1.FileName := SaveDialog1.FileName;
        WaveOut1.Run;
      end;
    end else
    begin
      StatusBar1.Panels[0].Text := 'Input file is not valid';
    end;
  end;
end;

procedure TForm15.Button2Click(Sender: TObject);
begin
  WaveOut1.Stop;
end;

procedure TForm15.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WaveOut1.Stop(False);
end;

procedure TForm15.FormCreate(Sender: TObject);
begin
  SpinEdit1.MaxValue := DXAudioIn1.DeviceCount - 1;
  Label1.Caption := DXAudioIn1.DeviceName[SpinEdit1.Value];
end;

procedure TForm15.GainIndicator1GainData(Sender: TComponent);
begin
  ProgressBar1.Position := FastGainIndicator1.GainValue;
end;

procedure TForm15.GainIndicator2GainData(Sender: TComponent);
begin
  ProgressBar2.Position := FastGainIndicator2.GainValue;
end;

procedure TForm15.SpinEdit1Change(Sender: TObject);
begin
  DXAudioIn1.DeviceNumber := SpinEdit1.Value;
  Label1.Caption := DXAudioIn1.DeviceName[SpinEdit1.Value];
end;

procedure TForm15.WaveOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

end.
