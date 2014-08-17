(*
 NewAC sound recorder based on DirectSound API.
 Copyright (c) 2007, Andrei Borovsky, anb@symmetrica.net.
 Stores recorded data in Wave and Ogg formats.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Vorbis, StdCtrls, ComCtrls,
  ACS_DXAudio, Spin, ExtCtrls, ACS_Wave, ACS_FLAC, NewACIndicators, NewAC_DSP;

type
  TForm1 = class(TForm)
    SelectFileButton: TButton;
    RecordButton: TButton;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    StopButton: TButton;
    Timer1: TTimer;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    SREdit: TEdit;
    StereoCheckBox: TCheckBox;
    Label5: TLabel;
    RadioGroup1: TRadioGroup;
    PauseButton: TButton;
    VorbisOut1: TVorbisOut;
    WaveOut1: TWaveOut;
    DXAudioIn1: TDXAudioIn;
    FLACOut1: TFLACOut;
    ProgressBar1: TProgressBar;
    GainProcessor1: TGainProcessor;
    CheckBox2: TCheckBox;
    FastGainIndicator1: TFastGainIndicator;
    procedure RecordButtonClick(Sender: TObject);
    procedure SaveDialog1TypeChange(Sender: TObject);
    procedure OutputDone(Sender: TComponent);
    procedure StopButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SelectFileButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GainIndicator1GainData(Sender: TComponent);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
    Output : TAuFileOut;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.RecordButtonClick(Sender: TObject);
var
  Ext : String;
begin
  if SaveDialog1.FileName = '' then SelectFileButtonClick(Sender);
  Ext := ExtractFileExt(SaveDialog1.FileName);
  Ext := AnsiLowerCase(Ext);
  if Ext = '' then
    raise EAuException.Create('Cannot determine the input file format');
  if Ext = '.ogg' then
  begin
    Output := VorbisOut1;
    VorbisOut1.Compression := SpinEdit1.Value/10;
  end else
  if Ext = '.flac' then
  begin
    Output := FLACOut1;
  end else
  if Ext = '.wav' then
    Output := WaveOut1;
  Output.FileName := SaveDialog1.FileName;
  DXAudioIn1.InSampleRate := StrToInt(SREdit.Text);
  if StereoCheckBox.Checked then
    DXAudioIn1.InChannels := 2
  else
    DXAudioIn1.InChannels := 1;
  if RadioGroup1.ItemIndex = 0 then
    DXAudioIn1.InBitsPerSample := 16
  else
    DXAudioIn1.InBitsPerSample := 24;
  RecordButton.Enabled := False;
  SelectFileButton.Enabled := False;
  SpinEdit2.Enabled := False;
  Output.Run;
  StatusBar1.Panels.Items[0].Text := Format('Recording to "%s"', [ExtractFileName(SaveDialog1.FileName)]);
  Timer1.Interval := 1000;
end;


procedure TForm1.SaveDialog1TypeChange(Sender: TObject);
begin
  if (SaveDialog1.FilterIndex = 1) then
  SaveDialog1.DefaultExt := '.wav'
  else
  SaveDialog1.DefaultExt := '.ogg'
end;

procedure TForm1.OutputDone(Sender: TComponent);
begin
  SelectFileButton.Enabled := True;
  RecordButton.Enabled := True;
  SpinEdit2.Enabled := True;
  if Output <> nil then
  begin
    Output.Stop(False);
  end;
  ProgressBar1.Position := 0;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  if Output <> nil then
    Output.Stop;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Output <> nil then
  begin
    Self.StatusBar1.Panels.Items[1].Text := Format('%d seconds elapsed', [Output.TimeElapsed]);
    Self.StatusBar1.Panels.Items[2].Text := IntToStr(DXAudioIn1.Overruns) + ' overruns';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SpinEdit2.MaxValue := DXAudioIn1.DeviceCount - 1;
  SpinEdit2.MinValue := 0;
  SpinEdit2.Value := 0;
  Label4.Caption := DXAudioIn1.DeviceName[SpinEdit2.Value];
end;

procedure TForm1.GainIndicator1GainData(Sender: TComponent);
begin
  Self.ProgressBar1.Position := (Self.ProgressBar1.Position + FastGainIndicator1.GainValue) div 2;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  Label4.Caption := DXAudioIn1.DeviceName[SpinEdit2.Value];
  DXAudioIn1.DeviceNumber := SpinEdit2.Value;
end;

procedure TForm1.SelectFileButtonClick(Sender: TObject);
begin
  SaveDialog1.Execute;
end;

procedure TForm1.PauseButtonClick(Sender: TObject);
begin
  if Output <> nil then
  begin
    if Output.Status = tosPlaying then Output.Pause
    else
    if Output.Status = tosPaused then Output.Resume;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Output <> nil then
  begin
    Output.Stop(False);
  end;  
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  GainProcessor1.SkipSilenceEnabled := CheckBox2.Checked;
end;

end.
