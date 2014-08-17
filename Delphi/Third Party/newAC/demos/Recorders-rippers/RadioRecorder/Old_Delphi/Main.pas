(*
 NewAC Internet Radio Recorder demo.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  Buttons, ACS_WinMedia, ACS_DXAudio, ACS_Wave, NewACIndicators;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    DXAudioOut1: TDXAudioOut;
    Label14: TLabel;
    WMStreamedIn1: TWMStreamedIn;
    ComboBox1: TComboBox;
    RecordButton: TButton;
    PauseRecordButton: TButton;
    WMATap1: TWMATap;
    TrackBar1: TTrackBar;
    Label15: TLabel;
    Label16: TLabel;
    Edit1: TEdit;
    GainIndicator1: TGainIndicator;
    ProgressBar2: TProgressBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AudioOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure DXAudioOut1Underrun(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure WMStreamedIn1StreamOpened(Sender: TComponent);
    procedure WMStreamedIn1StartedPlaying(Sender: TComponent);
    procedure RecordButtonClick(Sender: TObject);
    procedure PauseRecordButtonClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure DXAudioOut1ThreadException(Sender: TComponent);
    procedure GainIndicator1GainData(Sender: TComponent);
  private
    { Private declarations }
    SL : TStringList;
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
begin
  WMStreamedIn1.FileName := SL.Values[ComboBox1.Text];
  StatusBar1.Panels[0].Text := WMStreamedIn1.FileName;
  WMStreamedIn1.BufferingTime := 2;
  Label14.Caption := 'Connecting...';
  if not WMStreamedIn1.HasAudio then
    raise Exception.Create('Could not find an audio stream in this file.');
  BitBtn1.Enabled := False;
  DXAudioOut1.Run;
  Label3.Caption :=  WMStreamedIn1.Id3v2Tags.Artist;
  Label9.Caption :=  WMStreamedIn1.Id3v2Tags.Album;
  Label11.Caption :=  WMStreamedIn1.Id3v2Tags.Title;
  Label13.Caption :=  WMStreamedIn1.Id3v2Tags.Year;
  Label1.Caption := IntToStr(WMStreamedIn1.BitRate div 1000) + ' kbps';
  Label4.Caption := IntToStr(WMStreamedIn1.SampleRate) + ' Hz';
  if WMStreamedIn1.Channels = 1 then Label8.Caption := 'Mono'
  else Label8.Caption := 'Stereo';
  Secs := IntToStr(WMStreamedIn1.TotalTime mod 60);
  if WMStreamedIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
  Label7.Caption := Format('%d:%s', [WMStreamedIn1.TotalTime div 60, Secs]);

end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := DXAudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   ProgressBar1.Position := 0;
   ProgressBar2.Position := 0;
   if DXAudioOut1.ExceptionMessage = '' then
     Label14.Caption := 'Ready';
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then
  begin
    DXAudioOut1.Pause;
    Label14.Caption := 'Paused';
  end else
  begin
    DXAudioOut1.Resume;
    if WMATap1.Status <> tosIdle then
        Label14.Caption := 'Recording'
    else
        Label14.Caption := 'Playing' 
  end;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
  SL.Free;
end;

procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
end;


procedure TForm1.DXAudioOut1Underrun(Sender: TComponent);
begin
  Label14.Caption := 'Underruns: ' + IntToStr(DXAudioOut1.Underruns);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  SL := TStringList.Create;
  SL.LoadFromFile('stations.ini');
  for i := 0 to SL.Count - 1 do
    ComboBox1.Items.Add(SL.Names[i]);
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.GainIndicator1GainData(Sender: TComponent);
begin
  Self.ProgressBar2.Position := Round(ProgressBar2.Position*0.67 + GainIndicator1.GainValue/60*33);
end;

procedure TForm1.WMStreamedIn1StreamOpened(Sender: TComponent);
begin
  Label14.Caption := 'Stream Opened...'
end;

procedure TForm1.WMStreamedIn1StartedPlaying(Sender: TComponent);
begin
  Label14.Caption := 'Playing';
  DXAudioOut1.Volume := TrackBar1.Position;
end;

procedure TForm1.RecordButtonClick(Sender: TObject);
begin
  if DXAudioOut1.Status = tosIdle then Exit;
  if WMATap1.Status = tosIdle then
  begin
    if OpenDialog1.Execute then
    begin
      WMATap1.FileName := OpenDialog1.FileName;
      Label14.Caption := 'Recording';
      WMATap1.Id3v2Tags.Title := WMStreamedIn1.Id3v2Tags.Title;
      WMATap1.Id3v2Tags.Artist := WMStreamedIn1.Id3v2Tags.Artist;
      WMATap1.DesiredBitrate := StrToInt(Edit1.Text);
      WMATap1.StartRecord;
    end;
  end else
  begin
    WMATap1.StopRecord;
    Label14.Caption := 'Playing';
  end;  
end;

procedure TForm1.PauseRecordButtonClick(Sender: TObject);
begin
  if WMATap1.Status = tosPaused then
  begin
    WMATap1.ResumeRecord;
    Label14.Caption := 'Recording'
  end else
  begin
    WMATap1.PauseRecord;
    Label14.Caption := 'Recording paused'
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  DXAudioOut1.Volume := TrackBar1.Position;
end;

procedure TForm1.DXAudioOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels.Items[0].Text := DXAudioOut1.ExceptionMessage; 
end;

end.
