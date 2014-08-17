(*
  This is the NewAC *.wav file converter demo main unit.
  Copyright (c) 2003-2008 Andrei Borovsky.
  You can contact me at anb@symmetrica.net
  This program demonstrates how to construct
  complex audio processing chains dynamically.
  WavConverter allows to change the properties
  of *.wav files, such as sample rate and number of channels.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ACS_Classes, ACS_Wave, ACS_Converters, ComCtrls,
  ACS_WinMedia;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    GroupBox2: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label16: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    Button4: TButton;
    ProgressBar1: TProgressBar;
    WaveOut1: TWaveOut;
    AudioConverter1: TAudioConverter;
    StatusBar1: TStatusBar;
    FastResampler1: TFastResampler;
    procedure Button3Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WaveOut1Progress(Sender: TComponent);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WaveOut1ThreadException(Sender: TComponent);
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

procedure TForm1.Button3Click(Sender: TObject);
var
  Ch, BPS, SR : Integer;
begin
  if Output = nil then
    raise EAuException.Create('The output file name is not set!');
  if Output.FileName = '' then
    raise EAuException.Create('The output file name is not set!');
  AudioConverter1.OutBitsPerSample := 0;
  AudioConverter1.OutChannels := 0;
  Ch := SpinEdit1.Value;
  SR := StrToInt(Edit1.Text);
  BPS := SpinEdit3.Value;
  if WaveIn1.Valid then
  begin
    if WaveIn1.BitsPerSample <> BPS then
      AudioConverter1.OutBitsPerSample := BPS;
    if WaveIn1.Channels <> Ch then
      AudioConverter1.OutChannels := Ch;
    FastResampler1.OutSampleRate := StrToInt(Edit1.Text);
    case ComboBox1.ItemIndex of
      0 : WaveOut1.WavType := wtPCM;
      1 : WaveOut1.WavType := wtDVIADPCM;
    end;
    Button3.Enabled := False;
    StatusBar1.Panels[0].Text := 'Converting...';
    Output.Run;
  end;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button3.Enabled := True;
//  ProgressBar1.Position := 0;
  if WaveOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Success!';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    if not WaveIn1.Valid then Exit;
    Label5.Caption := ExtractFileName(WaveIn1.FileName);
    Label6.Caption := IntToStr(WaveIn1.Channels);
    Label7.Caption := IntToStr(WaveIn1.SampleRate);
    Label8.Caption := IntToStr(WaveIn1.BitsPerSample);
    case WaveIn1.WavType of
      wtPCM : Label15.Caption := 'Raw PCM';
      wtDVIADPCM : Label15.Caption := 'DVI IMA ADPCM';
      wtMSADPCM : Label15.Caption := 'Microsoft ADPCM';
      wtACM : Label15.Caption := 'ACM';
      wtExtPCM : Label15.Caption := 'WaveFormatExt (PCM)';
      wtExtIEEEFloat : Label15.Caption := 'WaveFormatExt (IEEE Float)';
      wtIEEEFloat  : Label15.Caption := 'IEEE Float';
      wtUnsupported : Label15.Caption := 'Unsupported';
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    if ExtractFileExt(SaveDialog1.FileName) = '.wav' then
      Output := WaveOut1 as TAuFileOut;
//    else
//      Output := WMAOut1 as TAuFileOut;
    Output.FileName := SaveDialog1.FileName;
    Label10.Caption := ExtractFileName(Output.FileName);
  end;
end;

procedure TForm1.WaveOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WaveOut1.Progress;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  WaveOut1.Stop;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WaveOut1.Stop(False);
end;

procedure TForm1.WaveOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := WaveOut1.ExceptionMessage;
end;

end.
