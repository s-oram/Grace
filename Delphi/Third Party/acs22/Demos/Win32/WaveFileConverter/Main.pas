(*
  This is ACS *.wav file converter demo main unit.
  Copyright (c) 2003 Andrei Borovsky.
  You can contact me at aborovsky@mtu-net.ru
  This program demonstrates how to construct
  complex audio processing chains dynamically.
  WavConverter allows to change the properties
  of *.wav files, such as sample rate and number of channels.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ACS_Classes, ACS_Wave, ACS_Converters, ComCtrls;

type
  TForm1 = class(TForm)
    ProgressBar1: TProgressBar;
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
    RateConverter1: TRateConverter;
    SampleConverter1: TSampleConverter;
    MSConverter1: TMSConverter;
    SampleConverter2: TSampleConverter;
    WaveIn1: TWaveIn;
    WaveOut1: TWaveOut;
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
    procedure Button3Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WaveOut1Progress(Sender: TComponent);
  private
    { Private declarations }
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
  CI : TACSInput;
begin
  Ch := SpinEdit1.Value;
  SR := StrToInt(Edit1.Text);
  BPS := SpinEdit3.Value;
  if WaveIn1.Valid and ( WaveOut1.FileName <> '') then
  begin
    CI := WaveIn1;
    if WaveIn1.BitsPerSample = 8 then
    begin
      SampleConverter1.Input := CI;
      CI := SampleConverter1;
    end;
    if WaveIn1.Channels <> Ch then
    begin
      MSConverter1.Input := CI;
      CI := MSConverter1;
    end;
    if WaveIn1.SampleRate <> SR then
    begin
      RateConverter1.Input := CI;
      RateConverter1.OutSampleRate := SR;
      CI := RateConverter1;
    end;
    if BPS = 8 then
    begin
      SampleConverter2.Input := CI;
      CI := SampleConverter2;
    end;
    WaveOut1.Input := CI;
    case ComboBox1.ItemIndex of
      0 : WaveOut1.WavType := wtPCM;
      1 : WaveOut1.WavType := wtDVIADPCM;
    end;                                 
    WaveOut1.Run;
    Button3.Enabled := False;
  end;
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button3.Enabled := True;
  ProgressBar1.Position := 0;
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
      wtUnsupported : Label15.Caption := 'Unsupported';
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    WaveOut1.FileName := SaveDialog1.FileName;
    Label10.Caption := ExtractFileName(WaveOut1.FileName);
  end;
end;

procedure TForm1.WaveOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WaveOut1.Progress;
end;

end.
