(* This demo splits long audio files convrted from CD/DVD imagess into separate
  compositions using the supplied cue-sheets *)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_WavPack, ACS_FLAC, ACS_Classes, ACS_MAC, ACS_Misc, StdCtrls,
  ACS_LAME, ComCtrls, ACS_Wave, Vcl.ExtCtrls;

type
  TForm10 = class(TForm)
    Button1: TButton;
    MACIn1: TMACIn;
    FLACIn1: TFLACIn;
    WVIn1: TWVIn;
    OpenDialog1: TOpenDialog;
    MP3Out1: TMP3Out;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    ComboBox2: TComboBox;
    Label3: TLabel;
    CheckBox2: TCheckBox;
    Label4: TLabel;
    ComboBox3: TComboBox;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    WaveIn1: TWaveIn;
    StatusBar1: TStatusBar;
    CheckBox3: TCheckBox;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure MP3Out1Progress(Sender: TComponent);
    procedure MP3Out1Done(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
    Output : TAuTaggedFileOut;
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}


procedure TForm10.MP3Out1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  StatusBar1.Panels[0].Text := MP3Out1.ExceptionMessage;
end;

procedure TForm10.MP3Out1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := MP3Out1.Progress;
end;

procedure TForm10.Button1Click(Sender: TObject);
var
  Ext : WideString;
  FileName : String;
  SL : TStringList;
  i : Integer;
begin
  OpenDialog1.Title := 'Open Source';
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    Ext := ExtractFileExt(FileName);
    Ext := CharLower(@Ext[1]);
    if Ext = '.wav' then
    begin
      MP3Out1.Input := WaveIn1;
      WaveIn1.FileName := FileName;
    end else
    if Ext = '.wv' then
    begin
      MP3Out1.Input := WVIn1;
      WVIn1.FileName := FileName;
      MP3Out1.Id3v1Tags.Artist := WVIn1.APEv2Tags.Artist;
      MP3Out1.Id3v1Tags.Album := WVIn1.APEv2Tags.Album;
      MP3Out1.Id3v1Tags.Title := WVIn1.APEv2Tags.Title;
      MP3Out1.Id3v1Tags.Year := StrToIntDef(WVIn1.APEv2Tags.Year, 0);
    end else
    if Ext = '.ape' then
    begin
      MP3Out1.Input := MACIn1;
      MACIn1.FileName := FileName;
      MP3Out1.Id3v1Tags.Artist := MACIn1.APEv2Tags.Artist;
      MP3Out1.Id3v1Tags.Album := MACIn1.APEv2Tags.Album;
      MP3Out1.Id3v1Tags.Title := MACIn1.APEv2Tags.Title;
      MP3Out1.Id3v1Tags.Year := StrToIntDef(MACIn1.APEv2Tags.Year, 0);
    end else
    if Ext = '.flac' then
    begin
      MP3Out1.Input := FLACIn1;
      FLACIn1.FileName := FileName;
      MP3Out1.Id3v1Tags.Artist := FLACIn1.VorbisComments.Artist;
      MP3Out1.Id3v1Tags.Album := FLACIn1.VorbisComments.Album;
      MP3Out1.Id3v1Tags.Title := FLACIn1.VorbisComments.Title;
      MP3Out1.Id3v1Tags.Year := StrToIntDef(FLACIn1.VorbisComments.Date, 0);
    end;
    SaveDialog1.FileName := ChangeFileExt(OpenDialog1.FileName, '.mp3');
    if SaveDialog1.Execute then
    begin
      MP3Out1.FileName := SaveDialog1.FileName;
      if CheckBox1.Checked then
      begin
        MP3Out1.EnableVBR := True;
        MP3Out1.VBRQuality := TMP3Quality(ComboBox2.ItemIndex);
      end else
      begin
        MP3Out1.EnableVBR := False;
        MP3Out1.BitRate := TMP3Bitrate(ComboBox1.ItemIndex);
      end;
      MP3Out1.EnableBitReservoir := CheckBox2.Checked;
      MP3Out1.StrictISO := CheckBox3.Checked;
      Mp3Out1.Mode := TMP3Mode(ComboBox3.ItemIndex);
      Button1.Enabled := False;
      Mp3Out1.Run;
    end;
  end;

end;

end.
