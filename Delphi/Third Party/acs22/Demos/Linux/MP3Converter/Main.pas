(*
 ACS MP3 Converter demo main unit.
 (c) 2003, Andrei Borovsky.
 This converter is capable to convert MP3 files into
 wav and Ogg Vorbis formats.
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, ACS_Classes, ACS_MAD, ACS_Vorbis, ACS_Wave,
  QComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    WaveIn1: TWaveIn;
    VorbisOut1: TVorbisOut;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    Edit1: TEdit;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    MP3ToWav1: TMP3ToWav;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SaveDialog1FilterChange(Sender: TObject; NewIndex: Integer);
    procedure Button3Click(Sender: TObject);
    procedure MP3ToWav1Done(Sender: TComponent; Success: Boolean);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure VorbisOut1Progress(Sender: TComponent);
    procedure MP3ToWav1Progress(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
    DoingOgg : Boolean;
    MS : TMemoryStream;
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  MP3ToWav1.InputFile := OpenDialog1.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Ext : String;
begin
  if SaveDialog1.Execute then
  begin
    if ExtractFileExt(SaveDialog1.FileName) = '.ogg' then
    begin
      DoingOgg := True;
      MS := TMemoryStream.Create;
      MP3ToWav1.OutputFile := '';
      MP3ToWav1.OutputStream := MS;
      VorbisOut1.FileName := SaveDialog1.FileName;
    end else
    begin
      DoingOgg := False;
      MP3ToWav1.OutputFile := SaveDialog1.FileName;
    end;
  end;

end;

procedure TForm1.SaveDialog1FilterChange(Sender: TObject;
  NewIndex: Integer);
begin
  case NewIndex of
    0 : SaveDialog1.DefaultExt := 'wav';
    1 : SaveDialog1.DefaultExt := 'ogg';
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if MP3ToWav1.InputFile = '' then Button1Click(Self);
  if (MP3ToWav1.OutputFile = '') and (VorbisOut1.FileName = '') then Button2Click(Self);
  Button1.Enabled := False;
  Button2.Enabled := False;
  Button3.Enabled := False;
  MP3ToWav1.Run;
  if DoingOgg then VorbisOut1.Compression := StrToFloat(Edit1.Text);
  Label2.Caption := 'Converting MP3 to wav';
end;

procedure TForm1.MP3ToWav1Done(Sender: TComponent; Success: Boolean);
var
  S : String;
begin
  S := '';
  if DoingOgg then
  begin
    MS.Seek(0, soFromBeginning);
    WaveIn1.Stream := MS;
    S := 'Converting wav to ogg';
    VorbisOut1.Run;
  end else
  begin
    Button1.Enabled := True;
    Button2.Enabled := True;
    Button3.Enabled := True;
  end;
  ProgressBar1.Position := 0;
  Label2.Caption := S;
end;

procedure TForm1.VorbisOut1Done(Sender: TComponent);
begin
  MS.Free;
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
  ProgressBar1.Position := 0;
  Label2.Caption := '';
end;

procedure TForm1.VorbisOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := VorbisOut1.Progress;
end;

procedure TForm1.MP3ToWav1Progress(Sender: TComponent);
begin
 ProgressBar1.Position := MP3ToWav1.Progress;
end;

end.
