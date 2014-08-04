(*
 ACS MP3 Converter Demo main unit.
 Copyright (c) 2003, Andrei Borovsky.
 This Demo converts MP3 files into either wav or ogg files.
 Use Input to select an input mp3 file, Output to select the output file name
 and Convert to do the conversion.
 You will need MADLib.dll to run this demo (see ACS documentation on how to get it).
 In order to convert to ogg you will also need Ogg Vorbis libraries. This demo
 is functional without Ogg Vorbis codec, but in this case you will be able to
 convert into wav files only.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Vorbis, StdCtrls, ComCtrls, ACS_MAD, ACS_Wave;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    MP3ToWav1: TMP3ToWav;
    Button3: TButton;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    Edit1: TEdit;
    StatusBar1: TStatusBar;
    WaveIn1: TWaveIn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure MP3ToWav1Done(Sender: TComponent; Success: Boolean);
    procedure Button3Click(Sender: TObject);
    procedure MP3ToWav1Progress(Sender: TComponent);
  private
    { Private declarations }
    VorbisOut : TVorbisOut;
    HasVorbis : Boolean;
    MS : TMemoryStream;
    DoingWav : Boolean;
    procedure OnVorbisDone(Sender : TComponent);
    procedure OnVorbisProgress(Sender: TComponent);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  try
    VorbisOut := TVorbisOut.Create(Self);
    VorbisOut.OnDone := OnVorbisDone;
    VorbisOut.OnProgress := OnVorbisProgress;
    VorbisOut.Input := WaveIn1;
    HasVorbis := True;
  except
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MP3ToWav1.Stop;
  while MP3ToWav1.Buisy do;
  if HasVorbis then
  begin
    VorbisOut.Stop;
    while VorbisOut.Status <> tosIdle do;
    VorbisOut.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  MP3ToWav1.InputFile := OpenDialog1.FileName;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not HasVorbis then
  begin
    Edit1.Enabled := False;
    SaveDialog1.Filter := 'Wave|*.wav';
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Ext : String;
begin
  if SaveDialog1.Execute then
  begin
    Ext := ExtractFileExt(SaveDialog1.FileName);
    if AnsiCompareText(Ext, '.ogg') = 0 then
    begin
      if not HasVorbis then  ShowMessage('Ogg Vorbis libraries not found. Cannot convert to ogg.')
      else
      begin
        DoingWav := False;
        VorbisOut.FileName := SaveDialog1.FileName;
      end;
    end else
    begin
      DoingWav := True;
      MP3ToWav1.OutputFile := SaveDialog1.FileName;
    end;
  end;
end;

procedure TForm1.OnVorbisDone;
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
  ProgressBar1.Position := 0;
  MS.Free;
end;

procedure TForm1.MP3ToWav1Done(Sender: TComponent; Success: Boolean);
begin
  if DoingWav then
  begin
    Button1.Enabled := True;
    Button2.Enabled := True;
    Button3.Enabled := True;
    ProgressBar1.Position := 0;
  end else
  begin
    MS.Seek(0, soFromBeginning);
    WaveIn1.Stream := MS;
    VorbisOut.Run;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  StatusBar1.Panels.Items[0].Text := 'Converting to wav';
  if not DoingWav then
  begin
    MS := TMemoryStream.Create;
    MP3ToWav1.OutputFile := '';
    MP3ToWav1.OutputStream := MS;
    StatusBar1.Panels.Items[0].Text := 'Converting to ogg';
  end;
  Button1.Enabled := False;
  Button2.Enabled := False;
  Button3.Enabled := False;
  MP3ToWav1.Run;
end;

procedure TForm1.MP3ToWav1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := MP3ToWav1.Progress;
end;

procedure TForm1.OnVorbisProgress;
begin
  ProgressBar1.Position := VorbisOut.Progress;
end;

end.
