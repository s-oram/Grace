(*
 ACS Wav to Ogg file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
 You will need Ogg Vorbis codec libraries to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin, ACS_Vorbis,
  ExtCtrls;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    StatusBar1: TStatusBar;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    AlbumEdit: TEdit;
    ArtistEdit: TEdit;
    DateEdit: TEdit;
    GenreEdit: TEdit;
    TitleEdit: TEdit;
    VorbisOut1: TVorbisOut;
    Button2: TButton;
    TrackEdit: TEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure VorbisOut1Progress(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure VorbisOut1ThreadException(Sender: TComponent);
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
var
  S : WideString;
  i : Integer;
begin
  If WaveIn1.FileName <> '' then
  begin
    S := WaveIn1.FileName;
    SaveDialog1.FileName := ChangeFileExt(S, '.ogg');
    if SaveDialog1.Execute then
    begin
      VorbisOut1.FileName := SaveDialog1.FileName;
      VorbisOut1.Compression := SpinEdit1.Value/10;
      VorbisOut1.Comments.Clear;
      if Self.AlbumEdit.Text <> '' then
        VorbisOut1.Comments.Album := AlbumEdit.Text;
      if Self.ArtistEdit.Text <> '' then
        VorbisOut1.Comments.Artist := ArtistEdit.Text;
      if Self.DateEdit.Text <> '' then
        VorbisOut1.Comments.Date := DateEdit.Text;
      if Self.GenreEdit.Text <> '' then
        VorbisOut1.Comments.Genre := GenreEdit.Text;
      if Self.TitleEdit.Text <> '' then
        VorbisOut1.Comments.Title := TitleEdit.Text;
      if Self.TrackEdit.Text <> '' then
        VorbisOut1.Comments.Track := TrackEdit.Text;
      Button1.Enabled := False;
      VorbisOut1.Run;
      StatusBar1.Panels[0].Text := 'Converting to ' + ExtractFileName(VorbisOut1.FileName);
    end;
  end;
end;

procedure TForm1.VorbisOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  ProgressBar1.Position := 0;
  if VorbisOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Converted ' + ExtractFileName(VorbisOut1.FileName)
  else
    StatusBar1.Panels[0].Text := VorbisOut1.ExceptionMessage;
end;

procedure TForm1.VorbisOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := VorbisOut1.Progress;
end;



procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  VorbisOut1.Stop(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    Self.StatusBar1.Panels[0].Text := 'File to convert: ' + WaveIn1.FileName;
  end;  
end;

procedure TForm1.VorbisOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := VorbisOut1.ExceptionMessage;
end;

end.
