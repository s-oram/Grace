unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Vorbis, ACS_WinMedia, StdCtrls, ComCtrls, Spin,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    SpinEdit1: TSpinEdit;
    Button1: TButton;
    Button2: TButton;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    WMIn1: TWMIn;
    VorbisOut1: TVorbisOut;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure VorbisOut1Progress(Sender: TComponent);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    procedure FillFormats;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    WMIn1.FileName := OpenDialog1.FileName;
    if not WMIn1.Valid then
    begin
      StatusBar1.Panels[0].Text := 'Input file is not valid';
      WMIn1.FileName := '';
    end else
    begin
      StatusBar1.Panels[0].Text := 'File to convert: ' + WMIn1.FileName;
      WMIn1.HighPrecision := CheckBox1.Checked;
      FillFormats;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if WMIn1.FileName <> '' then
  begin
     SaveDialog1.FileName := ChangeFileExt(WMIn1.FileName, '.ogg');
     if SaveDialog1.Execute then
     begin
       Button1.Enabled := False;
       Button2.Enabled := False;
       WMIn1.FormatSelected := ListBox1.ItemIndex;
       VorbisOut1.FileName := SaveDialog1.FileName;
       VorbisOut1.Comments.Title := WMIn1.Id3v2Tags.Title;
       VorbisOut1.Comments.Artist := WMIn1.Id3v2Tags.Artist;
       VorbisOut1.Comments.Album := WMIn1.Id3v2Tags.Album;
       VorbisOut1.Comments.Date := WMIn1.Id3v2Tags.Year;
       VorbisOut1.Comments.Genre := WMIn1.Id3v2Tags.Genre;
       VorbisOut1.Comments.Track := WMIn1.Id3v2Tags.Track;
       VorbisOut1.Compression := SpinEdit1.Value/10;
       StatusBar1.Panels[0].Text := 'Converting to ' + VorbisOut1.FileName;
       VorbisOut1.Run;
     end;
  end;
end;

procedure TForm1.VorbisOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := VorbisOut1.Progress;
end;

procedure TForm1.VorbisOut1Done(Sender: TComponent);
begin
  if VorbisOut1.ExceptionMessage <> '' then
    StatusBar1.Panels[0].Text := 'Error: ' + VorbisOut1.ExceptionMessage
  else
    StatusBar1.Panels[0].Text := 'Success';
  Button1.Enabled := True;
  Button2.Enabled := True;
end;

procedure TForm1.FillFormats;
var
  i : Integer;
  FS : TWMAFormatSpec;
begin
  ListBox1.Clear;
  for i := 0 to WMIn1.FormatsCount - 1 do
  begin
    FS := WMIn1.FormatSpec[i];
    ListBox1.Items.Add(Format('%d channels, %d bps, %d Hz', [FS.Channels, FS.BitsPerSample, FS.SampleRate]));
  end;
  ListBox1.ItemIndex := 0;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  WMIn1.HighPrecision := CheckBox1.Checked;
  FillFormats;
end;

end.
