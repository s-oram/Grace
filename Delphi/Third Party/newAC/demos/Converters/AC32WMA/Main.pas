(*
 ACS Wav to WMA file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ExtCtrls, ACS_WinMedia,
  Spin, ACS_Misc, NewACAC3;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
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
    TrackSpinEdit: TSpinEdit;
    Button2: TButton;
    WMAOut1: TWMAOut;
    VBR: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    AC3In1: TAC3In;
    AudioPlayList1: TAudioPlayList;
    ListBox3: TListBox;
    Button3: TButton;
    procedure WMAOut1Done(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure VBRClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure WMAOut1ThreadException(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WMAOut1Done(Sender: TComponent);
begin
  Button3.Enabled := True;
  Button2.Enabled := True;
  if WMAOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Converted ' + ExtractFileName(WMAOut1.FileName)
  else
    StatusBar1.Panels[0].Text := WMAOut1.ExceptionMessage;
end;

procedure TForm1.WMAOut1ThreadException(Sender: TComponent);
begin
  Self.StatusBar1.Panels[0].Text := WMAOut1.ExceptionMessage;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WMAOut1.Stop(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S : WideString;
  i : Integer;
begin
  begin
//    AC3In1.Extract := True;
    if SaveDialog1.Execute then
    begin
      WMAOut1.FileName := SaveDialog1.FileName;
      WMAOut1.CodecIndex := ListBox1.ItemIndex;
      WMAOut1.FormatIndex := ListBox2.ItemIndex;      
      WMAOut1.Id3v2Tags.Clear;
      if Self.AlbumEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Album := AlbumEdit.Text;
      if Self.ArtistEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Artist := ArtistEdit.Text;
      if Self.DateEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Year := DateEdit.Text;
      if Self.GenreEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Genre := GenreEdit.Text;
      if Self.TitleEdit.Text <> '' then
        WMAOut1.Id3v2Tags.Title := TitleEdit.Text;
      if Self.TrackSpinEdit.Value <> 0 then
        WMAOut1.Id3v2Tags.Track := IntToStr(TrackSpinEdit.Value);
      Button3.Enabled := False;
      Button2.Enabled := False;
      StatusBar1.Panels[0].Text := 'Converting to ' + ExtractFileName(WMAOut1.FileName);
      WMAOut1.Run;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ListBox3.Items.AddStrings(OpenDialog1.Files);
    AudioPlayList1.Files.Assign(ListBox3.Items);
  end;
end;

procedure TForm1.VBRClick(Sender: TObject);
begin
  WMAOut1.VBR := VBR.Checked;
  ListBox2.Items.Assign(WMAOut1.Formats[ListBox1.ItemIndex]);
  ListBox2.ItemIndex := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.Items.Assign(WMAOut1.Codecs);
  ListBox1.ItemIndex := 0;
  ListBox2.Items.Assign(WMAOut1.Formats[ListBox1.ItemIndex]);
  ListBox2.ItemIndex := 0;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  ListBox2.Items.Assign(WMAOut1.Formats[ListBox1.ItemIndex]);
  ListBox2.ItemIndex := 0;
end;

end.
