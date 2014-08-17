unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Misc;

type
  TForm12 = class(TForm)
    TagEditor1: TTagEditor;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form12: TForm12;

implementation

{$R *.dfm}

procedure TForm12.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    TagEditor1.FileName := OpenDialog1.FileName;
    Edit1.Text := TagEditor1.Title;
    Edit2.Text := TagEditor1.Album;
    Edit3.Text := TagEditor1.Artist;
    Edit4.Text := TagEditor1.Genre;
    Edit5.Text := TagEditor1.Track;
    Edit6.Text := TagEditor1.Year;
    if TagEditor1.Valid then
    begin
      StatusBar1.Panels[0].Text := ExtractFileName(TagEditor1.FileName);
    end else
    begin
      StatusBar1.Panels[0].Text := 'No file loaded';
    end;
    StatusBar1.Panels[1].Text := Format('%d seconds', [TagEditor1.Duration]);
    StatusBar1.Panels[2].Text := Format('Bitrate: %d', [TagEditor1.Bitrate]);
  end;
end;

procedure TForm12.Button2Click(Sender: TObject);
begin
  TagEditor1.Title := Edit1.Text;
  TagEditor1.Album := Edit2.Text;
  TagEditor1.Artist := Edit3.Text;
  TagEditor1.Genre := Edit4.Text;
  TagEditor1.Track := Edit5.Text;
  TagEditor1.Year := Edit6.Text;
  TagEditor1.Save;
end;

end.
