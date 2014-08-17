(*
 NewAC Wav to TTA file converter.
 Copyright (c) Andrei Borovsky,
 You can contact me at anb@symmetrica.net
 You will need the TTAlib.dll library to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin,
  ExtCtrls, ACS_TTA;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    Button2: TButton;
    TTAOut1: TTTAOut;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label7: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure TTAOut1Done(Sender: TComponent);
    procedure TTAOut1Progress(Sender: TComponent);
    procedure TTAOut1ThreadException(Sender: TComponent);
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
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    StatusBar1.Panels[0].Text := 'File to convert: ' + WaveIn1.FileName;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TTAOut1.Stop(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S : WideString;
begin
  S := WaveIn1.FileName;
  SaveDialog1.FileName := ChangeFileExt(S, '.tta');;
  if SaveDialog1.Execute then
  begin
    StatusBar1.Panels[0].Text := 'Converting...';
    TTAOut1.FileName := SaveDialog1.FileName;
    Button1.Enabled := False;
    Button2.Enabled := False;
    TTAOut1.Id3v1Tags.Clear;
    TTAOut1.Id3v2Tags.Clear;
    if Edit1.Text <> '' then
    TTAOut1.Id3v1Tags.Artist := Edit1.Text;
    if Edit2.Text <> '' then
    TTAOut1.Id3v1Tags.Album := Edit2.Text;
    if Edit3.Text <> '' then
    TTAOut1.Id3v1Tags.Title := Edit3.Text;
    if Edit4.Text <> '' then
    TTAOut1.Id3v1Tags.Year := StrToInt(Edit4.Text);
    TTAOut1.Run;
  end;
end;

procedure TForm1.TTAOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  StatusBar1.Panels[0].Text := 'Success';
end;

procedure TForm1.TTAOut1Progress(Sender: TComponent);
begin
 ProgressBar1.Position := TTAOut1.Progress;
end;

procedure TForm1.TTAOut1ThreadException(Sender: TComponent);
begin
 StatusBar1.Panels[0].Text :=  TTAOut1.ExceptionMessage;
end;

end.
