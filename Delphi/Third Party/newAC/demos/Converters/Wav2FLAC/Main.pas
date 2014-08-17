(*
 NewAC Wav to FLAC file converter.
 Copyright (c) Andrei Borovsky,
 You can contact me at anb@symmetrica.net
 You will need the FLAC codec library to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin,
  ACS_FLAC, ExtCtrls;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    FLACOut1: TFLACOut;
    Edit6: TEdit;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FLACOut1Progress(Sender: TComponent);
    procedure FLACOut1Done(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure FLACOut1ThreadException(Sender: TComponent);
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
  if WaveIn1.FileName = '' then Exit;
  SaveDialog1.FileName := ChangeFileExt(WaveIn1.FileName, '.flac');
  if SaveDialog1.Execute then
  begin
    StatusBar1.Panels[0].Text := 'Converting...';
    FLACOut1.FileName := SaveDialog1.FileName;
    FLACOut1.CompressionLevel := SpinEdit1.Value;
    FLACOut1.Tags.Clear;
    if Edit1.Text <> '' then
      FLACOut1.Tags.Title := Edit1.Text;
    if Edit2.Text <> '' then
      FLACOut1.Tags.Album := Edit2.Text;
    if Edit3.Text <> '' then
      FLACOut1.Tags.Artist := Edit3.Text;
    if Edit4.Text <> '' then
      FLACOut1.Tags.Date := Edit4.Text;
    if Edit5.Text <> '' then
      FLACOut1.Tags.Genre := Edit5.Text;
    if Edit6.Text <> '' then
      FLACOut1.Tags.Track := Edit6.Text;
    Button1.Enabled := False;
    FLACOut1.Run;
  end;
end;

procedure TForm1.FLACOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := FLACOut1.Progress;
end;

procedure TForm1.FLACOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  if FLACOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Success';
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FLACOut1.Stop(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    StatusBar1.Panels[0].Text := ExtractFileName(WaveIn1.FileName);
  end;
end;

procedure TForm1.FLACOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := FLACOut1.ExceptionMessage;
end;

end.
