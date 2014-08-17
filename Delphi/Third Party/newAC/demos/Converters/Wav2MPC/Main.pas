(*
 NewAC Wav to MusePack file converter.
 Copyright (c) Andrei Borovsky,
 You can contact me at anb@symmetrica.net
 You will need the libmppenc.dll library to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin,
  ExtCtrls, ACS_MPC;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    Button2: TButton;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    MPCOut1: TMPCOut;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FLACOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure MPCOut1Done(Sender: TComponent);
    procedure MPCOut1Progress(Sender: TComponent);
    procedure MPCOut1ThreadException(Sender: TComponent);
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

procedure TForm1.FLACOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MPCOut1.Stop(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S : WideString;
begin
  S := WaveIn1.FileName;
  SaveDialog1.FileName := ChangeFileExt(S, '.mpc');;
  if SaveDialog1.Execute then
  begin
    StatusBar1.Panels[0].Text := 'Converting...';
    MPCOut1.FileName := SaveDialog1.FileName;
    MPCOut1.Quality := SpinEdit1.Value;
    Button1.Enabled := False;
    Button2.Enabled := False;
    MPCOut1.APEv2Tags.Clear;
    if Edit1.Text <> '' then
      MPCOut1.APEv2Tags.Title := Edit1.Text;
    if Edit2.Text <> '' then
      MPCOut1.APEv2Tags.Album := Edit2.Text;
    if Edit3.Text <> '' then
      MPCOut1.APEv2Tags.Artist := Edit3.Text;
    if Edit4.Text <> '' then
      MPCOut1.APEv2Tags.Year := Edit4.Text;
    if Edit5.Text <> '' then
      MPCOut1.APEv2Tags.Track := Edit5.Text;
    MPCOut1.Run;
  end;
end;

procedure TForm1.MPCOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  if MPCOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Success';
end;

procedure TForm1.MPCOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := MPCOut1.Progress;
end;

procedure TForm1.MPCOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := MPCOut1.ExceptionMessage;
end;

end.
