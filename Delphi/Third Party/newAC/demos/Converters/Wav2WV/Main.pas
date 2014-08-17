(*
 NewAC Wav to WavPack file converter.
 Copyright (c) Andrei Borovsky,
 You can contact me at anb@symmetrica.net
 You will need the WavPackDll library library to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Wave, Spin,
  ExtCtrls, ACS_WavPack;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button2: TButton;
    WVOut1: TWVOut;
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
    procedure FLACOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure WVOut1Done(Sender: TComponent);
    procedure WVOut1Progress(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure WVOut1ThreadException(Sender: TComponent);
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

procedure TForm1.WVOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  if WVOut1.ExceptionMessage =  '' then
    StatusBar1.Panels[0].Text := 'Success';
end;

procedure TForm1.WVOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := WVOut1.Progress;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WVOut1.Stop(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S : WideString;
begin
  S := WaveIn1.FileName;
  SaveDialog1.FileName := ChangeFileExt(S, '.wv');;
  if SaveDialog1.Execute then
  begin
    StatusBar1.Panels[0].Text := 'Converting...';
    WVOut1.FileName := SaveDialog1.FileName;
    case RadioGroup1.ItemIndex of
      0 : WVOut1.CompressionLevel := wvclFast;
      1 : WVOut1.CompressionLevel := wvclHigh;
      2 : WVOut1.CompressionLevel := wvclVeryHigh;
    end;
    WVOut1.HybridMode := CheckBox2.Checked;
    WVOut1.JointStereo := CheckBox1.Checked;
    Button1.Enabled := False;
    Button2.Enabled := False;
    WVOut1.APEv2Tags.Clear;
    if Edit1.Text <> '' then
      WVOut1.APEv2Tags.Artist := Edit1.Text;
    if Edit2.Text <> '' then
      WVOut1.APEv2Tags.Album := Edit2.Text;
    if Edit3.Text <> '' then
      WVOut1.APEv2Tags.Title := Edit3.Text;
    if Edit4.Text <> '' then
      WVOut1.APEv2Tags.Year := Edit4.Text;
    WVOut1.Run;
  end;
end;

procedure TForm1.WVOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := WVOut1.ExceptionMessage;
end;

end.
