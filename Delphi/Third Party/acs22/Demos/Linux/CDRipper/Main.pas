(*
 ACS CD Ripper demo main unit
 Copyright (c) Andrei Borovsky
 You can contact me at aborovsky@mtu-net.ru
 This program saves CD audio tracks in Ogg Vorbis
 Wave or FLAC format. Select the format in the "Save as" dialog what
 apperas after you have pressed "Rip!" button.
 Note that you will need libogg, libvorbis,
 and libvorbisenc, and libFLAC libraries to run this ripper.
 This demo also shows thread exceptions handling.
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, ACS_Classes, ACS_Vorbis, ACS_CDROM, QTypes, QExtCtrls,
  QComCtrls, ACS_FLAC, ACS_Wave;

type
  TForm1 = class(TForm)
    CDIn1: TCDIn;
    VorbisOut1: TVorbisOut;
    Label1: TLabel;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    WaveOut1: TWaveOut;
    FLACOut1: TFLACOut;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure VorbisOut1ThreadException(Sender: TComponent; E: Exception);
    procedure VorbisOut1Progress(Sender: TComponent);
    procedure SpinEdit1Enter(Sender: TObject);
    procedure SaveDialog1FilterChange(Sender: TObject; NewIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    FO : TACSFileOut;
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Ext : String;
begin
  StatusBar1.Panels[0].Text := '';
  if SaveDialog1.Execute then
  begin
    Ext := ExtractFileExt(SaveDialog1.FileName);
    if Ext = '.ogg' then FO := VorbisOut1
    else
    if Ext = '.flac' then FO := FLACOut1
    else FO := WaveOut1;
    FO.FileName := SaveDialog1.FileName;
    VorbisOut1.Compression := SpinEdit2.Value/10;
    if VorbisOut1.Compression = 0 then VorbisOut1.Compression := 0.2;
    VorbisOut1.Serial := Random(1000000);
    CDIn1.StartTrack := SpinEdit1.Value;
    CDIn1.EndTrack := CDIn1.StartTrack;
    SpinEdit1.Enabled := False;
    Button1.Enabled := False;
    FO.Run;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FO.Stop;
  DeleteFile(FO.FileName);
end;

procedure TForm1.VorbisOut1Done(Sender: TComponent);
begin
  SpinEdit1.Enabled := True;
  Button1.Enabled := True;
end;

procedure TForm1.VorbisOut1ThreadException(Sender: TComponent;
  E: Exception);
begin
  SpinEdit1.Enabled := True;
  Button1.Enabled := True;
  StatusBar1.Panels[0].Text := E.Message;
  VorbisOut1.Input.Reset;
end;

procedure TForm1.VorbisOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := VorbisOut1.Progress;
end;

procedure TForm1.SpinEdit1Enter(Sender: TObject);
begin
  SpinEdit1.Max := CDIn1.TracksCount;
end;

procedure TForm1.SaveDialog1FilterChange(Sender: TObject;
  NewIndex: Integer);
begin
  case NewIndex of
    0 : SaveDialog1.DefaultExt := 'ogg';
    1 : SaveDialog1.DefaultExt := 'flac';
    2 : SaveDialog1.DefaultExt := 'wav';
  end;
end;

end.
