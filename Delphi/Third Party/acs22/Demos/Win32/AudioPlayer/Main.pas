(*
  ACS Audio Player demo main unit.
  (c) Andrei Borovsky, aborovsky@mtu-net.ru
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Wave, ACS_Audio, StdCtrls, ComCtrls, ACS_Vorbis,
  ACS_MAC, ACS_FLAC;

type
  TForm1 = class(TForm)
    AudioOut1: TAudioOut;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    VorbisIn1: TVorbisIn;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    WaveIn1: TWaveIn;
    MACIn1: TMACIn;
    StatusBar1: TStatusBar;
    FLACIn1: TFLACIn;
    procedure Button1Click(Sender: TObject);
    procedure AudioOut1Done(Sender: TComponent);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure AudioOut1ThreadException(Sender: TComponent; E: Exception);
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
  Min, Sec : Integer;
  Fmt : String;
  FI : TACSFileIn;
  Ext : String;
begin
  if OpenDialog1.Execute then
  begin
    Ext := SysUtils.ExtractFileExt(OpenDialog1.FileName);
    FI := nil;
    if Ext = '.ogg' then FI := VorbisIn1;
    if Ext = '.wav' then FI := WaveIn1;
    if Ext = '.ape' then FI := MACIn1;
    if Ext = '.flac' then FI := FLACIn1;
    if FI = nil then
    begin
      StatusBar1.Panels[0].Text := 'Unknown file extension';
      Exit;
    end;
    FI.FileName := OpenDialog1.FileName;
    if not FI.Valid then Exit;
    Sec := FI.TotalTime;
    Min := Sec div 60;
    Sec := Sec - Min*60;
    if Sec < 10 then Fmt := '%d:0%d'
    else Fmt := '%d:%d';
    Label2.Caption := Format(Fmt, [Min, Sec]);
    Label4.Caption := ExtractFileName(FI.FileName);
    FI.Loop := CheckBox1.Checked;
    AudioOut1.Input := FI;
    Button1.Enabled := False;
    AudioOut1.Run;
  end;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := AudioOut1.Progress;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AudioOut1.Stop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 // AudioOut1.Delay := 10;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  FI : TACSFileIn;
begin
  if AudioOut1.Status <> tosIdle then
  begin
    FI := AudioOut1.Input as TACSFileIn;
    FI.Loop := CheckBox1.Checked;
  end;
end;

procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  E: Exception);
begin
  StatusBar1.Panels[0].Text := E.Message;
  Button1.Enabled := True;
end;

end.
