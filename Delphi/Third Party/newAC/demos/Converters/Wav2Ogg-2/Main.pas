(*
 NewAC Wav to Ogg file converter.
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
 You will need Ogg Vorbis codec libraries to
 run this demo.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ACS_Classes, ACS_Vorbis, ACS_Wave, Spin,
  ACS_Converters, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    ComboBox1: TComboBox;
    Label4: TLabel;
    StatusBar1: TStatusBar;
    VorbisOut1: TVorbisOut;
    Button2: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure VorbisOut1Done(Sender: TComponent);
    procedure VorbisOut1Progress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  S : String;
begin
  if not TAuFileIn(VorbisOut1.Input).Valid then
  begin
    StatusBar1.Panels[0].Text := 'Cannot open input file';
    Exit;
  end;  
  If WaveIn1.WideFileName <> '' then
  begin
    S := OpenDialog1.FileName;
    SetLength(S, Length(S) - 4);
    SaveDialog1.FileName := S + '.ogg';
    if SaveDialog1.Execute then
    begin
      StatusBar1.Panels[0].Text := 'Converting...';
      VorbisOut1.FileName := SaveDialog1.FileName;
      Button1.Enabled := False;
      VorbisOut1.Run;
    end;
  end;
end;

procedure TForm1.VorbisOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  if VorbisOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Success'
  else
    StatusBar1.Panels[0].Text := 'ERROR: ' +VorbisOut1.ExceptionMessage;
end;

procedure TForm1.VorbisOut1Progress(Sender: TComponent);
begin
  ProgressBar1.Position := VorbisOut1.Progress;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  VorbisOut1.DesiredNominalBitrate := bitrate192;
  VorbisOut1.DesiredMaximumBitrate := bitrate192;
  VorbisOut1.MinimumBitrate := bitrate192;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
      0 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate24;
        VorbisOut1.DesiredMaximumBitrate := bitrate24;
        VorbisOut1.MinimumBitrate := bitrate24;
      end;

      1 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate32;
        VorbisOut1.DesiredMaximumBitrate := bitrate32;
        VorbisOut1.MinimumBitrate := bitrate32;
      end;

      2 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate64;
        VorbisOut1.DesiredMaximumBitrate := bitrate64;
        VorbisOut1.MinimumBitrate := bitrate64;
      end;

      3 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate128;
        VorbisOut1.DesiredMaximumBitrate := bitrate128;
        VorbisOut1.MinimumBitrate := bitrate128;
      end;

      4 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate192;
        VorbisOut1.DesiredMaximumBitrate := bitrate192;
        VorbisOut1.MinimumBitrate := bitrate192;
      end;

      5 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate256;
        VorbisOut1.DesiredMaximumBitrate := bitrate256;
        VorbisOut1.MinimumBitrate := bitrate256;
      end;

      6 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate320;
        VorbisOut1.DesiredMaximumBitrate := bitrate320;
        VorbisOut1.MinimumBitrate := bitrate320;
      end;

      7 :
      begin
        VorbisOut1.DesiredNominalBitrate := bitrate499;
        VorbisOut1.DesiredMaximumBitrate := bitrate499;
        VorbisOut1.MinimumBitrate := bitrate499;
      end;
   end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Self.StatusBar1.Panels[0].Text := '';
  If OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    StatusBar1.Panels[0].Text := 'File to convert: ' + WaveIn1.FileName;
  end;  
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  VorbisOut1.Stop(False);
end;

procedure TForm1.VorbisOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := 'ERROR: ' + VorbisOut1.ExceptionMessage;
end;

end.
