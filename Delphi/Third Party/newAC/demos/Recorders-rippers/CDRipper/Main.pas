(*
 NewAC CD ripper demo  main unit.
 (c) Andrei Borovsky, anb@symmetrica.net
 You will need CDRip.dll to run this demo.
 This demo ripper can save ripped data into several formats:
 wav files, Ogg Vorbis, Monkey Audio, and FLACs. For all these
 formats, except wav format, you will need special encoder DLL libraries.
 See the NewAC documentation on where to find these libraries.
 If you don't have some encoder for some particular format in your system,
 remove a corresponding output component and all the references to it from the project.
*)

(* CVS: $Date: 2007/07/07 04:40:12 $ $Revision: 1.3 $*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_CDROM, ACS_Wave, ComCtrls, StdCtrls, ACS_Vorbis,
  ACS_MAC, ACS_FLAC;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    StatusBar1: TStatusBar;
    ComboBox3: TComboBox;
    Label3: TLabel;
    WaveOut1: TWaveOut;
    VorbisOut1: TVorbisOut;
    FLACOut1: TFLACOut;
    MACOut1: TMACOut;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    CDIn1: TCDIn;
    procedure Button1Click(Sender: TObject);
    procedure OutputDone(Sender: TComponent);
    procedure Progress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure ComboBox2Select(Sender: TObject);
    procedure ComboBox2Enter(Sender: TObject);
    procedure ComboBox2DropDown(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CurrentTrack : Integer;
    CurrentOutput : TAuOutput;
    procedure GetTracks;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
//  CDIn1.Paranoid := True;
//  CDIn1.ParanoiaMode := 0;
  CDIn1.LockTray := True;
  if ComboBox3.Text = 'Ogg' then
  begin
    SaveDialog1.Filter := 'Vorbis files|*.ogg';
    SaveDialog1.DefaultExt := 'ogg';
  end;
  if ComboBox3.Text = 'Wav' then
  begin
    SaveDialog1.Filter := 'Wave files|*.wav';
    SaveDialog1.DefaultExt := 'wav';
  end;
  if ComboBox3.Text = 'Ape' then
  begin
    SaveDialog1.Filter := 'Ape files|*.ape';
    SaveDialog1.DefaultExt := 'ape';
  end;
  if ComboBox3.Text = 'FLAC' then
  begin
    SaveDialog1.Filter := 'FLAC files|*.flac';
    SaveDialog1.DefaultExt := 'flac';
  end;
  if SaveDialog1.Execute then
  begin
    Button1.Enabled := False;
    ComboBox1.Enabled := False;
    ComboBox2.Enabled := False;
    ComboBox3.Enabled := False;
    CDIn1.StartTrack := CurrentTrack;
    CDIn1.EndTrack := CDIn1.StartTrack;
    if ComboBox3.Text = 'Ogg' then
    begin
      VorbisOut1.FileName := SaveDialog1.FileName;
      CurrentOutput := VorbisOut1;
      VorbisOut1.Run;
    end;
    if ComboBox3.Text = 'Wav' then
    begin
      WaveOut1.FileName := SaveDialog1.FileName;
      CurrentOutput := WaveOut1;
      WaveOut1.Run;
    end;
    if ComboBox3.Text = 'Ape' then
    begin
      MACOut1.FileName := SaveDialog1.FileName;
      CurrentOutput := MACOut1;
      MACOut1.Run;
    end;
    if ComboBox3.Text = 'FLAC' then
    begin
      FLACOut1.FileName := SaveDialog1.FileName;
      CurrentOutput := FLACOut1;
      FLACOut1.Run;
    end;

  end;
end;

procedure TForm1.OutputDone(Sender: TComponent);
begin
  Button1.Enabled := True;
  ComboBox1.Enabled := True;
  ComboBox2.Enabled := True;
  ComboBox3.Enabled := True;
  if (Sender as TAuOutput).ExceptionMessage = '' then
  begin
       StatusBar1.Font.Color := clBlack;
    StatusBar1.Panels[0].Text := 'Success'
  end
  else
  begin
    StatusBar1.Panels[0].Text := 'ERROR: ' + (Sender as TAuOutput).ExceptionMessage;
  end;
  Label4.Caption := Format('Jitter errors detected: %d', [CDIn1.LastJitterErrors]);

end;

procedure TForm1.Progress(Sender: TComponent);
begin
  ProgressBar1.Position := CurrentOutput.Progress;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to CDIn1.DrivesCount-1 do
  begin
    CDIn1.CurrentDrive := i;
    ComboBox1.Items.Add(CDIn1.DriveName);
  end;
  if ComboBox1.Items.Count > 0 then ComboBox1.ItemIndex := 0;
  CDIn1.CurrentDrive := 0;
  GetTracks;
end;

procedure TForm1.GetTracks;
var
  i : Integer;
  S : String;
  TI : TCDTrackInfo;
begin
  ComboBox2.Items.Clear;
  for i := 1 to CDIn1.TracksCount do
  begin
    TI := CDIn1.Tracks[i];
    if  TI.TrackLength.Second > 9 then S := '[%d] - %d:%d'
    else S := '[%d] - %d:0%d';
    S := Format(S, [i, TI.TrackLength.Minute, TI.TrackLength.Second]);
    ComboBox2.Items.Add(S)
  end;
  if ComboBox2.Items.Count > 0 then ComboBox2.ItemIndex := 0;
  CurrentTrack := 1;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CurrentOutput.Stop;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  CDIn1.EnableJitterCorrection := CheckBox1.Checked;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  CDIn1.CurrentDrive := ComboBox1.Items.IndexOf(ComboBox1.Text);
  GetTracks;
  CheckBox1.Checked := CDIn1.EnableJitterCorrection;
end;

procedure TForm1.ComboBox2Select(Sender: TObject);
begin
  CurrentTrack := ComboBox2.Items.IndexOf(ComboBox2.Text)+1;
end;

procedure TForm1.ComboBox2Enter(Sender: TObject);
begin
  CDIn1.CurrentDrive := ComboBox1.Items.IndexOf(ComboBox1.Text);
  GetTracks;
end;

procedure TForm1.ComboBox2DropDown(Sender: TObject);
begin
  CDIn1.CurrentDrive := ComboBox1.Items.IndexOf(ComboBox1.Text);
  GetTracks;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CDIn1.Eject;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CDIn1.CloseTray;
end;

end.



