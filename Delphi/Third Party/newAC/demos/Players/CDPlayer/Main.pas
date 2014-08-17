(*
 NewAC CD Player demo main unit
 Copyright (c) Andrei Borovsky
 You can contact me at anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_CDROM, ACS_Wave, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox2: TComboBox;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CDPlayer1: TCDPlayer;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox2Select(Sender: TObject);
    procedure ComboBox2Enter(Sender: TObject);
    procedure ComboBox2DropDown(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CurrentTrack : Integer;
    procedure GetTracks;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CDPlayer1.Play(CurrentTrack);
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to CDPlayer1.DrivesCount - 1 do
  ComboBox1.Items.Add(CDPlayer1.DriveLetter[i]);
  ComboBox1.ItemIndex := 0;
  GetTracks;
end;

procedure TForm1.GetTracks;
var
  i : Integer;
  S, sMSF : String;
  TI : TCDTrackInfo;
begin
  ComboBox2.Items.Clear;
  for i := 1 to CDPlayer1.TracksCount do
  begin
    TI := CDPlayer1.Tracks[i];
    if TI.TrackType = ttAudio then
    begin
      sMSF := MSFToStr(TI.TrackLength);
      S := '[%d] - %s';
      S := Format(S, [i, sMSF]);
      ComboBox2.Items.Add(S)
    end;
  end;
  if ComboBox2.Items.Count > 0 then ComboBox2.ItemIndex := 0;
  CurrentTrack := 1;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if CDPlayer1.Status = cdsPlaying then
  CDPlayer1.Pause
  else CDPlayer1.Resume;
end;

procedure TForm1.ComboBox2Select(Sender: TObject);
begin
  CurrentTrack := ComboBox2.Items.IndexOf(ComboBox2.Text)+1;
end;

procedure TForm1.ComboBox2Enter(Sender: TObject);
begin
  GetTracks;
end;

procedure TForm1.ComboBox2DropDown(Sender: TObject);
begin
  GetTracks;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CDPlayer1.Eject;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CDPlayer1.CloseTray;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  CDPlayer1.Stop;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  CDPlayer1.CurrentDrive := ComboBox1.Items.IndexOf(ComboBox1.Text);
  GetTracks;
end;

end.



