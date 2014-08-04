(*
 ACS CD Player demo main unit
 (c) 2002 Andrei Borovsky, all rights reserved
 You can contact me at aborovsky@mtu-net.ru
*)

unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QTypes, QExtCtrls, ACS_CDROM;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    ComboBox1: TComboBox;
    Button3: TButton;
    Button4: TButton;
    Timer1: TTimer;
    Button5: TButton;
    Button6: TButton;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Bevel1: TBevel;
    CDPlayer1: TCDPlayer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshCombo;
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Font.Color := clLime;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  From : TCDPosition;
begin
  FillChar(From, SizeOf(From), 0);
  if ComboBox1.ItemIndex < 0 then From.Track := 1
  else From.Track := ComboBox1.ItemIndex+1;
  case CDPlayer1.Status of
    cdsReady:  CDPlayer1.Play(From, EndOfDisc);
    cdsPaused: CDPlayer1.Resume;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CDPlayer1.Pause;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CDPlayer1.Stop;
  Label1.Caption := '0:00';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  CDTI : TCDTrackInfo;
  Pos : TCDMSF;
  Stat : TCDStatus;
begin
  Stat := CDPlayer1.Status;
  if Stat = cdsPlaying then
  begin
    CDTI := CDPlayer1.Tracks[CDPlayer1.Position.Track];
    pos := CDPlayer1.Position.MSF;
    Label1.Caption := '['+IntToStr(CDPlayer1.Position.Track)+'] - '+
    MSFToStr(pos)+' of '+ MSFToStr(CDTI.TrackLength);
  end;
  if Stat = cdsReady then
  begin
    if CDPlayer1.MediaChanged then RefreshCombo;
    Label1.Caption := '0:00';
  end;
  if Stat = cdsNotReady then
  if ComboBox1.Items.Count>0 then
  begin
    ComboBox1.Clear;
    ComboBox1.Refresh;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if CDPlayer1.Status in [cdsReady, cdsNotReady] then
  CDPlayer1.Eject;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  CDPlayer1.CloseTray;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TrackBar1.Position := 255 - CDPlayer1.LVolume;
  TrackBar2.Position := 255 - CDPlayer1.RVolume;
  RefreshCombo;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  CDPlayer1.LVolume := 255 - TrackBar1.Position;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  CDPlayer1.RVolume := 255 - TrackBar2.Position;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  pos : TCDPosition;
begin
  if CDPlayer1.Status in [cdsPlaying, cdsPaused] then
  begin
    pos := CDPlayer1.Position;
    if (pos.MSF.Second=0) and (pos.MSF.Minute=0) then
    if pos.Track>1 then Dec(Pos.Track);
    pos.MSF.Minute:=0;
    pos.MSF.Second:=0;
    pos.MSF.Frame:=0;
    CDPlayer1.Play(pos, EndOfDisc);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  pos : TCDPosition;
begin
  if CDPlayer1.Status in [cdsPlaying, cdsPaused] then
  begin
    pos := CDPlayer1.Position;
    if pos.Track<CDPlayer1.TracksCount then
    begin
      Inc(Pos.Track);
      pos.MSF.Minute:=0;
      pos.MSF.Second:=0;
      pos.MSF.Frame:=0;
      CDPlayer1.Play(pos, EndOfDisc);
    end;
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  Pos : TCDPosition;
  Secs : Integer;
begin
  if CDPlayer1.Status in [cdsPlaying, cdsPaused] then
  begin
    Pos := CDPlayer1.Position;
    Secs := Pos.MSF.Minute * 60 + Pos.MSF.Second;
    if Secs >= 15 then Dec(Secs, 15)
    else Secs := 0;
    Pos.MSF.Minute := Secs div 60;
    Pos.MSF.Second := Secs mod 60;
    CDPlayer1.Play(Pos, EndOfDisc);
  end;
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  Pos : TCDPosition;
  Secs, Secs2 : Integer;
  CDTI : TCDTrackInfo;
begin
  if CDPlayer1.Status in [cdsPlaying, cdsPaused] then
  begin
    Pos := CDPlayer1.Position;
    CDTI:=CDPlayer1.Tracks[Pos.Track];
    Secs := Pos.MSF.Minute * 60 + Pos.MSF.Second;
    Secs2 := CDTI.TrackLength.Minute * 60 + CDTI.TrackLength.Second;
    if Secs2 - Secs >= 15 then Inc(Secs, 15)
    else Secs := Secs2;
    Pos.MSF.Minute := Secs div 60;
    Pos.MSF.Second := Secs mod 60;
    CDPlayer1.Play(Pos, EndOfDisc);
  end;
end;

procedure TForm1.RefreshCombo;
var
  i : Integer;
  S : String;
  CDTI : TCDTrackInfo;
begin
  ComboBox1.Clear;
  if CDPlayer1.Status <> cdsNotReady then
  begin
    for i := 1 to CDPlayer1.TracksCount do
    begin
      CDTI := CDPlayer1.Tracks[i];
      S := '<'+IntToStr(i)+'>: ';
      if CDTI.TrackType = ttAudio then
      S := S + 'length '+MSFToStr(CDTI.TrackLength);
      ComboBox1.Items.Add(S);
    end;
    ComboBox1.ItemIndex := 0;
    ComboBox1.Refresh;
  end;
end;

end.

