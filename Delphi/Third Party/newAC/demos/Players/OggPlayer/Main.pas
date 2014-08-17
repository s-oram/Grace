(*
 NewAC Ogg Vorbis audio player demo main unit.
 To run this demo you will need Ogg Vorbis ibraries.
 See the ACS documentation on where to find these libraries.
 (c) Andrei Borovsky. You can contact me at anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  ACS_Vorbis, Buttons, ACS_Converters, ACS_DXAudio;

type
  TForm1 = class(TForm)
    VorbisIn1: TVorbisIn;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    Timer2: TTimer;
    ScrollBar1: TScrollBar;
    Label5: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Memo1: TMemo;
    Label9: TLabel;
    TrackBar1: TTrackBar;
    Label10: TLabel;
    DXAudioOut1: TDXAudioOut;
    StereoBalance1: TStereoBalance;
    Label3: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure Timer2Timer(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DXAudioOut1ThreadException(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  Secs : String;
begin
  if OpenDialog1.Execute then
  begin
    VorbisIn1.FileName := OpenDialog1.FileName;
    if not VorbisIn1.Valid then
    begin
      Label3.Caption := 'Ogg file is not valid.';
      Exit;
    end;
    BitBtn1.Enabled := False;
    Label3.Caption := VorbisIn1.FileName;
    DXAudioOut1.Run;
    Label4.Caption := IntToStr(VorbisIn1.SampleRate);
    if VorbisIn1.Channels = 1 then
      Label8.Caption := 'Mono'

    else
    if VorbisIn1.Channels = 2 then
      Label8.Caption := 'Stereo'
    else
    if VorbisIn1.Channels = 6 then
      Label8.Caption := '5.1 channels'
    else
    if VorbisIn1.Channels = 8 then
      Label8.Caption := '7.1 channels';
    Secs := IntToStr(VorbisIn1.TotalTime mod 60);
    if VorbisIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [VorbisIn1.TotalTime div 60, Secs]);
    ScrollBar1.Enabled := True;
    Memo1.Clear;
    if VorbisIn1.Comments.Artist <> '' then
      Memo1.Lines.Add('Artist: ' + VorbisIn1.Comments.Artist);
    if VorbisIn1.Comments.Album <> '' then
      Memo1.Lines.Add('Album: ' + VorbisIn1.Comments.Album);
    if VorbisIn1.Comments.Title <> '' then
      Memo1.Lines.Add('Title: ' + VorbisIn1.Comments.Title);
    if VorbisIn1.Comments.Date <> '' then
      Memo1.Lines.Add('Date: ' + VorbisIn1.Comments.Date);
    if VorbisIn1.Comments.Genre <> '' then
      Memo1.Lines.Add('Genre: ' + VorbisIn1.Comments.Genre);
    if VorbisIn1.Comments.Track <> '' then
      Memo1.Lines.Add('Track: ' + VorbisIn1.Comments.Track);
  end;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ScrollBar1.Position := DXAudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   ScrollBar1.Enabled := False;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Label1.Caption := IntToStr(VorbisIn1.InstantBitRate);
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  DXAudioOut1.Jump((ScrollPos-DXAudioOut1.Progress)*10);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then
  DXAudioOut1.Pause
  else
  DXAudioOut1.Resume;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  StereoBalance1.Balance := TrackBar1.Position/10;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
end;

procedure TForm1.DXAudioOut1ThreadException(Sender: TComponent);
begin
  Label3.Caption := DXAudioOut1.ExceptionMessage;
end;

end.
