(*
 ACS Ogg Vorbis audio player demo main unit.
 To run this demo you will need Ogg Vorbis ibraries.
 See the ACS documentation on where to find these libraries.
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Indicator, ACS_Classes,
  ACS_Audio, ACS_Vorbis, Buttons, ACS_Converters;

type
  TForm1 = class(TForm)
    VorbisIn1: TVorbisIn;
    AudioOut1: TAudioOut;
    SoundIndicator1: TSoundIndicator;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Timer2: TTimer;
    Label3: TLabel;
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
    StereoBalance1: TStereoBalance;
    TrackBar1: TTrackBar;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure AudioOut1Done(Sender: TComponent);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.Picture.Bitmap.Width := Image1.Width;
  Image1.Picture.Bitmap.Height := Image1.Height;
  Image1.Picture.Bitmap.Canvas.Brush.Color := Color;
  Image1.Picture.Bitmap.Canvas.Pen.Color := Color;
  Image1.Picture.Bitmap.Canvas.Rectangle(0,0,Image1.Width,Image1.Height);
end;

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
    AudioOut1.Run;
    Label4.Caption := IntToStr(VorbisIn1.SampleRate);
    if VorbisIn1.Channels = 1 then Label8.Caption := 'Mono'
    else Label8.Caption := 'Stereo';
    Secs := IntToStr(VorbisIn1.TotalTime mod 60);
    if VorbisIn1.TotalTime mod 60 < 10 then Secs := '0'+Secs;
    Label7.Caption := Format('%d:%s', [VorbisIn1.TotalTime div 60, Secs]);
    ScrollBar1.Enabled := True;
    Memo1.Lines.Assign(VorbisIn1.Comments);
  end;
end;

procedure TForm1.AudioOut1Progress(Sender: TComponent);
begin
  ScrollBar1.Position := AudioOut1.Progress;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   ScrollBar1.Enabled := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Vals : array[0..31] of Double;
  i : Integer;
begin
  Image1.Picture.Bitmap.Canvas.Brush.Color := Color;
  Image1.Picture.Bitmap.Canvas.Pen.Color := Color;
  Image1.Picture.Bitmap.Canvas.Rectangle(0,0,Image1.Width,Image1.Height);
  SoundIndicator1.GetValues(Vals);
  Image1.Picture.Bitmap.Canvas.Pen.Color := clAqua;
  for i := 0 to 31 do
  begin
    Image1.Picture.Bitmap.Canvas.MoveTo(i shl 1, Image1.Picture.Bitmap.Height);
    Image1.Picture.Bitmap.Canvas.LineTo(i shl 1, Image1.Picture.Bitmap.Height-Round(Vals[i]));
  end;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Label1.Caption := IntToStr(VorbisIn1.InstantBitRate);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AudioOut1.Stop;
  while AudioOut1.Status <> tosIdle do;
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  VorbisIn1.Jump(ScrollPos-AudioOut1.Progress);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if AudioOut1.Status = tosPlaying then
  AudioOut1.Pause
  else
  AudioOut1.Resume;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  AudioOut1.Stop;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  StereoBalance1.Balance := TrackBar1.Position/10;
end;

end.
