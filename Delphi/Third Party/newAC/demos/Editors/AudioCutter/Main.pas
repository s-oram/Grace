(*
  Simple audio editor demo.
  This one shows, among other things, how to work with files stored in memory.
*)
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_DXAudio, StdCtrls, ComCtrls, ACS_Classes, ACS_Wave,
  ACS_Converters, ExtCtrls, Buttons, ACS_WinMedia;

type
  TForm1 = class(TForm)
    WaveIn1: TWaveIn;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Label2: TLabel;
    Panel2: TPanel;
    Image1: TImage;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    DXAudioOut1: TDXAudioOut;
    WaveOut1: TWaveOut;
    AudioConverter1: TAudioConverter;
    procedure TrackBarChange(Sender: TObject);
    procedure Progress(Sender: TComponent);
    procedure Done(Sender: TComponent);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    InputMS : TMemoryStream;
    values : array of array [0..1] of Byte;
    procedure LoadFileToMemory(const FileName : String);
    procedure UnloadFileFromMemory;
    procedure GetWaveForm;
    procedure ClearGraph;
    procedure DrawGraph;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TrackBarChange(Sender: TObject);
begin
  if TrackBar1.Position >  TrackBar2.Position then
    TrackBar1.Position :=  TrackBar2.Position;
  Self.DrawGraph;
end;

procedure TForm1.Progress(Sender: TComponent);
begin
  DrawGraph;
end;

procedure TForm1.Done(Sender: TComponent);
begin
  SpeedButton1.Enabled := True;
  SpeedButton2.Enabled := True;
  SpeedButton3.Enabled := True;
  TrackBar1.Enabled := True;
  TrackBar2.Enabled := True;
  DrawGraph;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    UnloadFileFromMemory;
    LoadFileToMemory(OpenDialog1.FileName);
    WaveIn1.Stream := InputMS;
    TrackBar1.Max := WaveIn1.TotalTime * 10; // the minimum step of 1/10 of a second
    TrackBar1.Position := 0;
    TrackBar2.Max := TrackBar1.Max;
    TrackBar2.Position := TrackBar2.Max;
    ClearGraph;
    GetWaveForm;
    DrawGraph;
    Caption := 'AudioCutter - ' + ExtractFileName(OpenDialog1.FileName);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    if WaveIn1.Valid then
    begin
      WaveOut1.FileName := SaveDialog1.FileName;
      SpeedButton1.Enabled := False;
      SpeedButton2.Enabled := False;
      SpeedButton3.Enabled := False;
      WaveIn1.StartSample := Round(WaveIn1.TotalSamples * TrackBar1.Position / TrackBar1.Max);
      WaveIn1.EndSample := Round(WaveIn1.TotalSamples * TrackBar2.Position / TrackBar2.Max);
      WaveOut1.Run;
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if WaveIn1.Valid then
  begin
    SpeedButton1.Enabled := False;
    SpeedButton2.Enabled := False;
    SpeedButton3.Enabled := False;
    TrackBar1.Enabled := False;
    TrackBar2.Enabled := False;
    WaveIn1.StartSample := Round(WaveIn1.TotalSamples * TrackBar1.Position / TrackBar1.Max);
    WaveIn1.EndSample := Round(WaveIn1.TotalSamples * TrackBar2.Position / TrackBar2.Max);
    DXAudioOut1.Run;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;

procedure TForm1.LoadFileToMemory;
begin
  InputMS := TMemoryStream.Create;
  InputMS.LoadFromFile(FileName);
end;

procedure TForm1.UnloadFileFromMemory;
begin
  InputMS.Free;
  InputMS := nil;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WaveOut1.Stop(False);
  DXAudioOut1.Stop(False);
  WaveIn1.Stream := nil;
  UnloadFileFromMemory;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Image1.Picture.Bitmap := TBitmap.Create;
  Image1.Picture.Bitmap.Width := Panel2.Width;
  Image1.Picture.Bitmap.Height := Panel2.Height;
  ClearGraph;
end;

procedure TForm1.ClearGraph;
var
  Rect : TRect;
begin
  Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Rect.Top := 0;
  Rect.Left := 0;
  Rect.Bottom := Image1.Picture.Bitmap.Height;
  Rect.Right := Image1.Picture.Bitmap.Width;
  Image1.Picture.Bitmap.Canvas.FillRect(Rect);
end;

procedure TForm1.DrawGraph;
var
  i, k : Integer;
begin
  for i := 0 to Length(Values) - 1 do
  begin
    k := i*TrackBar1.Max div Image1.Picture.Width;
    if (k >= TrackBar1.Position) and
       (k <= TrackBar2.Position) then
      Image1.Canvas.Pen.Color := clYellow
    else
      Image1.Canvas.Pen.Color := clGreen;
    if DXAudioOut1.Status = tosPlaying then
      if (k >= TrackBar1.Position) and (k <= TrackBar1.Position + (DXAudioOut1.Progress * (TrackBar2.Position - TrackBar1.Position) div 100)) then
      Image1.Canvas.Pen.Color := clRed;
    Image1.Canvas.MoveTo(i, Round((Values[i][0]/48)*(Image1.Height)) - 48);
    Image1.Canvas.LineTo(i, Round((Values[i][1]/48)*(Image1.Height)) - 48);
  end;
end;

procedure TForm1.GetWaveForm;
var
  dx, dy, Summ, Sump : Int64;
  i, l, l1, x, acc : Integer;
  Buf : array[0..1023] of Byte;
begin
  SetLength(Values, Image1.Picture.Width);
//  FillChar(Values[0], Length(Values)*2, 0);
  AudioConverter1.Init;
  dx := AudioConverter1.Size;
  dy := Image1.Width;
  x := 0;
  Summ := 0;
  Sump := 0;
  acc := 0;
  while True do
  begin
    l := 0;
    repeat
      l1 := AudioConverter1.CopyData(@Buf[l], 1024 - l);
      inc(l, l1);
    until (l1 = 0) or (l = 1024);
    if l = 0 then
      Break;
    for i := 0 to l - 1 do
    begin
      Dec(dx, dy);
      if Buf[i] >= 128 then
      Inc(Sump, Buf[i])
      else
      Inc(Summ, Buf[i]);
      Inc(acc);
      if dx <= 0 then
      begin
        Values[x][0] := Summ div acc;
        Values[x][1] := Sump div acc;
        Inc(x);
        Summ := 0;
        Sump := 0;
        acc := 0;
        Inc(dx, AudioConverter1.Size);
      end;
    end;
  end;
  AudioConverter1.Flush;
end;

end.
