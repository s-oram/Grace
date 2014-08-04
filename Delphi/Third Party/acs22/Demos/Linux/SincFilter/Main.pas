(*
  This is the main unit file for the ACS Sinc Filter Demo.
  The diagram shows the filter kernel frequency response 
  for the  filter type specified.
*) 

unit Main;

interface

uses
  SysUtils, Variants, Classes, QGraphics, QForms,
  QDialogs, QComCtrls, ACS_Classes, ACS_Audio, ACS_Filters,
  QExtCtrls, ACS_Types, ACS_Procs, Math, ACS_Wave,
  QStdCtrls, QControls;

type
  TForm1 = class(TForm)
    SincFilter1: TSincFilter;
    AudioOut1: TAudioOut;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Image1: TImage;
    RadioGroup1: TRadioGroup;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button2: TButton;
    WaveIn1: TWaveIn;
    procedure Button1Click(Sender: TObject);
    procedure AudioOut1Done(Sender: TComponent);
    procedure AudioOut1Progress(Sender: TComponent);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure DrawFreqResp;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    Button1.Enabled := False;
    AudioOut1.Run;
    SincFilter1.LowFreq := Round(TrackBar1.Position/100*SincFilter1.SampleRate);
    SincFilter1.HighFreq := Round(TrackBar2.Position/100*SincFilter1.SampleRate);
    DrawFreqResp;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.Picture.Bitmap.Width := Image1.Width;
  Image1.Picture.Bitmap.Height := Image1.Height;
  with Image1.Picture.Bitmap do
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Pen.Color := clBlue;
  end;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  if AudioOut1.Status <> tosPlaying then Exit;
  if TrackBar2.Position < TrackBar1.Position then
  TrackBar2.Position := TrackBar1.Position;
  SincFilter1.HighFreq := Round(TrackBar2.Position/100*SincFilter1.SampleRate);
  DrawFreqResp;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  if AudioOut1.Status <> tosPlaying then Exit;
  if TrackBar2.Position < TrackBar1.Position then
  TrackBar2.Position := TrackBar1.Position;
  SincFilter1.LowFreq := Round(TrackBar1.Position/100*SincFilter1.SampleRate);
  SincFilter1.HighFreq := Round(TrackBar2.Position/100*SincFilter1.SampleRate);
  DrawFreqResp;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0 : SincFilter1.FilterType := ftLowPass;
    1 : SincFilter1.FilterType := ftHighPass;
    2 : SincFilter1.FilterType := ftBandPass;
    3 : SincFilter1.FilterType := ftBandReject;
    4 : SincFilter1.FilterType := ftAllPass;
  end;
  DrawFreqResp;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AudioOut1.Stop;
end;

procedure TForm1.DrawFreqResp;
var
  DA : Array of TComplex;
  K : PDoubleArray;
  Size, Step, i : Integer;
begin
  if AudioOut1.Status <> tosPlaying then Exit;
  Size := 1 shl Ceil(Log2(SincFilter1.KernelWidth));
  SetLength(DA, Size);
  FillChar(DA[0], Size*8, 0);
  SincFilter1.GetKernel(K);
  for i := 0 to SincFilter1.KernelWidth-1 do
  DA[i].Re := K[i];
  ComplexFFT(@DA[0], Size, 1);
  with Image1.Picture.Bitmap do
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Pen.Color := clOlive;
  end;
  for i := 1 to 10 do
  with Image1.Picture.Bitmap do
  begin
    Canvas.MoveTo(i*(Width div 10), 0);
    Canvas.LineTo(i*(Width div 10), Height);
    Canvas.MoveTo(0, i*(Width div 10));
    Canvas.LineTo(Width, i*(Width div 10));
  end;
  Image1.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  Image1.Picture.Bitmap.Canvas.Pen.Width := 2;
  Step := Round(2*Image1.Width/Size);
  Image1.Picture.Bitmap.Canvas.MoveTo(0, Image1.Height -Round(Hypot(DA[0].Re, DA[0].Im)*5000));
  for i := 1 to Size shr 1 do
  begin
    Image1.Picture.Bitmap.Canvas.LineTo(i*Step, Image1.Height - 2 - Round(Hypot(DA[i].Re, DA[i].Im)*5000));
  end;
  Image1.Picture.Bitmap.Canvas.Pen.Width := 1;
  DA := nil;
end;

end.
