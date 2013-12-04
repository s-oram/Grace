unit uMainForm;

interface

uses
  AudioIO,  eeSampleFloat, VamSampleDisplayBackBuffer, VamSamplePeakBuffer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl, VamWinControl,
  VamSampleDisplay, RedFoxContainer, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    RedFoxContainer1: TRedFoxContainer;
    SampleDisplay: TVamSampleDisplay;
    FileOpenDialog1: TFileOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    Sample : TSampleFloat;
    Peakbuffer : IPeakBuffer;
    ImageBuffer : ISampleImageBuffer;

    a : integer;
    b : cardinal;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VamLib.Utils;



procedure TForm1.FormCreate(Sender: TObject);
begin
  Sample := TSampleFloat.Create;
  PeakBuffer := TPeakbuffer.Create;
  ImageBuffer := TSampleImageBuffer.Create;


  a := 50;
  b := 2000000000000;

  ShowMessage(IntToStr(a + Integer(b)));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Sample.Free;
  PeakBuffer := nil;
  ImageBuffer := nil;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  sdw : integer;
  img : TSampleImageBuffer;
begin
  sdw := SampleDisplay.Width;


  img := (ImageBuffer.GetObject as TSampleImageBuffer);

  img.Resize(SampleDisplay.Width, SampleDisplay.Height);

  FileOpenDialog1.DefaultFolder := 'D:\Audio\Data\Samples (Old disorganised)\Loops\rekkerd_free_loops_05';
  if FileOpenDialog1.Execute then
  begin
    if Sample.LoadFromFile(FileOpenDialog1.FileName) then
    begin
      PeakBuffer.GeneratePeaks(Sample.Properties.Ch1, Sample.Properties.SampleFrames, sdw);
      Img.DrawSample(PeakBuffer);
      SampleDisplay.DrawSample(ImageBuffer);
    end;
  end;

end;


end.
