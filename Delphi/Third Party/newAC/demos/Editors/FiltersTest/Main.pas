unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, NewAC_DSP, ACS_Filters, ACS_Wave, ExtCtrls,
  StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    WaveIn1: TWaveIn;
    SincFilter1: TSincFilter;
    ChebyshevFilter1: TChebyshevFilter;
    FrequencyAnalysis1: TFrequencyAnalysis;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    FrequencyAnalysis2: TFrequencyAnalysis;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    RadioGroup2: TRadioGroup;
    SpinEdit1: TSpinEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    SpinEdit2: TSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FrequencyAnalysis1Done(Sender: TComponent);
    procedure FrequencyAnalysis2Done(Sender: TComponent);
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
  i : Integer;
begin
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.BoundsRect);
  Image1.Canvas.Pen.Color := clMaroon;
  Image1.Canvas.Pen.Width := 1;
  for i := 1 to 7 do
  begin
    Image1.Canvas.MoveTo(i*128, 0);
    Image1.Canvas.LineTo(i*128, 400);
    Image1.Canvas.MoveTo(0, i*50);
    Image1.Canvas.LineTo(1024, i*50);
  end;
  if OpenDialog1.Execute then
  begin
    WaveIn1.FileName := OpenDialog1.FileName;
    Label7.Caption := IntToStr(WaveIn1.SampleRate);
    Button1.Enabled := False;
    RadioGroup1.Enabled := False;
    RadioGroup2.Enabled := False;
    Edit1.Enabled := False;
    Edit2.Enabled := False;
    Edit3.Enabled := False;
    SpinEdit1.Enabled := False;
    SpinEdit2.Enabled := False;
    SincFilter1.LowFreq := StrToInt(Edit1.Text);
    SincFilter1.HighFreq := StrToInt(Edit2.Text);
    ChebyshevFilter1.LowFreq := StrToInt(Edit1.Text);
    ChebyshevFilter1.HighFreq := StrToInt(Edit2.Text);
    case RadioGroup2.ItemIndex of
      0 :
      begin
        SincFilter1.FilterType := ftLowPass;
        ChebyshevFilter1.FilterType := ftLowPass;
      end;
      1 :
      begin
        SincFilter1.FilterType := ftHighPass;
        ChebyshevFilter1.FilterType := ftHighPass;
      end;
      2 :
      begin
        SincFilter1.FilterType := ftBandPass;
        ChebyshevFilter1.FilterType := ftBandPass;
      end;
      3 :
      begin
        SincFilter1.FilterType := ftBandReject;
        ChebyshevFilter1.FilterType := ftBandReject;
      end;
    end;
    ChebyshevFilter1.NumberOfPoles := SpinEdit1.Value;
    ChebyshevFilter1.Ripple := StrToInt(Edit3.Text)/100;
    SincFilter1.KernelWidth := SpinEdit2.Value;
    case RadioGroup1.ItemIndex of
      0 : FrequencyAnalysis1.Input := SincFilter1;
      1 : FrequencyAnalysis1.Input := ChebyshevFilter1;
    end;
    FrequencyAnalysis1.Run;
  end;
end;

procedure TForm1.FrequencyAnalysis1Done(Sender: TComponent);
var
  i : Integer;
begin
  Image1.Canvas.MoveTo(0, Round((FrequencyAnalysis1.LogMagnitude[0, 0] + 1)/-6*350));
  Image1.Canvas.Pen.Color := clBlue;
  Image1.Canvas.Pen.Width := 2;
  for i := 1 to FrequencyAnalysis1.N div 2 do
     Image1.Canvas.LineTo(i, Round((FrequencyAnalysis1.LogMagnitude[0, i] + 1)/-6*350));
  FrequencyAnalysis2.Run;
end;

procedure TForm1.FrequencyAnalysis2Done(Sender: TComponent);
var
  i : Integer;
begin
  Image1.Canvas.Pen.Color := clRed;
  Image1.Canvas.Pen.Width := 1;
//  Image1.Canvas.MoveTo(0,0);
  Image1.Canvas.MoveTo(0, Round((FrequencyAnalysis2.LogMagnitude[0, 0] + 1)/-6*350));
  for i := 1 to FrequencyAnalysis2.N div 2 do
     Image1.Canvas.LineTo(i, Round((FrequencyAnalysis2.LogMagnitude[0, i] + 1)/-6*350));
  Image1.Canvas.Font.Style := [];
  Image1.Canvas.Font.Color := clRed;
  Image1.Canvas.TextOut(100, 25, 'Original signal');
  Image1.Canvas.Font.Style := [fsBold];
  Image1.Canvas.Font.Color := clBlue;
  Image1.Canvas.TextOut(100, 28 - Image1.Canvas.Font.Height, 'Filtered signal');
  RadioGroup1.Enabled := True;
  RadioGroup2.Enabled := True;
  Edit1.Enabled := True;
  Edit2.Enabled := True;
  Edit3.Enabled := True;
  Button1.Enabled := True;
  SpinEdit1.Enabled := True;
  SpinEdit2.Enabled := True;
end;

end.
