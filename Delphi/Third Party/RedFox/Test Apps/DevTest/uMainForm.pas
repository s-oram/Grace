unit uMainForm;

interface

uses
  VamQuery,
  Generics.collections,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxContainer, VamScrollBar,
  RedFoxGraphicControl, VamLabel, RedFoxWinControl, VamPanel, VamWinControl,
  VamGraphicControl, VamScrollBox, Vcl.StdCtrls, VamSamplerKeys, VamSampleMap,
  VamDiv, Vcl.ExtCtrls, VamCustomTreeView, VamTreeView, VamScrollPanel, VamKnob,
  VamModularJack, Vcl.ImgList, VamButton, VamSampleZoomControl, VamTextBox,
  VamVectorSequence, VamXYPad, VamTabs, VamNumericKnob, VamArrows,
  VamCompoundNumericKnob, VamCompoundLabel, VamCompoundModMatrixSection,
  VamTabPanel, VamMultiLineTextBox, VamStatusLed, VamMemo, VamSlider;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RedFoxContainer1: TRedFoxContainer;
    FileOpenDialog1: TFileOpenDialog;
    Label1: TLabel;
    VamMemo1: TVamMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  WinApi.Oleacc,
  RedFoxColor;


procedure TForm1.FormCreate(Sender: TObject);
begin
  //VamScrollBar2.HitTest := true;

  //CableOverlay.Left := 0;
  //CableOverlay.Top  := 0;
  //CableOverlay.Width := RedFoxContainer1.Width;
  //CableOverlay.Height := RedFoxContainer1.Height;

  //CableOverlay.BringToFront;

  //VamXYPad2.Layout.Move(0,-50);


end;

end.
