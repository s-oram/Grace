unit uMainForm;

interface

uses
  VamLib.UniqueID,
  VamLib.ZeroObject,
  VamLib.Collections.Lists,
  VamLib.MultiEvent,
  VamLib.Debouncer,
  VamLib.Animation,
  AudioIO,
  eeSampleFloat, VamSampleDisplayBackBuffer, VamSamplePeakBuffer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl, VamWinControl,
  VamSampleDisplay, RedFoxContainer, Vcl.StdCtrls, VamLabel, VamKnob,
  VamModSelector, VamCompoundNumericKnob, VamNumericKnob,
  LucidityGui.DropBoxSelector, VamShortMessageOverlay;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    RedFoxContainer1: TRedFoxContainer;
    Knob1: TVamNumericKnob;
    Knob2: TVamNumericKnob;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    VamShortMessageOverlay1: TVamShortMessageOverlay;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateLabel;
  public
    procedure UpdateMemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VamLib.Throttler,
  eeEnumHelper,
  Generics.Collections,
  VamLib.Threads,
  VamLib.Utils;

type
  TProcDictionary = TDictionary<integer, TDateTime>;

var
  GlobalDict : TProcDictionary;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ///


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ////
end;


procedure TForm1.UpdateLabel;
begin

end;

procedure TForm1.UpdateMemo;
begin
  {
  Memo1.Clear;
  for c1 := 0 to FooList.Count-1 do
  begin
    Memo1.Lines.Add(FooList[c1].Text);
  end;
  }
end;

initialization
  GlobalDict := TProcDictionary.Create(100);

finalization
  GlobalDict.Free;

end.
