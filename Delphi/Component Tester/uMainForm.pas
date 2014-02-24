unit uMainForm;

interface

uses
  eeKnobSmoother,
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
  LucidityGui.DropBoxSelector, VamShortMessageOverlay, Vcl.ExtCtrls;

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
    Button5: TButton;
    VamKnob1: TVamKnob;
    VamKnob2: TVamKnob;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure VamKnob1KnobPosChanged(Sender: TObject);
  private
    ID : TUniqueID;
    KnobValue : single;
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
  ID.Init;


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ////
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AniObj : TSingleAnimation;
begin
  AniObj := TSingleAnimation.Create;
  AniObj.StartValue := 0;
  AniObj.EndValue   := 400;
  AniObj.RunTime    := 5000;
  AniObj.ApplyMethod := procedure(CurrentValue:single)
  begin
    Button2.Left := round(CurrentValue);
  end;

  GlobalAnimator.Animate(ID, AniObj);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Button2.Free;
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




procedure TForm1.VamKnob1KnobPosChanged(Sender: TObject);
begin
  VamKnob2.Pos := VamKnob1.Pos;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  //VamKnob2.Pos := KnobValue;
end;



initialization
  GlobalDict := TProcDictionary.Create(100);

finalization
  GlobalDict.Free;

end.
