unit uMainForm;

interface

uses
  VamLib.Collections.Lists,
  VamLib.Debouncer,
  AudioIO,
  eeSampleFloat, VamSampleDisplayBackBuffer, VamSamplePeakBuffer,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl, VamWinControl,
  VamSampleDisplay, RedFoxContainer, Vcl.StdCtrls, VamLabel, VamKnob,
  VamModSelector, VamCompoundNumericKnob;

type
  TFoo = class
  private
    fText: string;
  public
    destructor Destroy; override;
    property Text : string read fText write fText;
  end;

  TFooList = TSimpleObjectList<TFoo>;

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    RedFoxContainer1: TRedFoxContainer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Knob1Changed(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateLabel;
  public
    FooList : TFooList;

    Debouncer : TDebouncer;

    Sample : TSampleFloat;
    Peakbuffer : IPeakBuffer;
    ImageBuffer : ISampleImageBuffer;

    a : integer;
    b : cardinal;
    procedure UpdateMemo;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VamLib.Threads,
  VamLib.Utils;



procedure TForm1.FormCreate(Sender: TObject);
var
  c1: Integer;
begin
  Sample := TSampleFloat.Create;
  PeakBuffer := TPeakbuffer.Create;
  ImageBuffer := TSampleImageBuffer.Create;


  a := 50;
  b := 2000000000000;

  //ShowMessage(IntToStr(a + Integer(b)));

  Debouncer := TDebouncer.Create;
  Debouncer.DebounceTime := 100;


  FooList := TFooList.Create;
  FooList.OwnsObjects := true;
  for c1 := 0 to 10 do
  begin
    FooList.Add(TFoo.Create);
    FooList[c1].Text := IntToStr(c1) + ' bottle(s) on the wall!';
  end;

  FooList.Delete(4);

  UpdateMemo;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Debouncer.Free;
  Sample.Free;
  PeakBuffer := nil;
  ImageBuffer := nil;
  FooList.Free;
end;


procedure TForm1.UpdateLabel;
begin

end;

procedure TForm1.UpdateMemo;
var
  c1: Integer;
begin
  Memo1.Clear;
  for c1 := 0 to FooList.Count-1 do
  begin
    Memo1.Lines.Add(FooList[c1].Text);
  end;
end;

procedure TForm1.Knob1Changed(Sender: TObject);
begin

end;

{ TFoo }

destructor TFoo.Destroy;
begin
  //ShowMessage(Text);
  inherited;
end;

end.
