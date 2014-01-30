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
  VamModSelector, VamCompoundNumericKnob, VamNumericKnob;

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
    Knob1: TVamNumericKnob;
    Knob2: TVamNumericKnob;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VamNumericKnob1Changed(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

    procedure Foo(Sender : TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Generics.Collections,
  VamLib.Threads,
  VamLib.Utils;

type
  TProcDictionary = TDictionary<integer, TDateTime>;

var
  GlobalDict : TProcDictionary;



procedure Debounce(id : integer; TimeMS : integer; aProc : TProc);
var
  CurrentTime : TDateTime;
  LastTime : TDateTime;
  foo:  TMethod;
  procAddress : integer;
begin
  CurrentTime := Now;



  if GlobalDict.TryGetValue(id, LastTime) then
  begin
    GlobalDict.AddOrSetValue(id, CurrentTime);
  end else
  begin
    GlobalDict.AddOrSetValue(id, CurrentTime);
  end;

  {
  if GlobalDict.ContainsKey(aProc) then
  begin
    GlobalDict.AddOrSetValue(aProc, CurrentTime);
  end else
  begin
    GlobalDict.Add(aProc, CurrentTime);
  end;
  }

  //procAddress := Integer(Addr(aProc));
  //showMessage(IntToStr(ProcAddress));



end;



procedure TForm1.Foo(Sender: TObject);
begin
  showMessage((Sender as TControl).Name);
end;

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

procedure TForm1.VamNumericKnob1Changed(Sender: TObject);
begin
  {
  Debounce(150, procedure
  begin
    Knob2.KnobValue := Knob1.KnobValue;
  end);
  }
end;

procedure TForm1.Button1Click(Sender: TObject);
type
  TExec = procedure of object;
var
  aMethod : TMethod;
  Exec: TExec;
begin
  aMethod.Data := @Sender;
  aMethod.Code := @TForm1.Foo;
  Exec := TExec(aMethod);
  Exec;

  {
  Debounce(14, 150, procedure
  begin
    Knob2.KnobValue := Knob1.KnobValue;
  end);
  }
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  Debounce(13, 150, procedure
  begin
    Knob2.KnobValue := Knob1.KnobValue;
  end);
end;



procedure TForm1.Button3Click(Sender: TObject);
begin
  Debounce(12, 150, procedure
  begin
    Knob2.KnobValue := Knob1.KnobValue * 2;
  end);
end;

{ TFoo }

destructor TFoo.Destroy;
begin
  //ShowMessage(Text);
  inherited;
end;


initialization
  GlobalDict := TProcDictionary.Create(100);

finalization
  GlobalDict.Free;

end.
