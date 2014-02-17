unit uMainForm;

interface

uses
  VamLib.UniqueID,
  EasyEffect.ZeroObject,
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
  LucidityGui.DropBoxSelector;

type
  IFoo = interface(IZeroObject)
    ['{7A691176-973B-40FE-BAE6-C48AE61C1BB9}']
  end;

  TFoo = class(TZeroObject, IFoo)
  private
    fText: string;
  public
    constructor Create;
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
    Button4: TButton;
    DropBoxSelector1: TDropBoxSelector;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VamNumericKnob1Changed(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateLabel;
  public
    id1, id2 : TUniqueID;
    AnimateController : TAnimateController;

    Foo1, Foo2 : IFoo;

    MotherShip : TMothership;


    MultiEvent : TNotifyMultiEvent;
    FooList : TFooList;

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
var
  c1: Integer;
begin
  MotherShip := TMotherShip.Create;

  AnimateController := TAnimateController.Create;


  id1.Init;
  id2.Init;

  //Foo1.Free;
  //Foo2.Free;

  //Foo1 := nil;
  //Foo2 := nil;


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AnimateController.Free;
  MotherShip.Free;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  AniA : TSingleAnimation;
begin

  AniA := TSingleAnimation.Create(id1, 0,100,1500, procedure(AniObj : TCustomAnimation)
  begin
    Button3.Left := round((AniObj as TSingleAnimation).CurrentValue);
    //Knob1.KnobValue := (AniObj as TSingleAnimation).CurrentValue;
  end);

  AnimateController.Animate(AniA);

  AniA := TSingleAnimation.Create(id2, 100,0,1500, procedure(AniObj : TCustomAnimation)
  begin
    Button4.Left := round((AniObj as TSingleAnimation).CurrentValue);
    //Knob1.KnobValue := (AniObj as TSingleAnimation).CurrentValue;
  end);

  AnimateController.Animate(AniA);

end;


procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowMessage(IntToStr(MotherShip.ZeroObjectCount));
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

procedure TForm1.VamNumericKnob1Changed(Sender: TObject);
begin

end;


{ TFoo }

constructor TFoo.Create;
begin
  //self.FIsReferenceCounted := false;
  self.FIsReferenceCounted := true;
  self.Text := IntToStr(Random(100));


end;

destructor TFoo.Destroy;
begin
  ShowMessage(Text);
  inherited;
end;


initialization
  GlobalDict := TProcDictionary.Create(100);

finalization
  GlobalDict.Free;

end.
