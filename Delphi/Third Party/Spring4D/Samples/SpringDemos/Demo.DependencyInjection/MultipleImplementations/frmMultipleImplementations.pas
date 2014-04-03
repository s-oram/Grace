unit frmMultipleImplementations;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, GIFImg, Spin

  , uCreditCardInterface
  , Spring.Services
  , uCreditCards
  , Spring.Container
  ;

type
  TMultipleImplementationsForm = class(TForm)
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Image1: TImage;
    Edit1: TEdit;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
  private
    { Private declarations }
    CurrentCard: ICreditCard;
  public
    { Public declarations }
  end;

var
  MultipleImplementationsForm: TMultipleImplementationsForm;

implementation

{$R *.dfm}

function RandomString(aLength: Integer; aInputChars: string): string;
begin
  Result := '';
  if Length(aInputChars) <= 0 then
  begin
    Exit;
  end;

  repeat
    Result := Result + aInputChars[Random(Length(aInputChars)) + 1];
  until (Length(Result) = aLength);
end;

procedure TMultipleImplementationsForm.Button1Click(Sender: TObject);
begin
  CurrentCard.IsValid(Edit1.Text);
  Edit1.Text := RandomString(Edit1.MaxLength, '1234567890');
end;

procedure TMultipleImplementationsForm.Button2Click(Sender: TObject);
begin
  CurrentCard.ChargeAmount(Edit1.Text, SpinEdit1.Value);
  SpinEdit1.Value := Random(SpinEdit1.MaxValue) + SpinEdit1.MinValue;
end;

procedure TMultipleImplementationsForm.FormCreate(Sender: TObject);
begin
  RegisterCreditCards(Memo1.Lines);
  GlobalContainer.Build;
  RadioButton1.Checked := True;
  Randomize;
end;

procedure TMultipleImplementationsForm.Image1Click(Sender: TObject);
begin
  RadioButton1.Checked := True;
end;

procedure TMultipleImplementationsForm.Image2Click(Sender: TObject);
begin
  RadioButton4.Checked := True;
end;

procedure TMultipleImplementationsForm.Image3Click(Sender: TObject);
begin
  RadioButton2.Checked := True;
end;

procedure TMultipleImplementationsForm.Image4Click(Sender: TObject);
begin
  RadioButton3.Checked := True;
end;

procedure TMultipleImplementationsForm.RadioButton1Click(Sender: TObject);
begin
  CurrentCard := ServiceLocator.GetService<ICreditCard>(VISA);
end;

procedure TMultipleImplementationsForm.RadioButton2Click(Sender: TObject);
begin
  CurrentCard := ServiceLocator.GetService<ICreditCard>(MasterCard);
end;

procedure TMultipleImplementationsForm.RadioButton3Click(Sender: TObject);
begin
  CurrentCard := ServiceLocator.GetService<ICreditCard>(Discover);
end;

procedure TMultipleImplementationsForm.RadioButton4Click(Sender: TObject);
begin
  CurrentCard := ServiceLocator.GetService<ICreditCard>(AMEX);
end;

end.
