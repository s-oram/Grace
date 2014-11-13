unit VamCompoundLabel;

interface

uses
  WinApi.Messages,
  Types, Controls, Classes, StdCtrls, Graphics,
  RedFox, RedFoxColor,
  VamWinControl, VamLabel;

type
  TVamCompoundLabel = class(TVamWinControl)
  private
    fColor2: TColor;
    fColor1: TColor;
    fText2: string;
    fText1: string;
    fText2Align: TRedFoxAlign;
    procedure SetColor1(const Value: TColor);
    procedure SetColor2(const Value: TColor);
    procedure SetText1(const Value: string);
    procedure SetText2(const Value: string);
    procedure SetText2Align(const Value: TRedFoxAlign);

    // NOTE: I think this is the correct way to respond to font changes.
    // http://stackoverflow.com/a/4998033/395461
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    Label1, Label2 : TVamLabel;
    procedure SetFont(const Value: TFont); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;



  published
    property Font;

    property Color1 : TColor read fColor1 write SetColor1;
    property Color2 : TColor read fColor2 write SetColor2;

    property Text1 : string read fText1 write SetText1;
    property Text2 : string read fText2 write SetText2;

    property Text2Align : TRedFoxAlign read fText2Align write SetText2Align;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

{ TVamCompoundLabel }

constructor TVamCompoundLabel.Create(AOwner: TComponent);
begin
  inherited;

  Label1 := TVamLabel.Create(self);
  Label1.HitTest := false;
  Label1.Parent := self;
  Label1.Align := alLeft;
  Label1.Visible := true;
  Label1.AutoSize := true;
  Label1.IsSubComponent := true;


  Label2 := TVamLabel.Create(self);
  Label2.HitTest := false;
  Label2.Parent := self;
  Label2.Align := alClient;
  Label2.Visible := true;
  Label2.TextAlign := TRedFoxAlign.AlignFar;
  Label2.IsSubComponent := true;

  Color1 := clBlack;
  Color2 := clBlack;

  Text1 := 'Label';
  Text2 := 'Text';

  Text2Align := TRedFoxAlign.AlignFar;
end;

destructor TVamCompoundLabel.Destroy;
begin
  Label1.Free;
  Label2.Free;
  inherited;
end;

procedure TVamCompoundLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;

  if (assigned(Label1)) and (assigned(Label2)) then
  begin
    Label1.Font := Font;
    Label2.Font := Font;

    Label1.Font.Color := fColor1;
    Label2.Font.Color := fColor2;
  end;
end;




procedure TVamCompoundLabel.SetColor1(const Value: TColor);
begin
  fColor1 := Value;
  Label1.Font.Color := Value;
end;

procedure TVamCompoundLabel.SetColor2(const Value: TColor);
begin
  fColor2 := Value;
  Label2.Font.Color := Value;
end;

procedure TVamCompoundLabel.SetFont(const Value: TFont);
begin
  inherited;
end;

procedure TVamCompoundLabel.SetText1(const Value: string);
begin
  fText1 := Value;
  Label1.Text := Value;
end;

procedure TVamCompoundLabel.SetText2(const Value: string);
begin
  fText2 := Value;
  Label2.Text := Value;
end;

procedure TVamCompoundLabel.SetText2Align(const Value: TRedFoxAlign);
begin
  fText2Align := Value;
  Label2.TextAlign := Value;
end;

procedure TVamCompoundLabel.Paint;
begin
  inherited;

  //NOTE: clear the background becuase it's not completed in the child component.
  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
end;


end.
