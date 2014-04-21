unit VamShortMessageOverlay;

interface

uses
  Graphics, Controls,
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamShortMessageOverlay = class(TVamWinControl)
  private
    IsMouseOver : boolean;
    fColor : TRedFoxColor;
    fColorBorder : TRedFoxColor;
    fColorText   : TRedFoxColor;
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fShowBorder: boolean;
    fBorderWidth: integer;
    fCornerRadius2: double;
    fCornerRadius3: double;
    fCornerRadius1: double;
    fCornerRadius4: double;
    FTextPadding: TPadding;
    fAutoSizeBackground: boolean;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    function GetColor: TRedFoxColorString;
    procedure SetColor(const Value: TRedFoxColorString);
    function GetColorBorder: TRedFoxColorString;
    procedure SetColorBorder(const Value: TRedFoxColorString);
    procedure SetShowBorder(const Value: boolean);
    function GetColorText: TRedFoxColorString;
    procedure SetColorText(const Value: TRedFoxColorString);
    procedure SetBorderWidth(const Value: integer);
    procedure SetCornerRadius1(const Value: double);
    procedure SetCornerRadius2(const Value: double);
    procedure SetCornerRadius3(const Value: double);
    procedure SetCornerRadius4(const Value: double);
    procedure SetTextPadding(const Value: TPadding);
    procedure SetAutoSizeBackground(const Value: boolean);
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;

    procedure EventHandle_TextPaddingChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property AutoSizeBackground : boolean        read fAutoSizeBackground write SetAutoSizeBackground;
    property Color          : TRedFoxColorString read GetColor        write SetColor;
    property ColorBorder    : TRedFoxColorString read GetColorBorder  write SetColorBorder;
    property ColorText      : TRedFoxColorString read GetColorText    write SetColorText;

    property BorderWidth    : integer            read fBorderWidth    write SetBorderWidth;
    property ShowBorder     : boolean            read fShowBorder     write SetShowBorder;

    property CornerRadius1 : double read fCornerRadius1 write SetCornerRadius1;
    property CornerRadius2 : double read fCornerRadius2 write SetCornerRadius2;
    property CornerRadius3 : double read fCornerRadius3 write SetCornerRadius3;
    property CornerRadius4 : double read fCornerRadius4 write SetCornerRadius4;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;

    property TextPadding : TPadding read FTextPadding write SetTextPadding;

    property Font;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  Math,
  SysUtils, Types, AggPixelFormat;

{ TVamTextBox }

constructor TVamShortMessageOverlay.Create(AOwner: TComponent);
begin
  inherited;

  fColor     := '$FFFFFFFF';
  fColorText := '$FF000000';
  fColorBorder := '$FF000000';

  fTextVAlign := TRedFoxAlign.AlignCenter;
  fTextAlign  := TRedFoxAlign.AlignCenter;

  fCornerRadius1 := 3;
  fCornerRadius2 := 3;
  fCornerRadius3 := 3;
  fCornerRadius4 := 3;

  FTextPadding := TPadding.Create(Self);
  FTextPadding.SetBounds(0,0,0,0);
  fTextPadding.OnChange := EventHandle_TextPaddingChange;

  FAutoSizeBackground := true;
end;

destructor TVamShortMessageOverlay.Destroy;
begin
  FreeAndNil(FTextPadding);

  inherited;
end;

procedure TVamShortMessageOverlay.EventHandle_TextPaddingChange(Sender: TObject);
begin
  Invalidate;
end;

function TVamShortMessageOverlay.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

function TVamShortMessageOverlay.GetColorBorder: TRedFoxColorString;
begin
  result := fColorBorder.AsString;
end;

function TVamShortMessageOverlay.GetColorText: TRedFoxColorString;
begin
  result := FColorText.AsString;
end;

procedure TVamShortMessageOverlay.MouseEnter;
begin
  inherited;

  IsMouseOver := true;
  Invalidate;
end;

procedure TVamShortMessageOverlay.MouseLeave;
begin
  inherited;

  IsMouseOver := false;
  Invalidate;
end;

procedure TVamShortMessageOverlay.SetAutoSizeBackground(const Value: boolean);
begin
  if Value <> fAutoSizeBackground then
  begin
    fAutoSizeBackground := Value;
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetBorderWidth(const Value: integer);
begin
  if Value <> FBorderWidth then
  begin
    fBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetColor(const Value: TRedFoxColorString);
begin
  if Value <> fColor.AsString then
  begin
    fColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetColorBorder(const Value: TRedFoxColorString);
begin
  if Value <> fColorBorder.AsString then
  begin
    fColorBorder.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetColorText(const Value: TRedFoxColorString);
begin
  FColorText := Value;
end;

procedure TVamShortMessageOverlay.SetCornerRadius1(const Value: double);
begin
  fCornerRadius1 := Value;
end;

procedure TVamShortMessageOverlay.SetCornerRadius2(const Value: double);
begin
  fCornerRadius2 := Value;
end;

procedure TVamShortMessageOverlay.SetCornerRadius3(const Value: double);
begin
  fCornerRadius3 := Value;
end;

procedure TVamShortMessageOverlay.SetCornerRadius4(const Value: double);
begin
  fCornerRadius4 := Value;
end;

procedure TVamShortMessageOverlay.SetShowBorder(const Value: boolean);
begin
  if Value <> fShowBorder then
  begin
    fShowBorder := Value;
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetTextPadding(const Value: TPadding);
begin
  FTextPadding.Assign(Value);
end;

procedure TVamShortMessageOverlay.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.Paint;
var
  TextBounds : TRect;
  IntPadding : integer;
  BackgroundRect : TRect;
  w, h : integer;
begin
  inherited;

  BackBuffer.UpdateFont(Font);


  if AutoSizeBackground then
  begin
    w := round(BackBuffer.TextWidth(Text) + FTextPadding.Left + FTextPadding.Right  + BorderWidth);
    h := round(BackBuffer.TextHeight      + FTextPadding.Top  + FTextPadding.Bottom + BorderWidth);
    BackgroundRect.Left   := round((Width - w) * 0.5);
    BackgroundRect.Right  := BackgroundRect.Left + w;
    BackgroundRect.Top    := round((Height - h) * 0.5);
    BackgroundRect.Bottom := BackgroundRect.Top + h;

    TextBounds.Left   := BackgroundRect.Left   + TextPadding.Left;
    TextBounds.Top    := BackgroundRect.Top    + TextPadding.Top;
    TextBounds.Right  := BackgroundRect.Right  - TextPadding.Right;
    TextBounds.Bottom := BackgroundRect.Bottom - TextPadding.Bottom;
  end else
  begin
    IntPadding := Ceil(BorderWidth * 0.5);
    BackgroundRect := Rect(IntPadding, IntPadding, Width-IntPadding, Height-IntPadding);
    TextBounds := Rect(0, 0, Width, Height);
  end;



  BackBuffer.BufferInterface.ClearAll(fColor.WithAlpha(0));
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;






  //== draw the background ==
  if ShowBorder then
  begin
    BackBuffer.BufferInterface.LineColor := fColorBorder;
    BackBuffer.BufferInterface.LineWidth := BorderWidth;
  end else
  begin
    BackBuffer.BufferInterface.NoLine;
  end;

  BackBuffer.BufferInterface.FillColor := fColor.AsAggRgba8;
  BackBuffer.BufferInterface.RoundedRect(BackgroundRect.Left, BackgroundRect.Top, BackgroundRect.Right, BackgroundRect.Bottom, fCornerRadius1, fCornerRadius2, fCornerRadius3, fCornerRadius4);

  //== draw the text ==
  //TODO: see if text draw can be improved by incorporating RedFoxTextBuffer.
  BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds, FColorText);

end;

end.
