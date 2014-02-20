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
    fColorMouseOver : TRedFoxColor;
    fColorBorder : TRedFoxColor;
    fColorText   : TRedFoxColor;
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fShowBorder: boolean;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    function GetColor: TRedFoxColorString;
    function GetColorMouseOver: TRedFoxColorString;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure SetColorMouseOver(const Value: TRedFoxColorString);
    procedure SetMenuText(Value : string);
    function GetColorBorder: TRedFoxColorString;
    procedure SetColorBorder(const Value: TRedFoxColorString);
    procedure SetShowBorder(const Value: boolean);
    function GetColorText: TRedFoxColorString;
    procedure SetColorText(const Value: TRedFoxColorString);
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color          : TRedFoxColorString read GetColor        write SetColor;
    property ColorBorder    : TRedFoxColorString read GetColorBorder  write SetColorBorder;
    property ColorText      : TRedFoxColorString read GetColorText    write SetColorText;
    property ShowBorder     : boolean            read fShowBorder     write SetShowBorder;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;

    property Font;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
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
end;

destructor TVamShortMessageOverlay.Destroy;
begin
  inherited;
end;

function TVamShortMessageOverlay.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

function TVamShortMessageOverlay.GetColorBorder: TRedFoxColorString;
begin
  result := fColorBorder.AsString;
end;

function TVamShortMessageOverlay.GetColorMouseOver: TRedFoxColorString;
begin
  result := fColorMouseOver.AsString;
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

procedure TVamShortMessageOverlay.SetColorMouseOver(const Value: TRedFoxColorString);
begin
  if Value <> fColorMouseOver.AsString then
  begin
    fColorMouseOver.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetColorText(const Value: TRedFoxColorString);
begin
  FColorText := Value;
end;

procedure TVamShortMessageOverlay.SetImageOverlay(const Value: TBitmap);
begin
  if fImageOverlay <> Value then
  begin
    fImageOverlay := Value;
    Invalidate;
  end;
end;

procedure TVamShortMessageOverlay.SetMenuText(Value: string);
begin
  SetText(Value);
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
  aColor : TRedFoxColor;
  TextBounds : TRect;
  SrcRect : TRect;
  DstRect : TRect;
begin
  inherited;

  // HACK: The Agg blending doesn't work that well on transparent backgrounds.
  // Setting the color to the same as the font allows the anti-aliasing to
  // be a bit brighter. Otherwise the AA gamma gets all messed up.
  if aColor.A < 10 then aColor := GetRedFoxColor(Font.Color);

  BackBuffer.BufferInterface.ClearAll(aColor.WithAlpha(0));
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  if IsMouseOver = false
    then aColor := fColor
    else aColor := fColorMouseOver;

  //== draw the background ==
  if ShowBorder then
  begin
    BackBuffer.BufferInterface.LineColor := fColorBorder;
    BackBuffer.BufferInterface.LineWidth := 1;
  end else
  begin
    BackBuffer.BufferInterface.NoLine;
  end;
  BackBuffer.BufferInterface.FillColor := aColor.AsAggRgba8;
  BackBuffer.BufferInterface.RoundedRect(0, 0, Width, Height, 3);

  //== draw the text ==
  //TODO: see if text draw can be improved by incorporating RedFoxTextBuffer.
  TextBounds := Rect(0, 0, Width, Height);
  BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds, FColorText);




end;

end.
