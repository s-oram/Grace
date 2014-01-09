unit VamModSelector;

interface

uses
  Graphics, Controls,
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamModSelector = class(TVamWinControl)
  private
    IsMouseOver : boolean;
    fColor : TRedFoxColor;
    fColorMouseOver : TRedFoxColor;
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fImageOverlay: TBitmap;
    FTextPadding: TPadding;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    function GetColor: TRedFoxColorString;
    function GetColorMouseOver: TRedFoxColorString;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure SetColorMouseOver(const Value: TRedFoxColorString);
    procedure SetImageOverlay(const Value: TBitmap);
    procedure SetTextPadding(const Value: TPadding);
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;

    procedure EventHandle_TextPaddingChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


  published
    property Color          : TRedFoxColorString read GetColor          write SetColor;
    property ColorMouseOver : TRedFoxColorString read GetColorMouseOver write SetColorMouseOver;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;
    property TextPadding : TPadding read FTextPadding write SetTextPadding;

    property Font;

    property ImageOverlay:TBitmap read fImageOverlay write SetImageOverlay;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils, Types, AggPixelFormat;

{ TVamTextBox }

constructor TVamModSelector.Create(AOwner: TComponent);
begin
  inherited;

  fColor.SetColor('$FF3E3E3E');
  fColorMouseOver.SetColor('$FF3E3E3E');

  Font.Color := clWhite;

  FTextPadding := TPadding.Create(Self);
  FTextPadding.SetBounds(0,0,0,0);
  fTextPadding.OnChange := EventHandle_TextPaddingChange;
end;

destructor TVamModSelector.Destroy;
begin
  FreeAndNil(FTextPadding);
  inherited;
end;

procedure TVamModSelector.EventHandle_TextPaddingChange(Sender: TObject);
begin
  Invalidate;
end;

function TVamModSelector.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

function TVamModSelector.GetColorMouseOver: TRedFoxColorString;
begin
  result := fColorMouseOver.AsString;
end;

procedure TVamModSelector.MouseEnter;
begin
  inherited;

  IsMouseOver := true;
  Invalidate;
end;

procedure TVamModSelector.MouseLeave;
begin
  inherited;

  IsMouseOver := false;
  Invalidate;
end;

procedure TVamModSelector.SetColor(const Value: TRedFoxColorString);
begin
  if Value <> fColor.AsString then
  begin
    fColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamModSelector.SetColorMouseOver(const Value: TRedFoxColorString);
begin
  if Value <> fColorMouseOver.AsString then
  begin
    fColorMouseOver.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamModSelector.SetImageOverlay(const Value: TBitmap);
begin
  if fImageOverlay <> Value then
  begin
    fImageOverlay := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.SetTextPadding(const Value: TPadding);
begin
  FTextPadding.Assign(Value);
end;

procedure TVamModSelector.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.Paint;
var
  aColor : TRedFoxColor;
  TextBounds : TRect;
  SrcRect : TRect;
  DstRect : TRect;
begin
  inherited;

  if IsMouseOver = false
    then aColor := fColor
    else aColor := fColorMouseOver;

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
  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := aColor.AsAggRgba8;
  BackBuffer.BufferInterface.RoundedRect(0, 0, Width, Height, 3);

  //== draw the text ==
  TextBounds := Rect(TextPadding.Left, TextPadding.Top, Width-TextPadding.Right, Height-TextPadding.Bottom);
  BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);


  if assigned(ImageOverlay) then
  begin
    SrcRect.Left   := 0;
    SrcRect.Width  := ImageOverlay.Width;
    SrcRect.Top    := 0;
    SrcRect.Bottom := ImageOverlay.Height;

    DstRect.Left   := (Width - SrcRect.Width)   div 2;
    DstRect.Right  := DstRect.Left + SrcRect.Width;
    DstRect.Top    := (Height - SrcRect.Height) div 2;
    DstRect.Bottom := DstRect.Top + SrcRect.Height;

    BackBuffer.TransformImage(ImageOverlay, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
  end;

end;

end.
