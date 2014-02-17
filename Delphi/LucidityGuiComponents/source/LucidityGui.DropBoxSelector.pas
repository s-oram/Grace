unit LucidityGui.DropBoxSelector;

interface

uses
  VamGuicontrolInterfaces,
  Graphics, Controls,
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TDropBoxSelector = class(TVamWinControl, IMenuControl)
  private
    fColorTextA : TRedFoxColor;
    fColorTextB : TRedFoxColor;
    IsMouseOver : boolean;
    fColor : TRedFoxColor;
    fColorMouseOver : TRedFoxColor;
    fColorBorder : TRedFoxColor;
    fTextA: string;
    fImageOverlay: TBitmap;
    FTextPadding: TPadding;
    fShowBorder: boolean;
    fTextB: string;
    procedure SetTextA(const Value: string);
    function GetColor: TRedFoxColorString;
    function GetColorMouseOver: TRedFoxColorString;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure SetColorMouseOver(const Value: TRedFoxColorString);
    procedure SetImageOverlay(const Value: TBitmap);
    procedure SetTextPadding(const Value: TPadding);

    procedure SetMenuText(Value : string);
    function GetColorBorder: TRedFoxColorString;
    procedure SetColorBorder(const Value: TRedFoxColorString);
    procedure SetShowBorder(const Value: boolean);
    function GetColorB(const Index: Integer): TRedFoxColorString;
    procedure SetColorB(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetTextB(const Value: string);
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;

    procedure EventHandle_TextPaddingChange(Sender : TObject);

    procedure Draw_DropArrow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ColorTextA     : TRedFoxColorString index 0 read GetColorB write SetColorB;
    property ColorTextB     : TRedFoxColorString index 1 read GetColorB write SetColorB;

    property Color          : TRedFoxColorString read GetColor write SetColor;
    property ColorMouseOver : TRedFoxColorString read GetColorMouseOver write SetColorMouseOver;

    property ColorBorder    : TRedFoxColorString read GetColorBorder    write SetColorBorder;
    property ShowBorder : boolean read fShowBorder write SetShowBorder;

    property TextA       : string   read fTextA       write SetTextA;
    property TextB       : string   read fTextB       write SetTextB;
    property TextPadding : TPadding read FTextPadding write SetTextPadding;

    property Font;

    property ImageOverlay:TBitmap read fImageOverlay write SetImageOverlay;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  AggBasics,
  SysUtils, Types, AggPixelFormat;

{ TVamTextBox }

constructor TDropBoxSelector.Create(AOwner: TComponent);
begin
  inherited;

  fColor.SetColor('$FF3E3E3E');
  fColorMouseOver.SetColor('$FF3E3E3E');

  Font.Color := clWhite;

  FTextPadding := TPadding.Create(Self);
  FTextPadding.SetBounds(0,0,0,0);
  fTextPadding.OnChange := EventHandle_TextPaddingChange;
end;

destructor TDropBoxSelector.Destroy;
begin
  FreeAndNil(FTextPadding);
  inherited;
end;

function TDropBoxSelector.GetColorB(const Index: Integer): TRedFoxColorString;
begin
  case Index of
    0: result := fColorTextA;
    1: result := fColorTextB;
  else
    result := '$00000000';
  end;

end;

procedure TDropBoxSelector.SetColorB(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColor;
begin
  case Index of
    0: pc := @fColorTextA;
    1: pc := @fColorTextB;
  else
    pc := nil;
    exit;
  end;

  if assigned(pc) then
  begin
    if pc^.AsString <> Value then
    begin
      pc^ := Value;
      Invalidate;
    end;
  end;
end;





procedure TDropBoxSelector.EventHandle_TextPaddingChange(Sender: TObject);
begin
  Invalidate;
end;

function TDropBoxSelector.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

function TDropBoxSelector.GetColorBorder: TRedFoxColorString;
begin
  result := fColorBorder.AsString;
end;

function TDropBoxSelector.GetColorMouseOver: TRedFoxColorString;
begin
  result := fColorMouseOver.AsString;
end;

procedure TDropBoxSelector.MouseEnter;
begin
  inherited;

  IsMouseOver := true;
  Invalidate;
end;

procedure TDropBoxSelector.MouseLeave;
begin
  inherited;

  IsMouseOver := false;
  Invalidate;
end;

procedure TDropBoxSelector.SetColor(const Value: TRedFoxColorString);
begin
  if Value <> fColor.AsString then
  begin
    fColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TDropBoxSelector.SetColorBorder(const Value: TRedFoxColorString);
begin
  if Value <> fColorBorder.AsString then
  begin
    fColorBorder.SetColor(Value);
    Invalidate;
  end;
end;

procedure TDropBoxSelector.SetColorMouseOver(const Value: TRedFoxColorString);
begin
  if Value <> fColorMouseOver.AsString then
  begin
    fColorMouseOver.SetColor(Value);
    Invalidate;
  end;
end;

procedure TDropBoxSelector.SetImageOverlay(const Value: TBitmap);
begin
  if fImageOverlay <> Value then
  begin
    fImageOverlay := Value;
    Invalidate;
  end;
end;

procedure TDropBoxSelector.SetMenuText(Value: string);
begin
  SetTextB(Value);
end;

procedure TDropBoxSelector.SetShowBorder(const Value: boolean);
begin
  if Value <> fShowBorder then
  begin
    fShowBorder := Value;
    Invalidate;
  end;
end;

procedure TDropBoxSelector.SetTextA(const Value: string);
begin
  if Value <> fTextA then
  begin
    fTextA := Value;
    Invalidate;
  end;
end;

procedure TDropBoxSelector.SetTextB(const Value: string);
begin
  if Value <> fTextB then
  begin
    fTextB := Value;
    Invalidate;
  end;
end;

procedure TDropBoxSelector.SetTextPadding(const Value: TPadding);
begin
  FTextPadding.Assign(Value);
end;

procedure TDropBoxSelector.Paint;
var
  aColor : TRedFoxColor;
  TextBounds : TRect;
  SrcRect : TRect;
  DstRect : TRect;
  PaddingRight : integer;
  LabelTextWidth : integer;

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
  PaddingRight := TextPadding.Right + round(Height * 1/3);

  TextBounds := Rect(TextPadding.Left, TextPadding.Top, Width-PaddingRight, Height-TextPadding.Bottom);
  BackBuffer.DrawText(TextA, Font, TRedFoxAlign.AlignNear, TRedFoxAlign.AlignCenter, TextBounds, ColorTextA);

  LabelTextWidth := round(BackBuffer.BufferInterface.TextWidth(TextA + '--'));

  TextBounds := Rect(TextPadding.Left + LabelTextWidth, TextPadding.Top, Width-PaddingRight, Height-TextPadding.Bottom);
  BackBuffer.DrawText(TextB, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextBounds, ColorTextB);



  //=============================================================================
  Draw_DropArrow;
  //=============================================================================


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

procedure TDropBoxSelector.Draw_DropArrow;
var
  Points : array[0..2] of TPointDouble;
begin
  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := fColorTextB;

  Points[0].X := Width - TextPadding.Right;
  Points[0].Y := Height * (1/3);

  Points[1].X := Width - TextPadding.Right - (Height * 1/3);
  Points[1].Y := Height * (1/3);

  Points[2].X := Width - TextPadding.Right - (Height * 1/3 * 0.5);
  Points[2].Y := Height * (2/3);

  BackBuffer.BufferInterface.Polygon(@Points[0], 3);
end;



end.
