unit VamTextBox;

interface

uses
  VamGuicontrolInterfaces,
  Graphics, Controls,
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamTextBox = class(TVamWinControl, IMenuControl)
  private
    IsMouseOver : boolean;
    fColor : TRedFoxColor;
    fColorMouseOver : TRedFoxColor;
    fColorBorder : TRedFoxColor;
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fImageOverlay: TBitmap;
    FTextPadding: TPadding;
    fShowBorder: boolean;
    fParameterName: string;
    fImageOverlayVertAlign: TRedFoxAlign;
    fImageOverlayHorzAlign: TRedFoxAlign;
    fImageOverlayOffsetY: integer;
    fImageOverlayOffsetX: integer;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
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
    procedure SetParameterName(const Value: string);
    function GetParameterName: string;
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;

    procedure EventHandle_TextPaddingChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // ParameterName is a 'Tag' type property used to store a VST parameter name.
    property ParameterName  : string  read GetParameterName  write SetParameterName;

  published
    property Color : TRedFoxColorString read GetColor write SetColor;
    property ColorMouseOver : TRedFoxColorString read GetColorMouseOver write SetColorMouseOver;

    property ColorBorder    : TRedFoxColorString read GetColorBorder    write SetColorBorder;
    property ShowBorder : boolean read fShowBorder write SetShowBorder;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;
    property TextPadding : TPadding read FTextPadding write SetTextPadding;

    property Font;

    property ImageOverlay:TBitmap read fImageOverlay write SetImageOverlay;
    property ImageOverlayVertAlign : TRedFoxAlign read fImageOverlayVertAlign write fImageOverlayVertAlign;
    property ImageOverlayHorzAlign : TRedFoxAlign read fImageOverlayHorzAlign write fImageOverlayHorzAlign;
    property ImageOverlayOffsetX   : integer      read fImageOverlayOffsetX   write fImageOverlayOffsetX;
    property ImageOverlayOffsetY   : integer      read fImageOverlayOffsetY   write fImageOverlayOffsetY;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils, Types, AggPixelFormat;

{ TVamTextBox }

constructor TVamTextBox.Create(AOwner: TComponent);
begin
  inherited;

  fColor.SetColor('$FF3E3E3E');
  fColorMouseOver.SetColor('$FF3E3E3E');

  Font.Color := clWhite;

  FTextPadding := TPadding.Create(Self);
  FTextPadding.SetBounds(0,0,0,0);
  fTextPadding.OnChange := EventHandle_TextPaddingChange;

  ImageOverlayOffsetX := 0;
  ImageOverlayOffsetY := 0;

  ImageOverlayVertAlign := TRedFoxAlign.AlignCenter;
  ImageOverlayHorzAlign := TRedFoxAlign.AlignCenter;
end;

destructor TVamTextBox.Destroy;
begin
  FreeAndNil(FTextPadding);
  inherited;
end;

procedure TVamTextBox.EventHandle_TextPaddingChange(Sender: TObject);
begin
  Invalidate;
end;

function TVamTextBox.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

function TVamTextBox.GetColorBorder: TRedFoxColorString;
begin
  result := fColorBorder.AsString;
end;

function TVamTextBox.GetColorMouseOver: TRedFoxColorString;
begin
  result := fColorMouseOver.AsString;
end;

function TVamTextBox.GetParameterName: string;
begin
  result := self.fParameterName;
end;

procedure TVamTextBox.MouseEnter;
begin
  inherited;

  IsMouseOver := true;
  Invalidate;
end;

procedure TVamTextBox.MouseLeave;
begin
  inherited;

  IsMouseOver := false;
  Invalidate;
end;

procedure TVamTextBox.SetColor(const Value: TRedFoxColorString);
begin
  if Value <> fColor.AsString then
  begin
    fColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamTextBox.SetColorBorder(const Value: TRedFoxColorString);
begin
  if Value <> fColorBorder.AsString then
  begin
    fColorBorder.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamTextBox.SetColorMouseOver(const Value: TRedFoxColorString);
begin
  if Value <> fColorMouseOver.AsString then
  begin
    fColorMouseOver.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamTextBox.SetImageOverlay(const Value: TBitmap);
begin
  if fImageOverlay <> Value then
  begin
    fImageOverlay := Value;
    Invalidate;
  end;
end;

procedure TVamTextBox.SetMenuText(Value: string);
begin
  SetText(Value);
end;

procedure TVamTextBox.SetParameterName(const Value: string);
begin
  fParameterName := Value;
end;

procedure TVamTextBox.SetShowBorder(const Value: boolean);
begin
  if Value <> fShowBorder then
  begin
    fShowBorder := Value;
    Invalidate;
  end;
end;

procedure TVamTextBox.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TVamTextBox.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    Invalidate;
  end;
end;

procedure TVamTextBox.SetTextPadding(const Value: TPadding);
begin
  FTextPadding.Assign(Value);
end;

procedure TVamTextBox.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    Invalidate;
  end;
end;

procedure TVamTextBox.Paint;
var
  aColor : TRedFoxColor;
  TextBounds : TRect;
  SrcRect : TRect;
  DstRect : TRect;
  OverlayDestX, OverlayDestY : integer;
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
  TextBounds := Rect(TextPadding.Left, TextPadding.Top, Width-TextPadding.Right, Height-TextPadding.Bottom);
  BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);


  if assigned(ImageOverlay) then
  begin
    SrcRect.Left   := 0;
    SrcRect.Width  := ImageOverlay.Width;
    SrcRect.Top    := 0;
    SrcRect.Bottom := ImageOverlay.Height;

    case ImageOverlayVertAlign of
      AlignNear:   OverlayDestY := 0;
      AlignCenter: OverlayDestY := (Height - SrcRect.Height) div 2;
      AlignFar:    OverlayDestY := (Height - SrcRect.Height);
    else
      raise Exception.Create('Type not handled.');
    end;

    case ImageOverlayHorzAlign of
      AlignNear:   OverlayDestX := 0;
      AlignCenter: OverlayDestX := (Width - SrcRect.Width) div 2;
      AlignFar:    OverlayDestX := (Width - SrcRect.Width);
    else
      raise Exception.Create('Type not handled.');
    end;

    DstRect.Left   := OverlayDestX + ImageOverlayOffsetX;
    DstRect.Right  := DstRect.Left + SrcRect.Width;
    DstRect.Top    := OverlayDestY + ImageOverlayOffsetY;
    DstRect.Bottom := DstRect.Top + SrcRect.Height;

    BackBuffer.TransformImage(ImageOverlay, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
  end;

end;



end.

