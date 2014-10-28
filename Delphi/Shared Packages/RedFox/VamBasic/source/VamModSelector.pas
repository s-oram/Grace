unit VamModSelector;

interface

uses
  Types,
  Graphics, Controls,
  Classes,
  AggColor,
  RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamModSelector = class(TVamWinControl)
  private
    IsMouseOver : boolean;
    fColorBorder : TRedFoxColor;
    fColor : TRedFoxColor;
    fColorMouseOver : TRedFoxColor;
    fColor_ModAmountOff : TRedFoxColor;
    fColor_ModAmountOn  : TRedFoxColor;
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fImageOverlay: TBitmap;
    FTextPadding: TPadding;
    fTextB: string;
    fOnShowModSourceMenu: TNotifyEvent;
    fOnShowModViaMenu: TNotifyEvent;
    fModAmountX1: single;
    fShowModAmount: boolean;
    fShowMuteIcon: boolean;
    fModAmountX2: single;
    fOnShowContextMenu: TNotifyEvent;
    fShowModulationActiveIcon: boolean;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    function GetColor: TRedFoxColorString;
    function GetColorMouseOver: TRedFoxColorString;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure SetColorMouseOver(const Value: TRedFoxColorString);
    procedure SetImageOverlay(const Value: TBitmap);
    procedure SetTextPadding(const Value: TPadding);
    function GetColorBorder: TRedFoxColorString;
    procedure SetColorBorder(const Value: TRedFoxColorString);
    procedure SetTextB(const Value: string);
    function GetColor_ModAmountOff: TRedFoxColorString;
    function GetColor_ModAmountOn: TRedFoxColorString;
    procedure SetColor_ModAmountOff(const Value: TRedFoxColorString);
    procedure SetColor_ModAmountOn(const Value: TRedFoxColorString);
    procedure SetModAmountX1(const Value: single);
    procedure SetShowModAmount(const Value: boolean);
    procedure SetShowMuteIcon(const Value: boolean);
    procedure SetModAmountX2(const Value: single);
    procedure SetShowModulationActiveIcon(const Value: boolean);

  protected type
      TActiveControlRegion = (ModDisplay, ViaDisplay);

  protected var
    IsControlGrabbed : boolean;

    function GetActiveControlRegion(const x, y : integer):TActiveControlRegion;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Paint; override;

    procedure Draw_Text(TextAreaBounds : TRect);
    procedure Draw_ModAmountSlider(SliderBounds : TRect);
    procedure Draw_MuteIcon(TextAreaBounds : TRect);
    procedure Draw_ModSlotActiveIcon(TextAreaBounds : TRect);

    procedure EventHandle_TextPaddingChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


  published
    property ColorBorder    : TRedFoxColorString read GetColorBorder    write SetColorBorder;
    property Color          : TRedFoxColorString read GetColor          write SetColor;
    property ColorMouseOver     : TRedFoxColorString read GetColorMouseOver write SetColorMouseOver;
    property Color_ModAmountOff : TRedFoxColorString read GetColor_ModAmountOff write SetColor_ModAmountOff;
    property Color_ModAmountOn  : TRedFoxColorString read GetColor_ModAmountOn  write SetColor_ModAmountOn;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;
    property TextB : string read fTextB write SetTextB;
    property TextPadding : TPadding read FTextPadding write SetTextPadding;

    property Font;

    property ImageOverlay:TBitmap read fImageOverlay write SetImageOverlay;


    property ShowModAmount : boolean read fShowModAmount write SetShowModAmount;
    property ModAmountX1 : single read fModAmountX1 write SetModAmountX1;
    property ModAmountX2 : single read fModAmountX2 write SetModAmountX2;
    property ShowMuteIcon : boolean read fShowMuteIcon write SetShowMuteIcon;
    property ShowModulationActiveIcon : boolean read fShowModulationActiveIcon write SetShowModulationActiveIcon;

    property OnShowContextMenu   : TNotifyEvent read fOnShowContextMenu   write fOnShowContextMenu;
    property OnShowModSourceMenu : TNotifyEvent read fOnShowModSourceMenu write fOnShowModSourceMenu;
    property OnShowModViaMenu    : TNotifyEvent read fOnShowModViaMenu    write fOnShowModViaMenu;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils, AggPixelFormat;

{ TVamTextBox }

constructor TVamModSelector.Create(AOwner: TComponent);
begin
  inherited;

  fColor.SetColor('$FF3E3E3E');
  fColorMouseOver.SetColor('$FF3E3E3E');

  fColor_ModAmountOff := '$FF666666';
  fColor_ModAmountOn  := '$FFcccccc';

  Font.Color := clWhite;

  FTextPadding := TPadding.Create(Self);
  FTextPadding.SetBounds(0,0,0,0);
  fTextPadding.OnChange := EventHandle_TextPaddingChange;

  fText := 'Bang';
  fTextB := 'James';

  fShowModAmount := true;
  fModAmountX1 := 0.3;
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

function TVamModSelector.GetActiveControlRegion(const x, y: integer): TActiveControlRegion;
begin
  if Y < Height div 2
    then result := TActiveControlRegion.ModDisplay
    else result := TActiveControlRegion.ViaDisplay;
end;

function TVamModSelector.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

function TVamModSelector.GetColorBorder: TRedFoxColorString;
begin
  result := fColorBorder.AsString;
end;

function TVamModSelector.GetColorMouseOver: TRedFoxColorString;
begin
  result := fColorMouseOver.AsString;
end;

function TVamModSelector.GetColor_ModAmountOff: TRedFoxColorString;
begin
  result := fColor_ModAmountOff;
end;

function TVamModSelector.GetColor_ModAmountOn: TRedFoxColorString;
begin
  result := fColor_ModAmountOn;
end;

procedure TVamModSelector.MouseEnter;
begin
  inherited;

  IsMouseOver := true;
  Invalidate;
end;

procedure TVamModSelector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbRight) then
  begin
    IsControlGrabbed := true;
  end;

end;

procedure TVamModSelector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbRight) and (IsControlGrabbed) then
  begin
    IsControlGrabbed := false;
    if assigned(OnShowContextMenu) then OnShowContextMenu(self);
  end;
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

procedure TVamModSelector.SetColorBorder(const Value: TRedFoxColorString);
begin
  if Value <> fColorBorder.AsString then
  begin
    fColorBorder.SetColor(Value);
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

procedure TVamModSelector.SetColor_ModAmountOff(const Value: TRedFoxColorString);
begin
  if fColor_ModAmountOff <> Value then
  begin
    fColor_ModAmountOff := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.SetColor_ModAmountOn(const Value: TRedFoxColorString);
begin
  if fColor_ModAmountOn <> Value then
  begin
    fColor_ModAmountOn := Value;
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

procedure TVamModSelector.SetModAmountX1(const Value: single);
begin
  if Value <> fModAmountX1 then
  begin
    fModAmountX1 := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.SetModAmountX2(const Value: single);
begin
  if fModAmountX2 <> Value then
  begin
    fModAmountX2 := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.SetShowModAmount(const Value: boolean);
begin
  if Value <> fShowModAmount then
  begin
    fShowModAmount := Value;
    Invalidate;
  end;
end;

procedure TVamModSelector.SetShowModulationActiveIcon(const Value: boolean);
begin
  if fShowModulationActiveIcon <> Value then
  begin
    fShowModulationActiveIcon := Value;
  end;
end;

procedure TVamModSelector.SetShowMuteIcon(const Value: boolean);
begin
  if Value <> fShowMuteIcon then
  begin
    fShowMuteIcon := Value;
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

procedure TVamModSelector.SetTextB(const Value: string);
begin
  if Value <> fTextB then
  begin
    fTextB := Value;
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
const
  kModSliderHeight = 4;
  kModSliderMargin = 4;
var
  aColor : TRedFoxColor;
  TextBounds : TRect;
  ElementBounds : TRect;
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
  TextBounds.Top    := TextPadding.Top;
  TextBounds.Bottom := Height - TextPadding.Bottom - kModSliderHeight - kModSliderMargin * 2;
  TextBounds.Left   := TextPadding.Left;
  TextBounds.Right  := Width - TextPadding.Right;
  Draw_Text(TextBounds);

  if ShowModulationActiveIcon
    then Draw_ModSlotActiveIcon(TextBounds);

  if ShowMuteIcon
    then Draw_MuteIcon(TextBounds);


  //== draw the mod amount ==
  if (ShowModAmount) then
  begin
    ElementBounds.Top := Height - kModSliderHeight - kModSliderMargin * 2;
    ElementBounds.Bottom := Height - kModSliderMargin;
    ElementBounds.Left   := kModSliderMargin;
    ElementBounds.Right  := Width - kModSliderMargin;
    Draw_ModAmountSlider(ElementBounds);
  end;

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


  //== draw the border ==
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineColor := fColorBorder;
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.RoundedRect(0, 0, Width, Height, 3);

end;

procedure TVamModSelector.Draw_Text(TextAreaBounds : TRect);
var
  TextBounds : TRect;
  VOffset : integer;
  VHeight : integer;
begin
  VOffset := TextAreaBounds.Top;
  VHeight := TextAreaBounds.Bottom - TextAreaBounds.Top;
  TextBounds.Left   := 0;
  TextBounds.Right  := Width;

  TextBounds.Top    := round(VHeight * 0/3) + VOffset;
  TextBounds.Bottom := round(VHeight * 1/3) + VOffset;
  BackBuffer.DrawText(Text, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextBounds);

  TextBounds.Top    := round(VHeight * 1/3) + VOffset;
  TextBounds.Bottom := round(VHeight * 2/3) + VOffset;
  BackBuffer.DrawText('via', Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextBounds);

  TextBounds.Top    := round(VHeight * 2/3) + VOffset;
  TextBounds.Bottom := round(VHeight * 3/3) + VOffset;
  BackBuffer.DrawText(TextB, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextBounds);
end;

procedure TVamModSelector.Draw_ModAmountSlider(SliderBounds: TRect);
var
  x1, x2 : single;
begin
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := fColor_ModAmountOff;
  BackBuffer.BufferInterface.RoundedRect(SliderBounds.Left, SliderBounds.Top, SliderBounds.Right, SliderBounds.Bottom, 1.5);


  if ModAmountX1 < ModAmountX2 then
  begin
    BackBuffer.BufferInterface.FillColor := fColor_ModAmountOn;
    x1 := SliderBounds.Left + (SliderBounds.Width * ModAmountX1);
    x2 := SliderBounds.Left + (SliderBounds.Width * ModAmountX2);
    BackBuffer.BufferInterface.RoundedRect(x1, SliderBounds.Top, x2, SliderBounds.Bottom, 1.5);
  end;

  if ModAmountX1 > ModAmountX2 then
  begin
    BackBuffer.BufferInterface.FillColor := fColor_ModAmountOn;
    x1 := SliderBounds.Left + (SliderBounds.Width * ModAmountX2);
    x2 := SliderBounds.Left + (SliderBounds.Width * ModAmountX1);
    BackBuffer.BufferInterface.RoundedRect(x1, SliderBounds.Top, x2, SliderBounds.Bottom, 1.5);
  end;

  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineColor := fColor_ModAmountOff;
  BackBuffer.BufferInterface.RoundedRect(SliderBounds.Left-0.5, SliderBounds.Top-0.5, SliderBounds.Right+0.5, SliderBounds.Bottom+0.5, 1.5);
end;

procedure TVamModSelector.Draw_ModSlotActiveIcon(TextAreaBounds: TRect);
var
  ptx, pty : single;
begin
  pty := TextAreaBounds.Top + TextAreaBounds.Height * 0.5 + 1;
  ptx := TextAreaBounds.Left + TextAreaBounds.Width * 1/6;

  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := GetAggColor(clGreen);
  BackBuffer.BufferInterface.Circle(ptx, pty, 3);
end;

procedure TVamModSelector.Draw_MuteIcon(TextAreaBounds: TRect);
const
  //TODO:MED HACK: This color is hard coded because it doesn't corrospond to
  // any of the existing color properties.
  kTextColor = '$ff242B39';
var
  ptx, pty : single;
  ElementBounds : TRect;
begin
  pty := TextAreaBounds.Top + TextAreaBounds.Height * 0.5;
  ptx := TextAreaBounds.Left + TextAreaBounds.Width * 1/5;

  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := GetAggColor(clRed);
  BackBuffer.BufferInterface.Circle(ptx, pty, 7);

  ElementBounds.Left   := round(ptx - 8);
  Elementbounds.Right  := round(ptx + 9);
  ElementBounds.Top    := round(pty - 8);
  ElementBounds.Bottom := round(pty + 9);

  BackBuffer.DrawText('M', Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, ElementBounds, kTextColor);
end;

end.
