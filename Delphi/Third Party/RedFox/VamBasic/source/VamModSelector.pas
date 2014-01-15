unit VamModSelector;

interface

uses
  Graphics, Controls,
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamModSelector = class(TVamWinControl)
  private
    IsMouseOver : boolean;
    fColorBorder : TRedFoxColor;
    fColor : TRedFoxColor;
    fColorMouseOver : TRedFoxColor;
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fImageOverlay: TBitmap;
    FTextPadding: TPadding;
    fTextB: string;
    fOnShowModSourceMenu: TNotifyEvent;
    fOnShowModViaMenu: TNotifyEvent;
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

    procedure EventHandle_TextPaddingChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


  published
    property ColorBorder    : TRedFoxColorString read GetColorBorder    write SetColorBorder;
    property Color          : TRedFoxColorString read GetColor          write SetColor;
    property ColorMouseOver : TRedFoxColorString read GetColorMouseOver write SetColorMouseOver;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;
    property TextB : string read fTextB write SetTextB;
    property TextPadding : TPadding read FTextPadding write SetTextPadding;

    property Font;

    property ImageOverlay:TBitmap read fImageOverlay write SetImageOverlay;


    property OnShowModSourceMenu : TNotifyEvent read fOnShowModSourceMenu write fOnShowModSourceMenu;
    property OnShowModViaMenu    : TNotifyEvent read fOnShowModViaMenu    write fOnShowModViaMenu;

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

  fText := 'Bang';
  fTextB := 'James';
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
var
  cr : TActiveControlRegion;
begin
  inherited;

  if (Button = mbRight) and (IsControlGrabbed) then
  begin
    IsControlGrabbed := false;

    cr := GetActiveControlRegion(x, y);

    case cr of
      ModDisplay: if assigned(OnShowModSourceMenu) then OnShowModSourceMenu(self);
      ViaDisplay: if assigned(OnShowModViaMenu)    then OnShowModViaMenu(self);
    else
      raise Exception.Create('Type not handled.');
    end;
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
var
  aColor : TRedFoxColor;
  TextBounds : TRect;
  SrcRect : TRect;
  DstRect : TRect;
  VOffset : integer;
  VHeight : integer;
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
  //TextBounds := Rect(TextPadding.Left, TextPadding.Top, Width-TextPadding.Right, Height-TextPadding.Bottom);
  //BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);

  VOffset := TextPadding.Top;
  VHeight := Height - TextPadding.Top - TextPadding.Bottom;
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

end.
