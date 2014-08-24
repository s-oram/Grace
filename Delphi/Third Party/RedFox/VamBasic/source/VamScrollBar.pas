unit VamScrollBar;

interface

uses
  Types, Controls, Classes, RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  {$SCOPEDENUMS ON}
  TVamScrollBarStyle = (SquareCorners, RoundCorners, RoundCornersLeft, RoundCornersBottom);

  TVamSliderType = (stVertical, stHorizontal);

  TVamScrollBar = class(TVamWinControl)
  private
    fIndexPos: single;
    fIndexSize: single;
    fSliderType: TVamSliderType;
    fOnChanged: TNotifyEvent;
    fColor_Foreground: TRedFoxColor;
    fColor_Background: TRedFoxColor;
    fColor_Border: TRedFoxColor;
    procedure SetIndexPos(Value: single);
    procedure SetIndexSize(Value: single);
    procedure SetSliderType(const Value: TVamSliderType);
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    function GetColor(const Index: Integer): TRedFoxColorString;
    function GetCornerRadius(Index: integer): double;
    procedure SetCornerRadius(Index: integer; const Value: double);
  protected
    IndexRect: TSingleRect;
    IsGrabbed : boolean;
    GrabbedMouseOffset : single;
    GrabbedPixelOffset : single;
    fCornerRadius : array[0..3] of single;
    UseRoundCorners : boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PixelToSliderPos(Pixel : single): single;
    function SliderPosToPixel(SliderPos : single) : single;

    property CornerRadius[Index : integer]: double read GetCornerRadius write SetCornerRadius;
  published
    property IndexPos : single read fIndexPos write SetIndexPos; //range 0..1
    property IndexSize : single read fIndexSize write SetIndexSize; //range 0..1

    property Color_Border     : TRedFoxColorString index 0 read GetColor write SetColors;
    property Color_Background : TRedFoxColorString index 1 read GetColor write SetColors;
    property Color_Foreground : TRedFoxColorString index 2 read GetColor write SetColors;

    property SliderType : TVamSliderType read fSliderType write SetSliderType;

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils, Graphics, AggPixelFormat;

{ TRedFoxVectorScrollBar }

constructor TVamScrollBar.Create(AOwner: TComponent);
begin
  inherited;

  fColor_Border     := '$FF000000';
  fColor_Background := '$FF888888';
  fColor_Foreground := '$FFcccccc';

  IndexRect.x1 := 0;
  IndexRect.x2 := 0;
  IndexRect.y1 := 0;
  IndexRect.y2 := 0;

  fIndexSize := 0.25;

  IndexSize := 0.25;
  IndexPos  := 0;

  fCornerRadius[0] := 0;
  fCornerRadius[1] := 0;
  fCornerRadius[2] := 0;
  fCornerRadius[3] := 0;
  UseRoundCorners := false;

end;

destructor TVamScrollBar.Destroy;
begin

  inherited;
end;

procedure TVamScrollBar.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TVamScrollBar.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColor;
begin
  case Index of
    0 : pc := @fColor_Border;
    1 : pc := @fColor_Background;
    2 : pc := @fColor_Foreground;
  else
    raise Exception.Create('Index not handled.');
  end;

  if pc^ <> Value then
  begin
    pc^ := Value;
    Invalidate;
  end;
end;

procedure TVamScrollBar.SetCornerRadius(Index: integer; const Value: double);
var
  c1: Integer;
begin
  assert(Value >= 0);
  fCornerRadius[Index] := Value;

  UseRoundCorners := false;
  for c1 := 0 to 3 do
  begin
    if fCornerRadius[c1] > 0 then
    begin
      UseRoundCorners := true;
      break;
    end;
  end;

  Invalidate;
end;

function TVamScrollBar.GetColor(const Index: Integer): TRedFoxColorString;
begin
  case Index of
    0 : result := fColor_Border.AsString;
    1 : result := fColor_Background.AsString;
    2 : result := fColor_Foreground.AsString;
  else
    raise Exception.Create('Index not handled.');
  end;
end;





function TVamScrollBar.GetCornerRadius(Index: integer): double;
begin
  result := fCornerRadius[Index];
end;

procedure TVamScrollBar.SetIndexPos(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;

  if Value <> fIndexPos then
  begin
    fIndexPos := Value;
    Invalidate;
  end;
end;

procedure TVamScrollBar.SetIndexSize(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;

  if Value <> fIndexSize then
  begin
    fIndexSize := Value;
    Invalidate;
  end;
end;

procedure TVamScrollBar.SetSliderType(const Value: TVamSliderType);
begin
  if Value <> fSliderType then
  begin
    fSliderType := Value;
    Invalidate;
  end;
end;

procedure TVamScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewPos : single;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = TMouseButton.mbLeft) and (InRect(x,y, IndexRect)) then
  begin
    // Slider grabbed inside slider handle rect.
    IsGrabbed := true;

    case fSliderType of
      TVamSliderType.stVertical:   GrabbedPixelOffset := Y - SliderPosToPixel(IndexPos);
      TVamSliderType.stHorizontal: GrabbedPixelOffset := X - SliderPosToPixel(IndexPos);
    else
      raise Exception.Create('Unexpected SliderType value.');
    end;

  end else
  begin
    // Slider grabbed *outside* slider handle rect.
    IsGrabbed := true;

    case SliderType of
      TVamSliderType.stVertical:
      begin
        NewPos := PixelToSliderPos(Y);
        IndexPos := NewPos;
      end;

      TVamSliderType.stHorizontal:
      begin
        NewPos := PixelToSliderPos(x);
        IndexPos := NewPos;
      end;
    end;
  end;

  Invalidate;
end;

procedure TVamScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPos : single;
begin
  inherited;

  if IsGrabbed then
  begin
    case SliderType of
      TVamSliderType.stVertical:   NewPos := PixelToSliderPos(Y - GrabbedPixelOffset);
      TVamSliderType.stHorizontal: NewPos := PixelToSliderPos(X - GrabbedPixelOffset);
    else
      raise Exception.Create('Unknown slider type.');
    end;

    IndexPos := NewPos;
    Changed;
    Invalidate;
  end;
end;

procedure TVamScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    IsGrabbed := false;
    Invalidate;
  end;

end;

function TVamScrollBar.PixelToSliderPos(Pixel: single): single;
var
  Low, High, Span : single;
  SliderPos : single;
begin
  case SliderType of
    TVamSliderType.stVertical:
    begin
      Low  := 2 + IndexRect.Height * 0.5;
      High := Height - (2 + IndexRect.Height * 0.5);
      Span := High - Low;

      SliderPos := (Pixel - Low) / Span;
      if SliderPos > 1 then SliderPos := 1;
      SliderPos := 1 - SliderPos;
    end;

    TVamSliderType.stHorizontal:
    begin
      Low  := 2 + IndexRect.Width * 0.5;
      High := Width - (2 + IndexRect.Width * 0.5);
      Span := High - Low;
      SliderPos := (Pixel - Low) / Span;
      if SliderPos > 1 then SliderPos := 1;
    end;
  else
    //SliderPos := 0;
    raise Exception.Create('Unexpected SliderType value.');
  end;

  result := SliderPos;
end;

function TVamScrollBar.SliderPosToPixel(SliderPos: single): single;
var
  Low, High, Span : single;
  PixelPos : single;
begin
  if SliderPos > 1 then SliderPos := 1;
  if SliderPos < 0 then SliderPos := 0;

  case SliderType of
    TVamSliderType.stVertical:
    begin
      Low  := 2 + IndexRect.Height * 0.5;
      High := Height - (2 + IndexRect.Height * 0.5);
      Span := High - Low;
      PixelPos := (1 - SliderPos) * Span + Low;
    end;

    TVamSliderType.stHorizontal:
    begin
      Low  := 2 + IndexRect.Width * 0.5;
      High := Width - (2 + IndexRect.Width * 0.5);
      Span := High - Low;
      PixelPos := SliderPos * Span + Low;
    end;
  else
    //PixelPos := 0;
    raise Exception.Create('Unexpected SliderType value.');
  end;

  result := PixelPos;
end;



procedure TVamScrollBar.Paint;
var
  x1,y1,x2,y2 : single;
  IndexPixels : integer;
  IndexPixelPos : integer;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;


  //==== Draw the background ====
  BackBuffer.BufferInterface.LineWidth := 1;
  //BackBuffer.BufferInterface.LineColor := GetRedFoxColor(fColor_Border);
  //BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_Background);
  BackBuffer.BufferInterface.LineColor := fColor_Border;
  BackBuffer.BufferInterface.FillColor := fColor_Background;

  if UseRoundCorners
    then BackBuffer.BufferInterface.RoundedRectEx(0.5, 0.5, Width-0.5, Height-0.5, fCornerRadius[0],fCornerRadius[1],fCornerRadius[2],fCornerRadius[3])
    else BackBuffer.BufferInterface.Rectangle(0.5, 0.5, Width-0.5, Height-0.5);

  //===== Draw the handle ====
  case SliderType of
    TVamSliderType.stVertical:
    begin
      IndexPixels := Round(IndexSize * (Height - 2));
      IndexPixelPos := Round(((Height - 2 - IndexPixels) * (1-IndexPos)));
      x1 := 1;
      x2 := Width - 1;
      y1 := 1 + IndexPixelPos;
      y2 := y1 + IndexPixels;

      IndexRect.x1 := x1;
      IndexRect.x2 := x2;
      IndexRect.y1 := y1;
      IndexRect.y2 := y2;
    end;

    TVamSliderType.stHorizontal:
    begin
      IndexPixels := Round(IndexSize * (Width - 2));
      IndexPixelPos := Round(((Width - 2 - IndexPixels) * IndexPos));
      x1 := 1 + IndexPixelPos;
      x2 := x1 + IndexPixels;
      y1 := 1;
      y2 := Height - 1;

      IndexRect.x1 := x1;
      IndexRect.x2 := x2;
      IndexRect.y1 := y1;
      IndexRect.y2 := y2;
    end;
  end;

  BackBuffer.BufferInterface.FillColor := fColor_Foreground;

  if UseRoundCorners
    then BackBuffer.BufferInterface.RoundedRectEx(IndexRect.x1-0.5, IndexRect.y1-0.5, IndexRect.x2+0.5, IndexRect.y2+0.5, fCornerRadius[0],fCornerRadius[1],fCornerRadius[2],fCornerRadius[3])
    else BackBuffer.BufferInterface.Rectangle(IndexRect.x1-0.5, IndexRect.y1-0.5, IndexRect.x2+0.5, IndexRect.y2+0.5);

end;




end.
