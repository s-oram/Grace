unit VamXYPad;

interface

uses
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl, VamGuiControlInterfaces;

const
  kPuckPadding = 3;
  kPuckSize    = 20;

type
  TVamXYPad = class(TVamWinControl, IXYPadControl)
  private
    fOnChanged: TNotifyEvent;
    fPosX: single;
    fPosY: single;
    fIsMouseOverPuck: boolean;
    fPadX_VstParameterIndex: integer;
    fPadY_VstParameterIndex: integer;
    fColor_Background: TRedFoxColorString;
    fColor_Border: TRedFoxColorString;
    fCornerRadius: single;
    fColor_Puck: TRedFoxColorString;
    fParameterNameX: string;
    fParameterNameY: string;
    procedure SetPosX(const Value: single);
    procedure SetPosY(const Value: single);
    procedure SetIsMouseOverPuck(const Value: boolean);
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetCornerRadius(const Value: single);
  protected
    IsGrabbed : boolean;
    GrabbedOffset : TPointF;

    ReferencePoint   : TPoint;
    ReferencePos     : single;

    PuckPixelPosX, PuckPixelPosY : single;

    function PixelPosToPuckPos(const PixelX, PixelY:integer):TPointF;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    //TODO: Need mouse enter mouse leave functions added.

    procedure Changed; inline;

    property IsMouseOverPuck : boolean read fIsMouseOverPuck write SetIsMouseOverPuck;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CornerRadius : single read fCornerRadius write SetCornerRadius;

    property Color_Background : TRedFoxColorString index 0 read fColor_Background write SetColors;
    property Color_Border     : TRedFoxColorString index 1 read fColor_Border write SetColors;
    property Color_Puck       : TRedFoxColorString index 2 read fColor_Puck write SetColors;

    property PosX : single read fPosX write SetPosX;  //Range 0..1
    property PosY : single read fPosY write SetPosY;  //Range 0..1

    //Used by Gui Standard for automatic VST parameter linking.
    property PadX_VstParameterIndex : integer read fPadX_VstParameterIndex write fPadX_VstParameterIndex;
    property PadY_VstParameterIndex : integer read fPadY_VstParameterIndex write fPadY_VstParameterIndex;

    // typically used to store the linked parameter name.
    property ParameterNameX  : string  read fParameterNameX  write fParameterNameX;
    property ParameterNameY  : string  read fParameterNameY  write fParameterNameY;

    {$INCLUDE TControlProperties.inc}
    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;
  end;



implementation

uses
  SysUtils,
  AggPixelFormat, AggBasics;

{ TVamXYPad }

constructor TVamXYPad.Create(AOwner: TComponent);
begin
  inherited;

  fColor_Background := '$ff242B39';
  fColor_Border     := '$ff242B39';
  fColor_Puck       := '$ffA1BDED';
  fCornerRadius     := 3;

  PosX := 1;
  PosY := 1;

  PuckPixelPosX := 1;
  PuckPixelPosY := 1;

  IsMouseOverPuck := false;

end;

destructor TVamXYPad.Destroy;
begin
  inherited;
end;

procedure TVamXYPad.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TVamXYPad.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PuckValue : TPointF;
begin
  inherited;

  if (abs(x - PuckPixelPosX) <= (kPuckSize * 0.5)) and (abs(Y - PuckPixelPosY) <= (kPuckSize * 0.5))
      then IsMouseOverPuck := true
      else IsMouseOverPuck := false;


  if (Button = mbLeft) and ((ssCtrl in Shift) = false) and (IsMouseOverPuck = true) then
  begin
    IsGrabbed := true;

    PuckValue := PixelPosToPuckPos(X, Y);

    GrabbedOffset.X := PosX - PuckValue.X;
    GrabbedOffset.Y := PosY - PuckValue.Y;

    Changed;
  end;


  if (Button = mbLeft) and ((ssCtrl in Shift) = false) and (IsMouseOverPuck = false) then
  begin
    IsGrabbed := true;

    PuckValue := PixelPosToPuckPos(X, Y);
    if PuckValue.X < 0 then PuckValue.X := 0;
    if PuckValue.X > 1 then PuckValue.X := 1;
    if PuckValue.Y < 0 then PuckValue.Y := 0;
    if PuckValue.Y > 1 then PuckValue.Y := 1;

    self.PosX := PuckValue.X;
    self.PosY := PuckValue.Y;

    GrabbedOffset.X := 0;
    GrabbedOffset.Y := 0;

    Changed;
  end;

end;

procedure TVamXYPad.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  PuckValue : TPointF;
  pvX, pvY : single;
begin
  inherited;

  if (IsGrabbed = false) then
  begin
    if (abs(x - PuckPixelPosX) <= (kPuckSize * 0.5)) and (abs(Y - PuckPixelPosY) <= (kPuckSize * 0.5))
      then IsMouseOverPuck := true
      else IsMouseOverPuck := false;
  end;


  if (IsGrabbed = true) then
  begin
    PuckValue := PixelPosToPuckPos(X, Y);
    pvX := PuckValue.X + GrabbedOffset.X;
    pvY := PuckValue.Y + GrabbedOffset.Y;

    if pvX < 0 then pvX := 0;
    if pvX > 1 then pvX := 1;
    if pvY < 0 then pvY := 0;
    if pvY > 1 then pvY := 1;

    if (PosX <> pvX) or (PosY <> pvY) then
    begin
      PosX := pvX;
      PosY := pvY;
      Changed;
    end;
  end;

end;

procedure TVamXYPad.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PuckValue : TPointF;
  pvX, pvY : single;
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    PuckValue := PixelPosToPuckPos(X, Y);
    pvX := PuckValue.X + GrabbedOffset.X;
    pvY := PuckValue.Y + GrabbedOffset.Y;

    if pvX < 0 then pvX := 0;
    if pvX > 1 then pvX := 1;
    if pvY < 0 then pvY := 0;
    if pvY > 1 then pvY := 1;

    if (PosX <> pvX) or (PosY <> pvY) then
    begin
      PosX := pvX;
      PosY := pvY;
      Changed;
    end;

    IsGrabbed := false;
  end;

  if (abs(x - PuckPixelPosX) <= (kPuckSize * 0.5)) and (abs(Y - PuckPixelPosY) <= (kPuckSize * 0.5))
      then IsMouseOverPuck := true
      else IsMouseOverPuck := false;

end;



procedure TVamXYPad.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  xColor : PRedFoxColorString;
begin
  case Index of
    0 : xColor := @fColor_Background;
    1 : xColor := @fColor_Border;
    2 : xColor := @fColor_Puck;
  else
    raise Exception.Create('Index not handled');
  end;

  if xColor^ <> Value then
  begin
    xColor ^ := Value;
    Invalidate;
  end;

end;

procedure TVamXYPad.SetCornerRadius(const Value: single);
begin
  if Value <> fCornerRadius then
  begin
    fCornerRadius := Value;
    Invalidate;
  end;
end;

procedure TVamXYPad.SetIsMouseOverPuck(const Value: boolean);
begin
  if Value <> fIsMouseOverPuck then
  begin
    fIsMouseOverPuck := Value;
    Invalidate;
  end;
end;

procedure TVamXYPad.SetPosX(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  if fPosX <> Value then
  begin
    fPosX := Value;
    Invalidate;
  end;
end;

procedure TVamXYPad.SetPosY(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));
  if fPosY <> Value then
  begin
    fPosY := Value;
    Invalidate;
  end;
end;

procedure TVamXYPad.Paint;
var
  x1, y1, x2, y2 : single;
  xW, xH : single;
  PuckRadius : single;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(GetRedFoxColor(fColor_Background).WithAlpha(0));
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  //=== Paint the background ==
  x1 := 0.5;
  y1 := 0.5;
  x2 := Width-0.5;
  y2 := Height - 0.5;

  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.LineColor := GetRedFoxColor(fColor_Border);
  BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_Background);


  if CornerRadius > 0 then
  begin
    BackBuffer.BufferInterface.LineCap := TAggLineCap.lcRound;
    BackBuffer.BufferInterface.RoundedRect(x1, y1, x2, y2, fCornerRadius);
  end else
  begin
    BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);
  end;

  //=== Draw the puck ==
  xW := Width  - kPuckSize - (kPuckPadding * 2);
  xH := Height - kPuckSize - (kPuckPadding * 2);

  x1 := kPuckPadding + (xW * PosX) + (kPuckSize * 0.5);
  y1 := kPuckPadding + (xH * (1 - PosY)) + (kPuckSize * 0.5);
  PuckRadius := (kPuckSize * 0.5);

  if (IsMouseOverPuck) or (IsGrabbed)
    then BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_Puck)
    else BackBuffer.BufferInterface.NoFill;

  BackBuffer.BufferInterface.LineWidth := 3;
  BackBuffer.BufferInterface.LineColor := GetRedFoxColor(fColor_Puck);
  BackBuffer.BufferInterface.Circle(x1, y1, PuckRadius);

  PuckPixelPosX := x1;
  PuckPixelPosY := y1;


end;

function TVamXYPad.PixelPosToPuckPos(const PixelX, PixelY: integer): TPointF;
var
  xW, xH : single;

begin
  xW := Width  - kPuckSize - (kPuckPadding * 2);
  xH := Height - kPuckSize - (kPuckPadding * 2);


  result.X := (PixelX - (kPuckSize * 0.5) - kPuckPadding) / xW;
  result.Y := 1 - ((PixelY - (kPuckSize * 0.5) - kPuckPadding) / xH);
end;




end.

