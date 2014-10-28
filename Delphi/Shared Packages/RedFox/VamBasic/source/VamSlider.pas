unit VamSlider;

interface

uses
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TVamActiveDirection = (adUnknown, adVert, adHorz);
  TVamSliderType = (stVert, stHorz);

  TVamSlider = class(TVamWinControl)
  private
    fOnChanged: TNotifyEvent;
    fPos: single;
    fImageStrip: TBitmap;
    fImageStripGlyphCount: integer;
    fVisibleSteps: integer;
    fEnabled: boolean;
    fDisabledImage: TBitmap;
    fSliderType: TVamSliderType;
    procedure SetPos(Value: single);
    procedure SetImageStripGlyphCount(const Value: integer);
    procedure SetImageStrip(const Value: TBitmap);
    procedure SetVisibleSteps(const Value: integer);
    procedure SetDisabledImage(const Value: TBitmap);
    procedure SetSliderType(const Value: TVamSliderType);
    function GetColor(const Index: Integer): TRedFoxColorString;
    procedure SetColor(const Index: Integer; const Value: TRedFoxColorString);
  protected
    IsGrabbed : boolean;
    MoveReferencePoint : TPoint;
    ReferencePoint   : TPoint;
    ReferencePos     : single;
    IsFineAdjustment : boolean;

    fColorBackground, fColorSlider : TRedFoxColor;

    ActiveDirection : TVamActiveDirection;


    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed;
    procedure Paint; override;

    procedure DrawKnob_VectorStyle;
    procedure DrawKnob_BitmapStyle;

    property SliderType : TVamSliderType read fSliderType write SetSliderType;

    procedure SetEnabled(Value: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property ImageStrip:TBitmap read fImageStrip write SetImageStrip;
    property ImageStripGlyphCount : integer read fImageStripGlyphCount write SetImageStripGlyphCount;

    property DisabledImage : TBitmap read fDisabledImage write SetDisabledImage;

  published

    property ColorBackground : TRedFoxColorString index 0 read GetColor write SetColor;
    property ColorSlider     : TRedFoxColorString index 1 read GetColor write SetColor;

    property Enabled   : boolean read fEnabled   write SetEnabled;
    property VisibleSteps : integer read fVisibleSteps write SetVisibleSteps;

    property Pos : single read fPos write SetPos;
    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils,
  Math,
  AggPixelFormat;


// TranslateAngleRadiusToXY is used by VstKnob to draw the Pointer line.
procedure TranslateAngleRadiusToXY(const MiddleX,MiddleY, Degrees, Radius:single; out X, Y:single);
var
  rad:single;
begin
  rad := DegToRad(Degrees);
  X := Radius / (sin(0.5 * pi)) * sin(rad);

  X := MiddleX - X;

  rad := DegToRad(90 - Degrees);
  Y := Radius / (sin(0.5 * pi)) * sin(rad);

  Y := MiddleY + Y;
end;

{ TVamSlider }

constructor TVamSlider.Create(AOwner: TComponent);
begin
  inherited;
  fVisibleSteps := 0;
  fEnabled := true;

  fColorBackground := '$00000000';
  fColorSlider     := '$FF777777';
end;

destructor TVamSlider.Destroy;
begin

  inherited;
end;

procedure TVamSlider.SetColor(const Index: Integer; const Value: TRedFoxColorString);
var
  pColor : PRedFoxColor;
begin
  case Index of
    0: pColor := @fColorBackground;
    1: pColor := @fColorSlider;
  else
    pColor := nil
  end;

  if (pColor^ <> Value) then
  begin
    pColor^ := Value;
    Invalidate;
  end;

end;

function TVamSlider.GetColor(const Index: Integer): TRedFoxColorString;
begin
  case Index of
    0: result := fColorBackground;
    1: result := fColorSlider;
  end;
end;






procedure TVamSlider.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TVamSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and ((ssCtrl in Shift) = false) then
  begin
    IsGrabbed := true;
    MoveReferencePoint := Point(X, Y);
    ReferencePoint     := Point(X, Y);
    ReferencePos       := fPos;

    if (ssShift in Shift)
      then IsFineAdjustment := true
      else IsFineAdjustment := false;

    ActiveDirection := adUnknown;

  end;

end;

procedure TVamSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  DistX, DistY : single;
  Dist : single;
  NewPos : single;
  ScaleFactor : single;
  CurrentAdjustmentState : boolean;
begin
  inherited;

  if (IsGrabbed = true) then
  begin
    if (ssShift in Shift)
      then CurrentAdjustmentState := true
      else CurrentAdjustmentState := false;

    if IsFineAdjustment <> CurrentAdjustmentState then
    begin
      IsFineAdjustment := CurrentAdjustmentState;
      //NOTE: Reset the reference point when changing from/to 'Fine Adjustment' mode.
      ReferencePoint := Point(X, Y);
      ReferencePos   := fPos;
    end;


    if IsFineAdjustment = false
      then ScaleFactor := 0.005
      else ScaleFactor := 0.00175;


    DistX := (MoveReferencePoint.X - X);
    DistY := (Y - MoveReferencePoint.Y);


    if abs(DistX) > abs(DistY) then
    begin
      if ActiveDirection <> adHorz then
      begin
        //DistX := 0;
        //DistY := 0;
        ActiveDirection := adHorz;
        ReferencePoint := Point(X, Y);
        ReferencePos   := fPos;
      end;

      DistX := (ReferencePoint.X - X) * ScaleFactor;
      //DistY := (Y - ReferencePoint.Y) * ScaleFactor;
      Dist := DistX;
    end else
    begin
      if ActiveDirection <> adVert then
      begin
        //DistX := 0;
        //DistY := 0;
        ActiveDirection := adVert;
        ReferencePoint := Point(X, Y);
        ReferencePos   := fPos;
      end;

      //DistX := (ReferencePoint.X - X) * ScaleFactor;
      DistY := (Y - ReferencePoint.Y) * ScaleFactor;
      Dist := DistY;
    end;

    NewPos := ReferencePos - Dist;

    if NewPos > 1 then
    begin
      NewPos := 1;

      // NOTE: Reset the reference point whenever the knob position limit is exceeded.
      // This prevents overshoot when the user reverses mouse direction at the knob
      // position limits.
      ReferencePoint := Point(X, Y);
      ReferencePos   := fPos;
    end;

    if NewPos < 0 then
    begin
      NewPos := 0;

      // NOTE: Reset the reference point.
      ReferencePoint := Point(X, Y);
      ReferencePos   := fPos;
    end;

    if fPos <> NewPos then
    begin
      fPos := NewPos;
      Invalidate;
      Changed;
    end;
  end;

  MoveReferencePoint := Point(X, Y);
end;

procedure TVamSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    IsGrabbed := false;
  end;

end;


procedure TVamSlider.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth > 0) and (AHeight > 0) then
  begin
    if (AHeight >= AWidth)
      then SliderType := stVert
      else SliderType := stHorz;
  end;
end;

procedure TVamSlider.SetDisabledImage(const Value: TBitmap);
begin
  if Value <> fDisabledImage then
  begin
    fDisabledImage := Value;
    Invalidate;
  end;
end;

procedure TVamSlider.SetImageStrip(const Value: TBitmap);
begin
  if Value <> fImageStrip then
  begin
    fImageStrip := Value;
    Invalidate;
  end;
end;

procedure TVamSlider.SetImageStripGlyphCount(const Value: integer);
begin
  if Value <> fImageStripGlyphCount then
  begin
    fImageStripGlyphCount := Value;
    Invalidate;
  end;
end;

procedure TVamSlider.SetEnabled(Value: boolean);
begin
  if Value <> fEnabled then
  begin
    fEnabled := Value;
    Invalidate;
  end;
end;

procedure TVamSlider.SetPos(Value: single);
begin
  if Value < 0 then Value := 0
  else if Value > 1 then Value := 1;

  if Value <> fPos then
  begin
    fPos := Value;
    Invalidate;
  end;
end;

procedure TVamSlider.SetSliderType(const Value: TVamSliderType);
begin
  if Value <> fSliderType then
  begin
    fSliderType := Value;
    Invalidate;
  end;
end;

procedure TVamSlider.SetVisibleSteps(const Value: integer);
begin
  // NOTE: if VisibleSteps is 0, steping is disabled.
  if Value <> fVisibleSteps then
  begin
    fVisibleSteps := Value;
    Invalidate;
  end;
end;

procedure TVamSlider.Paint;
var
  SrcRect : TRect;
  DstRect : TRect;
begin
  inherited;

  if (Enabled) or (not assigned(DisabledImage))then
  begin
    if assigned(fImageStrip)
      then DrawKnob_BitmapStyle
      else DrawKnob_VectorStyle;
  end else
  begin
    BackBuffer.BufferInterface.ClearAll(0,0,0,0);
    BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

    SrcRect.Left   := 0;
    SrcRect.Width  := DisabledImage.Width;
    SrcRect.Top    := 0;
    SrcRect.Bottom := DisabledImage.Height;

    DstRect.Left   := (Width - SrcRect.Width)   div 2;
    DstRect.Right  := DstRect.Left + SrcRect.Width;
    DstRect.Top    := (Height - SrcRect.Height) div 2;
    DstRect.Bottom := DstRect.Top + SrcRect.Height;

    BackBuffer.TransformImage(DisabledImage, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
  end;

end;

procedure TVamSlider.DrawKnob_BitmapStyle;
var
  xPos : single;
  SrcRect : TRect;
  DstRect : TRect;
  BitmapIndex : integer;
begin
  if ImageStripGlyphCount <= 0  then exit;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);

  if VisibleSteps > 0
    then xPos := round(Pos * VisibleSteps) / VisibleSteps
    else xPos := Pos;

  BitmapIndex := floor((ImageStripGlyphCount-1) * xPos);

  SrcRect.Left   := 0;
  SrcRect.Width  := ImageStrip.Width;
  SrcRect.Top    := round(BitmapIndex / ImageStripGlyphCount * ImageStrip.Height);
  SrcRect.Bottom := round((BitmapIndex+1) / ImageStripGlyphCount * ImageStrip.Height)-1;

  DstRect.Left := (Width - SrcRect.Width)   div 2;
  DstRect.Right := DstRect.Left + SrcRect.Width;
  DstRect.Top  := (Height - SrcRect.Height) div 2;
  DstRect.Bottom := DstRect.Top + SrcRect.Height;

  BackBuffer.TransformImage(ImageStrip, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
end;

procedure TVamSlider.DrawKnob_VectorStyle;
var
  //MiddleX, MiddleY:single;
  //EndX, EndY   : single;
  //EndX2, EndY2 : single;
  //Angle : single;
  //Radius1:single;
  //Radius2:single;
  xPos : single;
  r : TRectF;
  vx1, vx2 : integer;
  x1, x2, y1, y2     : integer;
begin
  if VisibleSteps > 0
    then xPos := round(Pos * VisibleSteps) / VisibleSteps
    else xPos := Pos;

  assert(InRange(xPos, 0,1));

  BackBuffer.BufferInterface.ClearAll(fColorBackground);

  BackBuffer.BufferInterface.LineColor := fColorSlider;
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineWidth := 1;

  r := RectF(0.5, 0.5, Width-0.5, Height-0.5);

  BackBuffer.BufferInterface.Rectangle(r.Left, r.Top, r.Right, r.Bottom);



  BackBuffer.BufferInterface.FillColor := fColorSlider;
  BackBuffer.BufferInterface.NoLine;

  case SliderType of
    stVert:
    begin

    end;

    stHorz:
    begin
      vx1 := round(Width div 2);
      vx2 := round((Width - 4) * xPos + 2);

      if vx1 < vx2 then
      begin
        x1 := vx1;
        x2 := vx2;
      end else
      begin
        x1 := vx2;
        x2 := vx1;
      end;

      y1 := 2;
      y2 := Height-2;

      BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);
    end;
  else
    raise Exception.Create('Type not handled.');
  end;



  {
  if Width < Height then
  begin
    Radius1 := Width / 2 - 3;
    //Radius2 := Width / 2 - 6;
  end else
  begin
    Radius1 := Height / 2 - 3;
    //Radius2 := Height / 2 - 6;
  end;

  MiddleX := Width / 2;
  MiddleY := Height / 2;

  //Set some appearence settings.
  BackBuffer.BufferInterface.LineWidth := 2;
  BackBuffer.BufferInterface.NoFill;

  //Draw the knob base.
  BackBuffer.BufferInterface.LineWidth := 2;
  BackBuffer.BufferInterface.LineColor := GetAggColor(clBlack); // <-- set color to black.
  BackBuffer.BufferInterface.Ellipse(MiddleX, MiddleY, Radius1, Radius1);

  //Draw the knob value indicator arc.
  BackBuffer.BufferInterface.LineWidth := 4;
  BackBuffer.BufferInterface.LineColor := GetAggColor(clRed);   // <-- set color to red.

  s1 := (fpos * 300) + 120;
  s2 := s1 - (fpos * 300) ;
  s1 := s1 / 360 * 2 * pi;
  s2 := s2 / 360 * 2 * pi;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY,Radius1, Radius1, s1, s2);



  BackBuffer.BufferInterface.LineColor := GetAggColor(clBlack); // <-- set color to black.

  //Draw the minimum indicator
  Angle := 30;
  Radius2 := Width / 2 - 5;

  TranslateAngleRadiusToXY(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  TranslateAngleRadiusToXY(MiddleX, MiddleY, Angle, Radius2, EndX2, EndY2);
  BackBuffer.BufferInterface.Line(EndX,EndY, EndX2, EndY2);

  //Draw the maximum indicator
  Angle := 180;
  TranslateAngleRadiusToXY(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  TranslateAngleRadiusToXY(MiddleX, MiddleY, Angle, Radius2, EndX2, EndY2);
  BackBuffer.BufferInterface.Line(EndX,EndY, EndX2, EndY2);

  //Draw the maximum indicator
  Angle := 330;
  TranslateAngleRadiusToXY(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  TranslateAngleRadiusToXY(MiddleX, MiddleY, Angle, Radius2, EndX2, EndY2);
  BackBuffer.BufferInterface.Line(EndX,EndY, EndX2, EndY2);


  //Draw the knob indicator line.
  BackBuffer.BufferInterface.LineWidth := 2;
  Angle := 30 + (300 * xPos);
  TranslateAngleRadiusToXY(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  BackBuffer.BufferInterface.Line(MiddleX,MiddleY, EndX, EndY);
  }
end;



end.
