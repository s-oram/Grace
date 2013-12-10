unit VamKnob;

interface

uses
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TVamKnob = class(TVamWinControl)
  private
    fOnChanged: TNotifyEvent;
    fPos: single;
    fImageStrip: TBitmap;
    fImageStripGlyphCount: integer;
    fVisibleSteps: integer;
    fIsKnobEnabled: boolean;
    fDisabledImage: TBitmap;
    procedure SetPos(Value: single);
    procedure SetImageStripGlyphCount(const Value: integer);
    procedure SetImageStrip(const Value: TBitmap);
    procedure SetVisibleSteps(const Value: integer);
    procedure SetIsKnobEnabled(const Value: boolean);
    procedure SetDisabledImage(const Value: TBitmap);
  protected
    IsGrabbed : boolean;
    ReferencePoint   : TPoint;
    ReferencePos     : single;
    IsFineAdjustment : boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed;
    procedure Paint; override;

    procedure DrawKnob_VectorStyle;
    procedure DrawKnob_BitmapStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ImageStrip:TBitmap read fImageStrip write SetImageStrip;
    property ImageStripGlyphCount : integer read fImageStripGlyphCount write SetImageStripGlyphCount;

    property DisabledImage : TBitmap read fDisabledImage write SetDisabledImage;

  published
    property IsKnobEnabled   : boolean read fIsKnobEnabled   write SetIsKnobEnabled;
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

{ TVamKnob }

constructor TVamKnob.Create(AOwner: TComponent);
begin
  inherited;
  fVisibleSteps := 0;
  fIsKnobEnabled := true;
end;

destructor TVamKnob.Destroy;
begin

  inherited;
end;



procedure TVamKnob.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TVamKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (IsKnobEnabled) and (Button = mbLeft) and ((ssCtrl in Shift) = false) then
  begin
    IsGrabbed := true;
    ReferencePoint := Point(X, Y);
    ReferencePos   := fPos;

    if (ssShift in Shift)
      then IsFineAdjustment := true
      else IsFineAdjustment := false;
  end;

end;

procedure TVamKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
var
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

    Dist := (Y - ReferencePoint.Y) * ScaleFactor;

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
end;

procedure TVamKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    IsGrabbed := false;
  end;

end;


procedure TVamKnob.SetDisabledImage(const Value: TBitmap);
begin
  if Value <> fDisabledImage then
  begin
    fDisabledImage := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.SetImageStrip(const Value: TBitmap);
begin
  if Value <> fImageStrip then
  begin
    fImageStrip := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.SetImageStripGlyphCount(const Value: integer);
begin
  if Value <> fImageStripGlyphCount then
  begin
    fImageStripGlyphCount := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.SetIsKnobEnabled(const Value: boolean);
begin
  if Value <> fIsKnobEnabled then
  begin
    fIsKnobEnabled := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.SetPos(Value: single);
begin
  if Value < 0 then Value := 0
  else if Value > 1 then Value := 1;

  if Value <> fPos then
  begin
    fPos := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.SetVisibleSteps(const Value: integer);
begin
  // NOTE: if VisibleSteps is 0, steping is disabled.
  if Value <> fVisibleSteps then
  begin
    fVisibleSteps := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.Paint;
var
  SrcRect : TRect;
  DstRect : TRect;
begin
  inherited;

  if (IsKnobEnabled) or (not assigned(DisabledImage))then
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

procedure TVamKnob.DrawKnob_BitmapStyle;
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

procedure TVamKnob.DrawKnob_VectorStyle;
var
  MiddleX, MiddleY:single;
  EndX, EndY   : single;
  EndX2, EndY2 : single;
  Angle : single;
  Radius1:single;
  Radius2:single;
  s1, s2 : single;
  xPos : single;
begin
  if VisibleSteps > 0
    then xPos := round(Pos * VisibleSteps) / VisibleSteps
    else xPos := Pos;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);

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
end;


end.
