unit VamKnob;

interface

uses
  //VamKnobSmoother, //TODO:MED Remove the knob smoother if adding parameter smoothing.['{0CA14E6F-028E-4D3F-97BC-B30B897876E3}']
  VamGuiControlInterfaces,
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

{$SCOPEDENUMS ON}

type
  TVamKnob = class(TVamWinControl, IKnobControl)
  private
    fOnChanged: TNotifyEvent;
    ExternalPos: single;
    fImageStrip: TBitmap;
    fImageStripGlyphCount: integer;
    fVisibleSteps: integer;
    fIsKnobEnabled: boolean;
    fDisabledImage: TBitmap;
    fImage_KnobUpper: TBitmap;
    fImage_KnobLower: TBitmap;
    fIndicatorSize: single;
    fIndicatorDist: single;
    fModLineDist: single;
    fMaxModDepth: single;
    fMinModDepth: single;
    fModLineColor : TRedFoxColor;
    fModLineOffColor : TRedFoxColor;
    fKnobMode: TKnobMode;
    fParameterIndex: integer;
    ExternalModAmount: single;
    fOnModAmountChanged: TNotifyEvent;
    fModLineWidth: single;
    fIsBipolarKnob: boolean;
    fInternalPos: single;
    fInternalModAmount: single;
    fParameterName: string;
    fModEditRadius: single;
    procedure SetPos(Value: single);
    procedure SetImageStripGlyphCount(const Value: integer);
    procedure SetImageStrip(const Value: TBitmap);
    procedure SetVisibleSteps(const Value: integer);
    procedure SetIsKnobEnabled(const Value: boolean);
    procedure SetDisabledImage(const Value: TBitmap);


    //== Added to satisfy IKnobControl ================
    function GetKnobValue : single;
    procedure SetKnobValue(Value : single);
    procedure SetOnKnobPosChanged(Handler:TNotifyEvent);
    procedure SetOnModAmountChanged(Handler:TNotifyEvent);
    procedure SetImage_KnobLower(const Value: TBitmap);
    procedure SetImage_KnobUpper(const Value: TBitmap);
    procedure SetMaxModDepth(const Value: single);
    procedure SetMinModDepth(const Value: single);
    function GetModLineColor: TRedFoxColorString;
    procedure SetModLineColor(const Value: TRedFoxColorString);
    procedure SetKnobMode(const Value: TKnobMode);
    function GetKnobMode:TKnobMode;
    procedure SetParameterIndex(Index : integer);
    function GetParameterIndex:integer;
    procedure SetModAmount(const Value: single);
    function GetModAmountValue : single;
    procedure SetModAmountValue(Value : single);
    function GetModLineOffColor: TRedFoxColorString;
    procedure SetModLineOffColor(const Value: TRedFoxColorString);
    procedure SetParameterName(aName:string);
    function GetParameterName:string;
    procedure SetModEditRadius(const Value: single);
    //=================================================
  protected
    IsBeingEdited : boolean;
    IsGrabbed : boolean;
    IsFineAdjustment : boolean;

    CurrentEditMode : TKnobMode;
    MouseOverEditMode : TKnobMode;
    ShowMouseOverEditMode : boolean;

    ReferencePoint   : TPoint;
    ReferenceValue   : single;
    procedure UpdateReferencePoints(const X, Y:integer);

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KnobPosChanged;
    procedure ModAmountChanged;

    procedure Paint; override;

    procedure DrawKnob_VectorStyle;
    procedure DrawKnob_BitmapStyle;


    procedure DrawKnob;

    procedure DrawKnob_Lower;
    procedure DrawKnob_Upper;
    procedure DrawKnob_PositionArc;
    procedure DrawKnob_Arc;
    procedure DrawKnob_ModDepth;
    procedure DrawKnob_ModAmount;
    procedure DrawKnob_Indicator;

    procedure DrawKnob_ModEditOverlay;
    procedure DrawKnob_PosEditOverlay;

    //InternalPos is the knob pos. It is only available internally.
    property InternalPos       : single read fInternalPos       write fInternalPos;
    property InternalModAmount : single read fInternalModAmount write fInternalModAmount;

    function CalculateKnobEditMode(const Shift: TShiftState; const X, Y : integer):TKnobMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Image_KnobUpper : TBitmap read fImage_KnobUpper write SetImage_KnobUpper;
    property Image_KnobLower : TBitmap read fImage_KnobLower write SetImage_KnobLower;

    property ImageStrip:TBitmap read fImageStrip write SetImageStrip;
    property ImageStripGlyphCount : integer read fImageStripGlyphCount write SetImageStripGlyphCount;

    property DisabledImage : TBitmap read fDisabledImage write SetDisabledImage;
  published
    property ModLineDist   : single read fModLineDist   write fModLineDist;
    property ModLineWidth  : single read fModLineWidth  write fModLineWidth;
    property ModLineColor    : TRedFoxColorString read GetModLineColor write SetModLineColor;
    property ModLineOffColor : TRedFoxColorString read GetModLineOffColor write SetModLineOffColor;
    property IndicatorSize : single read fIndicatorSize write fIndicatorSize;
    property IndicatorDist : single read fIndicatorDist write fIndicatorDist;
    property IsBipolarKnob : boolean read fIsBipolarKnob write fIsBipolarKnob;
    property IsKnobEnabled : boolean   read fIsKnobEnabled  write SetIsKnobEnabled;
    property KnobMode      : TKnobMode read fKnobMode       write SetKnobMode;   // Knob Edit Mode. Position Edit or Modulation Edit.
    property ModEditRadius : single    read fModEditRadius  write SetModEditRadius; // Range 0..1 as a percentage.

    property VisibleSteps  : integer   read fVisibleSteps   write SetVisibleSteps;

    property Pos       : single read ExternalPos       write SetPos;
    property ModAmount : single read ExternalModAmount write SetModAmount;

    property MinModDepth : single read fMinModDepth write SetMinModDepth;
    property MaxModDepth : single read fMaxModDepth write SetMaxModDepth;

    // typically used to store the linked parameter name.
    property ParameterName  : string  read fParameterName  write fParameterName;

    // typically used to store the VST parameter index.
    property ParameterIndex : integer read fParameterIndex write fParameterIndex;

    // OnChanged should only be called when the control changes through user interaction.
    property OnKnobPosChanged   : TNotifyEvent read fOnChanged          write fOnChanged;
    property OnModAmountChanged : TNotifyEvent read fOnModAmountChanged write fOnModAmountChanged;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  AggArc,
  AggPathStorage,
  Agg2d,
  AggBasics,
  VamLib.Utils,
  SysUtils,
  Math,
  AggPixelFormat;

const
  kMinAngle = 30;
  kMaxAngle = 300;
  kArcSpan  = 300;


// TranslateAngleRadiusToXY is used by VstKnob to draw the Pointer line.
procedure PolarToCartesian(const MiddleX,MiddleY, Degrees, Radius:single; out X, Y:single);
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

//function Clamp(Value : single; const MinValue, MaxValue : single):single;
//begin
//  if Value < MinValue then exit(MinValue);
//  if Value > MaxValue then exit(MaxValue);
//  result := Value;
//end;

procedure CalcStartSweep(const Angle1, Angle2 : single; out Start, Sweep : single);
begin
  if Angle2 > Angle1 then
  begin
    Start := (Angle2+90) / 360 * 2 * pi;
    Sweep := (Angle1+90) / 360 * 2 * pi;
  end else
  begin
    Start := (Angle1+90) / 360 * 2 * pi;
    Sweep := (Angle2+90) / 360 * 2 * pi;
  end;
end;

{ TVamKnob }

constructor TVamKnob.Create(AOwner: TComponent);
begin
  inherited;

  fModEditRadius := 0.6;
  fVisibleSteps := 0;
  fIsKnobEnabled := true;

  fIndicatorSize := 2.5;
  fIndicatorDist := 9;

  fModLineDist := 17;
  fModLineWidth := 3;

  fMinModDepth := 0;
  fMaxModDepth := 0;
  fModLineColor    := GetAggColor(clRed);
  fModLineOffColor := GetAggColor(clSilver);


  fInternalPos := 0;
end;

destructor TVamKnob.Destroy;
begin

  inherited;
end;

function TVamKnob.CalculateKnobEditMode(const Shift: TShiftState; const X, Y: integer): TKnobMode;
var
  MaxRadius  : single;
  MidX, MidY : single;
  RadiusFrac : single;
  CurRadius  : single;
begin
  if (KnobMode = TKnobMode.PositionEdit) then
  begin
    result := TKnobMode.PositionEdit;
  end else
  begin
    assert(KnobMode = TKnobMode.ModEdit);

    MidX := Width  * 0.5;
    MidY := Height * 0.5;
    CurRadius := DistanceBetweenTwoPoints(MidX, MidY, x, y);
    MaxRadius := DistanceBetweenTwoPoints(0, 0, MidX, MidY);
    RadiusFrac := CurRadius / MaxRadius;

    if (ssAlt in Shift) or (RadiusFrac >= ModEditRadius)
      then result := TKnobMode.ModEdit
      else result := TKnobMode.PositionEdit;
  end;

end;



procedure TVamKnob.KnobPosChanged;
begin
  if assigned(OnKnobPosChanged) then OnKnobPosChanged(self);
  ChangedMultiEvent.TriggerAll(self);
end;

procedure TVamKnob.ModAmountChanged;
begin
  if assigned(OnModAmountChanged) then OnModAmountChanged(Self);
end;

procedure TVamKnob.UpdateReferencePoints(const X, Y:integer);
begin
  ReferencePoint := Point(X, Y);

  if CurrentEditMode = TKnobMode.PositionEdit
    then ReferenceValue   := InternalPos
    else ReferenceValue   := InternalModAmount;
end;



procedure TVamKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (IsKnobEnabled) and (Button = mbLeft) and ((ssCtrl in Shift) = false) then
  begin
    ShowMouseOverEditMode := false;

    CurrentEditMode := CalculateKnobEditMode(Shift, x, y);

    IsGrabbed := true;
    IsBeingEdited := true;
    UpdateReferencePoints(X, Y);

    if (ssShift in Shift)
      then IsFineAdjustment := true
      else IsFineAdjustment := false;
  end;
end;

procedure TVamKnob.MouseEnter;
begin
  inherited;
  ShowMouseOverEditMode := true;
  Invalidate;
end;

procedure TVamKnob.MouseLeave;
begin
  inherited;
  ShowMouseOverEditMode := false;
  Invalidate;
end;

procedure TVamKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Dist : single;
  NewValue : single;
  ScaleFactor : single;
  CurrentAdjustmentState : boolean;
  EditMode : TKnobMode;
begin
  inherited;

  if (IsGrabbed = false) then
  begin
    EditMode := CalculateKnobEditMode(Shift, x, y);

    if EditMode <> MouseOverEditMode then
    begin
      MouseOverEditMode := EditMode;
      Invalidate;
    end;

    if not ShowMouseOverEditMode then
    begin
      ShowMouseOverEditMode := true;
      Invalidate;
    end;
  end;


  if (IsGrabbed = true) then
  begin
    if (ssShift in Shift)
      then CurrentAdjustmentState := true
      else CurrentAdjustmentState := false;

    if IsFineAdjustment <> CurrentAdjustmentState then
    begin
      IsFineAdjustment := CurrentAdjustmentState;
      //NOTE: Reset the reference point when changing from/to 'Fine Adjustment' mode.
      UpdateReferencePoints(X, Y);
    end;

    if IsFineAdjustment = false
      then ScaleFactor := 0.005
      else ScaleFactor := 0.00175;

    Dist := (Y - ReferencePoint.Y) * ScaleFactor;

    NewValue := ReferenceValue - Dist;

    if CurrentEditMode = TKnobMode.PositionEdit then
    begin
      // NOTE: Reset the reference point whenever the knob position limit is exceeded.
      // This prevents overshoot when the user reverses mouse direction at the knob
      // position limits.
      if NewValue > 1 then
      begin
        NewValue := 1;
        UpdateReferencePoints(X, Y);
      end;

      if NewValue < 0 then
      begin
        NewValue := 0;
        UpdateReferencePoints(X, Y);
      end;

      InternalPos := NewValue;

      if InternalPos <> ExternalPos then
      begin
        ExternalPos := InternalPos;
        Invalidate;
        KnobPosChanged;
      end;
    end else
    begin
      // NOTE: Reset the reference point whenever the knob position limit is exceeded.
      // This prevents overshoot when the user reverses mouse direction at the knob
      // position limits.

      // NOTE: Clamp the mod amount values to the end of the knob ranges.
      if InternalPos + NewValue > 1 then
      begin
        NewValue := 1 - InternalPos;
        UpdateReferencePoints(X, Y);
      end;

      if ExternalPos + NewValue < 0 then
      begin
        NewValue := 0 - InternalPos;
        UpdateReferencePoints(X, Y);
      end;

      InternalModAmount := NewValue;

      if InternalModAmount <> ExternalModAmount then
      begin
        ExternalModAmount := InternalModAmount;
        Invalidate;
        ModAmountChanged;
      end;
    end;

    Invalidate;
  end;
end;

procedure TVamKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    IsGrabbed := false;
    IsBeingEdited := false;

    if InternalPos + InternalModAmount > 1
      then InternalModAmount := 1 - InternalPos;

    if InternalPos + InternalModAmount < 0
      then InternalModAmount := 0 - InternalPos;

    if InternalPos <> ExternalPos then
    begin
      ExternalPos := InternalPos;
      Invalidate;
      KnobPosChanged;
    end;

    if InternalModAmount <> ExternalModAmount then
    begin
      ExternalModAmount := InternalModAmount;
      Invalidate;
      ModAmountChanged;
    end;
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

procedure TVamKnob.SetImage_KnobLower(const Value: TBitmap);
begin
  fImage_KnobLower := Value;
  Invalidate;
end;

procedure TVamKnob.SetImage_KnobUpper(const Value: TBitmap);
begin
  fImage_KnobUpper := Value;
  Invalidate;
end;

procedure TVamKnob.SetIsKnobEnabled(const Value: boolean);
begin
  if Value <> fIsKnobEnabled then
  begin
    fIsKnobEnabled := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.SetKnobMode(const Value: TKnobMode);
begin
  if Value <> fKnobMode then
  begin
    fKnobMode := Value;
    Invalidate;
  end;
end;

procedure TVamKnob.SetKnobValue(Value: single);
begin
  SetPos(Value);
end;

procedure TVamKnob.SetMinModDepth(const Value: single);
begin
  assert(Value >= -1);
  assert(Value <= 0);

  if Value <> fMinModDepth then
  begin
    fMinModDepth := Value;
    if KnobMode = TKnobMode.PositionEdit
      then Invalidate;
  end;
end;

procedure TVamKnob.SetMaxModDepth(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  if Value <> fMaxModDepth then
  begin
    fMaxModDepth := Value;
    if KnobMode = TKnobMode.PositionEdit
      then Invalidate;
  end;
end;

procedure TVamKnob.SetModAmount(const Value: single);
begin
  if IsBeingEdited then exit;

  assert(Value >= -1);
  assert(Value <= 1);

  if (Value <> ExternalModAmount) or (Value <> InternalModAmount) then
  begin
    ExternalModAmount := Value;
    InternalModAmount := Value;
    if KnobMode = TKnobMode.ModEdit
      then Invalidate;
  end;
end;

procedure TVamKnob.SetModAmountValue(Value: single);
begin
  SetModAmount(Value);
end;

procedure TVamKnob.SetModEditRadius(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fModEditRadius := Value;
end;

procedure TVamKnob.SetModLineColor(const Value: TRedFoxColorString);
begin
  fModLineColor := Value;
end;

procedure TVamKnob.SetModLineOffColor(const Value: TRedFoxColorString);
begin
  fModLineOffColor := Value;
end;

procedure TVamKnob.SetOnKnobPosChanged(Handler: TNotifyEvent);
begin
  OnKnobPosChanged := Handler;
end;

procedure TVamKnob.SetOnModAmountChanged(Handler: TNotifyEvent);
begin
  OnModAmountChanged := Handler;
end;

function TVamKnob.GetKnobMode: TKnobMode;
begin
  result := fKnobMode;
end;

function TVamKnob.GetKnobValue: single;
begin
  result := ExternalPos;
end;

function TVamKnob.GetModAmountValue: single;
begin
  result := ExternalModAmount;
end;

function TVamKnob.GetModLineColor: TRedFoxColorString;
begin
  result := fModLineColor
end;

function TVamKnob.GetModLineOffColor: TRedFoxColorString;
begin
  result := fModLineOffColor;
end;

function TVamKnob.GetParameterIndex: integer;
begin
  result := fParameterIndex;
end;

function TVamKnob.GetParameterName: string;
begin
  result := fParameterName;
end;

procedure TVamKnob.SetParameterIndex(Index: integer);
begin
  fParameterIndex := Index;
end;

procedure TVamKnob.SetParameterName(aName: string);
begin
  fParameterName := aName;
end;

procedure TVamKnob.SetPos(Value: single);
begin
  if IsBeingEdited then exit;

  if Value < 0 then Value := 0
  else if Value > 1 then Value := 1;

  if (Value <> ExternalPos) or (Value <> InternalPos) then
  begin
    ExternalPos := Value;
    InternalPos := Value;
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

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);


  if (IsKnobEnabled) then
  begin
    BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;

    BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
    DrawKnob_Upper;
    DrawKnob_Indicator;

    DrawKnob_Arc;

    if KnobMode = TKnobMode.PositionEdit
      then DrawKnob_ModDepth
      else DrawKnob_ModAmount;


    if (ShowMouseOverEditMode) and (KnobMode = TKnobMode.ModEdit) then
    begin
      if (MouseOverEditMode = TKnobMode.ModEdit)
        then DrawKnob_ModEditOverlay
        else DrawKnob_PosEditOverlay;
    end;

  end else
  begin
    if assigned(DisabledImage) then
    begin
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
end;

procedure TVamKnob.DrawKnob;
begin
end;

procedure TVamKnob.DrawKnob_Lower;
begin
  if assigned(Image_KnobLower) then
  begin
    BackBuffer.TransformImage(Image_KnobLower);
  end;
end;

procedure TVamKnob.DrawKnob_Upper;
var
  SrcRect : TRect;
  x1, y1 : integer;
begin
  if assigned(Image_KnobUpper) then
  begin
    SrcRect := Rect(0,0, Image_KnobUpper.Width, Image_KnobUpper.Height);
    //DstRect.Left   := (Width  - Image_KnobUpper.Width)  div 2;
    //DstRect.Height := (Height - Image_KnobUpper.Height) div 2;

    x1 := (Width  - Image_KnobUpper.Width)  div 2;
    y1 := (Height - Image_KnobUpper.Height) div 2;

    BackBuffer.TransformImage(Image_Knobupper, x1, y1);
  end;
end;

procedure TVamKnob.DrawKnob_Arc;
var
  MiddleX, MiddleY : single;
  Angle1, Angle2 : single;
  s1, s2 : single;
begin
  MiddleX := Width * 0.5;
  MiddleY := Height * 0.5;

  BackBuffer.BufferInterface.LineWidth := ModLineWidth;
  BackBuffer.BufferInterface.LineColor := fModLineOffColor;

  Angle1 := kMinAngle;
  Angle2 := kMinAngle + kArcSpan;

  CalcStartSweep(Angle1, Angle2, s1, s2);
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);

  {
  Color2 := fModLineColor;
  Color2.AdjustLightness(0.2);

  BackBuffer.BufferInterface.LineWidth := ModLineWidth * 0.7;
  BackBuffer.BufferInterface.LineColor := Color2;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);

  Color2.AdjustLightness(0.1);

  BackBuffer.BufferInterface.LineWidth := ModLineWidth * 0.3;
  BackBuffer.BufferInterface.LineColor := Color2;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);
  }
end;



procedure TVamKnob.DrawKnob_ModDepth;
var
  MiddleX, MiddleY : single;
  Angle1, Angle2 : single;
  s1, s2 : single;
begin
  MiddleX := Width * 0.5;
  MiddleY := Height * 0.5;

  BackBuffer.BufferInterface.LineWidth := ModLineWidth;
  BackBuffer.BufferInterface.LineColor := fModLineColor;

  Angle1 := kMinAngle + kArcSpan * Clamp((InternalPos + MinModDepth), 0, 1);
  Angle2 := kMinAngle + kArcSpan * Clamp((InternalPos + MaxModDepth), 0, 1);

  CalcStartSweep(Angle1, Angle2, s1, s2);

  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);
end;

procedure TVamKnob.DrawKnob_ModEditOverlay;
  procedure CalcStartSweep(const Angle1, Angle2 : single; out Start, Sweep : single);
  begin
    Start := (Angle2+90) / 360 * 2 * pi;
    Sweep := (Angle1+90) / 360 * 2 * pi;
  end;
const
  kMinAngle = 30;
  kMaxAngle = 300;
  kArcSpan  = 300;
var
  MidX, MidY : single;
  Angle1, Angle2 : single;
  s1, s2 : single;
begin
  MidX := Width  * 0.5;
  MidY := Height * 0.5;

  BackBuffer.BufferInterface.LineWidth := ModLineWidth;
  BackBuffer.BufferInterface.LineColor := GetAggColor(clAqua, 100);

  Angle1 := kMinAngle;
  Angle2 := kMinAngle + kArcSpan;

  CalcStartSweep(Angle1, Angle2, s1, s2);
  BackBuffer.BufferInterface.Arc(MidX, MidY, ModLineDist, ModLineDist, s1, s2);
end;

procedure TVamKnob.DrawKnob_PosEditOverlay;
var
  MidX, MidY : single;
  r : single;
begin
  MidX := Width  * 0.5;
  MidY := Height * 0.5;
  r := 0.2 * Min(Width, Height);

  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := GetAggColor(clAqua, 100);
  BackBuffer.BufferInterface.Circle(MidX, MidY, r);
end;

procedure TVamKnob.DrawKnob_PositionArc;
var
  MiddleX, MiddleY : single;
  Angle1, Angle2 : single;
  s1, s2 : single;
  color2 : TRedFoxColor;
begin
  MiddleX := Width * 0.5;
  MiddleY := Height * 0.5;

  BackBuffer.BufferInterface.LineWidth := ModLineWidth;
  BackBuffer.BufferInterface.LineColor := fModLineColor;
  //BackBuffer.BufferInterface.FillColor := GetAggColor(clWhite, 255);

  if IsBipolarKnob
    then Angle1 := kMinAngle + kArcSpan * 0.5
    else Angle1 := kMinAngle;

  Angle2 := kMinAngle + kArcSpan * (InternalPos);

  CalcStartSweep(Angle1, Angle2, s1, s2);

  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);


  Color2 := fModLineColor;
  Color2.AdjustLightness(0.2);

  BackBuffer.BufferInterface.LineWidth := ModLineWidth * 0.7;
  BackBuffer.BufferInterface.LineColor := Color2;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);

  Color2.AdjustLightness(0.1);

  BackBuffer.BufferInterface.LineWidth := ModLineWidth * 0.3;
  BackBuffer.BufferInterface.LineColor := Color2;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);
end;

procedure TVamKnob.DrawKnob_ModAmount;
var
  MiddleX, MiddleY : single;
  Angle1, Angle2 : single;
  s1, s2 : single;
  color2 : TRedFoxColor;
begin
  MiddleX := Width * 0.5;
  MiddleY := Height * 0.5;

  BackBuffer.BufferInterface.LineWidth := ModLineWidth;
  BackBuffer.BufferInterface.LineColor := fModLineColor;

  Angle1 := kMinAngle + kArcSpan * Clamp((InternalPos), 0, 1);
  Angle2 := kMinAngle + kArcSpan * Clamp((InternalPos + ModAmount), 0, 1);

  CalcStartSweep(Angle1, Angle2, s1, s2);

  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);

  Color2 := fModLineColor;
  Color2.AdjustLightness(0.1);

  BackBuffer.BufferInterface.LineWidth := ModLineWidth * 0.7;
  BackBuffer.BufferInterface.LineColor := Color2;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);

  {

  Color2.AdjustLightness(0.1);

  BackBuffer.BufferInterface.LineWidth := ModLineWidth * 0.3;
  BackBuffer.BufferInterface.LineColor := Color2;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY, ModLineDist, ModLineDist, s1, s2);
  }
end;



procedure TVamKnob.DrawKnob_Indicator;
var
  Angle : single;
  MiddleX, MiddleY : single;
  IndicatorX, IndicatorY : single;
begin
  MiddleX := Width * 0.5;
  MiddleY := Height * 0.5;
  Angle   := kMinAngle + kArcSpan * InternalPos;
  PolarToCartesian(MiddleX, MiddleY, Angle, IndicatorDist, IndicatorX, IndicatorY);

  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := GetAggColor(clBlack);
  BackBuffer.BufferInterface.Circle(IndicatorX, IndicatorY, IndicatorSize);
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
    then xPos := round(InternalPos * VisibleSteps) / VisibleSteps
    else xPos := InternalPos;

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
    then xPos := round(InternalPos * VisibleSteps) / VisibleSteps
    else xPos := InternalPos;

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

  s1 := (InternalPos * 300) + 120;
  s2 := s1 - (InternalPos * 300) ;
  s1 := s1 / 360 * 2 * pi;
  s2 := s2 / 360 * 2 * pi;
  BackBuffer.BufferInterface.Arc(MiddleX, MiddleY,Radius1, Radius1, s1, s2);



  BackBuffer.BufferInterface.LineColor := GetAggColor(clBlack); // <-- set color to black.

  //Draw the minimum indicator
  Angle := 30;
  Radius2 := Width / 2 - 5;

  PolarToCartesian(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  PolarToCartesian(MiddleX, MiddleY, Angle, Radius2, EndX2, EndY2);
  BackBuffer.BufferInterface.Line(EndX,EndY, EndX2, EndY2);

  //Draw the maximum indicator
  Angle := 180;
  PolarToCartesian(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  PolarToCartesian(MiddleX, MiddleY, Angle, Radius2, EndX2, EndY2);
  BackBuffer.BufferInterface.Line(EndX,EndY, EndX2, EndY2);

  //Draw the maximum indicator
  Angle := 330;
  PolarToCartesian(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  PolarToCartesian(MiddleX, MiddleY, Angle, Radius2, EndX2, EndY2);
  BackBuffer.BufferInterface.Line(EndX,EndY, EndX2, EndY2);


  //Draw the knob indicator line.
  BackBuffer.BufferInterface.LineWidth := 2;
  Angle := 30 + (300 * xPos);
  PolarToCartesian(MiddleX, MiddleY, Angle, Radius1, EndX, EndY);
  BackBuffer.BufferInterface.Line(MiddleX,MiddleY, EndX, EndY);
end;

end.

