unit VamNumericKnob;

interface

uses
  Types,
  Classes, Controls, RedFox, RedFoxWinControl, VamWinControl;

type
  TNumericStyle = (nsInteger, nsFloat, nsCustom);

  TVamNumericKnob = class(TVamWinControl)
  private
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fNumericStyle: TNumericStyle;
    fKnobMin: integer;
    fKnobMax: integer;
    fDecimalPlaces: integer;
    fOnChanged: TNotifyEvent;
    fUnits: string;
    fCustomText: string;
    fOnRotaryStepUp: TNotifyEvent;
    fOnRotaryStepDown: TNotifyEvent;
    fSensitivity: single;
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    procedure SetNumericStyle(const Value: TNumericStyle);
    procedure SetKnobMin(const Value: integer);
    procedure SetKnobMax(const Value: integer);
    procedure SetDecimalPlaces(const Value: integer);
    procedure SetKnobValue(const Value: double);
    procedure SetUnits(const Value: string);
    procedure SetCustomText(const Value: string);
  protected
    ReferenceKnobValue : double;
    fKnobValue : double;
    GraphicSplitPoint : single;
    IntAdjust : boolean;

    IsGrabbed : boolean;
    ReferencePoint   : TPoint;
    ReferencePos     : single;
    IsFineAdjustment : boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published


    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Font;

    property KnobValue : double read fKnobValue write SetKnobValue;
    property KnobMin : integer read fKnobMin write SetKnobMin;
    property KnobMax : integer read fKnobMax write SetKnobMax;

    property Sensitivity : single read fSensitivity write fSensitivity;

    property NumericStyle  : TNumericStyle read fNumericStyle  write SetNumericStyle;
    property DecimalPlaces : integer       read fDecimalPlaces write SetDecimalPlaces;
    property Units         : string        read fUnits         write SetUnits;

    property CustomText : string read fCustomText write SetCustomText;

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;
    property OnRotaryStepUp   : TNotifyEvent read fOnRotaryStepUp   write fOnRotaryStepUp;
    property OnRotaryStepDown : TNotifyEvent read fOnRotaryStepDown write fOnRotaryStepDown;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  Math,
  SysUtils,
  RedFoxColor,
  Graphics;


{ TVamNumericKnob }

constructor TVamNumericKnob.Create(AOwner: TComponent);
begin
  inherited;

  fCustomText := '';
  fKnobValue := 0;

  fTextAlign := TRedFoxAlign.AlignCenter;
  fTextVAlign := TRedFoxAlign.AlignCenter;

  fKnobMin := 0;
  fKnobMax := 100;

  fDecimalPlaces := 2;

  //fNumericStyle := nsInteger;
  fNumericStyle := nsFloat;

  GraphicSplitPoint := 0;

  fUnits := '';

  Sensitivity := 1;
end;

destructor TVamNumericKnob.Destroy;
begin

  inherited;
end;


procedure TVamNumericKnob.Changed;
begin
  if assigned(OnChanged) then OnChanged(Self);
end;



procedure TVamNumericKnob.SetCustomText(const Value: string);
begin
  if (Value <> fCustomText) then
  begin
    fCustomText := Value;

    if NumericStyle = nsCustom
      then Invalidate;
  end;
end;

procedure TVamNumericKnob.SetDecimalPlaces(const Value: integer);
begin
  if (Value >= 1) and (Value <= 10) then
  begin
    fDecimalPlaces := Value;
    Invalidate;
  end;
end;

procedure TVamNumericKnob.SetKnobValue(const Value: double);
begin
  if (Value <> fKnobValue) and (Value >= KnobMin) and (Value <= KnobMax) then
  begin
    fKnobValue := Value;
    Invalidate;
  end;
end;

procedure TVamNumericKnob.SetKnobMax(const Value: integer);
begin
  fKnobMax := Value;
end;

procedure TVamNumericKnob.SetKnobMin(const Value: integer);
begin
  fKnobMin := Value;
end;

procedure TVamNumericKnob.SetNumericStyle(const Value: TNumericStyle);
begin
  fNumericStyle := Value;
end;

procedure TVamNumericKnob.SetTextAlign(const Value: TRedFoxAlign);
begin
  if fTextAlign <> Value then
  begin
    fTextAlign := Value;
    Invalidate;
  end;
end;

procedure TVamNumericKnob.SetTextVAlign(const Value: TRedFoxAlign);
begin
    if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    Invalidate;
  end;
end;

procedure TVamNumericKnob.SetUnits(const Value: string);
begin
  if fUnits <> Value then
  begin
    fUnits := Value;
    Invalidate;
  end;
end;

procedure TVamNumericKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and ((ssCtrl in Shift)) then
  begin
    // TODO: Reset event should be fired here!
    //fPos := 0.5;

    Invalidate;
    Changed;

  end;

  if (Button = mbLeft) and ((ssCtrl in Shift) = false) then
  begin
    IsGrabbed := true;

    if (ssShift in Shift)
      then IsFineAdjustment := true
      else IsFineAdjustment := false;

    if X <= GraphicSplitPoint
      then IntAdjust := true
      else IntAdjust := false;

    //== Reset reference points =====
    ReferencePoint := Point(X, Y);
    ReferenceKnobValue := fKnobValue;
    //===============================
  end;

end;

procedure TVamNumericKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Dist : double;
  ScaleFactor : double;
  NewKnobValue : double;
begin
  inherited;


  // Check to see if the Shift key has been pressed or released..
  // If so, we need to re-anchor our reference points to the current knob value
  // and mouse cursor position.
  if (IsGrabbed) and ((ssShift in Shift) <> IsFineAdjustment) then
  begin
    if (ssShift in Shift)
      then IsFineAdjustment := true
      else IsFineAdjustment := false;

    //== Reset reference points =====
    ReferencePoint := Point(X, Y);
    ReferenceKnobValue := fKnobValue;
    //===============================
  end;


  if (IsGrabbed) and ((NumericStyle = nsInteger) or (NumericStyle = nsFloat)) then
  begin
    Dist := 0;

    if (NumericStyle = nsInteger) or (NumericStyle = nsCustom) then
    begin
      if IsFineAdjustment = false
        then ScaleFactor := 0.005
        else ScaleFactor := 0.00175;

      ScaleFactor := ScaleFactor * (KnobMax - KnobMin) * Sensitivity;

      Dist := (ReferencePoint.Y - Y) * ScaleFactor;
      Dist := Round(Dist);
    end;

    if (NumericStyle = nsFloat) and (IntAdjust = true) then
    begin
      if IsFineAdjustment = false
        then ScaleFactor := 0.005
        else ScaleFactor := 0.00175;

      ScaleFactor := ScaleFactor * (KnobMax - KnobMin) * Sensitivity;

      Dist := (ReferencePoint.Y - Y) * ScaleFactor;
      Dist := Round(Dist);
    end;

    if (NumericStyle = nsFloat) and (IntAdjust = false) then
    begin
      if IsFineAdjustment = false
        then ScaleFactor := 0.005
        else ScaleFactor := 0.075 / Power(10, DecimalPlaces);

      Dist := (ReferencePoint.Y - Y) * ScaleFactor * Sensitivity;
    end;

    NewKnobValue := ReferenceKnobValue + Dist;
    if NewKnobValue > KnobMax then
    begin
      NewKnobValue := KnobMax;
      //== Reset reference points =====
      ReferencePoint := Point(X, Y);
      ReferenceKnobValue := fKnobValue;
      //===============================
    end;

    if NewKnobValue < KnobMin then
    begin
      NewKnobValue := KnobMin;
      //== Reset reference points =====
      ReferencePoint := Point(X, Y);
      ReferenceKnobValue := fKnobValue;
      //===============================
    end;

    if NewKnobValue <> fKnobValue then
    begin
      fKnobValue := NewKnobValue;
      Invalidate;
      Changed;
    end;
  end;


  if (IsGrabbed) and (NumericStyle = nsCustom) then
  begin
    //Dist := 0;

    if IsFineAdjustment = false
      then ScaleFactor := 0.005
      else ScaleFactor := 0.00175;

    ScaleFactor := ScaleFactor * (KnobMax - KnobMin) * Sensitivity;

    Dist := (ReferencePoint.Y - Y) * ScaleFactor;
    Dist := Round(Dist);

    NewKnobValue := ReferenceKnobValue + Dist;


    while (NewKnobValue - fKnobValue) >= 1 do
    begin
      if assigned(OnRotaryStepUp) then OnRotaryStepUp(self);
      fKnobValue := fKnobValue + 1;
    end;

    while (NewKnobValue - fKnobValue) <= -1 do
    begin
      if assigned(OnRotaryStepDown) then OnRotaryStepDown(self);
      fKnobValue := fKnobValue - 1;
    end;
  end;


end;

procedure TVamNumericKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    if (NumericStyle = nsCustom) then
    begin
      fKnobValue := 0;
    end;
    IsGrabbed := false;
    Invalidate;
  end;
end;

procedure TVamNumericKnob.Paint;
var
  TextBounds : TRect;
  Text : string;
  TextA, TextB : string;

  breakPoint : integer;

  ActualTextBounds : TRect;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);

  TextBounds := Rect(0,0, Width, Height);

  if NumericStyle = nsCustom then
  begin
    Text := fCustomText;
    BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);
  end
  else if NumericStyle = nsInteger then
  begin
    Text := IntToStr(Round(fKnobValue)) + fUnits;
    BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);
  end else
  begin
    Text := FloatToStrF(fKnobValue, TFloatFormat.ffFixed, 18, DecimalPlaces) + fUnits;

    BreakPoint := Pos('.', Text);

    TextA := Copy(Text, 1, BreakPoint-1);
    TextB := Copy(Text, BreakPoint+1, Length(Text) - BreakPoint);

    ActualTextBounds := BackBuffer.CalcActualTextBounds(Text, Font, TextAlign, TextVAlign, TextBounds);

    GraphicSplitPoint := ActualTextBounds.Left + BackBuffer.TextWidth(TextA);

    //BackBuffer.BufferInterface.FillColor :=  GetAggColor(clRed);
    //BackBuffer.BufferInterface.Rectangle(ActualTextBounds.Left, ActualTextBounds.Top, ActualTextBounds.Right, ActualTextBounds.Bottom);

    BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);
  end;



end;

end.
