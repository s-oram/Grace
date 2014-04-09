unit LucidityGui.VectorSequence;

interface


uses
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl,
  VamGuiControlInterfaces;

const
  kMaxSeqLength = 64;

type
  TLucidityVectorSequence = class(TVamWinControl)
  private
    fOnChanged: TNotifyEvent;
    fSequenceLength: integer;
    fCurrentStep: integer;
    fColor_Background: TRedFoxColorString;
    fColor_Border: TRedFoxColorString;
    fColor_StepActive: TRedFoxColorString;
    fColor_Step: TRedFoxColorString;
    fSequenceData: IStepSequenceDataObject;

    function GetSequenceValue(Index: integer): single;
    procedure SetSequenceValue(Index: integer; const Value: single);
    procedure SetSequenceLength(const Value: integer);
    procedure SetCurrentStep(const Value: integer);
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetSequenceData(const Value: IStepSequenceDataObject);
  protected
    fSequenceValues : array[0..kMaxSeqLength] of single;

    fPos : single; //TODO: Remove this variable.

    IsGrabbed : boolean;
    ReferencePoint   : TPoint;
    ReferencePos     : single;
    IsFineAdjustment : boolean;

    InternalPadding : TPadding;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed; inline;

    function MouseToSequencePosition(const PixelX:integer):integer;
    function MouseToSequenceValue(Const PixelY : integer):single;
    //function GetSequence(const PixelY:integer):integer;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SequenceData : IStepSequenceDataObject read fSequenceData write SetSequenceData;

    property SequenceValue[Index:integer]:single read GetSequenceValue write SetSequenceValue; //sequence value range -1..1
  published

    property Color_Background : TRedFoxColorString index 0 read fColor_Background write SetColors;
    property Color_Border     : TRedFoxColorString index 1 read fColor_Border     write SetColors;
    property Color_Step       : TRedFoxColorString index 2 read fColor_Step       write SetColors;
    property Color_StepActive : TRedFoxColorString index 3 read fColor_StepActive write SetColors;


    property CurrentStep    : integer read fCurrentStep    write SetCurrentStep;
    property SequenceLength : integer read fSequenceLength write SetSequenceLength;
    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;

    {$INCLUDE TControlProperties.inc}
  end;


implementation

uses
  SysUtils,
  Math;



{ TVamVectorSequence }

constructor TLucidityVectorSequence.Create(AOwner: TComponent);
var
  c1: Integer;
begin
  inherited;

  InternalPadding := TPadding.Create(Self);
  InternalPadding.SetBounds(2,2,2,2);

  fColor_Background := '$FF000000';
  fColor_Border     := '$FF000000';
  fColor_Step       := '$FFD6DADF';
  fColor_StepActive := '$55FFFFFF';

  fSequenceLength := 8;

  {
  for c1 := 0 to kMaxSeqLength-1 do
  begin
    SequenceValue[c1] := random * 2 - 1;
  end;
  }
end;

destructor TLucidityVectorSequence.Destroy;
begin
  InternalPadding.Free;
  inherited;
end;

function TLucidityVectorSequence.MouseToSequencePosition(const PixelX: integer): integer;
var
  x : integer;
  InternalBounds : TRect;
begin
  InternalBounds.Left   := InternalPadding.Left;
  InternalBounds.Top    := InternalPadding.Top;
  InternalBounds.Right  := Width - InternalPadding.Right;
  InternalBounds.Bottom := Height - InternalPadding.Bottom;

  x := floor((PixelX-InternalBounds.Left) / InternalBounds.Width * fSequenceLength);
  if x < 0 then x := 0;
  if x >= fSequenceLength then x := fSequenceLength-1;
  result := x;
end;

function TLucidityVectorSequence.MouseToSequenceValue(const PixelY: integer): single;
var
  x : single;
begin
  x := 1 - (PixelY / Height);
  x := x * 2 - 1;
  if x > 1  then x := 1;
  if x < -1 then x := -1;
  result := x;
end;

function TLucidityVectorSequence.GetSequenceValue(Index: integer): single;
begin
  if assigned(SequenceData) then
  begin
    result := SequenceData.GetStepValue(Index);
  end else
  begin
    result := 0;
  end;
end;

procedure TLucidityVectorSequence.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColorString;
begin
  case Index of
    0: pc := @fColor_Background;
    1: pc := @fColor_Border;
    2: pc := @fColor_Step;
    3: pc := @fColor_StepActive;
  else
    raise Exception.Create('Index value not handled.');
  end;

  if pc^ <> Value then
  begin
    pc^ := Value;
    Invalidate;
  end;

end;

procedure TLucidityVectorSequence.SetCurrentStep(const Value: integer);
begin
  if Value <> fCurrentStep then
  begin
    fCurrentStep := Value;
    Invalidate;
  end;
end;

procedure TLucidityVectorSequence.SetSequenceData(const Value: IStepSequenceDataObject);
begin
  fSequenceData := Value;
  Invalidate;
end;

procedure TLucidityVectorSequence.SetSequenceLength(const Value: integer);
begin
  if (Value <> fSequenceLength) and (Value > 0) and (Value <= kMaxSeqLength) then
  begin
    fSequenceLength := Value;
    Invalidate;
  end;

end;

procedure TLucidityVectorSequence.SetSequenceValue(Index: integer; const Value: single);
begin
  assert(Value >= -1);
  assert(Value <= 1);

  if assigned(SequenceData) then
  begin
    if Value <> SequenceData.GetStepValue(Index) then
    begin
      SequenceData.SetStepValue(Index, Value);
      Invalidate;
    end;
  end else
  begin
    if Value <> fSequenceValues[Index] then
    begin
      fSequenceValues[Index] := Value;
      Invalidate;
    end;
  end;
end;


procedure TLucidityVectorSequence.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;



procedure TLucidityVectorSequence.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  seqPos : integer;
begin
  inherited;

  if (Button = mbLeft) then
  begin
    IsGrabbed := true;
    seqPos := MouseToSequencePosition(X);
    if (ssCtrl in Shift)
      then SequenceValue[SeqPos] := 0
      else SequenceValue[SeqPos] := MouseToSequenceValue(Y);
    Invalidate;
    Changed;
  end;


  {
  if (Button = mbLeft) and ((ssCtrl in Shift)) then
  begin
    fPos := 0.5;
    Invalidate;
    Changed;
  end;

  if (Button = mbLeft) and ((ssCtrl in Shift) = false) then
  begin
    IsGrabbed := true;
    ReferencePoint := Point(X, Y);
    ReferencePos   := fPos;

    if (ssShift in Shift)
      then IsFineAdjustment := true
      else IsFineAdjustment := false;
  end;
  }
end;

procedure TLucidityVectorSequence.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  //Dist : single;
  //NewPos : single;
  //ScaleFactor : single;
  //CurrentAdjustmentState : boolean;
  seqPos : integer;
begin
  inherited;

  if (IsGrabbed = true) then
  begin
    seqPos := MouseToSequencePosition(X);
    if (ssCtrl in Shift)
      then SequenceValue[SeqPos] := 0
      else SequenceValue[SeqPos] := MouseToSequenceValue(Y);
    Invalidate;
    Changed;
  end;


  {
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
  }
end;

procedure TLucidityVectorSequence.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    IsGrabbed := false;
    Invalidate;
    Changed;
  end;
end;

procedure TLucidityVectorSequence.Paint;
const
  kStepValueColor  = '$FFD6DADF';
  kLineColor       = '$FF000000';
  kActiveStepColor = '$55FFFFFF';
var
  x1, y1, x2, y2 : single;
  c1: Integer;
  InternalBounds : TRect;
begin
  inherited;

  if not assigned(fSequenceData) then
  begin
    BackBuffer.BufferInterface.ClearAll(0,0,0,0);
    exit;
  end;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);

  InternalBounds.Left   := InternalPadding.Left;
  InternalBounds.Top    := InternalPadding.Top;
  InternalBounds.Right  := Width - InternalPadding.Right;
  InternalBounds.Bottom := Height - InternalPadding.Bottom;


  //=== Paint the background ==
  x1 := 0;
  y1 := 0;
  x2 := Width;
  y2 := Height;

  //BackBuffer.BufferInterface.LineWidth := 1;
  //BackBuffer.BufferInterface.LineColor := GetRedFoxColor(Color_Background);
  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := GetRedFoxColor(Color_Background);

  //BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);
  BackBuffer.BufferInterface.RoundedRect(x1, y1, x2, y2, 3);

  //==== Draw sequence value bars ====
  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := GetRedFoxColor(Color_Step);

  y1 := round(InternalBounds.Height / 2) + InternalBounds.Top;
  for c1 := 0 to SequenceLength-1 do
  begin
    y2 := SequenceValue[c1];
    y2 := 1 - (y2 * 0.5 + 0.5);
    y2 := y2 * InternalBounds.Height + InternalBounds.Top;

    x1 := c1 / SequenceLength * (InternalBounds.Width + 1);
    x1 := round(x1) + InternalBounds.Left;

    x2 := (c1+1) / SequenceLength * (InternalBounds.Width + 1);
    x2 := round(x2) + InternalBounds.Left - 1;

    BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);
  end;


  //==== Draw active step ====
  if (CurrentStep >= 0) and (CurrentStep < SequenceLength) then
  begin
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.FillColor := GetRedFoxColor(Color_StepActive,60);

    x1 := CurrentStep / SequenceLength * (InternalBounds.Width + 1);
    x1 := round(x1) + InternalBounds.Left;

    x2 := (CurrentStep+1) / SequenceLength * (InternalBounds.Width + 1);
    x2 := round(x2) + InternalBounds.Left - 1;

    y1 := InternalBounds.Top    + 0.5;
    y2 := InternalBounds.Bottom - 0.5;

    BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);
  end;





  //==== Draw horizontal midpoint line ====
  BackBuffer.BufferInterface.LineColor := GetRedFoxColor(Color_StepActive, 200);
  x1 := InternalBounds.Left  + 0.5;
  x2 := InternalBounds.Right - 0.5;
  y1 := round(InternalBounds.Height / 2) + 0.5 + InternalBounds.Top;

  BackBuffer.BufferInterface.Line(x1, y1, x2, y1);
end;



end.
