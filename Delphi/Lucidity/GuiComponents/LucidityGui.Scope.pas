unit LucidityGui.Scope;

interface

uses
  VamLib.UniqueID,
  VamLib.Animation,
  //=================================================
  // NOTE: Any Lucidity specific units must be
  // drawn from the Ludcidity Base Classes project.
  // I want to avoid circural type depencies where
  uLucidityEnums,
  //=================================================
  LucidityGui.Scope.FreqAnalyzer,
  LucidityGui.Scope.SignalRecorder,
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor, RedFoxImageBuffer,
  VamGraphicControl, VamWinControl;

const
  kRandomValueCount = 64;


type
  TLfoDrawingRoutines = class;

  {$SCOPEDENUMS ON}
  TScopeDisplayMode = (
    DisplayOff,
    ADSR,
    LFO,
    Filter,
    FilterBlend
  );


  // NOTE: Generally all section values below should be
  // between 0..1 range.

  TScopeAdsrValues = record
    Attack  : single;
    Hold    : single;
    Decay   : single;
    Sustain : single;
    Release : single;
  end;

  TScopeLfoValues = record
  private
    LastRandomUpdate : TDateTime;
    IsRandomInitialised : boolean;
  public
    Shape : TLfoShape;
    Par1  : single;
    Par2  : single;
    Par3  : single;
    RandomValues : array[0..kRandomValueCount-1] of single;

    procedure UpdateRandomValues;
    function FakeRandom(var Seed : integer):single;
  end;

  TScopeFilterValues = record
    FilterType : TFilterType;
    Par1 : single;
    Par2 : single;
    Par3 : single;
    Par4 : single;
  end;

  TScopeFilterBlendValues = record
    FilterRouting : TFilterRouting;
    BlendAmt : single;
  end;



  TLucidityScope = class(TVamWinControl)
  private
    fText: string;
    fScopeMode: TScopeDisplayMode;
    fSignalRecorder: IScopeSignalRecorder;
    fFreqAnalyzer: IFreqAnalyzer;
    function GetColors(const Index: Integer): TRedFoxColorString;
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetText(const Value: string);
    procedure SetScopeDisplayMode(const Value: TScopeDisplayMode);

  protected
    SignalDisplay : TSignalDisplay;
    FreqDisplay   : TFreqDisplay;

    fColorBackground : TRedFoxColor;
    fColorBorder     : TRedFoxColor;
    fColorForeground : TRedFoxColor;

    ScopeRect : TRect;

    SignalAniID : TUniqueID;
    SignalOpacity : byte;


    DiagramBuffer : TRedFoxImageBuffer;
    DiagramBufferAlpha : byte;
    DiagramBufferAnimationID : TUniqueID;

    procedure Draw_ADSR;
    procedure Draw_Lfo;
    procedure Draw_Filter;
    procedure Draw_FilterBlend;
  public
    AdsrValues        : TScopeAdsrValues;
    LfoValues         : TScopeLfoValues;
    FilterValues      : TScopeFilterValues;
    FilterBlendValues : TScopeFilterBlendValues;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;


    //== AHDSR values ==
    //== Lfo values ==
    //== Filter values ==
    property SignalRecorder : IScopeSignalRecorder read fSignalRecorder write fSignalRecorder;
    property FreqAnalyzer   : IFreqAnalyzer        read fFreqAnalyzer   write fFreqAnalyzer;
  published
    property ColorBackground : TRedFoxColorString index 0 read GetColors write SetColors;
    property ColorBorder     : TRedFoxColorString index 1 read GetColors write SetColors;
    property ColorForeground : TRedFoxColorString index 2 read GetColors write SetColors;

    property ScopeMode : TScopeDisplayMode read fScopeMode write SetScopeDisplayMode;
    property Font;
    property Text : string read fText write SetText;

    {$INCLUDE TControlProperties.inc}
  end;


  TLfoDrawingRoutines = class
  private
  public
    class procedure Draw_Lfo_SawUp(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_SawDown(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_Tri(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_Sine(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_Square(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_RandomStepped(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_RandomSmooth(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_AttackDecay(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
    class procedure Draw_Lfo_Cycle(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; LfoValues : TScopeLfoValues);
  end;

  TFilterBlendDrawingRoutines = class
  private
    class procedure Draw_SignalTriangleRight(BackBuffer:TRedFoxImageBuffer; TargetPointX, TargetPointY : single);
    class procedure Draw_SignalTriangleDown(BackBuffer:TRedFoxImageBuffer; TargetPointX, TargetPointY : single);
    class procedure Draw_Boxes(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; Font : TFont; const BlendAmt : single; Color:TRedFoxColor; out Box1, Box2, Box3 : TRect);
    class procedure Draw_InputOutputLines(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; Color:TRedFoxColor; FR : TFilterRouting; const Box1, Box2, Box3 : TRect);
    class procedure Draw_BlendAmountLines(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; Font : TFont; const BlendAmt : single; Color:TRedFoxColor; out Box1, Box2, Box3 : TRect);
  public
    class procedure Draw_Serial(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; Font : TFont; const BlendAmt : single; Color:TRedFoxColor);
    class procedure Draw_Parallel(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; Font : TFont; const BlendAmt : single; Color:TRedFoxColor);
    class procedure Draw_FiftyFifty(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; Font : TFont; const BlendAmt : single; Color:TRedFoxColor);
  end;

  TAdsrDrawingRoutines = class
  private
  public
    class procedure Draw_Adsr(BackBuffer:TRedFoxImageBuffer; ScopeRect : TRect; Color:TRedFoxColor; AdsrValues : TScopeAdsrValues);
  end;



implementation

uses
  DateUtils,
  SysUtils,
  Math,
  VamLib.Utils,
  AggPixelFormat,
  AggColor,
  Agg2D,
  AggBasics,
  RedFox2D;

type
  TDrawFunction = reference to function(x:single):single;

procedure DrawFunction(const Canvas:TAgg2D; const Bounds : TRect; const Steps : integer; aFunction : TDrawFunction);
var
  c1: Integer;
  tx, ty : single;
  x1,y1,x2,y2 : single;
  Width : integer;
  Height : integer;

  OldClipBox : TRectDouble;

begin
  OldClipBox := Canvas.ClipBox;
  Canvas.ClipBox(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);

  Width := Bounds.Width;
  Height := Bounds.Height;

  tx := 0;
  ty := aFunction(tx) * 0.5 + 0.5;

  x1 := Bounds.Left;
  y1 := Bounds.Bottom - (Height * ty);

  for c1 := 1 to Steps-1 do
  begin
    tx := c1 / (Steps-1);
    ty := aFunction(tx) * 0.5 + 0.5;

    x2 := Bounds.Left   + (tx * Width);
    y2 := Bounds.Bottom - (ty * Height);

    Canvas.Line(x1, y1, x2, y2);

    x1 := x2;
    y1 := y2;
  end;

  //Reset the clip box.
  Canvas.ClipBox(OldClipBox.X1, OldClipBox.Y1, OldClipBox.X2, OldClipBox.Y2);
end;

function Quantise(const x : single; const Steps : integer):single;
begin
  result := round(x * Steps) / Steps;
end;

function LinearInterpolation(const f, a, b : single):single;
begin
  result := a * (1-f) + b * f;
end;

function SymmetryMod(Input:single; ModAmount:single):single;
var
  a : single;
  x : single;
  y : single;
begin
  assert(InRange(ModAmount, 0, 1));
  assert(InRange(Input, -1, 1));
  a := 1-(1/ModAmount);
  x := Input * 0.5 + 0.5;
  y := x / (x+a*(x-1));
  result := y * 2 - 1;
end;


function EnvCurve(const StartValue, EndValue, EnvPhase, CurveAmount : single):single;
var
  a : single;
  x : single;
  y : single;
begin
  assert(InRange(EnvPhase, 0 ,1));
  assert(InRange(CurveAmount, 0, 1));

  a := 1-(1/CurveAmount);
  x := EnvPhase;
  y := x / (x+a*(x-1));
  result := StartValue + (EndValue - StartValue) * y;
end;


procedure DrawEnvCurve(Buffer: TRedFoxImageBuffer; x1, y1, x2, y2, CurveAmount : single);
var
  Steps : integer;
  Dist  : single;
  c1: Integer;
  dx1, dx2, dy1, dy2 : single;
  EnvPhase : single;
begin
  assert(x2 > x1);
  Steps := ceil(x2 - x1);
  Dist  := x2 - x1;

  dx1 := x1;
  dy1 := y1;

  for c1 := 1 to Steps-1 do
  begin
    EnvPhase := c1 / (Steps-1);
    dx2 := x1 + (EnvPhase * Dist);
    dy2 := EnvCurve(y1, y2, EnvPhase, CurveAmount);
    Buffer.BufferInterface.Line(dx1, dy1, dx2, dy2);

    dx1 := dx2;
    dy1 := dy2;
  end;
end;




{ TLucidityScope }

constructor TLucidityScope.Create(AOwner: TComponent);
begin
  inherited;
  fColorBackground := '$00000000';
  fColorBorder     := '$00000000';
  fColorForeground := '$00000000';

  SignalDisplay := TSignalDisplay.Create;
  SignalDisplay.LineColor := fColorForeground;

  FreqDisplay   := TFreqDisplay.Create;
  FreqDisplay.LineColor := fColorForeground;

  LfoValues.UpdateRandomValues;

  SignalAniID.Init;
  DiagramBufferAnimationID.Init;

  SignalOpacity := 255;


  DiagramBuffer := TRedFoxImageBuffer.Create;
end;

destructor TLucidityScope.Destroy;
begin
  DiagramBuffer.Free;
  SignalDisplay.Free;
  FreqDisplay.Free;
  inherited;
end;

function TLucidityScope.GetColors(const Index: Integer): TRedFoxColorString;
begin
  case Index of
  0: result := fColorBackground;
  1: result := fColorBorder;
  2: result := fColorForeground;
  else
    result := '$00000000';
  end;
end;

procedure TLucidityScope.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (aWidth > 40) and (aHeight > 40) then
  begin
    ScopeRect := Rect(8,8,aWidth-8,aHeight-24);

    if assigned(DiagramBuffer) then
    begin
      DiagramBuffer.SetSize(AWidth, AHeight);
      DiagramBuffer.BufferInterface.ClearAll(0,0,0,0);
    end;

    if assigned(SignalDisplay)
      then SignalDisplay.SetSize(ScopeRect.Width, ScopeRect.Height);

    if assigned(FreqDisplay)
      then FreqDisplay.SetSize(ScopeRect.Width, ScopeRect.Height);



  end else
  begin
    ScopeRect := Rect(0,0,aWidth,aHeight);
  end;
end;

procedure TLucidityScope.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColor;
begin
  case Index of
    2:
    begin
      SignalDisplay.LineColor := Value;
      FreqDisplay.LineColor := Value;
    end;
  end;

  case Index of
  0: pc := @fColorBackground;
  1: pc := @fColorBorder;
  2: pc := @fColorForeground;
  else
    pc := nil;
  end;

  if assigned(pc) and (pc^.AsString <> Value) then
  begin
    pc^.SetColor(Value);
    Invalidate;
  end;


end;


procedure TLucidityScope.SetScopeDisplayMode(const Value: TScopeDisplayMode);
var
  Animation : TByteAnimation;
begin
  if Value <> fScopeMode then
  begin
    fScopeMode := Value;
    Invalidate;
  end;


  if fScopeMode = TScopeDisplayMode.DisplayOff then
  begin
    Animation := TByteAnimation.Create;
    Animation.RunTime := 500;
    Animation.StartValue := SignalOpacity;
    Animation.EndValue   := 255;
    Animation.ApplyMethod := procedure(CurrentValue : byte)
    begin
      SignalOpacity := CurrentValue;
    end;
    GlobalAnimator.Animate(SignalAniID, Animation);


    Animation := TByteAnimation.Create;
    Animation.RunTime := 500;
    Animation.StartValue := DiagramBufferAlpha;
    Animation.EndValue   := 0;
    Animation.ApplyMethod := procedure(CurrentValue : byte)
    begin
      DiagramBufferAlpha := CurrentValue;
    end;
    GlobalAnimator.Animate(DiagramBufferAnimationID, Animation);

  end else
  begin
    Animation := TByteAnimation.Create;
    Animation.RunTime := 250;
    Animation.StartValue := SignalOpacity;
    Animation.EndValue   := 65;
    Animation.ApplyMethod := procedure(CurrentValue : byte)
    begin
      SignalOpacity := CurrentValue;
    end;
    GlobalAnimator.Animate(SignalAniID, Animation);

    Animation := TByteAnimation.Create;
    Animation.RunTime := 250;
    Animation.StartValue := DiagramBufferAlpha;
    Animation.EndValue   := 255;
    Animation.ApplyMethod := procedure(CurrentValue : byte)
    begin
      DiagramBufferAlpha := CurrentValue;
    end;
    GlobalAnimator.Animate(DiagramBufferAnimationID, Animation);
  end;





end;

procedure TLucidityScope.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TLucidityScope.Paint;
var
  TextBounds : TRect;
  x1, y1, x2, y2 : single;
begin
  inherited;



  LfoValues.UpdateRandomValues;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);

  //=== Paint the background ==
  x1 := 0;
  y1 := 0;
  x2 := Width;
  y2 := Height;

  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := fColorBackground;

  BackBuffer.BufferInterface.RoundedRect(x1, y1, x2, y2, 3);


  //=== draw the lower text ====
  //TODO: see if text draw can be improved by incorporating RedFoxTextBuffer.
  TextBounds := Rect(0,Height-20, Width, Height);
  BackBuffer.DrawText(Text, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextBounds);



  if not (csDesigning in Self.ComponentState) and (assigned(SignalRecorder))then
  begin
    SignalDisplay.ProcessSignal(BackBuffer, ScopeRect, SignalRecorder);
    SignalDisplay.DrawTo(BackBuffer, ScopeRect);

    BackBuffer.BufferInterface.FillColor := fColorBackground.WithAlpha(255-SignalOpacity);
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.Rectangle(ScopeRect.Left, ScopeRect.Top, ScopeRect.Right, ScopeRect.Bottom);
  end;


  if DiagramBufferAlpha > 0 then
  begin
    case ScopeMode of
      TScopeDisplayMode.ADSR:        Draw_ADSR;
      TScopeDisplayMode.LFO:         Draw_Lfo;
      TScopeDisplayMode.Filter:      Draw_Filter;
      TScopeDisplayMode.FilterBlend: Draw_FilterBlend;
    end;

    RedFox_AlphaBlit(BackBuffer.RedFoxInterface, DiagramBuffer.RedFoxInterface, 0, 0, DiagramBuffer.Width, DiagramBuffer.Height, 0, 0, DiagramBufferAlpha);
  end;


  BackBuffer.BufferInterface.LineColor := GetAggColor(clSilver);
  BackBuffer.BufferInterface.NoFill;

  //BackBuffer.BufferInterface.RoundedRect(ScopeRect.Left, ScopeRect.Top, ScopeRect.Right, ScopeRect.Bottom, 3);
end;

procedure TLucidityScope.Draw_ADSR;
begin
  DiagramBuffer.BufferInterface.ClearAll(0,0,0,0);
  TAdsrDrawingRoutines.Draw_Adsr(DiagramBuffer, ScopeRect, fColorForeground, AdsrValues);
end;

procedure TLucidityScope.Draw_Filter;
begin
  DiagramBuffer.BufferInterface.ClearAll(0,0,0,0);
  FreqDisplay.ProcessSignal(DiagramBuffer, ScopeRect, FreqAnalyzer);
end;

procedure TLucidityScope.Draw_FilterBlend;
begin
  DiagramBuffer.BufferInterface.ClearAll(0,0,0,0);

  case FilterBlendValues.FilterRouting of
    TFilterRouting.Serial:     TFilterBlendDrawingRoutines.Draw_Serial(DiagramBuffer, ScopeRect, Font, FilterBlendValues.BlendAmt, fColorForeground);
    TFilterRouting.Parallel:   TFilterBlendDrawingRoutines.Draw_Parallel(DiagramBuffer, ScopeRect, Font, FilterBlendValues.BlendAmt, fColorForeground);
    TFilterRouting.FiftyFifty: TFilterBlendDrawingRoutines.Draw_FiftyFifty(DiagramBuffer, ScopeRect, Font, FilterBlendValues.BlendAmt, fColorForeground);
  else
    raise Exception.Create('Type not handled.');
  end;
end;

procedure TLucidityScope.Draw_Lfo;
begin
  DiagramBuffer.BufferInterface.ClearAll(0,0,0,0);

  DiagramBuffer.BufferInterface.LineColor := fColorForeground;
  DiagramBuffer.BufferInterface.NoFill;
  DiagramBuffer.BufferInterface.LineWidth := 1.5;
  DiagramBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;

  case LfoValues.Shape of
    TLfoShape.SawUp:    TLfoDrawingRoutines.Draw_Lfo_SawUp(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.SawDown:  TLfoDrawingRoutines.Draw_Lfo_SawDown(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.Square:   TLfoDrawingRoutines.Draw_Lfo_Square(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.Triangle: TLfoDrawingRoutines.Draw_Lfo_Tri(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.Sine:     TLfoDrawingRoutines.Draw_Lfo_Sine(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.RandomStepped: TLfoDrawingRoutines.Draw_Lfo_RandomStepped(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.RandomSmooth:  TLfoDrawingRoutines.Draw_Lfo_RandomSmooth(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.AttackDecay:   TLfoDrawingRoutines.Draw_Lfo_AttackDecay(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.AttackRelease: TLfoDrawingRoutines.Draw_Lfo_AttackDecay(DiagramBuffer, ScopeRect, LfoValues);
    TLfoShape.Cycle:         TLfoDrawingRoutines.Draw_Lfo_Cycle(DiagramBuffer, ScopeRect, LfoValues);
  end;

end;

{ TLfoDrawingRoutines }



{ TLfoDrawingRoutines }

class procedure TLfoDrawingRoutines.Draw_Lfo_SawUp(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
var
  aFunction : TDrawFunction;
begin
  aFunction := function(x:single):single
  var
    tx : single;
  begin
    //Generate Ramp Up Wave
    tx := (x) * (LfoValues.Par1 * 10 + 3); //rate
    tx := tx + (LfoValues.Par2 * 2 - 1);  //Phase
    tx := Wrap(tx, -1, 1);
    tx := SymmetryMod(tx, (LfoValues.Par3 * 0.7 + 0.15));
    result := tx;
  end;

  DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width*8, aFunction);
end;

class procedure TLfoDrawingRoutines.Draw_Lfo_SawDown(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
var
  aFunction : TDrawFunction;
begin
  aFunction := function(x:single):single
  var
    tx : single;
  begin
    //Generate Ramp Up Wave
    tx := (x) * (LfoValues.Par1 * 10 + 3); //rate
    tx := tx + (LfoValues.Par2 * 2 - 1);  //Phase
    tx := Wrap(tx, -1, 1);
    tx := SymmetryMod(tx, (LfoValues.Par3 * 0.7 + 0.15));
    result := tx * -1;
  end;

  DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width*8, aFunction);
end;

class procedure TLfoDrawingRoutines.Draw_Lfo_Square(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
var
  aFunction : TDrawFunction;
begin
  aFunction := function(x:single):single
  var
    tx : single;
  begin
    //Generate Ramp Up Wave
    tx := (x) * (LfoValues.Par1 * 10 + 3); //rate
    tx := tx + (LfoValues.Par2 * 2 - 1);  //Phase
    tx := Wrap(tx, -1, 1);
    tx := SymmetryMod(tx, (LfoValues.Par3 * 0.7 + 0.15));
    result := -1 + (2 * Integer(tx >= 0));
  end;

  DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width*8, aFunction);
end;








class procedure TLfoDrawingRoutines.Draw_Lfo_Sine(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
var
  aFunction : TDrawFunction;
begin
  aFunction := function(x:single):single
  var
    tx : single;
  begin
    //Generate Ramp Up Wave
    tx := (x) * (LfoValues.Par1 * 10 + 3); //rate
    tx := tx + (LfoValues.Par2 * 2 - 1);  //Phase
    tx := Wrap(tx, -1, 1);
    tx := SymmetryMod(tx, (LfoValues.Par3 * 0.7 + 0.15));
    result := Sin(tx * pi);
  end;

  DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width*8, aFunction);
end;

class procedure TLfoDrawingRoutines.Draw_Lfo_Tri(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
var
  aFunction : TDrawFunction;
begin
  aFunction := function(x:single):single
  var
    tx : single;
  begin
    //Generate Ramp Up Wave
    tx := (x) * (LfoValues.Par1 * 10 + 3); //rate
    tx := tx + (LfoValues.Par2 * 2 - 1);  //Phase
    tx := Wrap(tx, -1, 1);
    tx := SymmetryMod(tx, (LfoValues.Par3 * 0.7 + 0.15));
    result := abs(tx) * 2 - 1;
  end;

  DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width*8, aFunction);
end;

class procedure TLfoDrawingRoutines.Draw_Lfo_AttackDecay(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
const
  kMinStageTime : single = 0.1;
var
  x1, y1 : single;
  //x2, y2 : single;
  //x3, y3 : single;
  x4, y4 : single;
  SectionWidth : single;
var
  CurveAmount : single;
begin
  SectionWidth := ScopeRect.Width / 2.3;


  CurveAmount := LfoValues.Par1 * 0.7 + 0.15;

  //== Draw Attack Stage ==
  x1 := ScopeRect.Left;
  y1 := ScopeRect.Bottom;
  x4 := x1 + SectionWidth * (LfoValues.Par2 + kMinStageTime);
  y4 := ScopeRect.Top;

  DrawEnvCurve(BackBuffer, x1,y1, x4,y4, CurveAmount);


  //== Draw Release Stage ==
  x1 := x4;
  y1 := y4;
  x4 := x1 + SectionWidth * (LfoValues.Par3 + kMinStageTime);
  y4 := ScopeRect.Bottom;

  DrawEnvCurve(BackBuffer, x1,y1, x4,y4, 1-CurveAmount);


  //== Draw Off Stage ==
  x1 := x4;
  y1 := y4;
  x4 := ScopeRect.Right;
  y4 := ScopeRect.Bottom;
  BackBuffer.BufferInterface.Line(x1,y1,x4,y4);
end;


class procedure TLfoDrawingRoutines.Draw_Lfo_Cycle(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
const
  kMinStageTime : single = 0.1;
var
  x1, y1 : single;
  //x2, y2 : single;
  //x3, y3 : single;
  x4, y4 : single;
  SectionWidth : single;
var
  CurveAmount : single;
begin
  SectionWidth := ScopeRect.Width / 2.3;


  CurveAmount := LfoValues.Par1 * 0.7 + 0.15;

  x1 := ScopeRect.Left;
  y1 := ScopeRect.Bottom;

  while x1 < ScopeRect.Right do
  begin
    //== Draw Attack Stage ==
    x4 := x1 + SectionWidth * (LfoValues.Par2 + kMinStageTime);
    y4 := ScopeRect.Top;
    DrawEnvCurve(BackBuffer, x1,y1, x4,y4, CurveAmount);

    //== Draw Release Stage ==
    x1 := x4;
    y1 := y4;
    x4 := x1 + SectionWidth * (LfoValues.Par3 + kMinStageTime);
    y4 := ScopeRect.Bottom;

    DrawEnvCurve(BackBuffer, x1,y1, x4,y4, 1-CurveAmount);

    x1 := x4;
    y1 := y4;
  end;

end;


class procedure TLfoDrawingRoutines.Draw_Lfo_RandomStepped(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
const
  kMinStageTime : single = 0.1;
var
  x1, y1 : single;
  x2, y2 : single;
  SectionWidth : single;
  RandomSeed : integer;
  ChanceOfNewValue : single;
  RandomNewOffset  : single;
  DoToggleChance   : single;
  Dist : single;
  yFrac : single;
begin
  RandomSeed := 0;
  SectionWidth := 30;

  x1 := ScopeRect.Left;
  y1 := ScopeRect.Bottom - (ScopeRect.Height * LfoValues.FakeRandom(RandomSeed));
  y2 := y1;
  //x2 := x1;

  while x1 <= ScopeRect.Right-2 do
  begin
    Dist := 3 + SectionWidth * LfoValues.Par1;
    x2 := x1 + Dist;
    if x2 >= ScopeRect.Right-1 then x2 := ScopeRect.Right-1;

    ChanceOfNewValue := LfoValues.FakeRandom(RandomSeed) - 0.1;
    RandomNewOffset  := LfoValues.FakeRandom(RandomSeed) - 0.5;
    DoToggleChance   := LfoValues.FakeRandom(RandomSeed);

    if ChanceOfNewValue <= LfoValues.Par2 then
    begin
      yFrac := (y1 - ScopeRect.Top) / ScopeRect.Height;
      yFrac := (yFrac - 0.5);
      if (sign(yFrac) = sign(RandomNewOffset)) and (abs(yFrac) > DoToggleChance) then
      begin
        RandomNewOffset := RandomNewOffset * -1;
      end;

      y2 := y1 + ScopeRect.Height * (RandomNewOffset) * LfoValues.Par3;
      y2 := Clamp(y2, ScopeRect.Top, ScopeRect.Bottom);
    end;

    BackBuffer.BufferInterface.Line(x1, y1, x1, y2);
    BackBuffer.BufferInterface.Line(x1, y2, x2, y2);

    x1 := x2;
    y1 := y2;
  end;
end;

class procedure TLfoDrawingRoutines.Draw_Lfo_RandomSmooth(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; LfoValues: TScopeLfoValues);
const
  kMinStageTime : single = 0.1;
var
  x1, y1 : single;
  x2, y2 : single;
  cx1, cy1 : single;
  cx2, cy2 : single;
  SectionWidth : single;
  RandomSeed : integer;
  ChanceOfNewValue : single;
  RandomNewOffset  : single;
  DoToggleChance   : single;
  Dist : single;
  yFrac : single;
begin
  RandomSeed := 0;
  SectionWidth := 30;

  x1 := ScopeRect.Left;
  y1 := ScopeRect.Bottom - (ScopeRect.Height * LfoValues.FakeRandom(RandomSeed));
  y2 := y1;
  //x2 := x1;

  while x1 <= ScopeRect.Right-2 do
  begin
    Dist := 3 + SectionWidth * LfoValues.Par1;
    x2 := x1 + Dist;
    if x2 >= ScopeRect.Right-1 then x2 := ScopeRect.Right-1;

    ChanceOfNewValue := LfoValues.FakeRandom(RandomSeed) - 0.1;
    RandomNewOffset  := LfoValues.FakeRandom(RandomSeed) - 0.5;
    DoToggleChance   := LfoValues.FakeRandom(RandomSeed);

    if ChanceOfNewValue <= LfoValues.Par2 then
    begin
      yFrac := (y1 - ScopeRect.Top) / ScopeRect.Height;
      yFrac := (yFrac - 0.5);
      if (sign(yFrac) = sign(RandomNewOffset)) and (abs(yFrac) > DoToggleChance) then
      begin
        RandomNewOffset := RandomNewOffset * -1;
      end;

      y2 := y1 + ScopeRect.Height * (RandomNewOffset) * LfoValues.Par3;
      y2 := Clamp(y2, ScopeRect.Top, ScopeRect.Bottom);
    end;

    cx1 := x1 + (Dist * 0.5);
    cy1 := y1;

    cx2 := x2 - (Dist * 0.5);
    cy2 := y2;

    BackBuffer.BufferInterface.Curve(x1, y1, cx1, cy1, cx2, cy2, x2, y2);

    x1 := x2;
    y1 := y2;
  end;

end;


{ TScopeLfoValues }

function TScopeLfoValues.FakeRandom(var Seed: integer): single;
begin
  result := self.RandomValues[seed];
  inc(Seed);
  if seed >= kRandomValueCount
    then seed := 0;

end;

procedure TScopeLfoValues.UpdateRandomValues;
var
  c1: Integer;
begin
  if (self.IsRandomInitialised = false) or (MilliSecondsBetween(Now, self.LastRandomUpdate) > 900) then
  begin
    self.LastRandomUpdate := Now;
    self.IsRandomInitialised := true;
    for c1 := 0 to kRandomValueCount-1 do
    begin
      self.RandomValues[c1] := Random;
    end;
  end;
end;

{ TFilterBlendDrawingRoutines }


{ TFilterBlendDrawingRoutines }

class procedure TFilterBlendDrawingRoutines.Draw_SignalTriangleDown(BackBuffer: TRedFoxImageBuffer; TargetPointX, TargetPointY: single);
var
  TriPoints : array[0..2] of TPointDouble;
begin
  TriPoints[0].X := TargetPointX;
  TriPoints[0].Y := TargetPointY;
  TriPoints[1].X := TargetPointX-2.5;
  TriPoints[1].Y := TargetPointY-5.5;
  TriPoints[2].X := TargetPointX+2.5;
  TriPoints[2].Y := TargetPointY-5.5;
  BackBuffer.BufferInterface.Polygon(@TriPoints[0], 3);
end;

class procedure TFilterBlendDrawingRoutines.Draw_SignalTriangleRight(BackBuffer: TRedFoxImageBuffer; TargetPointX, TargetPointY: single);
var
  TriPoints : array[0..2] of TPointDouble;
begin
  TriPoints[0].X := TargetPointX;
  TriPoints[0].Y := TargetPointY;
  TriPoints[1].X := TargetPointX-5.5;
  TriPoints[1].Y := TargetPointY-2.5;
  TriPoints[2].X := TargetPointX-5.5;
  TriPoints[2].Y := TargetPointY+2.5;
  BackBuffer.BufferInterface.Polygon(@TriPoints[0], 3);
end;

class procedure TFilterBlendDrawingRoutines.Draw_BlendAmountLines(BackBuffer: TRedFoxImageBuffer;
  ScopeRect: TRect; Font: TFont; const BlendAmt: single; Color: TRedFoxColor; out Box1, Box2, Box3: TRect);
var
  x1, y1, x2, y2 : single;
  cx1, cy1, cx2, cy2 : single;
  s : string;
  TextRect : TRect;
  px1, py1 : single;
  BlendFactor1 : single;
  BlendFactor2 : single;
begin
  if BlendAmt = 0 then
  begin
    BlendFactor1 := 1;
    BlendFactor2 := 0;
  end else
  if BlendAmt >= 1 then
  begin
    BlendFactor1 := 0;
    BlendFactor2 := 1;
  end else
  begin
    BlendFactor1 := 0.2 + 0.8 * (1-BlendAmt);
    BlendFactor2 := 0.2 + 0.8 * BlendAmt;
  end;



  if BlendFactor1 > 0 then
  begin
    BackBuffer.BufferInterface.LineWidth := 2 * BlendFactor1;

    x1 := Box1.Right;
    y1 := Box1.Top + Box1.Height * 0.5;
    x2 := Box3.Left;
    y2 := Box3.Top + (Box3.Height * 0.25);

    cx1 := x1 + 20;
    cy1 := y1;
    cx2 := x2 - 40;
    cy2 := y2;
    BackBuffer.BufferInterface.Curve(x1, y1, cx1, cy1, cx2, cy2, x2, y2);
    Draw_SignalTriangleRight(BackBuffer, x2, y2);

    px1 := Box1.Right + 44;
    py1 := ScopeRect.Top  + (1/5 * ScopeRect.Height);

    TextRect.Left   := round(px1 - 20);
    TextRect.Right  := round(px1 + 20);
    TextRect.Top    := round(py1 - 20);
    TextRect.Bottom := round(py1 + 20);

    s := IntToStr(Round((1-BlendAmt) * 100)) + '%';

    BackBuffer.DrawText(s, Font, TRedFoxAlign.AlignNear, TRedFoxAlign.AlignCenter, TextRect);
  end;


  if BlendFactor2 > 0 then
  begin
    BackBuffer.BufferInterface.LineWidth := 2 * BlendFactor2;

    x1 := Box2.Right;
    y1 := Box2.Top + Box2.Height * 0.5;
    x2 := Box3.Left;
    y2 := Box3.Bottom - (Box3.Height * 0.25);

    cx1 := x1 + 20;
    cy1 := y1;
    cx2 := x2 - 40;
    cy2 := y2;
    BackBuffer.BufferInterface.Curve(x1, y1, cx1, cy1, cx2, cy2, x2, y2);
    Draw_SignalTriangleRight(BackBuffer, x2, y2);


    px1 := Box1.Right + 44;
    py1 := ScopeRect.Top  + (4/5 * ScopeRect.Height);

    TextRect.Left   := round(px1 - 20);
    TextRect.Right  := round(px1 + 20);
    TextRect.Top    := round(py1 - 20);
    TextRect.Bottom := round(py1 + 20);

    s := IntToStr(Round(BlendAmt * 100)) + '%';

    BackBuffer.DrawText(s, Font, TRedFoxAlign.AlignNear, TRedFoxAlign.AlignCenter, TextRect);
  end;






end;

class procedure TFilterBlendDrawingRoutines.Draw_Boxes(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; Font: TFont; const BlendAmt: single; Color:TRedFoxColor; out Box1, Box2, Box3: TRect);
var
  tw : single;
  th : single;
  TextRect : TRect;
  px1, py1 : single;
begin
  //==== Filter One =====
  tw := BackBuffer.TextWidth('Filter 1') + 7;
  th := BackBuffer.TextHeight + 7;

  px1 := ScopeRect.Left + (1/4 * ScopeRect.Width);
  py1 := ScopeRect.Top  + (1/5 * ScopeRect.Height);

  TextRect.Left   := round(px1 - 20);
  TextRect.Right  := round(px1 + 20);
  TextRect.Top    := round(py1 - 20);
  TextRect.Bottom := round(py1 + 20);

  Box1.Left   := round(px1 - tw * 0.5);
  Box1.Right  := round(px1 + tw * 0.5);
  Box1.Top    := round(py1 - th * 0.5);
  Box1.Bottom := round(py1 + th * 0.5);


  BackBuffer.BufferInterface.LineColor := Color;
  BackBuffer.BufferInterface.FillColor := Color;
  BackBuffer.DrawText('Filter 1', Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextRect);

  BackBuffer.BufferInterface.LineColor := Color;
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.Rectangle(Box1.Left - 0.5, Box1.Top + 0.5, Box1.Right - 0.5, Box1.Bottom - 0.5);




  //==== Filter Two =====
  tw := BackBuffer.TextWidth('Filter 2') + 7;
  th := BackBuffer.TextHeight + 7;

  px1 := ScopeRect.Left + (1/4 * ScopeRect.Width);
  py1 := ScopeRect.Top  + (4/5 * ScopeRect.Height);

  TextRect.Left   := round(px1 - 20);
  TextRect.Right  := round(px1 + 20);
  TextRect.Top    := round(py1 - 20);
  TextRect.Bottom := round(py1 + 20);

  Box2.Left   := round(px1 - tw * 0.5);
  Box2.Right  := round(px1 + tw * 0.5);
  Box2.Top    := round(py1 - th * 0.5);
  Box2.Bottom := round(py1 + th * 0.5);

  BackBuffer.BufferInterface.LineColor := Color;
  BackBuffer.BufferInterface.FillColor := Color;
  BackBuffer.DrawText('Filter 2', Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextRect);

  BackBuffer.BufferInterface.LineColor := Color;
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.Rectangle(Box2.Left - 0.5, Box2.Top + 0.5, Box2.Right - 0.5, Box2.Bottom - 0.5);





  //==== Output =====
  tw := BackBuffer.TextWidth('Output') + 7;
  th := BackBuffer.TextHeight + 7;

  px1 := ScopeRect.Left + (4/5 * ScopeRect.Width);
  py1 := ScopeRect.Top  + (1/2 * ScopeRect.Height);

  TextRect.Left   := round(px1 - 20);
  TextRect.Right  := round(px1 + 20);
  TextRect.Top    := round(py1 - 20);
  TextRect.Bottom := round(py1 + 20);

  Box3.Left   := round(px1 - tw * 0.5);
  Box3.Right  := round(px1 + tw * 0.5);
  Box3.Top    := round(py1 - th * 0.5);
  Box3.Bottom := round(py1 + th * 0.5);

  BackBuffer.BufferInterface.LineColor := Color;
  BackBuffer.BufferInterface.FillColor := Color;
  BackBuffer.DrawText('Output', Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextRect);

  BackBuffer.BufferInterface.LineColor := Color;
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.Rectangle(Box3.Left - 0.5, Box3.Top + 0.5, Box3.Right - 0.5, Box3.Bottom - 0.5);
end;

class procedure TFilterBlendDrawingRoutines.Draw_InputOutputLines(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; Color: TRedFoxColor; FR : TFilterRouting; const Box1, Box2, Box3: TRect);
var
  x1, y1, x2, y2 : single;
begin
  //== draw the in out arrows ==
  BackBuffer.BufferInterface.FillColor := color;

  x1 := ScopeRect.Left;
  x2 := Box1.Left;
  y1 := Box1.Top + Box1.Height * 0.5;
  y1 := round(y1) - 0.5;
  y2 := y1;

  BackBuffer.BufferInterface.Line(x1, y1, x2, y2);
  Draw_SignalTriangleRight(BackBuffer, x2, y2);


  x1 := Box3.Right;
  x2 := ScopeRect.Right;
  y1 := Box3.Top + Box3.Height * 0.5;
  y1 := round(y1) - 0.5;
  y2 := y1;

  BackBuffer.BufferInterface.Line(x1, y1, x2, y2);
  Draw_SignalTriangleRight(BackBuffer, x2, y2);


  if (FR = TFilterRouting.Parallel) or (FR = TFilterRouting.FiftyFifty) then
  begin
    x1 := ScopeRect.Left;
    x2 := Box2.Left;
    y1 := Box2.Top + Box2.Height * 0.5;
    y1 := round(y1) - 0.5;
    y2 := y1;

    BackBuffer.BufferInterface.Line(x1, y1, x2, y2);
    Draw_SignalTriangleRight(BackBuffer, x2, y2);
  end;

  if (FR = TFilterRouting.Serial) or (FR = TFilterRouting.FiftyFifty) then
  begin
    x1 := Box1.Left + Box1.Width * 0.5;
    x1 := round(x1) - 0.5;
    x2 := x1;
    y1 := Box1.Bottom;
    y2 := Box2.Top;

    BackBuffer.BufferInterface.Line(x1, y1, x2, y2);
    Draw_SignalTriangleDown(BackBuffer, x2, y2);
  end;

end;




class procedure TFilterBlendDrawingRoutines.Draw_FiftyFifty(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; Font : TFont; const BlendAmt: single; Color:TRedFoxColor);
var
  Box1, Box2, Box3 : TRect;
begin
  BackBuffer.UpdateFont(Font);
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;

  Draw_Boxes(BackBuffer, ScopeRect, Font, BlendAmt, Color, Box1, Box2, Box3);
  Draw_InputOutputLines(BackBuffer, ScopeRect, Color, TFilterRouting.FiftyFifty, Box1, Box2, Box3);
  Draw_BlendAmountLines(BackBuffer, ScopeRect, Font, BlendAmt, Color, Box1, Box2, Box3);
end;

class procedure TFilterBlendDrawingRoutines.Draw_Parallel(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; Font : TFont; const BlendAmt: single; Color:TRedFoxColor);
var
  Box1, Box2, Box3 : TRect;
begin
  BackBuffer.UpdateFont(Font);
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;

  Draw_Boxes(BackBuffer, ScopeRect, Font, BlendAmt, Color, Box1, Box2, Box3);
  Draw_InputOutputLines(BackBuffer, ScopeRect, Color, TFilterRouting.Parallel, Box1, Box2, Box3);
  Draw_BlendAmountLines(BackBuffer, ScopeRect, Font, BlendAmt, Color, Box1, Box2, Box3);
end;

class procedure TFilterBlendDrawingRoutines.Draw_Serial(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; Font : TFont; const BlendAmt: single; Color:TRedFoxColor);
var
  Box1, Box2, Box3 : TRect;
begin
  BackBuffer.UpdateFont(Font);
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;

  Draw_Boxes(BackBuffer, ScopeRect, Font, BlendAmt, Color, Box1, Box2, Box3);
  Draw_InputOutputLines(BackBuffer, ScopeRect, Color, TFilterRouting.Serial, Box1, Box2, Box3);
  Draw_BlendAmountLines(BackBuffer, ScopeRect, Font, BlendAmt, Color, Box1, Box2, Box3);
end;



{ TAdsrDrawingRoutines }

class procedure TAdsrDrawingRoutines.Draw_Adsr(BackBuffer: TRedFoxImageBuffer; ScopeRect: TRect; Color : TRedfoxColor; AdsrValues: TScopeAdsrValues);
const
  kMinStageTime : single = 0.1;
var
  x1, y1 : single;
  x2, y2 : single;
  x3, y3 : single;
  x4, y4 : single;
  SectionWidth : single;
begin
  BackBuffer.BufferInterface.LineColor := Color; //fColorForeground;
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineWidth := 1.5;
  BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;

  SectionWidth := ScopeRect.Width / 5;

  //== Draw Attack Stage ==
  x1 := ScopeRect.Left;
  y1 := ScopeRect.Bottom;
  x4 := x1 + SectionWidth * (AdsrValues.Attack + kMinStageTime);
  y4 := ScopeRect.Top;

  //BackBuffer.BufferInterface.Line(x1,y1,x4,y4);
  x2 := x1 + (x4 - x1) * 1/3;
  y2 := y1 + (y4 - y1) * 2.5/3;
  x3 := x1 + (x4 - x1) * 2/3;
  y3 := y1 + (y4 - y1) * 3/3;
  BackBuffer.BufferInterface.Curve(x1, y1, x2, y2, x3, y3, x4, y4);

  //== Draw Hold Stage ==
  x1 := x4;
  y1 := y4;
  x4 := x1 + SectionWidth * AdsrValues.Hold * 0.5;
  y4 := ScopeRect.Top;
  //BackBuffer.BufferInterface.Line(x1,y1,x4,y4);
  x2 := x1 + (x4 - x1) * 1/3;
  y2 := y1 + (y4 - y1) * 2.5/3;
  x3 := x1 + (x4 - x1) * 2/3;
  y3 := y1 + (y4 - y1) * 3/3;
  BackBuffer.BufferInterface.Curve(x1, y1, x2, y2, x3, y3, x4, y4);


  //== Draw Decay Stage ==
  x1 := x4;
  y1 := y4;
  x4 := x1 + SectionWidth * (AdsrValues.Decay + kMinStageTime);
  y4 := ScopeRect.Top + ScopeRect.Height * (1 - AdsrValues.Sustain);
  //BackBuffer.BufferInterface.Line(x1,y1,x4,y4);
  x2 := x1 + (x4 - x1) * 1/3;
  y2 := y1 + (y4 - y1) * 2.5/3;
  x3 := x1 + (x4 - x1) * 2/3;
  y3 := y1 + (y4 - y1) * 3/3;
  BackBuffer.BufferInterface.Curve(x1, y1, x2, y2, x3, y3, x4, y4);

  //== Draw Sustain Stage ==
  x1 := x4;
  y1 := y4;
  x4 := x1 + SectionWidth * ((1 - AdsrValues.Attack) + (1 - AdsrValues.Decay) + (1 - AdsrValues.Hold * 0.5));
  y4 := ScopeRect.Top + ScopeRect.Height * (1 - AdsrValues.Sustain);
  BackBuffer.BufferInterface.Line(x1,y1,x4,y4);



  //== Draw Release Stage ==
  x1 := x4;
  y1 := y4;
  x4 := x1 + SectionWidth * (AdsrValues.Release + kMinStageTime);
  y4 := ScopeRect.Bottom;
  //BackBuffer.BufferInterface.Line(x1,y1,x4,y4);
  x2 := x1 + (x4 - x1) * 1/3;
  y2 := y1 + (y4 - y1) * 2.5/3;
  x3 := x1 + (x4 - x1) * 2/3;
  y3 := y1 + (y4 - y1) * 3/3;
  BackBuffer.BufferInterface.Curve(x1, y1, x2, y2, x3, y3, x4, y4);


  //== Draw Off Stage ==
  x1 := x4;
  y1 := y4;
  x4 := ScopeRect.Right;
  y4 := ScopeRect.Bottom;
  BackBuffer.BufferInterface.Line(x1,y1,x4,y4);
end;

end.
