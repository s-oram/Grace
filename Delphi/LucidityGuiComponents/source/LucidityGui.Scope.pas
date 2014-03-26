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

    procedure Draw_ADSR;
    procedure Draw_Lfo;

    procedure Draw_Filter;
    procedure Draw_FilterBlend;
    procedure Draw_Spectrum;
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



implementation

uses
  DateUtils,
  SysUtils,
  Math,
  VamLib.Utils,
  Agg2D,
  AggBasics;

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

  SignalOpacity := 255;
end;

destructor TLucidityScope.Destroy;
begin
  FreeAndNil(SignalDisplay);
  FreeAndNil(FreqDisplay);
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
  animation : TByteAnimation;
begin
  if Value <> fScopeMode then
  begin
    fScopeMode := Value;
    Invalidate;
  end;


  if fScopeMode = TScopeDisplayMode.DisplayOff then
  begin
    animation := TByteAnimation.Create;
    Animation.RunTime := 500;
    Animation.StartValue := SignalOpacity;
    Animation.EndValue   := 255;
    Animation.ApplyMethod := procedure(CurrentValue : byte)
    begin
      SignalOpacity := CurrentValue;
    end;
    GlobalAnimator.Animate(SignalAniID, Animation);
  end else
  begin
    animation := TByteAnimation.Create;
    Animation.RunTime := 150;
    Animation.StartValue := SignalOpacity;
    Animation.EndValue   := 100;
    Animation.ApplyMethod := procedure(CurrentValue : byte)
    begin
      SignalOpacity := CurrentValue;
    end;
    GlobalAnimator.Animate(SignalAniID, Animation);
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

  case ScopeMode of
    //TScopeDisplayMode.DisplayOff:  SignalDisplay.DrawTo(BackBuffer, ScopeRect);
    TScopeDisplayMode.ADSR:        Draw_ADSR;
    TScopeDisplayMode.LFO:         Draw_Lfo;
    TScopeDisplayMode.Filter:      Draw_Filter;
    TScopeDisplayMode.FilterBlend: Draw_FilterBlend;
  end;


  BackBuffer.BufferInterface.LineColor := GetAggColor(clSilver);
  BackBuffer.BufferInterface.NoFill;

  //BackBuffer.BufferInterface.RoundedRect(ScopeRect.Left, ScopeRect.Top, ScopeRect.Right, ScopeRect.Bottom, 3);
end;

procedure TLucidityScope.Draw_ADSR;
const
  kMinStageTime : single = 0.1;
var
  x1, y1 : single;
  x2, y2 : single;
  x3, y3 : single;
  x4, y4 : single;
  SectionWidth : single;
begin
  BackBuffer.BufferInterface.LineColor := fColorForeground;
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

procedure TLucidityScope.Draw_Filter;
var
  aFunction : TDrawFunction;
begin
  BackBuffer.BufferInterface.LineColor := fColorForeground;
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineWidth := 1.5;
  BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;

  Draw_Spectrum;

  {
  case FilterValues.FilterType of
    ftNone:
    begin

    end;

    ftLowPassA:
    begin
      aFunction := function(x:single):single
      const
        a = 1 - (1/0.1);
      var
        ScaledX : single;
        ResPeak : single;
        ResPeak2 : single;
        BaseValue : single;
      begin
        ResPeak := (0.7 - Power((FilterValues.Par1 - x),2) * 1800 * FilterValues.Par2 * FilterValues.Par2) * FilterValues.Par2;

        if (x < FilterValues.Par1) then
        begin
          ScaledX := 1 + (X - FilterValues.Par1);
          ResPeak2 := ScaledX / (ScaledX + a * (ScaledX - 1));
          ResPeak2 := ResPeak2;
          ResPeak2 := 0.3 + (ResPeak2 * 0.7) * FilterValues.Par2;
          result := ResPeak2;
        end else
        begin
          BaseValue := 0.3 - Power((FilterValues.Par1 - x),2) * 24;
          result := BaseValue + ResPeak;
        end;
      end;

      DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width, aFunction);
    end;

    ftBandPassA:
    begin
      aFunction := function(x:single):single
      const
        a = 1 - (1/0.1);
      var
        Cutoff : single;
        Res : single;
        ScaledX : single;
        ResPeak : single;
        ResPeak2 : single;
        BaseValue : single;
      begin
        CutOff := FilterValues.Par1;
        Res    := FilterValues.Par2 * 0.7 + 0.3;

        ResPeak := (0.7 - Power((Cutoff - x),2) * 1800 * Sqr(Res)) * Res;
        result := 0.3 + ResPeak;
      end;

      DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width, aFunction);

    end;

    ftHighPassA:
    begin
      aFunction := function(x:single):single
      const
        a = 1 - (1/0.1);
      var
        ScaledX : single;
        ResPeak : single;
        ResPeak2 : single;
        BaseValue : single;
      begin
        ResPeak := (0.7 - Power((FilterValues.Par1 - x),2) * 1800 * FilterValues.Par2 * FilterValues.Par2) * FilterValues.Par2;

        if (x > FilterValues.Par1) then
        begin
          ScaledX := 1 + (FilterValues.Par1 - X);
          ResPeak2 := ScaledX / (ScaledX + a * (ScaledX - 1));
          ResPeak2 := ResPeak2;
          ResPeak2 := 0.3 + (ResPeak2 * 0.7) * FilterValues.Par2;
          result := ResPeak2;
        end else
        begin
          BaseValue := 0.3 - Power((FilterValues.Par1 - x),2) * 24;
          result := BaseValue + ResPeak;
        end;
      end;

      DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width, aFunction);
    end;



    ftLofiA:
    begin

    end;

    ftRingModA:
    begin
      aFunction := function(x:single):single
        function TriEnvelope(const Cutoff, X:single):single;
        begin
          result := 1 - abs(Cutoff - x);
        end;
        function SmoothEnvelope(const Cutoff, X:single):single;
        const
          a = 1 - (1/0.23);
        var
          Env : single;
        begin
          Env := 1 - Clamp((abs((Cutoff - x) * 4)), 0, 1);
          Env := Env / (Env + a * (Env - 1));
          result := Env;
        end;
        function Peaks(const Cutoff, X:single):single;
        var
          Env : single;
        begin
          Env := ((2 * pi * (x - Cutoff)) * 12);
          Env := abs(sin(Env));
          result := Env;
        end;
      var
        Cutoff : single;
        Res : single;
        Env : single;
      begin
        CutOff := FilterValues.Par1;
        Res    := FilterValues.Par2;

        Cutoff := Quantise(Cutoff, ScopeRect.Width-1);
        x      := Quantise(x, ScopeRect.Width-1);

        Env := Peaks(Cutoff, x) * SmoothEnvelope(Cutoff, x) * 2.3 - 1.1;

        result := LinearInterpolation(Res, 0.3, Env);
      end;

      DrawFunction(BackBuffer.BufferInterface, ScopeRect, ScopeRect.Width, aFunction);

    end;

    ftCombA:
    begin

    end;

    ft2PoleLowPass,
    ft2PoleBandPass,
    ft2PoleHighPass,
    ft4PoleLowPass,
    ft4PoleBandPass,
    ft4PoleHighPass:
    begin
      Draw_Spectrum;
    end;
  end;
  }
end;

procedure TLucidityScope.Draw_FilterBlend;
begin

end;

procedure TLucidityScope.Draw_Lfo;
var
  aFunction : TDrawFunction;
begin
  BackBuffer.BufferInterface.LineColor := fColorForeground;
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineWidth := 1.5;
  BackBuffer.BufferInterface.LineCap := TAggLineCap.lcButt;



  case LfoValues.Shape of
    TLfoShape.SawUp:    TLfoDrawingRoutines.Draw_Lfo_SawUp(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.SawDown:  TLfoDrawingRoutines.Draw_Lfo_SawDown(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.Square:   TLfoDrawingRoutines.Draw_Lfo_Square(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.Triangle: TLfoDrawingRoutines.Draw_Lfo_Tri(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.Sine:     TLfoDrawingRoutines.Draw_Lfo_Sine(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.RandomStepped: TLfoDrawingRoutines.Draw_Lfo_RandomStepped(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.RandomSmooth:  TLfoDrawingRoutines.Draw_Lfo_RandomSmooth(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.AttackDecay:   TLfoDrawingRoutines.Draw_Lfo_AttackDecay(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.AttackRelease: TLfoDrawingRoutines.Draw_Lfo_AttackDecay(BackBuffer, ScopeRect, LfoValues);
    TLfoShape.Cycle:         TLfoDrawingRoutines.Draw_Lfo_Cycle(BackBuffer, ScopeRect, LfoValues);
  end;




end;

procedure TLucidityScope.Draw_Spectrum;
begin
  FreqDisplay.ProcessSignal(BackBuffer, ScopeRect, FreqAnalyzer);
  FreqDisplay.DrawTo(BackBuffer, ScopeRect);
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
  x2, y2 : single;
  x3, y3 : single;
  x4, y4 : single;
  SectionWidth : single;
var
  aFunction : TDrawFunction;
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
  x2, y2 : single;
  x3, y3 : single;
  x4, y4 : single;
  SectionWidth : single;
var
  aFunction : TDrawFunction;
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
  x2 := x1;

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
  x2 := x1;

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

end.
