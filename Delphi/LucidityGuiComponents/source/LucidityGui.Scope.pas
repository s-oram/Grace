unit LucidityGui.Scope;

interface

uses
  //=================================================
  // NOTE: Any Lucidity specific units must be
  // drawn from the Ludcidity Base Classes project.
  // I want to avoid circural type depencies where
  uLucidityEnums,
  //=================================================
  LucidityGui.Scope.SignalRecorder,
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;


type
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
    Shape : TLfoShape;
    Par1  : single;
    Par2  : single;
    Par3  : single;
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
    function GetColors(const Index: Integer): TRedFoxColorString;
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetText(const Value: string);
    procedure SetScopeDisplayMode(const Value: TScopeDisplayMode);

  protected
    fColorBackground : TRedFoxColor;
    fColorBorder     : TRedFoxColor;
    fColorForeground : TRedFoxColor;

    ScopeRect : TRect;

    procedure Draw_ADSR;
    procedure Draw_Lfo;
    procedure Draw_Filter;
    procedure Draw_FilterBlend;
    procedure Draw_Signal(Source : IScopeSignalRecorder);
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

  published
    property ColorBackground : TRedFoxColorString index 0 read GetColors write SetColors;
    property ColorBorder     : TRedFoxColorString index 1 read GetColors write SetColors;
    property ColorForeground : TRedFoxColorString index 2 read GetColors write SetColors;

    property ScopeMode : TScopeDisplayMode read fScopeMode write SetScopeDisplayMode;
    property Font;
    property Text : string read fText write SetText;

    {$INCLUDE TControlProperties.inc}
  end;



implementation

uses
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


{ TLucidityScope }

constructor TLucidityScope.Create(AOwner: TComponent);
begin
  inherited;
  fColorBackground := '$00000000';
  fColorBorder     := '$00000000';
  fColorForeground := '$00000000';
end;

destructor TLucidityScope.Destroy;
begin

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


end;

procedure TLucidityScope.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColor;
begin
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
begin
  if Value <> fScopeMode then
  begin
    fScopeMode := Value;
    Invalidate;
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




  ScopeRect := Rect(8,8,Width-8,Height-24);

  case ScopeMode of
    TScopeDisplayMode.DisplayOff:  Draw_Signal(SignalRecorder);
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
  end;
end;

procedure TLucidityScope.Draw_FilterBlend;
begin

end;

procedure TLucidityScope.Draw_Lfo;
begin

end;




procedure TLucidityScope.Draw_Signal(Source: IScopeSignalRecorder);
begin

end;

end.
