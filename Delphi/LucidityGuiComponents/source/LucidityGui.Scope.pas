unit LucidityGui.Scope;

interface

uses
  //=================================================
  // NOTE: Any Lucidity specific units must be
  // drawn from the Ludcidity Base Classes project.
  // I want to avoid circural type depencies where
  uLucidityEnums,
  //=================================================
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
  AggBasics;

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
    //TScopeDisplayMode.DisplayOff: ;
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
var
  x1, y1, x4, y4 : single;
  x2, y2, x3, y3 : single;
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
  x4 := x1 + SectionWidth * AdsrValues.Attack;
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
  x4 := x1 + SectionWidth * AdsrValues.Hold;
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
  x4 := x1 + SectionWidth * AdsrValues.Decay;
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
  x4 := x1 + SectionWidth;
  y4 := ScopeRect.Top + ScopeRect.Height * (1 - AdsrValues.Sustain);
  BackBuffer.BufferInterface.Line(x1,y1,x4,y4);
  


  //== Draw Release Stage ==
  x1 := x4;
  y1 := y4;
  x4 := x1 + SectionWidth * AdsrValues.Release;
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
begin

end;

procedure TLucidityScope.Draw_FilterBlend;
begin

end;

procedure TLucidityScope.Draw_Lfo;
begin

end;




end.
