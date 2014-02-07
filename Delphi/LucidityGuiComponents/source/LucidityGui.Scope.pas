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
    Filter
  );

  TScopeAdsrValues = record
    Attack  : single;
    Hold    : single;
    Decay   : single;
    Sustain : single;
    Release : single;
  end;

  TScopeLfoValues = record
    //Shape
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
  public
    AdsrValues : TScopeAdsrValues;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;


    //== AHDSR values ==


    //== Lfo values ==

    //== Filter values ==

  published
    property ColorBackground : TRedFoxColorString index 0 read GetColors write SetColors;
    property ColorBorder     : TRedFoxColorString index 1 read GetColors write SetColors;

    property ScopeMode : TScopeDisplayMode read fScopeMode write SetScopeDisplayMode;
    property Font;
    property Text : string read fText write SetText;

    {$INCLUDE TControlProperties.inc}
  end;



implementation

{ TLucidityScope }

constructor TLucidityScope.Create(AOwner: TComponent);
begin
  inherited;
  fColorBackground := '$00000000';
  fColorBorder     := '$00000000';
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
  else
    result := '$00000000';
  end;
end;

procedure TLucidityScope.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColor;
begin
  case Index of
  0: pc := @fColorBackground;
  1: pc := @fColorBorder;
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
end;


end.
