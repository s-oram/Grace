unit VamStatusLed;

interface

uses
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamWinControl;

type
  TVamStatusLed = class(TVamWinControl)
  private
    fColor_Low: TRedFoxColorString;
    fColor_High: TRedFoxColorString;
    fColor_Mid: TRedFoxColorString;
    fLedValue: single;
    fLedSize: single;
    fIntenseWidth: single;

    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetLedValue(const Value: single);
    procedure SetLedSize(const Value: single);
    procedure SetIntenseWidth(const Value: single);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Color_Low  : TRedFoxColorString index 0 read fColor_Low  write SetColors;
    property Color_Mid  : TRedFoxColorString index 1 read fColor_Mid  write SetColors;
    property Color_High : TRedFoxColorString index 2 read fColor_High write SetColors;

    property LedSize  : single read fLedSize  write SetLedSize;  // in pixels
    property LedValue : single read fLedValue write SetLedValue; //range -1..1
    property IntenseWidth : single read fIntenseWidth write SetIntenseWidth;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils,
  AggPixelFormat;

{ TVamStatusLed }

constructor TVamStatusLed.Create(AOwner: TComponent);
begin
  inherited;

  fLedSize := 8;

  fColor_Low  := '$ffFF0000';
  fColor_Mid  := '$ff000000';
  fColor_High := '$ff00FF00';

  fIntenseWidth := 2;
end;

destructor TVamStatusLed.Destroy;
begin

  inherited;
end;

procedure TVamStatusLed.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColorString;
begin
  case Index of
    0 : pc := @fColor_Low;
    1 : pc := @fColor_Mid;
    2 : pc := @fColor_High;
  else
    raise Exception.Create('Index not handled.');
  end;

  if pc^ <> Value then
  begin
    pc^ := Value;
    Invalidate;
  end;
end;

procedure TVamStatusLed.SetIntenseWidth(const Value: single);
begin
  if Value <> fIntenseWidth then
  begin
    fIntenseWidth := Value;
    Invalidate;
  end;
end;

procedure TVamStatusLed.SetLedSize(const Value: single);
begin
  if fLedSize <> Value then
  begin
    fLedSize := Value;
    Invalidate;
  end;
end;

procedure TVamStatusLed.SetLedValue(const Value: single);
begin
  assert(Value >= -1);
  assert(Value <= 1);

  if fLedValue <> Value then
  begin
    fLedValue := Value;
    Invalidate;
  end;

end;

procedure TVamStatusLed.Paint;
var
  fx : single;
  cx, cy, Radius, Radius2 : single;
  tc : TRedFoxColor;
  LedIntensity : byte;
begin
  inherited;



  if LedValue <= -1 then
  begin
    tc := Color_Low;
    LedIntensity := 255;
  end
  else if LedValue = 0 then
  begin
    tc := Color_Mid;
    LedIntensity := 0;
  end else if LedValue >= 1 then
  begin
    tc := Color_High;
    LedIntensity := 255;
  end else if LedValue < 0 then
  begin
    fx := LedValue * -1;
    assert(fx >= 0);
    assert(fx <= 1);
    tc := ColorFadeF(Color_Mid, Color_Low, fx);
    LedIntensity := round(fx * 255);
  end else
  begin
    fx := LedValue;
    assert(fx >= 0);
    assert(fx <= 1);
    tc := ColorFadeF(Color_Mid, Color_High, fx);
    LedIntensity := round(fx * 255);
  end;


  BackBuffer.BufferInterface.ClearAll(tc.WithAlpha(0));
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  BackBuffer.BufferInterface.FillColor := tc.AsAggRgba8;
  BackBuffer.BufferInterface.NoLine;

  cx      := Width * 0.5;
  cy      := Height * 0.5;
  Radius  := LedSize * 0.5;
  Radius2 := Radius + IntenseWidth;

  BackBuffer.BufferInterface.Ellipse(cx, cy, Radius, Radius);



  BackBuffer.BufferInterface.FillRadialGradient(cx,cy,Radius2, tc.WithAlpha(LedIntensity), tc.WithAlpha(0));
  BackBuffer.BufferInterface.Ellipse(cx, cy, Radius2, Radius2);



end;



end.
