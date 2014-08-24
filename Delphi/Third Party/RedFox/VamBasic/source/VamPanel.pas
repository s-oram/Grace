unit VamPanel;

interface

uses
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamPanel = class(TVamWinControl)
  private
    fColor : TRedFoxColor;
    fCornerRadius2: double;
    fCornerRadius3: double;
    fCornerRadius1: double;
    fCornerRadius4: double;
    function GetColor: TRedFoxColorString;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure SetCornerRadius1(const Value: double);
    procedure SetCornerRadius2(const Value: double);
    procedure SetCornerRadius3(const Value: double);
    procedure SetCornerRadius4(const Value: double);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color : TRedFoxColorString read GetColor write SetColor;

    property CornerRadius1 : double read fCornerRadius1 write SetCornerRadius1;
    property CornerRadius2 : double read fCornerRadius2 write SetCornerRadius2;
    property CornerRadius3 : double read fCornerRadius3 write SetCornerRadius3;
    property CornerRadius4 : double read fCornerRadius4 write SetCornerRadius4;

    property Transparent;

    {$INCLUDE TControlProperties.inc}
    {$INCLUDE TWinControlProperties.inc}
    property AutoSize;
  end;

implementation

uses
  Graphics,
  Controls, AggRoundedRect, AggPathStorage,
  AggPixelFormat;

{ TVamPanel }

constructor TVamPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := '$FFCCCCCC';
end;

destructor TVamPanel.Destroy;
begin

  inherited;
end;

function TVamPanel.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

procedure TVamPanel.SetColor(const Value: TRedFoxColorString);
begin
  if Value <> fColor.AsString then
  begin
    fColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamPanel.SetCornerRadius1(const Value: double);
begin
  if fCornerRadius1 <> Value then
  begin
    fCornerRadius1 := Value;
    Invalidate;
  end;
end;

procedure TVamPanel.SetCornerRadius2(const Value: double);
begin
  if fCornerRadius2 <> Value then
  begin
    fCornerRadius2 := Value;
    Invalidate;
  end;
end;

procedure TVamPanel.SetCornerRadius3(const Value: double);
begin
  if fCornerRadius3 <> Value then
  begin
    fCornerRadius3 := Value;
    Invalidate;
  end;
end;

procedure TVamPanel.SetCornerRadius4(const Value: double);
begin
  if fCornerRadius4 <> Value then
  begin
    fCornerRadius4 := Value;
    Invalidate;
  end;
end;

procedure TVamPanel.Paint;
var
  Rc: TAggRoundedRect;
  Path: TAggPathStorage;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(fColor.WithAlpha(0));
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  {
    TAggBlendMode = (bmClear, bmSource, bmDestination, bmSourceOver,
      bmDestinationOver, bmSourceIn, bmDestinationIn, bmSourceOut,
      bmDestinationOut, bmSourceATop, bmDestinationATop, bmXor, bmPlus, bmMinus,
      bmMultiply, bmScreen, bmOverlay, bmDarken, bmLighten, bmColorDodge,
      bmColorBurn, bmHardLight, bmSoftLight, bmDifference, bmExclusion,
      bmContrast, bmInvert, bmInvertRgb, bmAlpha);
  }


  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := fColor.AsAggRgba8;


  //BackBuffer.BufferInterface.RoundedRectEx(0, 0, Width, Height, fCornerRadius[0],fCornerRadius[1],fCornerRadius[2],fCornerRadius[3]);
  BackBuffer.BufferInterface.RoundedRectEx(0, 0, Width, Height, CornerRadius1,CornerRadius2,CornerRadius3,CornerRadius4);


  {
  rc := TAggRoundedRect.Create;
  Path := TAggPathStorage.Create;
  try
    rc.Rect(0,0,Width,Height);
    rc.Radius(CornerRadius1, CornerRadius1, CornerRadius2, CornerRadius2, CornerRadius3, CornerRadius3, CornerRadius4, CornerRadius4);
    Path.AddPath(rc);

    BackBuffer.BufferInterface.ResetPath;
    BackBuffer.BufferInterface.AddPath(Path);
    BackBuffer.BufferInterface.DrawPath;
  finally
    rc.Free;
    Path.Free;
  end;
  }
end;



end.
