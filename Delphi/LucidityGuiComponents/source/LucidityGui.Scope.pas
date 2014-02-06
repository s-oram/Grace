unit LucidityGui.Scope;

interface

uses
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;


type
  TLucidityScope = class(TVamWinControl)
  private
    function GetColors(const Index: Integer): TRedFoxColorString;
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);

  protected
    fColorBackground : TRedFoxColor;
    fColorBorder     : TRedFoxColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
  published
    property ColorBackground : TRedFoxColorString index 0 read GetColors write SetColors;
    property ColorBorder     : TRedFoxColorString index 1 read GetColors write SetColors;

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


procedure TLucidityScope.Paint;
var
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
end;


end.
