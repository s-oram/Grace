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
begin
  inherited;

end;


end.
