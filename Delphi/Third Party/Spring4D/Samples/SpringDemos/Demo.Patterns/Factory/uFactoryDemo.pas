unit uFactoryDemo;

interface

uses
      Spring.DesignPatterns
    ;

type
  TCar = class
  private
    FMaxSpeed: integer;
  public
    function ToString: string; override;
    constructor Create(aMaxSpeed: integer);
    property MaxSpeed: integer read FMaxSpeed;
  end;

  TCarFactory = class(TFactory<string, TCar>)

  end;

implementation

uses
      SysUtils
    ;

{ TCar }

constructor TCar.Create(aMaxSpeed: integer);
begin
  FMaxSpeed := aMaxSpeed;
end;

function TCar.ToString: string;
begin
  Result := Format('I have a maximum speed of %d MPH', [MaxSpeed]);
end;

end.
