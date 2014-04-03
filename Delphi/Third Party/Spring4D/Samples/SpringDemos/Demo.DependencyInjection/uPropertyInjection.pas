unit uPropertyInjection;

interface

type
  ISteeringWheel = interface
  ['{6EDC3A73-CA7E-4E69-A1D4-83895FD934B0}']
    procedure SetDirection(aNewDirection: integer);
    function GetDirection: integer;
    property Direction: integer read GetDirection write SetDirection;
  end;

  ICar = interface
  ['{E27D7E17-2305-4C5A-9785-A2615CC22AFF}']
    function GetSteeringWheel: ISteeringWheel;
    procedure SetSteeringWheel(aValue: ISteeringWheel);
    property SteeringWheel: ISteeringWheel read GetSteeringWheel write SetSteeringWheel;
  end;

implementation

uses
      Spring.Container
    , Spring.Services
    ;

type
  TCar = class(TInterfacedObject, ICar)
  private
    FSteeringWheel: ISteeringWheel;
    function GetSteeringWheel: ISteeringWheel;
    procedure SetSteeringWheel(aValue: ISteeringWheel);
  public
    [Inject]
    property SteeringWheel: ISteeringWheel read GetSteeringWheel write SetSteeringWheel;
  end;

  TSteeringWheel = class(TInterfacedObject, ISteeringWheel)
  private
    FDirection: integer;
  public
    procedure SetDirection(aNewDirection: integer);
    function GetDirection: integer;
    property Direction: Integer read GetDirection write SetDirection;
  end;

{ TVehicle }

function TCar.GetSteeringWheel: ISteeringWheel;
begin
  Result := FSteeringWheel;
end;

procedure TCar.SetSteeringWheel(aValue: ISteeringWheel);
begin
  FSteeringWheel := AValue;
end;

{ TSteeringWheel }

function TSteeringWheel.GetDirection: integer;
begin
  Result := FDirection;
end;

procedure TSteeringWheel.SetDirection(aNewDirection: integer);
begin
  FDirection := aNewDirection;
end;

initialization
  GlobalContainer.RegisterType<TSteeringWheel>.Implements<ISteeringWheel>;
  GlobalContainer.RegisterType<TCar>.Implements<ICar>;


end.
