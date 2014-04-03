unit uConstructorInjectionDemo;

interface

uses
       Types
     ;

type
  ISimpleInterface = interface
  ['{1D46C87E-87B7-468B-90A6-EC477EAE9F39}']
    function GetMyPoint: TPoint;
    procedure SetMyPoint(const Value: TPoint);
    property MyPoint: TPoint read GetMyPoint write SetMyPoint;
  end;

  ISimpleContainerInterface = interface
    ['{286D76D4-3567-4D6F-B2C0-E58F3BBDB807}']
    function GetSimpleClass: ISimpleInterface;
    procedure SetSimpleClass(const Value: ISimpleInterface);
    property SimpleClass: ISimpleInterface read GetSimpleClass write SetSimpleClass;
  end;

implementation

uses
     Spring.Container
   , Spring.Services
   ;

type
  TSimpleClass = class(TInterfacedObject, ISimpleInterface)
  private
    FMyPoint: TPoint;
    function GetMyPoint: TPoint;
    procedure SetMyPoint(const Value: TPoint);
  public
    constructor Create(aX, aY: integer);
    property MyPoint: TPoint read GetMyPoint write SetMyPoint;
  end;

  TSimpleClassConsumer = class(TInterfacedObject, ISimpleContainerInterface)
  private
    [Inject]
    FSimpleClass: ISimpleInterface;
    function GetSimpleClass: ISimpleInterface;
    procedure SetSimpleClass(const Value: ISimpleInterface);
  public
    property SimpleClass: ISimpleInterface read GetSimpleClass write SetSimpleClass;
  end;

{ TSimpleClass }

constructor TSimpleClass.Create(aX, aY: integer);
begin
   FMyPoint := Point(aX, aY);
end;

function TSimpleClass.GetMyPoint: TPoint;
begin
  Result := FMyPoint;
end;

procedure TSimpleClass.SetMyPoint(const Value: TPoint);
begin
  FMyPoint := Value;
end;

{ TSimpleClassConsumer }

function TSimpleClassConsumer.GetSimpleClass: ISimpleInterface;
begin
  Result := FSimpleClass;
end;

procedure TSimpleClassConsumer.SetSimpleClass(const Value: ISimpleInterface);
begin
  FSimpleClass := Value;
end;

initialization
  GlobalContainer.RegisterType<TSimpleClassConsumer>.Implements<ISimpleContainerInterface>;
  GlobalContainer.RegisterType<TSimpleClass>.Implements<ISimpleInterface>.InjectConstructor([56, 150]);
end.
