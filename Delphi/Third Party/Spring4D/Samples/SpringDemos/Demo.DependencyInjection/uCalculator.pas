unit uCalculator;

interface


implementation

uses
           uMathInterfaces
         , uServiceNames
         , Spring.Container
         , Spring.Services
         ;

type
  TCalculator = class(TInterfacedObject, ICalculator)
  private
    [Inject(NormalAdditionServiceName)]
    FMathService: IMathService;
  public
    function Addition(a, b: integer): integer;
    function Multiplication(a, b: integer): integer;
  end;

function TCalculator.Addition(a, b: integer): integer;
begin
  Result := FMathService.Add(a, b);
end;

function TCalculator.Multiplication(a, b: integer): integer;
begin
  Result := FMathService.Multiply(a, b);
end;

procedure RegisterCalculatorService;
begin
  GlobalContainer.RegisterType<TCalculator>.Implements<ICalculator>(CalculatorName);
end;

initialization
  RegisterCalculatorService;

end.
