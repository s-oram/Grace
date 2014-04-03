unit uNormalMathService;

interface


implementation

uses
       uMathInterfaces
     , uServiceNames
     , Spring.Container
     ;
type

  TNormalMathServiceImplemenation = class(TInterfacedObject, IMathService)
    function Add(a, b: integer): integer;
    function Multiply(a, b: integer): integer;
  end;

{ TNormalAdditionServiceImplemenation }

function TNormalMathServiceImplemenation.Add(a, b: integer): integer;
begin
  Result := a + b;
end;

function TNormalMathServiceImplemenation.Multiply(a, b: integer): integer;
begin
  Result := a * b;
end;

procedure RegisterNormalMathService;
begin
  GlobalContainer.RegisterType<TNormalMathServiceImplemenation>.Implements<IMathService>(NormalAdditionServiceName);
end;

initialization
  RegisterNormalMathService;

end.
