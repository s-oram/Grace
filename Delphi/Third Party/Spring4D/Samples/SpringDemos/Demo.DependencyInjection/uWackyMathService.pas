unit uWackyMathService;

interface

  // Hah!  There is nothing here!

implementation

uses
         uMathInterfaces
       , Spring.Container
       , uServiceNames;

type
  TWackyAdditionServiceImplementation = class(TInterfacedObject, IMathService)
    function Add(a, b: integer): integer;
    function Multiply(a, b: integer): integer;
  end;

{ TWackyAdditionServiceImplementation }

function TWackyAdditionServiceImplementation.Add(a, b: integer): integer;
begin
  Result := 42;
end;

function TWackyAdditionServiceImplementation.Multiply(a, b: integer): integer;
begin
  Result := 42;
end;

procedure RegisterwWackyMathService;
begin
  GlobalContainer.RegisterType<TWackyAdditionServiceImplementation>.Implements<IMathService>(WackyAdditionServiceName);
end;

initialization
  RegisterwWackyMathService;

end.
