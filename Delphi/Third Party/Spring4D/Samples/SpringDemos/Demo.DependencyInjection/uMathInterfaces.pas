unit uMathInterfaces;

interface

type
  IMathService = interface
    ['{BFC7867C-6098-4744-9774-35E0A8FE1A1D}']
    function Add(a, b: integer): integer;
    function Multiply(a, b: integer): integer;
  end;

  ICalculator = interface
    ['{CDE516EC-7DDA-4C19-81DE-3E1DA24D2FC8}']
    function Addition(a, b: integer): integer;
    function Multiplication(a, b: integer): integer;
  end;



implementation

end.
