unit uOrderInterfaces;

interface

uses
  uOrder;

type
  IOrderValidator = interface
  ['{6D0F52B4-A96F-4C96-97A4-DE45324FDE1B}']
    function ValidateOrder(aOrder: TOrder): Boolean;
  end;

  IOrderEntry = interface
  ['{8D272909-3324-4849-A128-C85E249520CD}']
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
  end;

  IOrderProcessor = interface
  ['{978361F2-65F0-49F7-A00C-964C05683682}']
    function ProcessOrder(aOrder: TOrder): Boolean;
  end;

implementation

end.
