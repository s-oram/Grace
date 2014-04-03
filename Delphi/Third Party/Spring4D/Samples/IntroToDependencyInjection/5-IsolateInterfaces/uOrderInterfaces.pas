unit uOrderInterfaces;

interface

uses
  uOrder;

type
  IOrderValidator = interface
    function ValidateOrder(aOrder: TOrder): Boolean;
  end;

  IOrderEntry = interface
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
  end;

  IOrderProcessor = interface
    function ProcessOrder(aOrder: TOrder): Boolean;
  end;

implementation

end.
