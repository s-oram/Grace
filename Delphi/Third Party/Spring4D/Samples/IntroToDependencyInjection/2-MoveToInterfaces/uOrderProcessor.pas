unit uOrderProcessor;

interface

uses
   uOrder,
   uOrderEntry,
   uOrderValidator;

type
  IOrderProcessor = interface
    function ProcessOrder(aOrder: TOrder): Boolean;
  end;

  TOrderProcessor = class(TInterfacedObject, IOrderProcessor)
  private
    FOrderValidator: IOrderValidator;
    FOrderEntry: IOrderEntry;
  public
    constructor Create;
    function ProcessOrder(aOrder: TOrder): Boolean;
  end;

implementation

{ TOrderProcessor }

constructor TOrderProcessor.Create;
begin
  FOrderValidator := TOrderValidator.Create;
  FOrderEntry := TOrderEntry.Create;
end;

function TOrderProcessor.ProcessOrder(aOrder: TOrder): Boolean;
var
  OrderIsValid: Boolean;
begin
  Result := False;
  OrderIsValid := FOrderValidator.ValidateOrder(aOrder);
  if OrderIsValid then
    Result := FOrderEntry.EnterOrderIntoDatabase(aOrder);
  Writeln('Order has been processed....');
end;

end.
