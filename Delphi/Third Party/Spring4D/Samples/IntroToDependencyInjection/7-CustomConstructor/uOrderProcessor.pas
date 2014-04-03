unit uOrderProcessor;

interface

uses
  uOrder,
  uOrderInterfaces;

type
  TOrderProcessor = class(TInterfacedObject, IOrderProcessor)
  private
    FOrderValidator: IOrderValidator;
    FOrderEntry: IOrderEntry;
  public
    constructor Create(aOrderValidator: IOrderValidator; aOrderEntry: IOrderEntry);
    function ProcessOrder(aOrder: TOrder): Boolean;
  end;

implementation

{ TOrderProcessor }

constructor TOrderProcessor.Create(aOrderValidator: IOrderValidator; aOrderEntry: IOrderEntry);
begin
  FOrderValidator := aOrderValidator;
  FOrderEntry := aOrderEntry;
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
