unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
  Spring.Container,
  uOrder,
  uOrderInterfaces,
  uOrderProcessor,
  uRegistrations;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;
  OrderValidator: IOrderValidator;
  OrderEntry: IOrderEntry;
begin
  RegisterTypes(GlobalContainer);
  Order := TOrder.Create;
  try
    OrderValidator := GlobalContainer.Resolve<IOrderValidator>;
    OrderEntry := GlobalContainer.Resolve<IOrderEntry>;
    OrderProcessor := TOrderProcessor.Create(OrderValidator, OrderEntry);
    if OrderProcessor.ProcessOrder(Order) then
      Writeln('Order successfully processed....');
  finally
    Order.Free;
  end;
end;

end.
