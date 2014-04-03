unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
  Spring.Container,
  uOrder,
  uOrderInterfaces,
  uRegistrations;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;
begin
  RegisterTypes(GlobalContainer);
  Order := TOrder.Create;
  try
    OrderProcessor := GlobalContainer.Resolve<IOrderProcessor>;
    if OrderProcessor.ProcessOrder(Order) then
      Writeln('Order successfully processed....');
  finally
    Order.Free;
  end;
end;

end.
