unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
  uOrder,
  uOrderProcessor,
  uOrderValidator,
  uOrderEntry;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;
begin
  Order := TOrder.Create;
  OrderProcessor := TOrderProcessor.Create(TOrderValidator.Create, TOrderEntry.Create);
  try
    if OrderProcessor.ProcessOrder(Order) then
      Writeln('Order successfully processed....');
  finally
    Order.Free;
  end;
end;

end.
