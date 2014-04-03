unit uDoOrderProcessing;

interface

procedure DoOrderProcessing;

implementation

uses
  uOrder,
  uOrderProcessor,
  uOrderValidatorMock,
  uOrderEntryMock;

procedure DoOrderProcessing;
var
  Order: TOrder;
  OrderProcessor: IOrderProcessor;
begin
  Order := TOrder.Create;
  OrderProcessor := TOrderProcessor.Create(TOrderValidatorMock.Create, TOrderEntryMock.Create);
  try
    if OrderProcessor.ProcessOrder(Order) then
      Writeln('Order successfully processed....');
  finally
    Order.Free;
  end;
end;

end.
