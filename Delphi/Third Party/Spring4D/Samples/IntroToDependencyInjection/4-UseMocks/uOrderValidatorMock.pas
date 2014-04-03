unit uOrderValidatorMock;

interface

uses
  uOrder,
  uOrderValidator;

type
  TOrderValidatorMock = class(TInterfacedObject, IOrderValidator)
  public
    function ValidateOrder(aOrder: TOrder): Boolean;
  end;

implementation

{ TOrderValidatorMock }

function TOrderValidatorMock.ValidateOrder(aOrder: TOrder): Boolean;
begin
  Result := True;
  Writeln('TOrderValidatorMock.ValidateOrder called');
end;

end.
