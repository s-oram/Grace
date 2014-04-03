unit uOrderValidator;

interface

uses
  uOrder;

type
  TOrderValidator = class
  public
    function ValidateOrder(aOrder: TOrder): Boolean;
  end;

implementation

{ TOrderValidator }

function TOrderValidator.ValidateOrder(aOrder: TOrder): Boolean;
begin
  Result := Assigned(aOrder);
  Writeln('Validating Order....');
end;

end.
