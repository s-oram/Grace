unit uOrderEntry;

interface

uses
  uOrder;

type
  IOrderEntry = interface
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
  end;

  TOrderEntry = class(TInterfacedObject, IOrderEntry)
  public
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
  end;

implementation

{ TOrderEntry }

function TOrderEntry.EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
begin
  Result := Assigned(aOrder);
  Writeln('Entering order into the database....');
end;

end.
