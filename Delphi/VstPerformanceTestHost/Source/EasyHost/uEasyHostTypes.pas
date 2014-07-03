unit uEasyHostTypes;

interface

type
  TMachineID = word;

  TConnection = class
  public
    FromMachine:TMachineID;
    FromPin:integer;
    ToMachine:TMachineID;
    ToPin:integer;
    constructor Create;
  end;

  TMachineType = (mtVst);

implementation

{ TConnection }

constructor TConnection.Create;
begin
  FromMachine := 0;
  FromPin := 0;

  ToMachine := 0;
  ToPin := 0;
end;

end.
