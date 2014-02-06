unit LucidityGui.Reg;

interface

uses
  LucidityGUI.Scope;


procedure Register;

implementation

uses
  Classes, DesignIntf;

procedure Register;
begin
  RegisterComponents('Lucidity GUI', [TLucidityScope]);
end;

end.
