unit LucidityGui.Reg;

interface

uses
  LucidityGUI.DropBoxSelector,
  LucidityGUI.Scope;


procedure Register;

implementation

uses
  Classes, DesignIntf;

procedure Register;
begin
  RegisterComponents('Lucidity GUI', [TDropBoxSelector]);
  RegisterComponents('Lucidity GUI', [TLucidityScope]);
end;

end.
