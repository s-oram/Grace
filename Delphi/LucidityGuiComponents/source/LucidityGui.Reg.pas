unit LucidityGui.Reg;

interface

uses
  LucidityGUI.DropBoxSelector,
  LucidityGUI.Scope,
  LucidityGui.VectorSequence;


procedure Register;

implementation

uses
  Classes, DesignIntf;

procedure Register;
begin
  RegisterComponents('Lucidity GUI', [TDropBoxSelector]);
  RegisterComponents('Lucidity GUI', [TLucidityScope]);
  RegisterComponents('Lucidity GUI', [TLucidityVectorSequence]);
end;

end.
