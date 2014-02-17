unit LucidityGui.Reg;

interface

uses
  LucidityGUI.DropBoxSelector,
  LucidityGUI.Scope,
  VamVectorSequence;


procedure Register;

implementation

uses
  Classes, DesignIntf;

procedure Register;
begin
  RegisterComponents('Lucidity GUI', [TDropBoxSelector]);
  RegisterComponents('Lucidity GUI', [TLucidityScope]);
  RegisterComponents('Lucidity GUI', [TVamVectorSequence]);
end;

end.
