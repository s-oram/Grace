unit DesignTimeRegistration;

interface

procedure Register;

implementation

uses
  DesignIntf,
  Classes,
  FarScape.VclContainer;

procedure Register;
begin
  RegisterComponents('FarScapeVCL', [TFarScapeContainerVCL]);
end;

end.
