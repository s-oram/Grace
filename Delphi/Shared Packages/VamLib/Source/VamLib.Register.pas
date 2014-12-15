unit VamLib.Register;

interface

uses
  VamLib.Vcl.ZeroFrame;

procedure Register;

implementation

uses
  DesignEditors,
  DesignIntF;

procedure Register;
begin
  RegisterCustomModule (TZeroFrame, TCustomModule);
end;

end.
