unit ksRegTools;

interface

uses
  Classes, ksComm, ksTimers;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ksTools', [TksComPort, TksTimer]);
end;

end.
