program MovingToInterfaces;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uDoOrderProcessing in 'uDoOrderProcessing.pas',
  uOrder in 'uOrder.pas',
  uOrderEntry in 'uOrderEntry.pas',
  uOrderProcessor in 'uOrderProcessor.pas',
  uOrderValidator in 'uOrderValidator.pas';

begin
  try
    DoOrderProcessing;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
