program AsmTester;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uAsmTest in 'uAsmTest.pas',
  AsmExample.CopyVector in 'AsmExample.CopyVector.pas',
  AsmDev.SimpleSVF in 'AsmDev.SimpleSVF.pas';

begin
  try
    RunProgram;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
