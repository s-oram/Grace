program CodeGen;

{$APPTYPE CONSOLE}

{$R *.res}



uses
  System.SysUtils,
  CodeGenRunner in 'Source\CodeGenRunner.pas';

begin
  try
    DoCodeGen;
    WriteLn('Finished');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
