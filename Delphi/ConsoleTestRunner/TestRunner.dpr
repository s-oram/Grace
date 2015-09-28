program TestRunner;

{$APPTYPE CONSOLE}

{$R *.res}

{-$T}

uses
  System.SysUtils,
  Main in 'source\Main.pas';

begin
  try
    Main.Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
