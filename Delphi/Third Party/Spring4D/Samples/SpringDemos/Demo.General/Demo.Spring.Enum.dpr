program Demo.Spring.Enum;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IF CompilerVersion >= 23.0}System.SysUtils{$ELSE}SysUtils{$IFEND},
  uEnumDemo in 'uEnumDemo.pas';

begin
  try
    DoEnumDemo;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
