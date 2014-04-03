program Demo.Spring.ConstructorInjection;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  Spring.Services,
  Spring.Container,
  uConstructorInjectionDemo in 'uConstructorInjectionDemo.pas';

var
  SCC: ISimpleContainerInterface;

begin
  try
    GlobalContainer.Build;
    SCC := ServiceLocator.GetService<ISimpleContainerInterface>;
    if SCC.SimpleClass <> nil then
    begin
      WriteLn('SCC.SimpleClass.MyPoint = ', SCC.SimpleClass.MyPoint.X, ', ', SCC.SimpleClass.MyPoint.Y);
    end else
    begin
      WriteLn('Constructor Injection Demo failed');
    end;

    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
