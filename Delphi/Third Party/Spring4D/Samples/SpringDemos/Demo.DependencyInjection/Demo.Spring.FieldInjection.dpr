program Demo.Spring.FieldInjection;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IF CompilerVersion >= 23.0}System.SysUtils{$ELSE}SysUtils{$IFEND},
  Spring.Container,
  Spring.Services,
  uFieldInjectionDemo in 'uFieldInjectionDemo.pas';

var
  FID: IFieldInjectionDemo;
begin
  try
    GlobalContainer.Build;
    FID := ServiceLocator.GetService<IFieldInjectionDemo>;
    FID.UseSomeClass;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
