program Demo.Spring.PropertyInjection;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IF CompilerVersion >= 23.0}System.SysUtils{$ELSE}SysUtils{$IFEND},
  Spring.Container,
  Spring.Services,
  uPropertyInjection in 'uPropertyInjection.pas';

var
  Car: ICar;

begin
  try
    GlobalContainer.Build;
    Car := ServiceLocator.GetService<ICar>;
    Car.SteeringWheel.Direction := 272;
    Writeln('Car is currently heading ', Car.SteeringWheel.Direction, ' degrees');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
