program Demo.Spring.SimpleInjection;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Spring.Services,
  Spring.Container,
  uSimpleInjection in 'uSimpleInjection.pas';

var
  Pet: IPetNoiseMaker;
begin
  try
    GlobalContainer.Build;

    Pet := ServiceLocator.GetService<IPetNoiseMaker>;
    Pet.MakePetNoises;
    Pet.MakePetNoises('Cat');
    Pet.MakePetNoises('Cow');

    Readln;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;
end.
