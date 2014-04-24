program ParNameUnitGen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UnitGenerator.ParName in 'UnitGenerator.ParName.pas',
  UnitGen.Tools in 'UnitGen.Tools.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }

    UnitGenerator.ParName.Generate;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
