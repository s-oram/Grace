unit SoundElement.Test.Utils;

interface

uses
  Contnrs,
  SoundElement.ModulePins,
  SoundElement.Modules,
  SoundElement.ModuleController;

function GetModuleNames(const Modules : TObjectList):string;

implementation

function GetModuleNames(const Modules : TObjectList):string;
var
  Names : string;
  c1: Integer;
begin
  if Modules.Count = 0 then exit('');

  Names := (Modules[0] as TCustomModule).Name;
  for c1 := 1 to Modules.Count-1 do
  begin
    Names := Names + '|' + (Modules[c1] as TCustomModule).Name;
  end;

  result := Names;
end;

end.
