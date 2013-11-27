unit eePluginInfo;

interface

uses
  eePluginSettings;

procedure GetPluginInfo(var Info:TeePluginSettings);

implementation


procedure GetPluginInfo(var Info:TeePluginSettings);
begin
  Info.PluginName := 'Plug Test';
  Info.PluginVendor := 'One Small Clue';
  Info.NumberOfPrograms := 12;
  Info.NumberOfParameters := 4;
end;

end.
