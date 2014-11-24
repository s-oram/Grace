unit ee3.VstPluginFactory;

interface

uses
  DAEffect,
  DAudioEffect,
  ee3.CustomPlugin;

type
  IVstPluginFactory = interface
    ['{CD10F7C6-A458-4741-83DC-EEEA09118A24}']
    function CreateVstPlugin(audioMaster: TAudioMasterCallbackFunc):PAEffect;
    function CreateVstPluginGui(aEffect: AudioEffect; SystemWindow : pointer; aInitialGuiWidth, aInitialGuiHeight : integer):TCustomVstGuiForm;
  end;

procedure RegisterVstPluginFactory(f : IVstPluginFactory);

function VstPluginFactory:IVstPluginFactory;

implementation

var
  GlobalPluginFactory : IVstPluginFactory;

function VstPluginFactory:IVstPluginFactory;
begin
  result := GlobalPluginFactory;
end;

procedure RegisterVstPluginFactory(f : IVstPluginFactory);
begin
  GlobalPluginFactory := f;
end;

initialization

finalization
  GlobalPluginFactory := nil;

end.
