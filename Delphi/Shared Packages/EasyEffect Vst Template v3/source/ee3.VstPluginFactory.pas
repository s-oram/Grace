unit ee3.VstPluginFactory;

interface

uses
  VamLib.MoreTypes,
  DAEffect,
  DAudioEffect,
  ee3.CustomPlugin;

type
  TCreateVstPluginGuiResult = record
    GuiForm : TCustomVstGuiForm;
    GuiMeta : TObject;
  end;

  IVstPluginFactory = interface
    ['{CD10F7C6-A458-4741-83DC-EEEA09118A24}']
    function CreateVstPlugin(audioMaster: TAudioMasterCallbackFunc):PAEffect;
    function CreateVstPluginGui(aEffect: AudioEffect; SystemWindow : pointer; aInitialGuiWidth, aInitialGuiHeight : integer):TCreateVstPluginGuiResult;
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
