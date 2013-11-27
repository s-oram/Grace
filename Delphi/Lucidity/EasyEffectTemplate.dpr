library EasyEffectTemplate;

{$R 'resources.res' 'resources.rc'}



uses
  SysUtils,
  Classes,
  eePlugin in 'eePlugin.pas',
  eePluginGui in 'eePluginGui.pas' {PluginGui},
  uConstants in 'uConstants.pas',
  eeVstParameter in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstParameter.pas',
  DAEffect in '..\..\Library\EasyEffect\EasyEffectTemplate\DAEffect.pas',
  DAEffectX in '..\..\Library\EasyEffect\EasyEffectTemplate\DAEffectX.pas',
  DAudioEffect in '..\..\Library\EasyEffect\EasyEffectTemplate\DAudioEffect.pas',
  DAudioEffectX in '..\..\Library\EasyEffect\EasyEffectTemplate\DAudioEffectX.pas',
  DVstFxStore in '..\..\Library\EasyEffect\EasyEffectTemplate\DVstFxStore.pas',
  DVSTUtils in '..\..\Library\EasyEffect\EasyEffectTemplate\DVSTUtils.pas',
  eeBuffer in '..\..\Library\EasyEffect\EasyEffectTemplate\eeBuffer.pas',
  eeBufferedEventList in '..\..\Library\EasyEffect\EasyEffectTemplate\eeBufferedEventList.pas',
  eeDetectHost in '..\..\Library\EasyEffect\EasyEffectTemplate\eeDetectHost.pas',
  eeDsp in '..\..\Library\EasyEffect\EasyEffectTemplate\eeDsp.pas',
  eeGlobals in '..\..\Library\EasyEffect\EasyEffectTemplate\eeGlobals.pas',
  eeHotKeySupport in '..\..\Library\EasyEffect\EasyEffectTemplate\eeHotKeySupport.pas',
  eeIniFile in '..\..\Library\EasyEffect\EasyEffectTemplate\eeIniFile.pas',
  eeKeyboardHook in '..\..\Library\EasyEffect\EasyEffectTemplate\eeKeyboardHook.pas',
  eeMidiAutomation in '..\..\Library\EasyEffect\EasyEffectTemplate\eeMidiAutomation.pas',
  eeMidiEvents in '..\..\Library\EasyEffect\EasyEffectTemplate\eeMidiEvents.pas',
  eePluginBase in '..\..\Library\EasyEffect\EasyEffectTemplate\eePluginBase.pas',
  eePluginSettings in '..\..\Library\EasyEffect\EasyEffectTemplate\eePluginSettings.pas',
  eeTypes in '..\..\Library\EasyEffect\EasyEffectTemplate\eeTypes.pas',
  eeVstAdapter in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstAdapter.pas',
  eeVstEditorAdapter in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstEditorAdapter.pas',
  eeVSTExtra in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVSTExtra.pas',
  eeVstMidiTypes in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstMidiTypes.pas',
  eeDSP.Oversampling in '..\..\Library\EasyEffect\EasyEffectTemplate\eeDSP.Oversampling.pas',
  uDataFolderUtils in 'uDataFolderUtils.pas';

{$R *.res}

var
  VstPlug : TeeVstAdapter;


function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin

  // get vst version
  if audioMaster(nil, audioMasterVersion, 0, 0, nil, 0) = 0 then
  begin
    Result := nil;
    Exit;
  end;


  VstPlug := TeeVstAdapter.Create(audioMaster,0,0);
  if not Assigned(VstPlug) then
  begin
    Result := nil;
    Exit;
  end;

  Result := VstPlug.Effect


end;

exports
  Main name 'main';

begin
end.


