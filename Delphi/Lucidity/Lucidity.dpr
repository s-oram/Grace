library Lucidity;

{$INCLUDE Defines.inc}

{$R 'resources.res' 'resources.rc'}
{$R 'VersionInfo.res' 'VersionInfo.rc'}

uses
  MadListModules,
  MadListProcesses,
  MadListHardware,
  MadLinkDisAsm,
  MadExcept,
  SysUtils,
  Classes,
  eePlugin in 'eePlugin.pas',
  eePluginGui in 'eePluginGui.pas' {PluginGui},
  uConstants in 'uConstants.pas',
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
  uDataFolderUtils in 'uDataFolderUtils.pas',
  uSampleMapFrame in 'frames\uSampleMapFrame.pas' {SampleMapFrame: TFrame},
  uFileBrowserFrame in 'frames\uFileBrowserFrame.pas' {FileBrowserFrame: TFrame},
  uSampleDisplayFrame in 'frames\uSampleDisplayFrame.pas' {SampleDisplayFrame: TFrame},
  uVoiceControlFrame in 'frames\uVoiceControlFrame.pas' {VoiceControlFrame: TFrame},
  uModControlFrame in 'frames\uModControlFrame.pas' {ModControlFrame: TFrame},
  uScrollPanelFrame in 'frames\uScrollPanelFrame.pas' {ScrollPanelFrame: TFrame},
  eeSampleIntF in '..\..\Library\EasyEffect\EasyEffectExtra\eeSampleIntF.pas',
  uSampleMap in 'uSampleMap.pas',
  eeSoundObject in '..\..\Library\EasyEffect\EasyEffectTemplate\eeSoundObject.pas',
  uLucidityKeyGroup in 'SoundObjects\uLucidityKeyGroup.pas',
  soLucidityVoice in 'SoundObjects\soLucidityVoice.pas',
  eeVstParameter in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstParameter.pas',
  eeVstParameterList in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstParameterList.pas',
  soADSR in 'SoundObjects\soADSR.pas',
  uSamplePhaseCounter in 'SoundObjects\uSamplePhaseCounter.pas',
  Lucidity.Osc.OneShotSampler in 'SoundObjects\Lucidity.Osc.OneShotSampler.pas',
  soCustomSynthFilter in 'SoundObjects\soCustomSynthFilter.pas',
  SynthModule_Custom in 'SoundObjects\SynthModule_Custom.pas',
  SynthModule_LowPass12 in 'SoundObjects\SynthModule_LowPass12.pas',
  uModularConnectionManager in 'SoundObjects\uModularConnectionManager.pas',
  LuciditySampleOverlay in 'GuiComponents\LuciditySampleOverlay.pas',
  eePatchObject in '..\..\Library\EasyEffect\EasyEffectTemplate\eePatchObject.pas',
  eePatchObject_XmlWrapper in '..\..\Library\EasyEffect\EasyEffectTemplate\eePatchObject_XmlWrapper.pas',
  eeHashes in '..\..\Library\EasyEffect\EasyEffectCrypto\eeHashes.pas',
  soLucidityWaveOsc in 'SoundObjects\soLucidityWaveOsc.pas',
  uBlitOsc in 'SoundObjects\uBlitOsc.pas',
  soDynamicWaveTableOsc in 'SoundObjects\soDynamicWaveTableOsc.pas',
  soDynamicWaveTableOsc.WaveGen in 'SoundObjects\soDynamicWaveTableOsc.WaveGen.pas',
  Lucidity.Osc.OneShotSampler.SubOsc in 'SoundObjects\Lucidity.Osc.OneShotSampler.SubOsc.pas',
  soSawSquareOsc in 'SoundObjects\soSawSquareOsc.pas',
  uHarmonicWaveTableGen in 'SoundObjects\uHarmonicWaveTableGen.pas',
  soLucidityFilter in 'SoundObjects\soLucidityFilter.pas',
  soFilter.Test in 'SoundObjects\soFilter.Test.pas',
  soFilter.LowPassA in 'SoundObjects\soFilter.LowPassA.pas',
  FilterCore.SimperSVF in 'SoundObjects\FilterCore.SimperSVF.pas',
  soFilter.BandPassA in 'SoundObjects\soFilter.BandPassA.pas',
  soFilter.HighPassA in 'SoundObjects\soFilter.HighPassA.pas',
  uLucidityLfo in 'SoundObjects\uLucidityLfo.pas',
  eeEnumHelper in '..\..\Library\EasyEffect\EasyEffectTemplate\eeEnumHelper.pas',
  eeGuiStandard_MenuBuilder in '..\..\Library\EasyEffect\EasyEffectTemplate\eeGuiStandard_MenuBuilder.pas',
  eeGuiStandard_Types in '..\..\Library\EasyEffect\EasyEffectTemplate\eeGuiStandard_Types.pas',
  eeGuiStandard_MenuController in '..\..\Library\EasyEffect\EasyEffectTemplate\eeGuiStandard_MenuController.pas',
  soModMatrix in 'SoundObjects\soModMatrix.pas',
  uGuiUtils in 'uGuiUtils.pas',
  SoundObjectTypes in 'SoundObjects\SoundObjectTypes.pas',
  eeVirtualCV in '..\..\Library\EasyEffect\EasyEffectExtra\eeVirtualCV.pas',
  SmartInspectLogging in 'SmartInspectLogging.pas',
  uGuiFeedbackData in 'uGuiFeedbackData.pas',
  eeVstProperties in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstProperties.pas',
  uLucidityStepSequencer in 'SoundObjects\uLucidityStepSequencer.pas',
  uLucidityClock in 'SoundObjects\uLucidityClock.pas',
  soGateEnvelope in 'SoundObjects\soGateEnvelope.pas',
  Lucidity.Osc.GrainStretch in 'SoundObjects\Lucidity.Osc.GrainStretch.pas',
  soGrainStretchSubOsc in 'SoundObjects\soGrainStretchSubOsc.pas',
  SampleOscUtils in 'SoundObjects\SampleOscUtils.pas',
  uLucidityCustomSampleOsc in 'SoundObjects\uLucidityCustomSampleOsc.pas',
  uMiniSampleDisplayFrame in 'frames\uMiniSampleDisplayFrame.pas' {MiniSampleDisplayFrame: TFrame},
  soFivePointEnvelope in 'SoundObjects\soFivePointEnvelope.pas',
  uLucidityVoiceAmp in 'SoundObjects\uLucidityVoiceAmp.pas',
  uLucidityVCA in 'SoundObjects\uLucidityVCA.pas',
  uLucidityPanner in 'SoundObjects\uLucidityPanner.pas',
  uXYPadsFrame in 'frames\uXYPadsFrame.pas' {XYPadsFrame: TFrame},
  uGuiState in 'uGuiState.pas',
  uLucidityXYPads in 'SoundObjects\uLucidityXYPads.pas',
  eeParSmoothingUtils in '..\..\Library\EasyEffect\EasyEffectExtra\eeParSmoothingUtils.pas',
  eeVstParameterManager in '..\..\Library\EasyEffect\EasyEffectTemplate\eeVstParameterManager.pas',
  soLucidityVoiceParameterWrapper in 'SoundObjects\soLucidityVoiceParameterWrapper.pas',
  uMenuBarFrame in 'frames\uMenuBarFrame.pas' {MenuBarFrame: TFrame},
  uKeyGroupManager in 'uKeyGroupManager.pas',
  uLucidityVoiceController in 'uLucidityVoiceController.pas',
  uLucidityKeyGroupInterface in 'SoundObjects\uLucidityKeyGroupInterface.pas',
  Menu.KeyGroupsMenu in 'frames\Menu.KeyGroupsMenu.pas',
  eeFastCode in '..\..\Library\EasyEffect\EasyEffectExtra\eeFastCode.pas',
  uRegionInfoFrame in 'frames\uRegionInfoFrame.pas' {RegionInfoFrame: TFrame},
  Menu.SamplesMenu in 'frames\Menu.SamplesMenu.pas',
  eePluginEx in 'eePluginEx.pas',
  VamKeyStateTracker in '..\..\Library\Components\RedFox\VamBasic\source\VamKeyStateTracker.pas',
  VamSyncObjects in '..\..\Library\Components\RedFox\VamBasic\source\VamSyncObjects.pas',
  uKeyStateTrackerOverlay in 'GuiComponents\uKeyStateTrackerOverlay.pas',
  eePluginHotkeys in '..\..\Library\EasyEffect\EasyEffectTemplate\eePluginHotkeys.pas',
  eePluginKeyHook in '..\..\Library\EasyEffect\EasyEffectTemplate\eePluginKeyHook.pas',
  uLucidityData in 'uLucidityData.pas',
  Menu.SampleDisplayMenu in 'frames\Menu.SampleDisplayMenu.pas',
  uLucidityPopUpMenu in 'frames\uLucidityPopUpMenu.pas',
  eeEnumMenu in '..\..\Library\EasyEffect\EasyEffectTemplate\eeEnumMenu.pas',
  uOutputMixer in 'SoundObjects\uOutputMixer.pas',
  eeGuiSetup in '..\..\Library\EasyEffect\Gui\eeGuiSetup.pas',
  Lucidity.Osc.LoopSampler in 'SoundObjects\Lucidity.Osc.LoopSampler.pas',
  eeMenus in '..\..\Library\EasyEffect\Gui\eeMenus.pas',
  soFilter.LofiA in 'SoundObjects\soFilter.LofiA.pas',
  soSineOsc in 'SoundObjects\soSineOsc.pas',
  soFilter.RingModA in 'SoundObjects\soFilter.RingModA.pas',
  soFilter.DistortionA in 'SoundObjects\soFilter.DistortionA.pas',
  soFilter.CombA in 'SoundObjects\soFilter.CombA.pas',
  uSampleZeroCrossings in 'uSampleZeroCrossings.pas',
  soStepFilter in 'SoundObjects\soStepFilter.pas',
  soStepInFilter in 'SoundObjects\soStepInFilter.pas',
  soStepOutFilter in 'SoundObjects\soStepOutFilter.pas',
  Lucidity.Osc.FadeOutSampler in 'SoundObjects\Lucidity.Osc.FadeOutSampler.pas',
  uLucidityEnums in 'uLucidityEnums.pas',
  uLucidityStateManager in 'uLucidityStateManager.pas',
  Menu.MainMenu in 'frames\Menu.MainMenu.pas',
  Menu.FileTreeMenu in 'frames\Menu.FileTreeMenu.pas',
  uAboutFrame in 'frames\uAboutFrame.pas' {AboutFrame: TFrame},
  Lucidity.Env.ADSR in 'SoundObjects\Lucidity.Env.ADSR.pas',
  soFilter.LowPassB in 'SoundObjects\soFilter.LowPassB.pas',
  soFilter.FilterBCore in 'SoundObjects\soFilter.FilterBCore.pas',
  eeProfilerV2 in '..\..\Library\EasyEffect\EasyEffectExtra\eeProfilerV2.pas',
  LucidityGlobals in 'LucidityGlobals.pas',
  uLucidityExtra in 'uLucidityExtra.pas',
  Menu.SampleMapContextMenu in 'frames\Menu.SampleMapContextMenu.pas',
  Lucidity.Env.ASR in 'SoundObjects\Lucidity.Env.ASR.pas',
  eeProcessController in '..\..\Library\EasyEffect\EasyEffectTemplate\eeProcessController.pas',
  uDialogDisplayArea in 'frames\uDialogDisplayArea.pas',
  Menu.CustomPopupMenu in 'frames\Menu.CustomPopupMenu.pas',
  uAboutDialog in 'Dialogs\uAboutDialog.pas' {AboutDialogForm},
  uLoopEditDialog in 'Dialogs\uLoopEditDialog.pas' {LoopEditForm},
  eeCustomGlobals in '..\..\Library\EasyEffect\EasyEffectTemplate\eeCustomGlobals.pas',
  eeGlobals in 'eeGlobals.pas',
  eeVstXML in '..\..\Library\EasyEffect\EasyEffectExtra\eeVstXML.pas',
  B2.MovingAverageFilter in '..\..\Library\EasyEffect\eeBlocks2\B2.MovingAverageFilter.pas',
  B2.DelayBuffer in '..\..\Library\EasyEffect\eeBlocks2\B2.DelayBuffer.pas',
  eeMidiInputSmoother in '..\..\Library\EasyEffect\EasyEffectTemplate\eeMidiInputSmoother.pas',
  eeGuiHelpers in '..\..\Library\EasyEffect\EasyEffectTemplate\eeGuiHelpers.pas',
  eeLogging in 'eeLogging.pas',
  soNoteStack in 'SoundObjects\soNoteStack.pas',
  B2.Filter.CriticallyDampedLowpass in '..\..\Library\EasyEffect\eeBlocks2\B2.Filter.CriticallyDampedLowpass.pas',
  EasyEffect.AudioInputController.Custom in '..\..\Library\EasyEffect\EasyEffectTemplate\EasyEffect.AudioInputController.Custom.pas',
  EasyEffect.AudioInputController.Standard in '..\..\Library\EasyEffect\EasyEffectTemplate\EasyEffect.AudioInputController.Standard.pas',
  EasyEffect.AudioInputController.DspMaster in '..\..\Library\EasyEffect\EasyEffectTemplate\EasyEffect.AudioInputController.DspMaster.pas',
  EasyEffect.AudioInputController.r8Brain in '..\..\Library\EasyEffect\EasyEffectTemplate\EasyEffect.AudioInputController.r8Brain.pas',
  r8bsrc in '..\..\Library\EasyEffect\ThirdParty\r8brain-free-1.3\r8bsrc.pas',
  R8BrainWrapper in '..\..\Library\EasyEffect\ThirdParty\r8brain-free-1.3\R8BrainWrapper.pas',
  Lucidity.StateHelpers in 'Lucidity.StateHelpers.pas',
  Menu.StepSequenceMenu in 'frames\Menu.StepSequenceMenu.pas',
  eeProfilerV2.Form in '..\..\Library\EasyEffect\EasyEffectExtra\eeProfilerV2.Form.pas' {ProfillerForm},
  uInfoBarFrame in 'frames\uInfoBarFrame.pas' {InfoBarFrame: TFrame},
  LucidityGui.InfoBarController in 'LucidityGui.InfoBarController.pas',
  LucidityParameterScaling in 'LucidityParameterScaling.pas',
  uModSystemFrame in 'frames\uModSystemFrame.pas' {ModSystemFrame: TFrame},
  LucidityControl.ModSection in 'Controls\LucidityControl.ModSection.pas',
  LucidityModConnections in 'LucidityModConnections.pas',
  Lucidity.Options in 'Lucidity.Options.pas',
  VamLib.MoreTypes in '..\..\Library\VamLib\VamLib.MoreTypes.pas',
  NativeXmlEx in '..\..\Library\EasyEffect\ThirdParty\NativeXml_401\nativexml\NativeXmlEx.pas',
  eePluginParameterWizard in 'eePluginParameterWizard.pas',
  Lucidity.Sfz in 'Third Party File Support\Lucidity.Sfz.pas',
  SfzParser.Types in '..\..\Library\EasyEffect\ThirdParty\SfzParser.Types.pas';

{$R *.res}

var
  VstPlug : TeeVstAdapter;

function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin
  try
    // get vst version
    if audioMaster(nil, audioMasterVersion, 0, 0, nil, 0) = 0 then
    begin
      Result := nil;
      Exit;
    end;

    VstPlug := TeeVstAdapter.Create(audioMaster,0,0);
    if assigned(VstPlug)
      then result := VstPlug.Effect
      else result := nil;
  except
    {$IFDEF MadExcept}
    result := nil;
    HandleException;
    {$ELSE}
    raise;
    {$ENDIF}
  end;
end;

// NOTE: The Main function is exported twice, once using 'main', the original name, and 'VSTPluginMain' as
// required by the VST 2.4 spec.
exports
  Main name 'main',
  Main name 'VSTPluginMain';

begin
end.


