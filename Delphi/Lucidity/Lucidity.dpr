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
  eePluginGui in 'eePluginGui.pas' {PluginGui},
  uDataFolderUtils in 'uDataFolderUtils.pas',
  uSampleMapFrame in 'frames\uSampleMapFrame.pas' {SampleMapFrame: TFrame},
  uFileBrowserFrame in 'frames\uFileBrowserFrame.pas' {FileBrowserFrame: TFrame},
  uSampleDisplayFrame in 'frames\uSampleDisplayFrame.pas' {SampleDisplayFrame: TFrame},
  uVoiceControlFrame in 'frames\uVoiceControlFrame.pas' {VoiceControlFrame: TFrame},
  uModControlFrame in 'frames\uModControlFrame.pas' {ModControlFrame: TFrame},
  uScrollPanelFrame in 'frames\uScrollPanelFrame.pas' {ScrollPanelFrame: TFrame},
  uLucidityKeyGroup in 'SoundObjects\uLucidityKeyGroup.pas',
  soLucidityVoice in 'SoundObjects\soLucidityVoice.pas',
  soADSR in 'SoundObjects\soADSR.pas',
  uSamplePhaseCounter in 'SoundObjects\uSamplePhaseCounter.pas',
  Lucidity.Osc.OneShotSampler in 'SoundObjects\Lucidity.Osc.OneShotSampler.pas',
  soCustomSynthFilter in 'SoundObjects\soCustomSynthFilter.pas',
  SynthModule_Custom in 'SoundObjects\SynthModule_Custom.pas',
  SynthModule_LowPass12 in 'SoundObjects\SynthModule_LowPass12.pas',
  uModularConnectionManager in 'SoundObjects\uModularConnectionManager.pas',
  LuciditySampleOverlay in 'GuiComponents\LuciditySampleOverlay.pas',
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
  soModMatrix in 'SoundObjects\soModMatrix.pas',
  uGuiUtils in 'uGuiUtils.pas',
  SoundObjectTypes in 'SoundObjects\SoundObjectTypes.pas',
  SmartInspectLogging in 'SmartInspectLogging.pas',
  uGuiFeedbackData in 'uGuiFeedbackData.pas',
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
  uGuiState in 'uGuiState.pas',
  uLucidityXYPads in 'SoundObjects\uLucidityXYPads.pas',
  soLucidityVoiceParameterWrapper in 'SoundObjects\soLucidityVoiceParameterWrapper.pas',
  uMenuBarFrame in 'frames\uMenuBarFrame.pas' {MenuBarFrame: TFrame},
  uKeyGroupManager in 'uKeyGroupManager.pas',
  uLucidityVoiceController in 'uLucidityVoiceController.pas',
  uLucidityKeyGroupInterface in 'SoundObjects\uLucidityKeyGroupInterface.pas',
  Menu.KeyGroupsMenu in 'frames\Menu.KeyGroupsMenu.pas',
  uRegionInfoFrame in 'frames\uRegionInfoFrame.pas' {RegionInfoFrame: TFrame},
  Menu.SamplesMenu in 'frames\Menu.SamplesMenu.pas',
  eePluginEx in 'eePluginEx.pas',
  uKeyStateTrackerOverlay in 'GuiComponents\uKeyStateTrackerOverlay.pas',
  uLucidityData in 'uLucidityData.pas',
  Menu.SampleDisplayMenu in 'frames\Menu.SampleDisplayMenu.pas',
  uLucidityPopUpMenu in 'frames\uLucidityPopUpMenu.pas',
  uOutputMixer in 'SoundObjects\uOutputMixer.pas',
  Lucidity.Osc.LoopSampler in 'SoundObjects\Lucidity.Osc.LoopSampler.pas',
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
  uLucidityStateManager in 'uLucidityStateManager.pas',
  Menu.MainMenu in 'frames\Menu.MainMenu.pas',
  Menu.FileTreeMenu in 'frames\Menu.FileTreeMenu.pas',
  uAboutFrame in 'frames\uAboutFrame.pas' {AboutFrame: TFrame},
  Lucidity.Env.ADSR in 'SoundObjects\Lucidity.Env.ADSR.pas',
  soFilter.LowPassB in 'SoundObjects\soFilter.LowPassB.pas',
  soFilter.FilterBCore in 'SoundObjects\soFilter.FilterBCore.pas',
  LucidityGlobals in 'LucidityGlobals.pas',
  uLucidityExtra in 'uLucidityExtra.pas',
  Menu.SampleMapContextMenu in 'frames\Menu.SampleMapContextMenu.pas',
  Lucidity.Env.ASR in 'SoundObjects\Lucidity.Env.ASR.pas',
  uDialogDisplayArea in 'frames\uDialogDisplayArea.pas',
  Menu.CustomPopupMenu in 'frames\Menu.CustomPopupMenu.pas',
  uAboutDialog in 'Dialogs\uAboutDialog.pas' {AboutDialogForm},
  uLoopEditDialog in 'Dialogs\uLoopEditDialog.pas' {LoopEditForm},
  eeGlobals in 'eeGlobals.pas',
  eeLogging in 'eeLogging.pas',
  soNoteStack in 'SoundObjects\soNoteStack.pas',
  Lucidity.StateHelpers in 'Lucidity.StateHelpers.pas',
  Menu.StepSequenceMenu in 'frames\Menu.StepSequenceMenu.pas',
  uInfoBarFrame in 'frames\uInfoBarFrame.pas' {InfoBarFrame: TFrame},
  LucidityGui.InfoBarController in 'LucidityGui.InfoBarController.pas',
  LucidityParameterScaling in 'LucidityParameterScaling.pas',
  LucidityControl.ModSection in 'Controls\LucidityControl.ModSection.pas',
  LucidityModConnections in 'LucidityModConnections.pas',
  Lucidity.Options in 'Lucidity.Options.pas',
  eePluginParameterWizard in 'eePluginParameterWizard.pas',
  Lucidity.Sfz in 'Third Party File Support\Lucidity.Sfz.pas',
  eeVstAdapter in '..\Third Party\EasyEffect\EasyEffectTemplate\eeVstAdapter.pas',
  DAEffectX in '..\Third Party\EasyEffect\EasyEffectTemplate\DAEffectX.pas',
  DAudioEffect in '..\Third Party\EasyEffect\EasyEffectTemplate\DAudioEffect.pas',
  DAudioEffectX in '..\Third Party\EasyEffect\EasyEffectTemplate\DAudioEffectX.pas',
  DAEffect in '..\Third Party\EasyEffect\EasyEffectTemplate\DAEffect.pas',
  eeVstEditorAdapter in '..\Third Party\EasyEffect\EasyEffectTemplate\eeVstEditorAdapter.pas',
  Lucidity.SampleImageRenderer in 'Lucidity.SampleImageRenderer.pas',
  Lucidity.FlexSampleRenderer in 'Lucidity.FlexSampleRenderer.pas',
  uModSystem2Frame in 'frames\uModSystem2Frame.pas' {ModSystem2Frame: TFrame},
  eePlugin in 'eePlugin.pas',
  eeGuiStandard in 'eeGuiStandard.pas',
  eeGuiStandard.RedFoxKnob in 'eeGuiStandard.RedFoxKnob.pas',
  eeGuiStandard.RedFoxMenu in 'eeGuiStandard.RedFoxMenu.pas',
  eeGlobals.InfoBarReceiver in 'eeGlobals.InfoBarReceiver.pas',
  eeVstParameter in '..\Third Party\EasyEffect\EasyEffectTemplate\eeVstParameter.pas',
  eeVstParameterList in '..\Third Party\EasyEffect\EasyEffectTemplate\eeVstParameterList.pas',
  eeVstParameterEx in 'eeVstParameterEx.pas',
  LucidityUtils in 'LucidityUtils.pas',
  Lucidity.Types in 'Lucidity.Types.pas',
  SampleMapFrame.Extra in 'frames\SampleMapFrame.Extra.pas',
  Lucidity.FirstRun in 'Lucidity.FirstRun.pas',
  eePluginGuiMeta in 'eePluginGuiMeta.pas',
  GuiMeta.ScopeHandler in 'GuiMeta.ScopeHandler.pas',
  soSignalRecorder in 'SoundObjects\soSignalRecorder.pas',
  uSequencerFrame in 'frames\uSequencerFrame.pas' {SequencerFrame: TFrame};

{$R *.res}

var
  VstPlug : TeeVstAdapter;

function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin
  SendMsg_StartProfiling;

  try
    //check a few things are in place before creating the plugin.
    FirstRunSetup;

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


