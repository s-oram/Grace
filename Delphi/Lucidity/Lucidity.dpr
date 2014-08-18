library Lucidity;

{$INCLUDE Defines.inc}
{$INCLUDE FastMM4Options.inc}

{$EXCESSPRECISION OFF}

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
  uVoiceControlFrame in 'frames\uVoiceControlFrame.pas' {VoiceControlFrame: TFrame},
  uModControlFrame in 'frames\uModControlFrame.pas' {ModControlFrame: TFrame},
  uScrollPanelFrame in 'frames\uScrollPanelFrame.pas' {ScrollPanelFrame: TFrame},
  Lucidity.KeyGroup in 'SoundObjects\Lucidity.KeyGroup.pas',
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
  Lucidity.Filter in 'SoundObjects\Lucidity.Filter.pas',
  soFilter.Test in 'SoundObjects\soFilter.Test.pas',
  soFilter.BlueFilter in 'SoundObjects\soFilter.BlueFilter.pas',
  FilterCore.SimperSVF in 'SoundObjects\FilterCore.SimperSVF.pas',
  uLucidity.Lfo in 'SoundObjects\uLucidity.Lfo.pas',
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
  Lucidity.KeyGroupManager in 'Lucidity.KeyGroupManager.pas',
  Menu.KeyGroupsMenu in 'frames\Menu.KeyGroupsMenu.pas',
  uRegionInfoFrame in 'frames\uRegionInfoFrame.pas' {RegionInfoFrame: TFrame},
  Menu.SamplesMenu in 'frames\Menu.SamplesMenu.pas',
  eePluginEx in 'eePluginEx.pas',
  uKeyStateTrackerOverlay in 'GuiComponents\uKeyStateTrackerOverlay.pas',
  uLucidityData in 'uLucidityData.pas',
  Menu.SampleContextMenu in 'frames\Menu.SampleContextMenu.pas',
  uLucidityPopUpMenu in 'frames\uLucidityPopUpMenu.pas',
  uOutputMixer in 'SoundObjects\uOutputMixer.pas',
  Lucidity.Osc.LoopSampler in 'SoundObjects\Lucidity.Osc.LoopSampler.pas',
  soSineOsc in 'SoundObjects\soSineOsc.pas',
  soFilter.RingModA in 'SoundObjects\soFilter.RingModA.pas',
  soFilter.DistortionA in 'SoundObjects\soFilter.DistortionA.pas',
  soFilter.CombA in 'SoundObjects\soFilter.CombA.pas',
  uSampleZeroCrossings in 'uSampleZeroCrossings.pas',
  soStepFilter in 'SoundObjects\soStepFilter.pas',
  soStepInFilter in 'SoundObjects\soStepInFilter.pas',
  soStepOutFilter in 'SoundObjects\soStepOutFilter.pas',
  Lucidity.Osc.FadeOutSampler in 'SoundObjects\Lucidity.Osc.FadeOutSampler.pas',
  Lucidity.StateManager in 'Lucidity.StateManager.pas',
  Menu.MainMenu in 'frames\Menu.MainMenu.pas',
  Menu.FileTreeMenu in 'frames\Menu.FileTreeMenu.pas',
  uAboutFrame in 'frames\uAboutFrame.pas' {AboutFrame: TFrame},
  Lucidity.Env.ADSR in 'SoundObjects\Lucidity.Env.ADSR.pas',
  Lucidity.Globals in 'Lucidity.Globals.pas',
  uLucidityExtra in 'uLucidityExtra.pas',
  Menu.SampleMapContextMenu in 'frames\Menu.SampleMapContextMenu.pas',
  Lucidity.Env.ASR in 'SoundObjects\Lucidity.Env.ASR.pas',
  uDialogDisplayArea in 'frames\uDialogDisplayArea.pas',
  Menu.CustomPopupMenu in 'frames\Menu.CustomPopupMenu.pas',
  uAboutDialog in 'Dialogs\uAboutDialog.pas' {AboutDialogForm},
  eeGlobals in 'eeGlobals.pas',
  eeLogging in 'eeLogging.pas',
  soNoteStack in 'SoundObjects\soNoteStack.pas',
  Lucidity.StateHelpers in 'Lucidity.StateHelpers.pas',
  Menu.StepSequenceMenu in 'frames\Menu.StepSequenceMenu.pas',
  LucidityParameterScaling in 'LucidityParameterScaling.pas',
  LucidityControl.ModSection in 'Controls\LucidityControl.ModSection.pas',
  LucidityModConnections in 'LucidityModConnections.pas',
  Lucidity.Options in 'Lucidity.Options.pas',
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
  LucidityUtils in 'LucidityUtils.pas',
  Lucidity.Types in 'Lucidity.Types.pas',
  SampleMapFrame.Extra in 'frames\SampleMapFrame.Extra.pas',
  Lucidity.FirstRun in 'Lucidity.FirstRun.pas',
  eePluginGuiMeta in 'eePluginGuiMeta.pas',
  GuiMeta.ScopeHandler in 'GuiMeta.ScopeHandler.pas',
  soSignalRecorder in 'SoundObjects\soSignalRecorder.pas',
  uSequencerFrame in 'frames\uSequencerFrame.pas' {SequencerFrame: TFrame},
  soLfo.WaveTableLfo in 'SoundObjects\soLfo.WaveTableLfo.pas',
  eeOscPhaseCounter in '..\Third Party\EasyEffect\eeGen\eeOscPhaseCounter.pas',
  soLfo.RandomLfo in 'SoundObjects\soLfo.RandomLfo.pas',
  soLfo.SlopeGen in 'SoundObjects\soLfo.SlopeGen.pas',
  GuiMeta.ActiveModDisplay in 'GuiMeta.ActiveModDisplay.pas',
  Menu.ModSelectorContextMenu in 'frames\Menu.ModSelectorContextMenu.pas',
  soFilter.MoogLadder in 'SoundObjects\soFilter.MoogLadder.pas',
  FilterCore.MoogLadder in 'SoundObjects\FilterCore.MoogLadder.pas',
  soFreqAnalyzer in 'SoundObjects\soFreqAnalyzer.pas',
  Lucidity.SequencerDataObject in 'Lucidity.SequencerDataObject.pas',
  uConstants in 'uConstants.pas',
  soFilter.OptimisedFilter in 'SoundObjects\soFilter.OptimisedFilter.pas',
  soLevelMeter in 'SoundObjects\soLevelMeter.pas',
  Lucidity.Interfaces in 'Lucidity.Interfaces.pas',
  LucidityGui.Scope in 'GuiComponents\LucidityGui.Scope.pas',
  LucidityGui.Scope.SignalRecorder in 'GuiComponents\LucidityGui.Scope.SignalRecorder.pas',
  LucidityGui.Scope.FreqAnalyzer in 'GuiComponents\LucidityGui.Scope.FreqAnalyzer.pas',
  eeEnumHelper in 'eeEnumHelper.pas',
  uLucidityEnums in 'uLucidityEnums.pas',
  Lucidity.KeyGroupPlayer in 'Lucidity.KeyGroupPlayer.pas',
  eeGuiStandardv2 in 'EasyEffectTemplate\eeGuiStandardv2.pas',
  Lucidity.PluginParameterController in 'Lucidity.PluginParameterController.pas',
  LucidityGui.KnobHandler in 'LucidityGui.KnobHandler.pas',
  Lucidity.PluginParameters in 'Lucidity.PluginParameters.pas',
  LucidityGui.MenuButtonHandler in 'LucidityGui.MenuButtonHandler.pas',
  eeGuiStandardv2_MenuBuilder in 'EasyEffectTemplate\eeGuiStandardv2_MenuBuilder.pas',
  eePublishedVstParameters in 'EasyEffectTemplate\eePublishedVstParameters.pas',
  eeTaskRunner in 'EasyEffectTemplate\eeTaskRunner.pas',
  eeStoredActionList in 'EasyEffectTemplate\eeStoredActionList.pas',
  eeMidiAutomationV2 in 'EasyEffectTemplate\eeMidiAutomationV2.pas',
  Effect.MidiAutomation in 'Custom Effect Template Files\Effect.MidiAutomation.pas',
  Lucidity.MidiInputProcessor in 'Lucidity.MidiInputProcessor.pas',
  Lucidity.VoiceController in 'Lucidity.VoiceController.pas',
  uZoomSampleDisplayFrame in 'frames\uZoomSampleDisplayFrame.pas' {ZoomSampleDisplayFrame: TFrame},
  uXYPadsFrame in 'frames\uXYPadsFrame.pas' {XYPadsFrame: TFrame},
  Menu.XYPadContextMenu in 'frames\Menu.XYPadContextMenu.pas',
  eeParSmoother in 'EasyEffectTemplate\eeParSmoother.pas',
  AudioEffect in 'AudioEffects\AudioEffect.pas',
  AudioEffect.Lofi in 'AudioEffects\AudioEffect.Lofi.pas',
  Menu.MissingSampleContextMenu in 'frames\Menu.MissingSampleContextMenu.pas',
  GuiDrawingRoutines in 'GuiDrawingRoutines.pas',
  XPLAT.Dialogs in 'EasyEffectTemplate\XPlat\XPLAT.Dialogs.pas',
  Lucidity.StateManager.DataClasses in 'Lucidity.StateManager.DataClasses.pas',
  SfzParser.SfzOpcodes in '..\Third Party\EasyEffect\ThirdParty\SfzParser.SfzOpcodes.pas',
  Lucidity.SfzOpcodeConversion in 'Third Party File Support\Lucidity.SfzOpcodeConversion.pas',
  SfzParser in '..\Third Party\EasyEffect\ThirdParty\SfzParser.pas',
  AudioIO_Mp3 in '..\Third Party\AudioIO\source\AudioIO_Mp3.pas',
  AudioIO_WavPack in '..\Third Party\AudioIO\source\AudioIO_WavPack.pas',
  Lucidity.SampleMap in 'Lucidity.SampleMap.pas';

{$R *.res}

var
  VstPlug : TeeVstAdapter;

function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin
  ReportMemoryLeaksOnShutDown := True;

  //LogMemoryUsage('VST Main Begin');

  SendMsg_StartProfiling;

  SetUpLogging;

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

  //LogMemoryUsage('VST Main End');
end;

// NOTE: The Main function is exported twice, once using 'main', the original name, and 'VSTPluginMain' as
// required by the VST 2.4 spec.
exports
  Main name 'main',
  Main name 'VSTPluginMain';

begin
end.


