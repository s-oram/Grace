unit LucidityUtils;

interface

uses
  Lucidity.PluginParameters;


procedure SetupPluginParameters(const PluginParameters : TPluginParameterManager);

implementation

procedure SetupPluginParameters(const PluginParameters : TPluginParameterManager);
  function CreateNamedParameter(const PluginParameters : TPluginParameterManager; aName : string):TPluginParameterClass;
  var
    Par : TPluginParameterClass;
  begin
    Par := TPluginParameterClass.Create;
    Par.Name := 'VoiceMode';
    PluginParameters.Add(Par);
    result := Par;
  end;
begin
  CreateNamedParameter(PluginParameters, 'VoiceMode');
  CreateNamedParameter(PluginParameters, 'VoiceGlide');
  CreateNamedParameter(PluginParameters, 'PitchTracking');
  CreateNamedParameter(PluginParameters, 'SamplePlaybackType');
  CreateNamedParameter(PluginParameters, 'SampleResetClockSource');
  CreateNamedParameter(PluginParameters, 'SamplerLoopBounds');
  CreateNamedParameter(PluginParameters, 'SamplerLoopMode');
  CreateNamedParameter(PluginParameters, 'OutputGain');
  CreateNamedParameter(PluginParameters, 'OutputPan');
  CreateNamedParameter(PluginParameters, 'VoicePitchOne');
  CreateNamedParameter(PluginParameters, 'VoicePitchTwo');
  CreateNamedParameter(PluginParameters, 'SampleStart');
  CreateNamedParameter(PluginParameters, 'SampleEnd');
  CreateNamedParameter(PluginParameters, 'LoopStart');
  CreateNamedParameter(PluginParameters, 'LoopEnd');
  CreateNamedParameter(PluginParameters, 'AmpAttack');
  CreateNamedParameter(PluginParameters, 'AmpHold');
  CreateNamedParameter(PluginParameters, 'AmpDecay');
  CreateNamedParameter(PluginParameters, 'AmpSustain');
  CreateNamedParameter(PluginParameters, 'AmpRelease');
  CreateNamedParameter(PluginParameters, 'AmpVelocity');
  CreateNamedParameter(PluginParameters, 'ModAttack');
  CreateNamedParameter(PluginParameters, 'ModHold');
  CreateNamedParameter(PluginParameters, 'ModDecay');
  CreateNamedParameter(PluginParameters, 'ModSustain');
  CreateNamedParameter(PluginParameters, 'ModRelease');
  CreateNamedParameter(PluginParameters, 'ModVelocity');
  CreateNamedParameter(PluginParameters, 'FilterRouting');
  CreateNamedParameter(PluginParameters, 'FilterOutputBlend');
  CreateNamedParameter(PluginParameters, 'Filter1Type');
  CreateNamedParameter(PluginParameters, 'Filter2Type');
  CreateNamedParameter(PluginParameters, 'Filter1KeyFollow');
  CreateNamedParameter(PluginParameters, 'Filter2KeyFollow');
  CreateNamedParameter(PluginParameters, 'Filter1Par1');
  CreateNamedParameter(PluginParameters, 'Filter1Par2');
  CreateNamedParameter(PluginParameters, 'Filter1Par3');
  CreateNamedParameter(PluginParameters, 'Filter1Par4');
  CreateNamedParameter(PluginParameters, 'Filter2Par1');
  CreateNamedParameter(PluginParameters, 'Filter2Par2');
  CreateNamedParameter(PluginParameters, 'Filter2Par3');
  CreateNamedParameter(PluginParameters, 'Filter2Par4');
  CreateNamedParameter(PluginParameters, 'Lfo1Shape');
  CreateNamedParameter(PluginParameters, 'Lfo2Shape');
  CreateNamedParameter(PluginParameters, 'Lfo1FreqMode');
  CreateNamedParameter(PluginParameters, 'Lfo2FreqMode');
  CreateNamedParameter(PluginParameters, 'Lfo1Range');
  CreateNamedParameter(PluginParameters, 'Lfo2Range');
  CreateNamedParameter(PluginParameters, 'Lfo1Par1');
  CreateNamedParameter(PluginParameters, 'Lfo1Par2');
  CreateNamedParameter(PluginParameters, 'Lfo1Par3');
  CreateNamedParameter(PluginParameters, 'Lfo2Par1');
  CreateNamedParameter(PluginParameters, 'Lfo2Par2');
  CreateNamedParameter(PluginParameters, 'Lfo2Par3');
  CreateNamedParameter(PluginParameters, 'Seq1Clock');
  CreateNamedParameter(PluginParameters, 'Seq1Direction');
  CreateNamedParameter(PluginParameters, 'Seq1Length');
  CreateNamedParameter(PluginParameters, 'Seq2Clock');
  CreateNamedParameter(PluginParameters, 'Seq2Direction');
  CreateNamedParameter(PluginParameters, 'Seq2Length');
  CreateNamedParameter(PluginParameters, 'PreviewVolume');
  CreateNamedParameter(PluginParameters, 'Preview');
  CreateNamedParameter(PluginParameters, 'PadX1');
  CreateNamedParameter(PluginParameters, 'PadY1');
  CreateNamedParameter(PluginParameters, 'PadX2');
  CreateNamedParameter(PluginParameters, 'PadY2');
  CreateNamedParameter(PluginParameters, 'PadX3');
  CreateNamedParameter(PluginParameters, 'PadY3');
  CreateNamedParameter(PluginParameters, 'PadX4');
  CreateNamedParameter(PluginParameters, 'PadY4');

end;

end.
