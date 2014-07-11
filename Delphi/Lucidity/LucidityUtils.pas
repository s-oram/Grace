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
    Par.Name := aName;
    PluginParameters.Add(Par);
    result := Par;
  end;
var
  ParClass : TPluginParameterClass;
begin
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.VoiceMode));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.VoiceGlide));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PitchTracking));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.SamplePlaybackType));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.SampleResetClockSource));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.SamplerLoopBounds));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.SamplerLoopMode));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.OutputGain));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.OutputPan));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.VoicePitchOne));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.VoicePitchTwo));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.SampleStart));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.SampleEnd));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.LoopStart));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.LoopEnd));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.AmpAttack));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.AmpHold));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.AmpDecay));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.AmpSustain));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.AmpRelease));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.AmpVelocity));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.ModAttack));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.ModHold));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.ModDecay));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.ModSustain));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.ModRelease));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.ModVelocity));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.FilterRouting));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.FilterOutputBlend));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter1Type));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter2Type));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter1KeyFollow));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter2KeyFollow));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter1Par1));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter1Par2));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter1Par3));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter1Par4));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter2Par1));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter2Par2));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter2Par3));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Filter2Par4));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo1Shape));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo2Shape));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo1FreqMode));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo2FreqMode));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo1Range));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo2Range));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo1Par1));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo1Par2));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo1Par3));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo2Par1));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo2Par2));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Lfo2Par3));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Seq1Clock));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Seq1Direction));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Seq1Length));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Seq2Clock));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Seq2Direction));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Seq2Length));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PreviewVolume));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.Preview));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadX1));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadY1));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadX2));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadY2));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadX3));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadY3));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadX4));
  CreateNamedParameter(PluginParameters, PluginParToName(TPluginParameter.PadY4));


  ParClass := PluginParameters.FindByName(PluginParToName(TPluginParameter.VoicePitchOne));
  ParClass.IsQuantised := true;
  ParClass.QuantisedMin := -24;
  ParClass.QuantisedMax := 24;
end;

end.
