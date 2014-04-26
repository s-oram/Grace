unit Lucidity.PluginParameterController;

interface

uses
  Lucidity.Types;


type
  TPluginParameterController = class
  private
  public
    class procedure SetPluginParameter(const aPlugin : TObject; const Scope : TParChangeScope; const KeyGroupName : string; const ParName : string; const Value : single); static; inline;
    class function GetPluginParameter(const aPlugin : TObject; const ParName : string):single; static; inline;
  end;

  // TODO: This class will contain all the code required to set/get the parameter values
  // to the internal audio engine.

implementation

uses
  SysUtils,
  soLucidityVoiceParameterWrapper,
  Lucidity.KeyGroup,
  Lucidity.Interfaces,
  uLucidityEnums,
  eePlugin;

{ TPluginParameterController }

class function TPluginParameterController.GetPluginParameter(const aPlugin : TObject; const ParName: string): single;
begin
  result := 0;
end;

class procedure TPluginParameterController.SetPluginParameter(
    const aPlugin : TObject;
    const Scope: TParChangeScope;
    const KeyGroupName: string;
    const ParName: string;
    const Value: single);
var
  Plugin : TeePlugin;
  Par : TPluginParameter;
  kg : IKeyGroup;
  VoicePar : TLucidityVoiceParameterWrapper;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  kg :=  Plugin.ActiveKeyGroup;

  VoicePar := (kg.GetObject as TKeyGroup).VoiceParameters;

  PluginParFromName(ParName);

  case Par of
    TPluginParameter.VoiceMode: ;
    TPluginParameter.VoiceGlide: ;
    TPluginParameter.PitchTracking: ;
    TPluginParameter.SamplePlaybackType: ;
    TPluginParameter.SampleResetClockSource: ;
    TPluginParameter.SamplerLoopBounds: ;
    TPluginParameter.SamplerLoopMode: ;
    TPluginParameter.GrainLoop: ;
    TPluginParameter.GrainLength: ;
    TPluginParameter.GrainRate: ;
    TPluginParameter.GrainPosition: ;
    TPluginParameter.OutputGain: ;
    TPluginParameter.OutputPan: ;
    TPluginParameter.VoicePitchOne: ;
    TPluginParameter.VoicePitchTwo: ;
    TPluginParameter.AuxALevel: ;
    TPluginParameter.AuxBLevel: ;
    TPluginParameter.OscShape: ;
    TPluginParameter.OscPulseWidth: ;
    TPluginParameter.NoiseLevel: ;
    TPluginParameter.SampleStart: ;
    TPluginParameter.SampleEnd: ;
    TPluginParameter.LoopStart: ;
    TPluginParameter.LoopEnd: ;
    TPluginParameter.AmpAttack: ;
    TPluginParameter.AmpHold: ;
    TPluginParameter.AmpDecay: ;
    TPluginParameter.AmpSustain: ;
    TPluginParameter.AmpRelease: ;
    TPluginParameter.AmpVelocity: ;
    TPluginParameter.FilterAttack: ;
    TPluginParameter.FilterHold: ;
    TPluginParameter.FilterDecay: ;
    TPluginParameter.FilterSustain: ;
    TPluginParameter.FilterRelease: ;
    TPluginParameter.FilterVelocity: ;
    TPluginParameter.FilterRouting: ;
    TPluginParameter.FilterOutputBlend: ;
    TPluginParameter.Filter1Type: ;
    TPluginParameter.Filter2Type: ;
    TPluginParameter.Filter1KeyFollow: ;
    TPluginParameter.Filter2KeyFollow: ;
    TPluginParameter.Filter1Par1: ;
    TPluginParameter.Filter1Par2: ;
    TPluginParameter.Filter1Par3: ;
    TPluginParameter.Filter1Par4: ;
    TPluginParameter.Filter2Par1: ;
    TPluginParameter.Filter2Par2: ;
    TPluginParameter.Filter2Par3: ;
    TPluginParameter.Filter2Par4: ;
    TPluginParameter.Lfo1Shape: ;
    TPluginParameter.Lfo2Shape: ;
    TPluginParameter.Lfo1FreqMode: ;
    TPluginParameter.Lfo2FreqMode: ;
    TPluginParameter.Lfo1Par1: ;
    TPluginParameter.Lfo1Par2: ;
    TPluginParameter.Lfo1Par3: ;
    TPluginParameter.Lfo2Par1: ;
    TPluginParameter.Lfo2Par2: ;
    TPluginParameter.Lfo2Par3: ;
    TPluginParameter.Seq1Clock: ;
    TPluginParameter.Seq1Direction: ;
    TPluginParameter.Seq1Length: ;
    TPluginParameter.Seq2Clock: ;
    TPluginParameter.Seq2Direction: ;
    TPluginParameter.Seq2Length: ;
    TPluginParameter.PreviewVolume: ;
    TPluginParameter.Preview: ;
  else
    raise Exception.Create('Plugin parameter not handled.');
  end;
end;

end.
