unit eePluginParameterWizard;

interface

uses
  eeVstParameterList, uLucidityVoiceController;


{
  NOTE: Rather then jam all the parameter set up in then TeePlugin constructor
  method I've pulled in out to TPluginParameterManager.
  This has the advantage of removing lots of code from the TeePlugin class..
  but the downside is, the parameter handling code still needs access to
  the main plugin class. To provide this I've kind of hacked in a way
  to access the plugin class. It circular reference and totally non-reusable.
  Normally this would be an absolutely huge code smell (and I wouldn't do it).
  But maybe I'm going to make an exception here....
}

type
  TPluginParameterWizard= class
  private
    VstParameters   : TVstParameterList;
  public
    constructor Create(aPlugin : TObject; VoiceController : TLucidityVoiceController);
    destructor Destroy; override;


  end;

implementation

uses
  SysUtils,
  uConstants,
  eePlugin, eeVstParameter,
  eeFunctions, eeGlobals,
  LucidityParameterScaling,
  uLucidityEnums;


{ TPluginParameterManger }

constructor TPluginParameterWizard.Create(aPlugin : TObject; VoiceController : TLucidityVoiceController);
var
  Plugin : TeePlugin;
  c1 : integer;
  InfoMethod : TStringFunction;
  aPar : TVstParameter;
  Globals : TGlobals;
begin
  Plugin := (aPlugin as TeePlugin);
  Globals := Plugin.Globals;

  VstParameters := TVstParameterList.Create;
  VstParameters.OwnsObjects := true;


  //== Create all parameters ==
  aPar := VstParameters.NewParameter('VoiceMode');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Voice Mode: ' + TVoiceModeHelper.ToShortGuiString(VoiceController.VoiceMode);
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      VoiceController.VoiceMode  := TVoiceModeHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TVoiceModeHelper.ToSingle(VoiceController.VoiceMode)
    end);




  aPar := VstParameters.NewParameter('VoiceGlide');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Glide Time: ' + IntToStr(round(VoiceController.VoiceGlide * 100));
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      VoiceController.VoiceGlide := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := VoiceController.VoiceGlide;
    end);

  aPar := VstParameters.NewParameter('PitchTracking');
    aPar.ParInfoMethod(function:string
    begin
      case Plugin.ActiveVoicePar.PitchTracking of
        TPitchTracking.Note: result := 'Pitch Tracking: Note';
        TPitchTracking.BPM:  result := 'Pitch Tracking: BPM (For Tempo Synced Loops)';
        TPitchTracking.Off:  result := 'Pitch Tracking: Off';
      else
        result := 'Pitch Tracking: ERROR';
      end;
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.PitchTracking   := TPitchTrackingHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      value := TPitchTrackingHelper.ToSingle(Plugin.ActiveVoicePar.PitchTracking);
    end);




  aPar := VstParameters.NewParameter('SamplePlaybackType');
    aPar.ParInfoMethod(function:string
    begin
      //TODO:
      result := 'Sample Playback Mode: ';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SamplePlaybackType   := TSamplePlaybackTypeHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_SAMPLE_OSC_TYPE_CHANGED);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      value := TSamplePlaybackTypeHelper.ToSingle(Plugin.ActiveVoicePar.SamplePlaybackType);
    end);




  aPar := VstParameters.NewParameter('SampleResetClockSource');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Sample Reset - Select Trigger';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SampleReset := TClockSourceHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TClockSourceHelper.ToSingle(Plugin.ActiveVoicePar.SampleReset);
    end);




  //-- one shot sampler osc ---
  aPar := VstParameters.NewParameter('SamplerLoopBounds');
    aPar.ParInfoMethod(function:string
    begin
      case Plugin.ActiveVoicePar.SamplerLoopBounds of
        TSamplerLoopBounds.LoopSample: result := 'Sampler Loop Bounds: Loop Entire Sample';
        TSamplerLoopBounds.LoopPoints: result := 'Sampler Loop Bounds: Loop Using Loop Points';
      else
        raise Exception.Create('Type not handled.');
      end;
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SamplerLoopBounds := TSamplerLoopBoundsHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_LOOP_TYPE_CHANGED);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSamplerLoopBoundsHelper.ToSingle(Plugin.ActiveVoicePar.SamplerLoopBounds);
    end);

  aPar := VstParameters.NewParameter('SamplerLoopMode');
    aPar.ParInfoMethod(function:string
    begin
      case Plugin.ActiveVoicePar.SamplerLoopMode of
        TSamplerLoopMode.LoopOff:         result := 'Sampler Loop Mode: Loop Off';
        TSamplerLoopMode.OneShot:         result := 'Sampler Loop Mode: One Shot';
        TSamplerLoopMode.LoopRelease:  result := 'Sampler Loop Mode: Loop Continuous';
        TSamplerLoopMode.LoopSustain:     result := 'Sampler Loop Mode: Loop Sustain';
      else
        raise Exception.Create('Type not handled.');
      end;
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SamplerLoopMode := TSamplerLoopModeHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_LOOP_TYPE_CHANGED);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSamplerLoopModeHelper.ToSingle(Plugin.ActiveVoicePar.SamplerLoopMode);
    end);

  //-- grain stretch osc ---
  aPar := VstParameters.NewParameter('GrainLoop');
  aPar := VstParameters.NewParameter('GrainLength');
  aPar := VstParameters.NewParameter('GrainRate').SetMinMax(-1,1).SetDefault(0.5);
  aPar := VstParameters.NewParameter('GrainPosition');





  aPar := VstParameters.NewParameter('OutputGain').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Output Gain: ' + IntToStr(round(Plugin.ActiveVoicePar.VoiceGain * 100));
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoiceGain := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoiceGain;
    end);




  aPar := VstParameters.NewParameter('OutputPan').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Output Pan: ' + IntToStr(round(Plugin.ActiveVoicePar.VoicePan * 100));
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoicePan := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePan;
    end);




  aPar := VstParameters.NewParameter('VoicePitchOne').SetMinMax(-1,1).SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    var
      x : integer;
    begin
      x := round(Plugin.ActiveVoicePar.VoicePitchOne * 12);
      result := 'Keygroup Tune: ' + IntToStr(x) + ' semitones';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoicePitchOne := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePitchOne;
    end);

  aPar := VstParameters.NewParameter('VoicePitchTwo').SetMinMax(-1,1).SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Keygroup Fine-Tune: ' + RoundFloatToStr(Plugin.ActiveVoicePar.VoicePitchTwo * 100) + ' cents';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoicePitchTwo := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePitchTwo;
    end);


  //TODO: Are these parameters needed?
  aPar := VstParameters.NewParameter('AuxALevel').SetDefault(0);
  aPar := VstParameters.NewParameter('AuxBLevel').SetDefault(0);
  aPar := VstParameters.NewParameter('OscShape');
  aPar := VstParameters.NewParameter('OscPulseWidth').SetDefault(0.5);
  aPar := VstParameters.NewParameter('NoiseLevel').SetDefault(0.5);

  aPar := VstParameters.NewParameter('SampleStart');
  aPar := VstParameters.NewParameter('SampleEnd').SetDefault(1);
  aPar := VstParameters.NewParameter('LoopStart');
  aPar := VstParameters.NewParameter('LoopEnd').SetDefault(1);




  aPar := VstParameters.NewParameter('AmpAttack').SetDefault(0);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Amp Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoicePar.AmpAttack)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpAttack := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpAttack;
    end);

  aPar := VstParameters.NewParameter('AmpHold').ParInfoMethod(InfoMethod).SetDefault(0);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Amp Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveVoicePar.AmpHold)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpHold := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpHold;
    end);




  aPar := VstParameters.NewParameter('AmpDecay').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Amp Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveVoicePar.AmpDecay)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpDecay := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpDecay;
    end);




  aPar := VstParameters.NewParameter('AmpSustain').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Amp Env Sustain: ' + IntToStr(round(Plugin.ActiveVoicePar.AmpSustain * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpSustain := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpSustain;
    end);




  aPar := VstParameters.NewParameter('AmpRelease');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Amp Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveVoicePar.AmpRelease)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpRelease := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpRelease;
    end);




  aPar := VstParameters.NewParameter('AmpVelocity'); //Amp Env Velocity.
    aPar.ParInfoMethod(function:string
    begin
      result := 'Env Velocity Depth: ' + TEnvVelocityDepthHelper.ToFullGuiString(Plugin.ActiveVoicePar.AmpVelocityDepth);
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpVelocityDepth := TEnvVelocityDepthHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TEnvVelocityDepthHelper.ToSingle(Plugin.ActiveVoicePar.AmpVelocityDepth);
    end);




  aPar := VstParameters.NewParameter('FilterAttack').SetDefault(0);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoicePar.FilterAttack)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterAttack := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterAttack;
    end);




  aPar := VstParameters.NewParameter('FilterHold').SetDefault(0);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveVoicePar.FilterHold)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterHold := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterHold;
    end);




  aPar := VstParameters.NewParameter('FilterDecay').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveVoicePar.FilterDecay)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterDecay := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterDecay;
    end);




  aPar := VstParameters.NewParameter('FilterSustain').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Env Sustain: ' + IntToStr(round(Plugin.ActiveVoicePar.FilterSustain * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterSustain := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterSustain;
    end);




  aPar := VstParameters.NewParameter('FilterRelease').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveVoicePar.FilterRelease)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterRelease := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterRelease;
    end);




  aPar := VstParameters.NewParameter('FilterVelocity');  //Filter Env Velocity.
    aPar.ParInfoMethod(function:string
    begin
      result := 'Env Velocity Depth: ' + TEnvVelocityDepthHelper.ToFullGuiString(Plugin.ActiveVoicePar.FilterVelocityDepth);
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterVelocityDepth := TEnvVelocityDepthHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TEnvVelocityDepthHelper.ToSingle(Plugin.ActiveVoicePar.FilterVelocityDepth);
    end);




  aPar := VstParameters.NewParameter('Filter1Type').SetDefault(0);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Type: ' + TFilterTypeHelper.ToFullGuiString(Plugin.ActiveVoicePar.Filter1Type);
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Type := TFilterTypeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_FILTER_CHANGED);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TFilterTypeHelper.ToSingle(Plugin.ActiveVoicePar.Filter1Type);
    end);




  aPar := VstParameters.NewParameter('Filter2Type').SetDefault(0);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Type: ' + TFilterTypeHelper.ToFullGuiString(Plugin.ActiveVoicePar.Filter2Type);
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Type := TFilterTypeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_FILTER_CHANGED);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TFilterTypeHelper.ToSingle(Plugin.ActiveVoicePar.Filter2Type);
    end);




  aPar := VstParameters.NewParameter('Filter1Par1').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par1 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par1 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par1;
    end);




  aPar := VstParameters.NewParameter('Filter1Par2').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par2 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par2 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par2;
    end);




  aPar := VstParameters.NewParameter('Filter1Par3').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par3 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par3 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par3;
    end);




  aPar := VstParameters.NewParameter('Filter1Par4').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par4 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par4 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par4;
    end);




  aPar := VstParameters.NewParameter('Filter2Par1').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par1 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par1 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par1;
    end);




  aPar := VstParameters.NewParameter('Filter2Par2').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par2 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par2 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par2;
    end);




  aPar := VstParameters.NewParameter('Filter2Par3').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par3 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par3 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par3;
    end);




  aPar := VstParameters.NewParameter('Filter2Par4').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par4 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par4 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par4;
    end);




  aPar := VstParameters.NewParameter('LfoShape1');
    aPar.ParInfoMethod(function:string
    begin
      result := 'LFO Shape';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoShape1 := TLfoShapeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoShapeHelper.ToSingle(Plugin.ActiveVoicePar.LfoShape1);
    end);




  aPar := VstParameters.NewParameter('LfoShape2');
    aPar.ParInfoMethod(function:string
    begin
      result := 'LFO Shape';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoShape2 := TLfoShapeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoShapeHelper.ToSingle(Plugin.ActiveVoicePar.LfoShape2);
    end);




  aPar := VstParameters.NewParameter('LfoRate1').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate1)) +'hz';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoRate1 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoRate1;
    end);




  aPar := VstParameters.NewParameter('LfoRate2').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate2)) +'hz';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
       Plugin.ActiveVoicePar.LfoRate2 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoRate2;
    end);



  aPar := VstParameters.NewParameter('LfoAPar2').SetDefault(1);
    aPar.ParInfoMethod(function:string
    begin
      result := 'LFO Mod: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoAPar2 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoAPar2 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoAPar2;
    end);




  aPar := VstParameters.NewParameter('LfoBPar2').SetDefault(1);
    aPar.ParInfoMethod(function:string
    begin
      result := 'LFO Mod: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoBPar2 * 100)) + '%';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoBPar2 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoBPar2;
    end);




  aPar := VstParameters.NewParameter('ModEnvAAttack').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Modulation Env Attack Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvAAttack)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvAAttack := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvAAttack;
    end);




  aPar := VstParameters.NewParameter('ModEnvADecay').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Modulation Env Decay Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvADecay)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvADecay := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvADecay;
    end);




  aPar := VstParameters.NewParameter('ModEnvAMode');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Modulation Env Type';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvAMode := TModEnvModeHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TModEnvModeHelper.ToSingle(Plugin.ActiveVoicePar.ModEnvAMode);
    end);




  aPar := VstParameters.NewParameter('ModEnvBAttack').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Modulation Env Attack Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvBAttack)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvBAttack := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvBAttack;
    end);




  aPar := VstParameters.NewParameter('ModEnvBDecay').SetDefault(0.5);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Modulation Env Decay Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvBDecay)) + 'ms';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvBDecay := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvBDecay;
    end);




  aPar := VstParameters.NewParameter('ModEnvBMode');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Modulation Env Type';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvBMode := TModEnvModeHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TModEnvModeHelper.ToSingle(Plugin.ActiveVoicePar.ModEnvBMode);
    end);




  aPar := VstParameters.NewParameter('Seq1Clock');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Sequencer Clock';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq1Clock := TSequencerClockHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSequencerClockHelper.ToSingle(Plugin.ActiveVoicePar.Seq1Clock);
    end);




  aPar := VstParameters.NewParameter('Seq1Direction');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Sequencer Direction';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq1Direction := TStepSequencerDirectionHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerDirectionHelper.ToSingle(Plugin.ActiveVoicePar.Seq1Direction);
    end);




  aPar := VstParameters.NewParameter('StepSeq1Length');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Sequencer Length';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.StepSeq1Length := TStepSequencerLengthHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerLengthHelper.ToSingle(Plugin.ActiveVoicePar.StepSeq1Length);
    end);




  aPar := VstParameters.NewParameter('Seq2Clock');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Sequencer Clock';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq2Clock := TSequencerClockHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSequencerClockHelper.ToSingle(Plugin.ActiveVoicePar.Seq2Clock);
    end);




  aPar := VstParameters.NewParameter('Seq2Direction');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Sequencer Direction';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq2Direction := TStepSequencerDirectionHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerDirectionHelper.ToSingle(Plugin.ActiveVoicePar.Seq2Direction);
    end);




  aPar := VstParameters.NewParameter('StepSeq2Length');
    aPar.ParInfoMethod(function:string
    begin
      result := 'Sequencer Length';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.StepSeq2Length := TStepSequencerLengthHelper.ToEnum(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerLengthHelper.ToSingle(Plugin.ActiveVoicePar.StepSeq2Length);
    end);




  aPar := VstParameters.NewParameter('PadX1').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX1 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX1;
    end);




  aPar := VstParameters.NewParameter('PadY1').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY1 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY1;
    end);




  aPar := VstParameters.NewParameter('PadX2').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX2 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX2;
    end);




  aPar := VstParameters.NewParameter('PadY2').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY2 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY2;
    end);




  aPar := VstParameters.NewParameter('PadX3').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX3 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX3;
    end);




  aPar := VstParameters.NewParameter('PadY3').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY3 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY3;
    end);




  aPar := VstParameters.NewParameter('PadX4').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX4 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX4;
    end);




  aPar := VstParameters.NewParameter('PadY4').SetDefault(0.5).SetPublished(true);
    aPar.ParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY4 := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY4;
    end);




  aPar := VstParameters.NewParameter('PreviewVolume').SetInputCurve(TParInputCurve.icSquare).SetMinMax(0,1.5).SetDefault(0.3);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Preview Volume';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.PreviewVolume := Value;
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.PreviewVolume;
    end);




  aPar := VstParameters.NewParameter('Preview').SetDefault(1);
    aPar.ParInfoMethod(function:string
    begin
      result := 'Preview On / Off';
    end);
    aPar.SetParValueCallback(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.IsPreviewEnabled := FloatToBoolean(Value);
    end);
    aPar.GetParValueCallback(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := BooleanToFloat(Plugin.IsPreviewEnabled);
    end);





  //============================================================================

  //== Additional parameter setup ==
  for c1 := 0 to VstParameters.Count-1 do
  begin
    //VstParameters[c1].OnParameterChanged := Plugin.VstParameterChanged;
    //VstParameters[c1].OnGetParameter     := GetVstParameter;
    Plugin.Globals.VstParameters.Add(VstParameters[c1]);
  end;

  //IMPORTANT: build published parameter info after adding all parameters...
  Plugin.Globals.VstParameters.BuildPublishedParameterInfo;
end;

destructor TPluginParameterWizard.Destroy;
begin
  VstParameters.Free;
  inherited;
end;


end.
