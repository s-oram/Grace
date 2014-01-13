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
  eePlugin,
  eeVstParameter,
  eeVstParameterEx,
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
  aPar := TVstParameter.Create('VoiceMode');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Voice Mode: ' + TVoiceModeHelper.ToShortGuiString(VoiceController.VoiceMode);
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      VoiceController.VoiceMode  := TVoiceModeHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TVoiceModeHelper.ToSingle(VoiceController.VoiceMode)
    end);




  aPar := TVstParameter.Create('VoiceGlide');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Glide Time: ' + IntToStr(round(VoiceController.VoiceGlide * 100));
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      VoiceController.VoiceGlide := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := VoiceController.VoiceGlide;
    end);

  aPar := TVstParameter.Create('PitchTracking');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      case Plugin.ActiveVoicePar.PitchTracking of
        TPitchTracking.Note: result := 'Pitch Tracking: Note';
        TPitchTracking.BPM:  result := 'Pitch Tracking: BPM (For Tempo Synced Loops)';
        TPitchTracking.Off:  result := 'Pitch Tracking: Off';
      else
        result := 'Pitch Tracking: ERROR';
      end;
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.PitchTracking   := TPitchTrackingHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      value := TPitchTrackingHelper.ToSingle(Plugin.ActiveVoicePar.PitchTracking);
    end);




  aPar := TVstParameter.Create('SamplePlaybackType');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      //TODO:
      result := 'Sample Playback Mode: ';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SamplePlaybackType   := TSamplePlaybackTypeHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_SAMPLE_OSC_TYPE_CHANGED);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      value := TSamplePlaybackTypeHelper.ToSingle(Plugin.ActiveVoicePar.SamplePlaybackType);
    end);




  aPar := TVstParameter.Create('SampleResetClockSource');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Sample Reset - Select Trigger';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SampleReset := TClockSourceHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TClockSourceHelper.ToSingle(Plugin.ActiveVoicePar.SampleReset);
    end);




  //-- one shot sampler osc ---
  aPar := TVstParameter.Create('SamplerLoopBounds');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      case Plugin.ActiveVoicePar.SamplerLoopBounds of
        TSamplerLoopBounds.LoopSample: result := 'Sampler Loop Bounds: Loop Entire Sample';
        TSamplerLoopBounds.LoopPoints: result := 'Sampler Loop Bounds: Loop Using Loop Points';
      else
        raise Exception.Create('Type not handled.');
      end;
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SamplerLoopBounds := TSamplerLoopBoundsHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_LOOP_TYPE_CHANGED);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSamplerLoopBoundsHelper.ToSingle(Plugin.ActiveVoicePar.SamplerLoopBounds);
    end);

  aPar := TVstParameter.Create('SamplerLoopMode');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
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
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.SamplerLoopMode := TSamplerLoopModeHelper.ToEnum(Value);
      Globals.SendWindowsMessage(UM_LOOP_TYPE_CHANGED);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSamplerLoopModeHelper.ToSingle(Plugin.ActiveVoicePar.SamplerLoopMode);
    end);

  //-- grain stretch osc ---
  aPar := TVstParameter.Create('GrainLoop');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('GrainLength');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('GrainRate').SetMinMax(-1,1).SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('GrainPosition');
  Plugin.Globals.VstParameters.Add(aPar);





  aPar := TVstParameter.Create('OutputGain').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Output Gain: ' + IntToStr(round(Plugin.ActiveVoicePar.VoiceGain * 100));
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoiceGain := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoiceGain;
    end);




  aPar := TVstParameter.Create('OutputPan').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Output Pan: ' + IntToStr(round(Plugin.ActiveVoicePar.VoicePan * 100));
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoicePan := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePan;
    end);




  aPar := TVstParameter.Create('VoicePitchOne').SetMinMax(-1,1).SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    var
      x : integer;
    begin
      x := round(Plugin.ActiveVoicePar.VoicePitchOne * 12);
      result := 'Keygroup Tune: ' + IntToStr(x) + ' semitones';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoicePitchOne := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePitchOne;
    end);

  aPar := TVstParameter.Create('VoicePitchTwo').SetMinMax(-1,1).SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Keygroup Fine-Tune: ' + RoundFloatToStr(Plugin.ActiveVoicePar.VoicePitchTwo * 100) + ' cents';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.VoicePitchTwo := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePitchTwo;
    end);


  //TODO: Are these parameters needed?
  aPar := TVstParameter.Create('AuxALevel').SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('AuxBLevel').SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('OscShape');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('OscPulseWidth').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('NoiseLevel').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('SampleStart');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('SampleEnd').SetDefault(1);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('LoopStart');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameter.Create('LoopEnd').SetDefault(1);
  Plugin.Globals.VstParameters.Add(aPar);





  aPar := TVstParameter.Create('AmpAttack').SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoicePar.AmpAttack)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpAttack := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpAttack;
    end);

  aPar := TVstParameter.Create('AmpHold').SetCallback_SetParInfoMethod(InfoMethod).SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveVoicePar.AmpHold)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpHold := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpHold;
    end);




  aPar := TVstParameter.Create('AmpDecay').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveVoicePar.AmpDecay)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpDecay := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpDecay;
    end);




  aPar := TVstParameter.Create('AmpSustain').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Sustain: ' + IntToStr(round(Plugin.ActiveVoicePar.AmpSustain * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpSustain := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpSustain;
    end);




  aPar := TVstParameter.Create('AmpRelease');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveVoicePar.AmpRelease)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpRelease := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpRelease;
    end);




  aPar := TVstParameter.Create('AmpVelocity'); //Amp Env Velocity.
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Env Velocity Depth: ' + TEnvVelocityDepthHelper.ToFullGuiString(Plugin.ActiveVoicePar.AmpVelocityDepth);
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.AmpVelocityDepth := TEnvVelocityDepthHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TEnvVelocityDepthHelper.ToSingle(Plugin.ActiveVoicePar.AmpVelocityDepth);
    end);




  aPar := TVstParameter.Create('FilterAttack').SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoicePar.FilterAttack)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterAttack := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterAttack;
    end);




  aPar := TVstParameter.Create('FilterHold').SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveVoicePar.FilterHold)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterHold := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterHold;
    end);




  aPar := TVstParameter.Create('FilterDecay').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveVoicePar.FilterDecay)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterDecay := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterDecay;
    end);




  aPar := TVstParameter.Create('FilterSustain').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Sustain: ' + IntToStr(round(Plugin.ActiveVoicePar.FilterSustain * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterSustain := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterSustain;
    end);




  aPar := TVstParameter.Create('FilterRelease').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveVoicePar.FilterRelease)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterRelease := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterRelease;
    end);




  aPar := TVstParameter.Create('FilterVelocity');  //Filter Env Velocity.
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Env Velocity Depth: ' + TEnvVelocityDepthHelper.ToFullGuiString(Plugin.ActiveVoicePar.FilterVelocityDepth);
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterVelocityDepth := TEnvVelocityDepthHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TEnvVelocityDepthHelper.ToSingle(Plugin.ActiveVoicePar.FilterVelocityDepth);
    end);




  aPar := TVstParameter.Create('Filter1Type').SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Type: ' + TFilterTypeHelper.ToFullGuiString(Plugin.ActiveVoicePar.Filter1Type);
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Type := TFilterTypeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_FILTER_CHANGED);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TFilterTypeHelper.ToSingle(Plugin.ActiveVoicePar.Filter1Type);
    end);




  aPar := TVstParameter.Create('Filter2Type').SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Type: ' + TFilterTypeHelper.ToFullGuiString(Plugin.ActiveVoicePar.Filter2Type);
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Type := TFilterTypeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_FILTER_CHANGED);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TFilterTypeHelper.ToSingle(Plugin.ActiveVoicePar.Filter2Type);
    end);




  aPar := TVstParameter.Create('Filter1Par1').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par1 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par1;
    end);




  aPar := TVstParameter.Create('Filter1Par2').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par2;
    end);




  aPar := TVstParameter.Create('Filter1Par3').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par3 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par3;
    end);




  aPar := TVstParameter.Create('Filter1Par4').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par4 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1Par4 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par4;
    end);




  aPar := TVstParameter.Create('Filter2Par1').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par1 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par1;
    end);




  aPar := TVstParameter.Create('Filter2Par2').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par2;
    end);




  aPar := TVstParameter.Create('Filter2Par3').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par3 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par3;
    end);




  aPar := TVstParameter.Create('Filter2Par4').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par4 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2Par4 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par4;
    end);




  aPar := TVstParameter.Create('LfoShape1');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Shape';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoShape1 := TLfoShapeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoShapeHelper.ToSingle(Plugin.ActiveVoicePar.LfoShape1);
    end);




  aPar := TVstParameter.Create('LfoShape2');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Shape';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoShape2 := TLfoShapeHelper.ToEnum(Value);
      Plugin.Globals.SendWindowsMessage(UM_Update_Control_Visibility);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoShapeHelper.ToSingle(Plugin.ActiveVoicePar.LfoShape2);
    end);




  aPar := TVstParameter.Create('LfoRate1').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate1)) +'hz';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoRate1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoRate1;
    end);




  aPar := TVstParameter.Create('LfoRate2').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate2)) +'hz';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
       Plugin.ActiveVoicePar.LfoRate2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoRate2;
    end);



  aPar := TVstParameter.Create('LfoAPar2').SetDefault(1);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Mod: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoAPar2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoAPar2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoAPar2;
    end);




  aPar := TVstParameter.Create('LfoBPar2').SetDefault(1);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Mod: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoBPar2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoBPar2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoBPar2;
    end);




  aPar := TVstParameter.Create('ModEnvAAttack').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Modulation Env Attack Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvAAttack)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvAAttack := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvAAttack;
    end);




  aPar := TVstParameter.Create('ModEnvADecay').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Modulation Env Decay Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvADecay)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvADecay := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvADecay;
    end);




  aPar := TVstParameter.Create('ModEnvAMode');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Modulation Env Type';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvAMode := TModEnvModeHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TModEnvModeHelper.ToSingle(Plugin.ActiveVoicePar.ModEnvAMode);
    end);




  aPar := TVstParameter.Create('ModEnvBAttack').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Modulation Env Attack Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvBAttack)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvBAttack := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvBAttack;
    end);




  aPar := TVstParameter.Create('ModEnvBDecay').SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Modulation Env Decay Time: ' + RoundFloatToStr(TParScaler.ModEnv_StageTimeToMS(Plugin.ActiveVoicePar.ModEnvBDecay)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvBDecay := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.ModEnvBDecay;
    end);




  aPar := TVstParameter.Create('ModEnvBMode');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Modulation Env Type';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.ModEnvBMode := TModEnvModeHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TModEnvModeHelper.ToSingle(Plugin.ActiveVoicePar.ModEnvBMode);
    end);




  aPar := TVstParameter.Create('Seq1Clock');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Sequencer Clock';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq1Clock := TSequencerClockHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSequencerClockHelper.ToSingle(Plugin.ActiveVoicePar.Seq1Clock);
    end);




  aPar := TVstParameter.Create('Seq1Direction');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Sequencer Direction';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq1Direction := TStepSequencerDirectionHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerDirectionHelper.ToSingle(Plugin.ActiveVoicePar.Seq1Direction);
    end);




  aPar := TVstParameter.Create('StepSeq1Length');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Sequencer Length';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.StepSeq1Length := TStepSequencerLengthHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerLengthHelper.ToSingle(Plugin.ActiveVoicePar.StepSeq1Length);
    end);




  aPar := TVstParameter.Create('Seq2Clock');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Sequencer Clock';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq2Clock := TSequencerClockHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSequencerClockHelper.ToSingle(Plugin.ActiveVoicePar.Seq2Clock);
    end);




  aPar := TVstParameter.Create('Seq2Direction');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Sequencer Direction';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Seq2Direction := TStepSequencerDirectionHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerDirectionHelper.ToSingle(Plugin.ActiveVoicePar.Seq2Direction);
    end);




  aPar := TVstParameter.Create('StepSeq2Length');
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Sequencer Length';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.StepSeq2Length := TStepSequencerLengthHelper.ToEnum(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TStepSequencerLengthHelper.ToSingle(Plugin.ActiveVoicePar.StepSeq2Length);
    end);




  aPar := TVstParameter.Create('PadX1').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX1;
    end);




  aPar := TVstParameter.Create('PadY1').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY1;
    end);




  aPar := TVstParameter.Create('PadX2').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX2;
    end);




  aPar := TVstParameter.Create('PadY2').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY2;
    end);




  aPar := TVstParameter.Create('PadX3').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX3 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX3;
    end);




  aPar := TVstParameter.Create('PadY3').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY3 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY3;
    end);




  aPar := TVstParameter.Create('PadX4').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadX4 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadX4;
    end);




  aPar := TVstParameter.Create('PadY4').SetDefault(0.5).SetPublished(true);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'XY Modulation Source';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.XYPads.PadY4 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.XYPads.PadY4;
    end);




  aPar := TVstParameter.Create('PreviewVolume').SetInputCurve(TParInputCurve.icSquare).SetMinMax(0,1.5).SetDefault(0.3);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Preview Volume';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.PreviewVolume := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.PreviewVolume;
    end);




  aPar := TVstParameter.Create('Preview').SetDefault(1);
  Plugin.Globals.VstParameters.Add(aPar);
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Preview On / Off';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.IsPreviewEnabled := FloatToBoolean(Value);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
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
