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
  aPar : TVstParameterEx;
  Globals : TGlobals;
begin
  Plugin := (aPlugin as TeePlugin);
  Globals := Plugin.Globals;

  VstParameters := TVstParameterList.Create;
  VstParameters.OwnsObjects := true;


  //== Create all parameters ==
  aPar := TVstParameterEx.Create('VoiceMode');
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




  aPar := TVstParameterEx.Create('VoiceGlide');
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

  aPar := TVstParameterEx.Create('PitchTracking');
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




  aPar := TVstParameterEx.Create('SamplePlaybackType');
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




  aPar := TVstParameterEx.Create('SampleResetClockSource');
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
  aPar := TVstParameterEx.Create('SamplerLoopBounds');
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

  aPar := TVstParameterEx.Create('SamplerLoopMode');
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
  aPar := TVstParameterEx.Create('GrainLoop');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('GrainLength');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('GrainRate');
  aPar.SetMinMax(-1,1).SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('GrainPosition');
  Plugin.Globals.VstParameters.Add(aPar);





  aPar := TVstParameterEx.Create('OutputGain');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('OutputPan');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('VoicePitchOne');
  aPar.SetMinMax(-1,1).SetDefault(0.5);
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

  aPar := TVstParameterEx.Create('VoicePitchTwo');
  aPar.SetMinMax(-1,1).SetDefault(0.5);
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
  aPar := TVstParameterEx.Create('AuxALevel');
  aPar.SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('AuxBLevel');
  aPar.SetDefault(0);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('OscShape');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('OscPulseWidth');
  aPar.SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('NoiseLevel');
  aPar.SetDefault(0.5);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('SampleStart');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('SampleEnd');
  aPar.SetDefault(1);
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('LoopStart');
  Plugin.Globals.VstParameters.Add(aPar);

  aPar := TVstParameterEx.Create('LoopEnd');
  aPar.SetDefault(1);
  Plugin.Globals.VstParameters.Add(aPar);





  aPar := TVstParameterEx.Create('AmpAttack');
  aPar.SetDefault(0);
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

  aPar := TVstParameterEx.Create('AmpHold');
  aPar.SetDefault(0);
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




  aPar := TVstParameterEx.Create('AmpDecay');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('AmpSustain');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('AmpRelease');
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




  aPar := TVstParameterEx.Create('AmpVelocity'); //Amp Env Velocity.
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




  aPar := TVstParameterEx.Create('FilterAttack');
  aPar.SetDefault(0);
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




  aPar := TVstParameterEx.Create('FilterHold');
  aPar.SetDefault(0);
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




  aPar := TVstParameterEx.Create('FilterDecay');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('FilterSustain');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('FilterRelease');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('FilterVelocity');  //Filter Env Velocity.
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




  aPar := TVstParameterEx.Create('Filter1Type');
  aPar.SetDefault(0);
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




  aPar := TVstParameterEx.Create('Filter2Type');
  aPar.SetDefault(0);
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




  aPar := TVstParameterEx.Create('Filter1Par1');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('Filter1Par2');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('Filter1Par3');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('Filter1Par4');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('Filter2Par1');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('Filter2Par2');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('Filter2Par3');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('Filter2Par4');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('LfoShape1');
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




  aPar := TVstParameterEx.Create('LfoShape2');
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




  aPar := TVstParameterEx.Create('LfoRate1');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('LfoRate2');
  aPar.SetDefault(0.5);
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



  aPar := TVstParameterEx.Create('LfoAPar2');
  aPar.SetDefault(1);
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




  aPar := TVstParameterEx.Create('LfoBPar2');
  aPar.SetDefault(1);
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




  aPar := TVstParameterEx.Create('ModEnvAAttack');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('ModEnvADecay');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('ModEnvAMode');
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




  aPar := TVstParameterEx.Create('ModEnvBAttack');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('ModEnvBDecay');
  aPar.SetDefault(0.5);
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




  aPar := TVstParameterEx.Create('ModEnvBMode');
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




  aPar := TVstParameterEx.Create('Seq1Clock');
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




  aPar := TVstParameterEx.Create('Seq1Direction');
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




  aPar := TVstParameterEx.Create('StepSeq1Length');
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




  aPar := TVstParameterEx.Create('Seq2Clock');
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




  aPar := TVstParameterEx.Create('Seq2Direction');
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




  aPar := TVstParameterEx.Create('StepSeq2Length');
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




  aPar := TVstParameterEx.Create('PadX1');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PadY1');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PadX2');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PadY2');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PadX3');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PadY3');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PadX4');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PadY4');
  aPar.SetDefault(0.5).SetPublished(true);
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




  aPar := TVstParameterEx.Create('PreviewVolume');
  aPar.SetInputCurve(TParInputCurve.icSquare).SetMinMax(0,1.5).SetDefault(0.3);
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




  aPar := TVstParameterEx.Create('Preview');
  aPar.SetDefault(1);
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
  //IMPORTANT: build published parameter info after adding all parameters...
  Plugin.Globals.VstParameters.BuildPublishedParameterInfo;
end;

destructor TPluginParameterWizard.Destroy;
begin
  VstParameters.Free;
  inherited;
end;


end.
