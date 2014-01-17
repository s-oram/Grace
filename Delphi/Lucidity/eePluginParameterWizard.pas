unit eePluginParameterWizard;

interface

uses
  eeVstParameterManager,
  eeVstParameterList,
  uLucidityVoiceController;

// GENERATE_PAR_INFO should be enabled when the parameter listing has changed and
// the automatically generated ParNames.inc file needs to be updated.
{_$define GENERATE_PAR_INFO}




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
    {$ifdef GENERATE_PAR_INFO}
    procedure GenerateParInfoText(ParManager : TVstParameterManager);
    {$endif}
  public
    constructor Create(aPlugin : TObject; VoiceController : TLucidityVoiceController; ParManager : TVstParameterManager);
    destructor Destroy; override;
  end;

implementation

uses
  LucidityUtils,
  Classes,
  VamLib.Utils,
  VamLib.Collections.Lists,
  SysUtils,
  uConstants,
  eePlugin,
  eeVstParameter,
  eeVstParameterEx,
  eeFunctions, eeGlobals,
  LucidityParameterScaling,
  uLucidityEnums;


{ TPluginParameterManger }

constructor TPluginParameterWizard.Create(aPlugin : TObject; VoiceController : TLucidityVoiceController; ParManager : TVstParameterManager);
  function GetModLinkIndex(var Index:integer): integer;
  begin
    result := Index;
    inc(index);
  end;
var
  Plugin : TeePlugin;
  c1 : integer;
  InfoMethod : TStringFunction;
  aPar : TVstParameterEx;
  Globals : TGlobals;
  aModLinkIndex : integer;
begin
  Plugin := (aPlugin as TeePlugin);

  Globals := Plugin.Globals;

  aModLinkIndex := 0;

  //== Create all parameters ==
  aPar := TVstParameterEx.Create('VoiceMode');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;



  aPar := TVstParameterEx.Create('VoiceGlide');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;

  aPar := TVstParameterEx.Create('PitchTracking');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('SamplePlaybackType');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('SampleResetClockSource');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  //-- one shot sampler osc ---
  aPar := TVstParameterEx.Create('SamplerLoopBounds');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;

  aPar := TVstParameterEx.Create('SamplerLoopMode');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;

  //-- grain stretch osc ---
  aPar := TVstParameterEx.Create('GrainLoop');
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('GrainLength');
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('GrainRate');
  aPar.SetMinMax(-1,1).SetDefault(0.5);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('GrainPosition');
  Parmanager.Add(aPar);





  aPar := TVstParameterEx.Create('OutputGain');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Output Gain: ' + IntToStr(round(Plugin.ActiveVoicePar.VoiceGain * 100));
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.VoiceGain := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoiceGain;
    end);
  end;




  aPar := TVstParameterEx.Create('OutputPan');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Output Pan: ' + IntToStr(round(Plugin.ActiveVoicePar.VoicePan * 100));
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.VoicePan := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePan;
    end);
  end;




  aPar := TVstParameterEx.Create('VoicePitchOne');
  aPar.SetMinMax(-1,1).SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    var
      x : integer;
    begin
      x := round(Plugin.ActiveVoicePar.VoicePitchOne * 12);
      result := 'Keygroup Tune: ' + IntToStr(x) + ' semitones';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.VoicePitchOne := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePitchOne;
    end);
  end;

  aPar := TVstParameterEx.Create('VoicePitchTwo');
  aPar.SetMinMax(-1,1).SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Keygroup Fine-Tune: ' + RoundFloatToStr(Plugin.ActiveVoicePar.VoicePitchTwo * 100) + ' cents';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.VoicePitchTwo := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.VoicePitchTwo;
    end);
  end;



  //TODO: Are these parameters needed?
  aPar := TVstParameterEx.Create('AuxALevel');
  aPar.SetDefault(0);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('AuxBLevel');
  aPar.SetDefault(0);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('OscShape');
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('OscPulseWidth');
  aPar.SetDefault(0.5);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('NoiseLevel');
  aPar.SetDefault(0.5);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('SampleStart');
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('SampleEnd');
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  aPar.SetDefault(1);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('LoopStart');
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create('LoopEnd');
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  aPar.SetDefault(1);
  Parmanager.Add(aPar);





  aPar := TVstParameterEx.Create('AmpAttack');
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoicePar.AmpAttack)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.AmpAttack := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpAttack;
    end);
  end;

  aPar := TVstParameterEx.Create('AmpHold');
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveVoicePar.AmpHold)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.AmpHold := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpHold;
    end);
  end;




  aPar := TVstParameterEx.Create('AmpDecay');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveVoicePar.AmpDecay)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.AmpDecay := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpDecay;
    end);
  end;




  aPar := TVstParameterEx.Create('AmpSustain');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Sustain: ' + IntToStr(round(Plugin.ActiveVoicePar.AmpSustain * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.AmpSustain := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpSustain;
    end);
  end;




  aPar := TVstParameterEx.Create('AmpRelease');
  Parmanager.Add(aPar);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveVoicePar.AmpRelease)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.AmpRelease := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.AmpRelease;
    end);
  end;




  aPar := TVstParameterEx.Create('AmpVelocity'); //Amp Env Velocity.
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('FilterAttack');
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoicePar.FilterAttack)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.FilterAttack := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterAttack;
    end);
  end;




  aPar := TVstParameterEx.Create('FilterHold');
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveVoicePar.FilterHold)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.FilterHold := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterHold;
    end);
  end;




  aPar := TVstParameterEx.Create('FilterDecay');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveVoicePar.FilterDecay)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.FilterDecay := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterDecay;
    end);
  end;




  aPar := TVstParameterEx.Create('FilterSustain');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Sustain: ' + IntToStr(round(Plugin.ActiveVoicePar.FilterSustain * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.FilterSustain := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterSustain;
    end);
  end;




  aPar := TVstParameterEx.Create('FilterRelease');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveVoicePar.FilterRelease)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.FilterRelease := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.FilterRelease;
    end);
  end;




  aPar := TVstParameterEx.Create('FilterVelocity');  //Filter Env Velocity.
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('Filter1Type');
  aPar.SetDefault(0);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('Filter2Type');
  aPar.SetDefault(0);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('Filter1Par1');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par1 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter1Par1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par1;
    end);
  end;




  aPar := TVstParameterEx.Create('Filter1Par2');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter1Par2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par2;
    end);
  end;




  aPar := TVstParameterEx.Create('Filter1Par3');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter1Par3 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par3;
    end);
  end;




  aPar := TVstParameterEx.Create('Filter1Par4');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par4 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter1Par4 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1Par4;
    end);
  end;




  aPar := TVstParameterEx.Create('Filter2Par1');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par1 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter2Par1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par1;
    end);
  end;




  aPar := TVstParameterEx.Create('Filter2Par2');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter2Par2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par2;
    end);
  end;




  aPar := TVstParameterEx.Create('Filter2Par3');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter2Par3 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par3;
    end);
  end;




  aPar := TVstParameterEx.Create('Filter2Par4');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par4 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.Filter2Par4 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2Par4;
    end);
  end;




  aPar := TVstParameterEx.Create('LfoShape1');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('LfoShape2');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;


  //TODO: Rename the lfo parameters to
  // LfoA-Par1
  // LfoA-Par2
  // LfoB-Par1
  // LfoB-Par2


  aPar := TVstParameterEx.Create('LfoRate1');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate1)) +'hz';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.LfoRate1 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoRate1;
    end);
  end;




  aPar := TVstParameterEx.Create('LfoRate2');
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate2)) +'hz';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
       assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.LfoRate2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoRate2;
    end);
  end;



  aPar := TVstParameterEx.Create('LfoAPar2');
  aPar.SetDefault(1);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Mod: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoAPar2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.LfoAPar2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoAPar2;
    end);
  end;




  aPar := TVstParameterEx.Create('LfoBPar2');
  aPar.SetDefault(1);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Mod: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoBPar2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      assert((Sender as TVstParameterEx).HasModLink);
      Plugin.ActiveVoiceModPar^[(Sender as TVstParameterEx).ModLinkIndex].ParValue := Value;
      Plugin.ActiveVoicePar.LfoBPar2 := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.LfoBPar2;
    end);
  end;





  aPar := TVstParameterEx.Create('Seq1Clock');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('Seq1Direction');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('StepSeq1Length');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('Seq2Clock');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('Seq2Direction');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('StepSeq2Length');
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;





  aPar := TVstParameterEx.Create('PreviewVolume');
  aPar.SetInputCurve(TParInputCurve.icSquare).SetMinMax(0,1.5).SetDefault(0.3);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  aPar := TVstParameterEx.Create('Preview');
  aPar.SetDefault(1);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
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
  end;




  for c1 := 0 to ParManager.Count-1 do
  begin
    ParInfoEx[c1].Name := ParManager[c1].Name;
    if (Parmanager[c1] as TVstParameterEx).HasModLink
      then ParInfoEx[c1].ModLinkIndex := (Parmanager[c1] as TVstParameterEx).ModLinkIndex
      else ParInfoEx[c1].ModLinkIndex := -1;

  end;

  //============================================================================
  //IMPORTANT: build published parameter info after adding all parameters...
  Parmanager.BuildPublishedParameterInfo;


  {$ifdef GENERATE_PAR_INFO}
  GenerateParInfoText(ParManager);
  {$endif}

  assert(ParManager.Count = kParameterCount);
  assert(aModLinkIndex = kModulatedParameterCount);
end;



destructor TPluginParameterWizard.Destroy;
begin

  inherited;
end;



{$ifdef GENERATE_PAR_INFO}
procedure TPluginParameterWizard.GenerateParInfoText(ParManager : TVstParameterManager);
// This method generates a text file of named indexes (as constants)
// for the VST parameters. This text file is then included into
// uConstants.pas. Ideally it should be done as part of the
// precompilation stage.
var
  Text : TStringList;
  c1 : integer;
  s : string;
  VstPar : TVstParameterEx;
  fn : string;
begin
  Text := TStringList.Create;
  AutoFree(@Text);


  Text.Add('type');
  Text.Add('  TVstParIndex = record');
  Text.Add('  const');

  //Add regular parameters...
  for c1 := 0 to Parmanager.Count-1 do
  begin
    VstPar := Parmanager[c1] as TVstParameterEx;

    s := '    ' + VstPar.Name + ' = ' + IntToStr(c1) + ';';
    Text.Add(s);
  end;

  Text.Add('  end;');

  s := '  ';
  Text.Add(s);
  Text.Add(s);


  Text.Add('  TModParIndex = record');
  Text.Add('  const');

  //Add modulated parameters...
  for c1 := 0 to Parmanager.Count-1 do
  begin
    VstPar := Parmanager[c1] as TVstParameterEx;
    if VstPar.HasModLink then
    begin
      s := '    ' + VstPar.Name + ' = ' + IntToStr(VstPar.ModLinkIndex) + ';';
      Text.Add(s);
    end;
  end;

  Text.Add('  end;');

  fn := 'D:\Delphi Projects\Lucidity\Delphi\Lucidity\ParNames.inc';

  Text.SaveToFile(fn);

end;

{$endif}

end.
