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

// TODO: Currently all parameters are 'private' parameters. I will
// need to expose some parameters as VST parameters
// so that they can be automated in plugin hosts.
// TODO: Check if Kontakt exposes any parameters.



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

  SetModPar_Callback: TSetParValueProcedure;
  GetModPar_Callback: TGetParValueProcedure;
begin
  Plugin := (aPlugin as TeePlugin);
  Globals := Plugin.Globals;
  aModLinkIndex := 0;


  SetModPar_Callback := procedure(Sender : TVstParameter; Value:single)
  var
    Index : integer;
  begin
    assert((Sender as TVstParameterEx).HasModLink);
    Index := (Sender as TVstParameterEx).ModLinkIndex;
    Plugin.ActiveKeyGroup.SetModParValue(Index, Value);
  end;


  GetModPar_Callback := procedure(Sender : TVstParameter; out Value:single)
  var
    Index : integer;
  begin
    assert((Sender as TVstParameterEx).HasModLink);
    Index := (Sender as TVstParameterEx).ModLinkIndex;
    Value := Plugin.ActiveKeyGroup.GetModParValue(Index);
  end;




  aPar := TVstParameterEx.Create(TParName.VoiceMode);
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
      Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_UpdateControlVisibility);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TVoiceModeHelper.ToSingle(VoiceController.VoiceMode)
    end);
  end;



  aPar := TVstParameterEx.Create(TParName.VoiceGlide);
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

  aPar := TVstParameterEx.Create(TParName.PitchTracking);
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
      Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_UpdateControlVisibility);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      value := TPitchTrackingHelper.ToSingle(Plugin.ActiveVoicePar.PitchTracking);
    end);
  end;




  aPar := TVstParameterEx.Create(TParName.SamplePlaybackType);
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
      Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleOscTypeChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      value := TSamplePlaybackTypeHelper.ToSingle(Plugin.ActiveVoicePar.SamplePlaybackType);
    end);
  end;




  aPar := TVstParameterEx.Create(TParName.SampleResetClockSource);
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
  aPar := TVstParameterEx.Create(TParName.SamplerLoopBounds);
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
      Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LoopTypeChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSamplerLoopBoundsHelper.ToSingle(Plugin.ActiveVoicePar.SamplerLoopBounds);
    end);
  end;

  aPar := TVstParameterEx.Create(TParName.SamplerLoopMode);
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
      Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LoopTypeChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TSamplerLoopModeHelper.ToSingle(Plugin.ActiveVoicePar.SamplerLoopMode);
    end);
  end;

  //-- grain stretch osc ---
  aPar := TVstParameterEx.Create(TParName.GrainLoop);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.GrainLength);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.GrainRate);
  aPar.SetMinMax(-1,1).SetDefault(0.5);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.GrainPosition);
  Parmanager.Add(aPar);





  aPar := TVstParameterEx.Create(TParName.OutputGain);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Output Gain: ' + IntToStr(round(Plugin.ActiveVoicePar.VoiceGain * 100));
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.OutputPan);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Output Pan: ' + IntToStr(round(Plugin.ActiveVoicePar.VoicePan * 100));
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;



  aPar := TVstParameterEx.Create(TParName.VoicePitchOne);
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
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;

  aPar := TVstParameterEx.Create(TParName.VoicePitchTwo);
  aPar.SetMinMax(-1,1).SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Keygroup Fine-Tune: ' + RoundFloatToStr(Plugin.ActiveVoicePar.VoicePitchTwo * 100) + ' cents';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;



  //TODO: Are these parameters needed?
  aPar := TVstParameterEx.Create(TParName.AuxALevel);
  aPar.SetDefault(0);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.AuxBLevel);
  aPar.SetDefault(0);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.OscShape);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.OscPulseWidth);
  aPar.SetDefault(0.5);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.NoiseLevel);
  aPar.SetDefault(0.5);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.SampleStart);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.SampleEnd);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  aPar.SetDefault(1);
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.LoopStart);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);

  aPar := TVstParameterEx.Create(TParName.LoopEnd);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  aPar.SetDefault(1);
  Parmanager.Add(aPar);





  aPar := TVstParameterEx.Create(TParName.AmpAttack);
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string

    begin
      //result := 'Amp Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoiceModPar^[TModParIndex.AmpAttack].ParValue)) + 'ms';
      result := 'Amp Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveKeyGroup.GetModParValue(TModParIndex.AmpAttack))) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;

  aPar := TVstParameterEx.Create(TParName.AmpHold);
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveKeyGroup.GetModParValue(TModParIndex.AmpHold))) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.AmpDecay);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveKeyGroup.GetModParValue(TModParIndex.AmpDecay))) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.AmpSustain);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Sustain: ' + IntToStr(round(Plugin.ActiveKeyGroup.GetModParValue(TModParIndex.AmpSustain) * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.AmpRelease);
  Parmanager.Add(aPar);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Amp Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveKeyGroup.GetModParValue(TModParIndex.AmpRelease))) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.AmpVelocity); //Amp Env Velocity.
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




  aPar := TVstParameterEx.Create(TParName.FilterAttack);
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Attack: ' + RoundFloatToStr(TParScaler.ADSR_AttackTimeToMS(Plugin.ActiveVoicePar.FilterAttack)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.FilterHold);
  aPar.SetDefault(0);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Hold: ' + RoundFloatToStr(TParScaler.ADSR_HoldTimeToMS(Plugin.ActiveVoicePar.FilterHold)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.FilterDecay);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Decay: ' + RoundFloatToStr(TParScaler.ADSR_DecayTimeToMS(Plugin.ActiveVoicePar.FilterDecay)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.FilterSustain);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Sustain: ' + IntToStr(round(Plugin.ActiveVoicePar.FilterSustain * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.FilterRelease);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Env Release: ' + RoundFloatToStr(TParScaler.ADSR_ReleaseTimeToMS(Plugin.ActiveVoicePar.FilterRelease)) + 'ms';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.FilterVelocity);  //Filter Env Velocity.
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


  aPar := TVstParameterEx.Create(TParName.FilterRouting);
  aPar.SetDefault(0);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Routing';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.FilterRouting := TFilterRoutingHelper.ToEnum(Value);
      Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.FilterChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TFilterRoutingHelper.ToSingle(Plugin.ActiveVoicePar.FilterRouting);
    end);
  end;

  aPar := TVstParameterEx.Create(TParName.FilterOutputBlend);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Blend';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter1Type);
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
      Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.FilterChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TFilterTypeHelper.ToSingle(Plugin.ActiveVoicePar.Filter1Type);
    end);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter2Type);
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
      Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.FilterChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TFilterTypeHelper.ToSingle(Plugin.ActiveVoicePar.Filter2Type);
    end);
  end;


  aPar := TVstParameterEx.Create(TParName.Filter1KeyFollow);
  aPar.SetDefault(0);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Key Follow: ';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter1KeyFollow := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter1KeyFollow;
    end);
  end;

  aPar := TVstParameterEx.Create(TParName.Filter2KeyFollow);
  aPar.SetDefault(0);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Key Follow: ';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.Filter2KeyFollow := Value;
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := Plugin.ActiveVoicePar.Filter2KeyFollow;
    end);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter1Par1);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par1 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter1Par2);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter1Par3);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter1Par4);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter1Par4 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter2Par1);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter One: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par1 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter2Par2);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Two: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter2Par3);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Three: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Filter2Par4);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'Filter Parameter Four: ' + IntToStr(round(Plugin.ActiveVoicePar.Filter2Par4 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;


  //TODO: Filter Blend Parameter




  aPar := TVstParameterEx.Create(TParName.Lfo1Shape);
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
      Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoShapeHelper.ToSingle(Plugin.ActiveVoicePar.LfoShape1);
    end);
  end;




  aPar := TVstParameterEx.Create(TParName.Lfo2Shape);
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
      Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoShapeHelper.ToSingle(Plugin.ActiveVoicePar.LfoShape2);
    end);
  end;


  //===========================================================================

  aPar := TVstParameterEx.Create(TParName.Lfo1FreqMode);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Freq Mode';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoFreqMode1 := TLfoFreqModeHelper.ToEnum(Value);
      Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoFreqModeHelper.ToSingle(Plugin.ActiveVoicePar.LfoFreqMode1);
    end);
  end;




  aPar := TVstParameterEx.Create(TParName.Lfo2FreqMode);
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Shape';
    end);
    aPar.SetCallback_SetParValue(procedure(Sender:TVstParameter; Value : single)
    begin
      Plugin.ActiveVoicePar.LfoFreqMode2 := TLfoFreqModeHelper.ToEnum(Value);
      Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
    end);
    aPar.SetCallback_GetParValue(procedure(Sender:TVstParameter; out Value : single)
    begin
      Value := TLfoFreqModeHelper.ToSingle(Plugin.ActiveVoicePar.LfoFreqMode2);
    end);
  end;

  //===========================================================================


  aPar := TVstParameterEx.Create(TParName.Lfo1Par1);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate1)) +'hz';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;

  aPar := TVstParameterEx.Create(TParName.Lfo1Par2);
  aPar.SetDefault(1);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO A Par2: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoAPar2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;

  aPar := TVstParameterEx.Create(TParName.Lfo1Par3);
  aPar.SetDefault(1);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      //TODO:
      result := 'LFO A Par3: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoAPar3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;




  aPar := TVstParameterEx.Create(TParName.Lfo2Par1);
  aPar.SetDefault(0.5);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO Rate: ' + RoundFloatToStr(TParScaler.LFO_SpeedToFrequency(Plugin.ActiveVoicePar.LfoRate2)) +'hz';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;

  aPar := TVstParameterEx.Create(TParName.Lfo2Par2);
  aPar.SetDefault(1);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      result := 'LFO B Par2: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoBPar2 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;


  aPar := TVstParameterEx.Create(TParName.Lfo2Par3);
  aPar.SetDefault(1);
  aPar.SetHasModLink(true, GetModLinkIndex(aModLinkIndex));
  Parmanager.Add(aPar);
  if (assigned(Plugin)) and (assigned(VoiceController)) then
  begin
    aPar.SetCallback_SetParInfoMethod(function:string
    begin
      //TODO:
      result := 'LFO B Par3: ' + IntToStr(round(Plugin.ActiveVoicePar.LfoBPar3 * 100)) + '%';
    end);
    aPar.SetCallback_SetParValue(SetModPar_Callback);
    aPar.SetCallback_GetParValue(GetModPar_Callback);
  end;





  aPar := TVstParameterEx.Create(TParName.Seq1Clock);
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




  aPar := TVstParameterEx.Create(TParName.Seq1Direction);
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




  aPar := TVstParameterEx.Create(TParName.Seq1Length);
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




  aPar := TVstParameterEx.Create(TParName.Seq2Clock);
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




  aPar := TVstParameterEx.Create(TParName.Seq2Direction);
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




  aPar := TVstParameterEx.Create(TParName.Seq2Length);
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





  aPar := TVstParameterEx.Create(TParName.PreviewVolume);
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




  aPar := TVstParameterEx.Create(TParName.Preview);
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



  {$ifndef GENERATE_PAR_INFO}

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

  {$endif}



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
