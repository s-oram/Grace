unit Lucidity.PluginParameterController;

interface

{$INCLUDE Defines.inc}

uses
  eeTypes,
  eePublishedVstParameters,
  VamLib.Utils,
  Lucidity.Interfaces,
  Lucidity.PluginParameters,
  Lucidity.Types;


type
  TPluginParameterController = class
  private
    class procedure SendMessages(const aPlugin : TObject; const Par : TPluginParameter; const ParValue : single); static;
  public
    class procedure SetPluginParameter(const aPlugin : TObject; const Scope : TParChangeScope; const KeyGroupName : string; const ParID : TPluginParameterID; const ParValue : single); static; inline;
    class function GetPluginParameter(const aPlugin : TObject; const ParID : TPluginParameterID):single; static; inline;

    class procedure SetParameterModAmount(const aPlugin : TObject; const Scope : TParChangeScope; const ParName : string; const ModSlot : integer; const ModAmount : single); static; inline;
    class function GetParameterModAmount(const aPlugin : TObject; const ParName : string; const ModSlot : integer):single; static; inline;

    class procedure GetModParModMinMax(const aPlugin : TObject; const ParName : string; out ModMin, ModMax:single); static; inline;

    class function GetParameterInfo(const aPlugin : TObject; const ParName : string) : TPluginParameterInfo; static; inline;

    class function GetPluginParameterVstInfo(const aPlugin : TObject; const ParName : string):TVstParameterInfo; static; inline;

    class procedure ApplyPluginParToKeyGroup(const KeyGroup : IKeyGroup; const Par : TPluginParameter; const ParValue : single; const SmoothingRequired : boolean = true); static;

    class procedure ResetKeyGroupParameters(const KeyGroup : IKeyGroup); static;
  end;

  // TODO: This class will contain all the code required to set/get the parameter values
  // to the internal audio engine.

implementation

uses
  Lucidity.KeyGroupManager,
  uConstants,
  SysUtils,
  soLucidityVoiceParameterWrapper,
  Lucidity.KeyGroup,
  Lucidity.Enums,
  eePlugin;

{ TPluginParameterController }

class function TPluginParameterController.GetPluginParameter(const aPlugin : TObject; const ParID : TPluginParameterID): single;
var
  Plugin : TeePlugin;
  Par : TPluginParameter;
  KeyGroup: IKeyGroup;
  VoicePar : TLucidityVoiceParameterWrapper;
  ModParIndex : integer;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  KeyGroup := Plugin.ActiveKeyGroup;
  if not assigned(KeyGroup) then exit(0);

  Par := PluginParFromID(ParID);


  //======= Plugin scope Parameters ======================
  if IsGlobalPluginPar(Par) then
  begin
    case Par of
      TPluginParameter.VoiceMode:     result := TVoiceModeHelper.ToSingle(Plugin.VoiceMode);
      TPluginParameter.VoiceGlide:    result := Plugin.VoiceGlide;
      TPluginParameter.PreviewVolume: result := Plugin.PreviewVolume;
      TPluginParameter.Preview:       result := BooleanToFloat(Plugin.IsPreviewEnabled);
      TPluginParameter.PadX1:         result := Plugin.XYPads.PadX1;
      TPluginParameter.PadY1:         result := Plugin.XYPads.PadY1;
      TPluginParameter.PadX2:         result := Plugin.XYPads.PadX2;
      TPluginParameter.PadY2:         result := Plugin.XYPads.PadY2;
      TPluginParameter.PadX3:         result := Plugin.XYPads.PadX3;
      TPluginParameter.PadY3:         result := Plugin.XYPads.PadY3;
      TPluginParameter.PadX4:         result := Plugin.XYPads.PadX4;
      TPluginParameter.PadY4:         result := Plugin.XYPads.PadY4;
    else
      raise Exception.Create('Type not handled.');
    end;
    exit; //=============================================>> exit >>=================>>
  end;

  //======= Voice / Key Group scope Parameters ======================
  if IsModPar(Par) then
  begin
    ModParIndex := GetModParIndex(Par);
    result := KeyGroup.GetModParValue(ModParIndex);
  end else
  begin
    VoicePar := (KeyGroup.GetObject as TKeyGroup).VoiceParameters;

    case Par of
      TPluginParameter.PitchTracking:          result := TPitchTrackingHelper.ToSingle(VoicePar.PitchTracking);
      TPluginParameter.SamplePlaybackType:     result := TSamplePlaybackTypeHelper.ToSingle(VoicePar.SamplePlaybackType);
      TPluginParameter.SampleResetClockSource: result := TClockSourceHelper.ToSingle(VoicePar.SampleReset);
      TPluginParameter.SamplerLoopBounds:      result := TSamplerLoopBoundsHelper.ToSingle(VoicePar.SamplerLoopBounds);
      TPluginParameter.SamplerTriggerMode:     result := TKeyGroupTriggerModeHelper.ToSingle(VoicePar.SamplerTriggerMode);
      TPluginParameter.AmpVelocity:            result := TEnvVelocityDepthHelper.ToSingle(VoicePar.AmpVelocityDepth);
      TPluginParameter.ModVelocity:            result := TEnvVelocityDepthHelper.ToSingle(VoicePar.ModVelocityDepth);
      TPluginParameter.AmpEnvSnap:             result := TEnvSnapHelper.ToSingle(VoicePar.AmpEnvSnap);
      TPluginParameter.ModEnvSnap:             result := TEnvSnapHelper.ToSingle(VoicePar.ModEnvSnap);
      TPluginParameter.FilterRouting:          result := TFilterRoutingHelper.ToSingle(VoicePar.FilterRouting);
      TPluginParameter.Filter1Type:            result := TFilterTypeHelper.ToSingle(VoicePar.Filter1Type);
      TPluginParameter.Filter2Type:            result := TFilterTypeHelper.ToSingle(VoicePar.Filter2Type);
      TPluginParameter.Filter1KeyFollow:       result := VoicePar.Filter1KeyFollow;
      TPluginParameter.Filter2KeyFollow:       result := VoicePar.Filter2KeyFollow;
      TPluginParameter.Lfo1Shape:              result := TLfoShapeHelper.ToSingle(VoicePar.LfoShape1);
      TPluginParameter.Lfo2Shape:              result := TLfoShapeHelper.ToSingle(VoicePar.LfoShape2);
      TPluginParameter.Lfo1FreqMode:           result := TLfoFreqModeHelper.ToSingle(VoicePar.LfoFreqMode1);
      TPluginParameter.Lfo2FreqMode:           result := TLfoFreqModeHelper.ToSingle(VoicePar.LfoFreqMode2);
      TPluginParameter.Seq1Clock:              result := TSequencerClockHelper.ToSingle(VoicePar.Seq1Clock);
      TPluginParameter.Seq1Direction:          result := TStepSequencerDirectionHelper.ToSingle(VoicePar.Seq1Direction);
      TPluginParameter.Seq1Length:             result := TStepSequencerLengthHelper.ToSingle(VoicePar.StepSeq1Length);
      TPluginParameter.Seq2Clock:              result := TSequencerClockHelper.ToSingle(VoicePar.Seq2Clock);
      TPluginParameter.Seq2Direction:          result := TStepSequencerDirectionHelper.ToSingle(VoicePar.Seq2Direction);
      TPluginParameter.Seq2Length:             result := TStepSequencerLengthHelper.ToSingle(VoicePar.StepSeq2Length);
    else
      raise Exception.Create('Type not handled.');
    end;
  end;
end;

class procedure TPluginParameterController.SetPluginParameter(
    const aPlugin : TObject;
    const Scope: TParChangeScope;
    const KeyGroupName: string;
    const ParID : TPluginParameterID;
    const ParValue: single);
var
  c1 : integer;
  Plugin : TeePlugin;
  Par : TPluginParameter;
  kg : IKeyGroup;
  kgInfo : IKeyGroupsInfo;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  //Par := PluginParFromName(ParName);
  Par := PluginParFromID(ParID);

  //======= Plugin scope Parameters ======================
  if IsGlobalPluginPar(Par) then
  begin
    //TODO: Setting the PadXY parameters here causes cpu spikes. I've no idea why.

    case Par of
      TPluginParameter.VoiceMode:
      begin
        Plugin.VoiceMode  := TVoiceModeHelper.ToEnum(ParValue);
        // TODO:HIGH remove all MsgVclTS calls from the plugin parameter controller.
        Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateControlVisibility, nil);
      end;
      TPluginParameter.VoiceGlide:    Plugin.VoiceGlide := ParValue;
      TPluginParameter.PreviewVolume: Plugin.PreviewVolume := ParValue;
      TPluginParameter.Preview:       Plugin.IsPreviewEnabled := FloatToBoolean(ParValue);
      TPluginParameter.PadX1:         Plugin.XYPads.PadX1 := ParValue;
      TPluginParameter.PadY1:         Plugin.XYPads.PadY1 := ParValue;
      TPluginParameter.PadX2:         Plugin.XYPads.PadX2 := ParValue;
      TPluginParameter.PadY2:         Plugin.XYPads.PadY2 := ParValue;
      TPluginParameter.PadX3:         Plugin.XYPads.PadX3 := ParValue;
      TPluginParameter.PadY3:         Plugin.XYPads.PadY3 := ParValue;
      TPluginParameter.PadX4:         Plugin.XYPads.PadX4 := ParValue;
      TPluginParameter.PadY4:         Plugin.XYPads.PadY4 := ParValue;
      else
        raise Exception.Create('Type not handed.');
    end;

    SendMessages(Plugin, Par, ParValue);

    exit; //=============================================>> exit >>=================>>
  end;

  //======= Voice / Key Group scope Parameters ======================
  case Scope of
    psGlobal:
    begin
      kgInfo := Plugin.KeyGroups.GetInfo;

      for c1 := 0 to kgInfo.GetKeyGroupCount-1 do
      begin
        kg := kgInfo.GetKeyGroup(c1);
        if assigned(kg) then
        begin
          ApplyPluginParToKeyGroup(kg, Par, ParValue);
          SendMessages(Plugin, Par, ParValue);
        end;
      end;
    end;

    psFocused:
    begin
      kg :=  Plugin.ActiveKeyGroup;
      if assigned(kg) then
      begin
        ApplyPluginParToKeyGroup(kg, Par, ParValue);
        SendMessages(Plugin, Par, ParValue);
      end;
    end;

  else
    raise Exception.Create('Type not handled.');
  end;

end;

class procedure TPluginParameterController.ApplyPluginParToKeyGroup(
  const KeyGroup: IKeyGroup;
  const Par: TPluginParameter;
  const ParValue: single;
  const SmoothingRequired : boolean = true);
var
  VoicePar : TLucidityVoiceParameterWrapper;
  ModParIndex : integer;
begin
  assert(IsGlobalPluginPar(Par) = false);

  if IsModPar(Par) then
  begin
    ModParIndex := GetModParIndex(Par);
    KeyGroup.SetModParValue(ModParIndex, ParValue);
  end else
  begin
    VoicePar := (KeyGroup.GetObject as TKeyGroup).VoiceParameters;

    case Par of
      TPluginParameter.PitchTracking:          VoicePar.PitchTracking       := TPitchTrackingHelper.ToEnum(ParValue);
      TPluginParameter.SamplePlaybackType:     VoicePar.SamplePlaybackType  := TSamplePlaybackTypeHelper.ToEnum(ParValue);
      TPluginParameter.SampleResetClockSource: VoicePar.SampleReset         := TClockSourceHelper.ToEnum(ParValue);
      TPluginParameter.SamplerLoopBounds:      VoicePar.SamplerLoopBounds   := TSamplerLoopBoundsHelper.ToEnum(ParValue);
      TPluginParameter.SamplerTriggerMode:     VoicePar.SamplerTriggerMode  := TKeyGroupTriggerModeHelper.ToEnum(ParValue);
      TPluginParameter.AmpVelocity:            VoicePar.AmpVelocityDepth    := TEnvVelocityDepthHelper.ToEnum(ParValue);
      TPluginParameter.ModVelocity:            VoicePar.ModVelocityDepth    := TEnvVelocityDepthHelper.ToEnum(ParValue);
      TPluginParameter.AmpEnvSnap:             VoicePar.AmpEnvSnap          := TEnvSnapHelper.ToEnum(ParValue);
      TPluginParameter.ModEnvSnap:             VoicePar.ModEnvSnap          := TEnvSnapHelper.ToEnum(ParValue);
      TPluginParameter.FilterRouting:          VoicePar.FilterRouting       := TFilterRoutingHelper.ToEnum(ParValue);
      TPluginParameter.Filter1Type:            VoicePar.Filter1Type         := TFilterTypeHelper.ToEnum(ParValue);
      TPluginParameter.Filter2Type:            VoicePar.Filter2Type         := TFilterTypeHelper.ToEnum(ParValue);
      TPluginParameter.Filter1KeyFollow:       VoicePar.Filter1KeyFollow    := ParValue;
      TPluginParameter.Filter2KeyFollow:       VoicePar.Filter2KeyFollow    := ParValue;
      TPluginParameter.Lfo1Shape:              VoicePar.LfoShape1           := TLfoShapeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo2Shape:              VoicePar.LfoShape2           := TLfoShapeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo1FreqMode:           VoicePar.LfoFreqMode1        := TLfoFreqModeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo2FreqMode:           VoicePar.LfoFreqMode2        := TLfoFreqModeHelper.ToEnum(ParValue);
      TPluginParameter.Seq1Clock:              VoicePar.Seq1Clock           := TSequencerClockHelper.ToEnum(ParValue);
      TPluginParameter.Seq1Direction:          VoicePar.Seq1Direction       := TStepSequencerDirectionHelper.ToEnum(ParValue);
      TPluginParameter.Seq1Length:             VoicePar.StepSeq1Length      := TStepSequencerLengthHelper.ToEnum(ParValue);
      TPluginParameter.Seq2Clock:              VoicePar.Seq2Clock           := TSequencerClockHelper.ToEnum(ParValue);
      TPluginParameter.Seq2Direction:          VoicePar.Seq2Direction       := TStepSequencerDirectionHelper.ToEnum(ParValue);
      TPluginParameter.Seq2Length:             VoicePar.StepSeq2Length      := TStepSequencerLengthHelper.ToEnum(ParValue);
    else
      raise Exception.Create('Type not handled.');
    end;

  end;
end;

class procedure TPluginParameterController.GetModParModMinMax(const aPlugin: TObject; const ParName: string; out ModMin, ModMax: single);
var
  Plugin : TeePlugin;
  Par : TPluginParameter;
  ModParIndex : integer;
  kg : IKeyGroup;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  Par := PluginParFromName(ParName);

  assert(IsModPar(Par));
  ModParIndex := GetModParIndex(Par);
  assert(ModParIndex <> -1);

  kg :=  Plugin.ActiveKeyGroup;
  if assigned(kg) then
  begin
    kg.GetModParModMinMax(ModParIndex, ModMin, ModMax);
  end else
  begin
    ModMin := 0;
    ModMax := 0;
  end;
end;

class function TPluginParameterController.GetParameterModAmount(
  const aPlugin: TObject;
  const ParName: string;
  const ModSlot: integer): single;
var
  Plugin : TeePlugin;
  Par : TPluginParameter;
  ModParIndex : integer;
  kg : IKeyGroup;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  Par := PluginParFromName(ParName);

  assert(IsModPar(Par));
  ModParIndex := GetModParIndex(Par);
  assert(ModParIndex <> -1);

  kg :=  Plugin.ActiveKeyGroup;
  if assigned(kg)
    then result := kg.GetModParModAmount(ModParIndex, ModSlot)
    else result := 0;
end;


class procedure TPluginParameterController.SetParameterModAmount(
  const aPlugin: TObject;
  const Scope: TParChangeScope;
  const ParName: string;
  const ModSlot: integer;
  const ModAmount: single);
var
  c1 : integer;
  Plugin : TeePlugin;
  Par : TPluginParameter;
  ModParIndex : integer;
  kg : IKeyGroup;
  kgInfo : IKeyGroupsInfo;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  Par := PluginParFromName(ParName);

  assert(IsModPar(Par));
  ModParIndex := GetModParIndex(Par);
  assert(ModParIndex <> -1);

  //======= Voice / Key Group scope Parameters ======================

  case Scope of
    psGlobal:
    begin
      kgInfo := Plugin.KeyGroups.GetInfo;

      for c1 := 0 to kgInfo.GetKeyGroupCount-1 do
      begin
        kg := kgInfo.GetKeyGroup(c1);
        kg.SetModParModAmount(ModParIndex, ModSlot, ModAmount);
      end;
    end;

    psFocused:
    begin
      kg :=  Plugin.ActiveKeyGroup;
      if assigned(kg)
        then kg.SetModParModAmount(ModParIndex, ModSlot, ModAmount);
    end;

  else
    raise Exception.Create('Type not handled.');
  end;

end;



class function TPluginParameterController.GetParameterInfo(const aPlugin: TObject; const ParName: string): TPluginParameterInfo;
begin
  //TODO:
end;

class function TPluginParameterController.GetPluginParameterVstInfo(const aPlugin: TObject; const ParName: string): TVstParameterInfo;
var
  Par : TPluginParameter;
begin
  result.Name := ParName;
  result.ShortName := ParName;
  result.Display := '';
  result.Units   := '';

  Par := PluginParFromName(ParName);


  //  Parameter Name...
  case Par of
    TPluginParameter.VoiceMode:               result.Name := 'Voice Mode';
    TPluginParameter.VoiceGlide:              result.Name := 'Voice Glide';
    TPluginParameter.PitchTracking:           result.Name := 'Pitch Tracking';
    //TPluginParameter.SamplePlaybackType:      result.Name := '';
    TPluginParameter.SampleResetClockSource:  result.Name := 'Sample Reset';
    TPluginParameter.SamplerLoopBounds:       result.Name := 'Sample Loop Bounds';
    TPluginParameter.SamplerTriggerMode:         result.Name := 'Sample Loop Mode';
    TPluginParameter.OutputGain:              result.Name := 'Voice Gain';
    TPluginParameter.OutputPan:               result.Name := 'Voice Pan';
    TPluginParameter.VoicePitchOne:           result.Name := 'Voice Tune';
    TPluginParameter.VoicePitchTwo:           result.Name := 'Voice Pan';
    //TPluginParameter.SampleStart:             result.Name := 'Sample Start';
    //TPluginParameter.SampleEnd:               result.Name := '';
    //TPluginParameter.LoopStart:               result.Name := '';
    //TPluginParameter.LoopEnd:                 result.Name := '';
    TPluginParameter.AmpAttack:               result.Name := 'Amp Attack';
    TPluginParameter.AmpHold:                 result.Name := 'Amp Hold';
    TPluginParameter.AmpDecay:                result.Name := 'Amp Decay';
    TPluginParameter.AmpSustain:              result.Name := 'Amp Sustain';
    TPluginParameter.AmpRelease:              result.Name := 'Amp Release';
    TPluginParameter.AmpVelocity:             result.Name := 'Amp Velocity';
    TPluginParameter.AmpEnvSnap:              result.Name := 'Amp Snap';
    TPluginParameter.ModAttack:               result.Name := 'Mod Attack';
    TPluginParameter.ModHold:                 result.Name := 'Mod Hold';
    TPluginParameter.ModDecay:                result.Name := 'Mod Decay';
    TPluginParameter.ModSustain:              result.Name := 'Mod Sustain';
    TPluginParameter.ModRelease:              result.Name := 'Mod Release';
    TPluginParameter.ModVelocity:             result.Name := 'Mod Velocity';
    TPluginParameter.ModEnvSnap:              result.Name := 'Mod Snap';
    TPluginParameter.FilterRouting:           result.Name := 'FilterRouting';
    TPluginParameter.FilterOutputBlend:       result.Name := 'FilterOutput Blend';
    TPluginParameter.Filter1Type:             result.Name := 'Filter1 Type';
    TPluginParameter.Filter2Type:             result.Name := 'Filter2 Type';
    TPluginParameter.Filter1KeyFollow:        result.Name := 'Filter1 Keyfollow';
    TPluginParameter.Filter2KeyFollow:        result.Name := 'Filter2 Keyfollow';
    TPluginParameter.Filter1Par1:             result.Name := 'Filter1 Par1';
    TPluginParameter.Filter1Par2:             result.Name := 'Filter1 Par2';
    TPluginParameter.Filter1Par3:             result.Name := 'Filter1 Par3';
    TPluginParameter.Filter1Par4:             result.Name := 'Filter1 Par4';
    TPluginParameter.Filter2Par1:             result.Name := 'Filter2 Par1';
    TPluginParameter.Filter2Par2:             result.Name := 'Filter2 Par2';
    TPluginParameter.Filter2Par3:             result.Name := 'Filter2 Par3';
    TPluginParameter.Filter2Par4:             result.Name := 'Filter2 Par4';
    TPluginParameter.Lfo1Shape:               result.Name := 'Lfo1 Shape';
    TPluginParameter.Lfo2Shape:               result.Name := 'Lfo2 Shape';
    TPluginParameter.Lfo1FreqMode:            result.Name := 'Lfo1 Freq Mode';
    TPluginParameter.Lfo2FreqMode:            result.Name := 'Lfo2 Freq Mode';
    TPluginParameter.Lfo1Par1:                result.Name := 'Lfo1 Par1';
    TPluginParameter.Lfo1Par2:                result.Name := 'Lfo1 Par2';
    TPluginParameter.Lfo1Par3:                result.Name := 'Lfo1 Par3';
    TPluginParameter.Lfo2Par1:                result.Name := 'Lfo2 Par1';
    TPluginParameter.Lfo2Par2:                result.Name := 'Lfo2 Par2';
    TPluginParameter.Lfo2Par3:                result.Name := 'Lfo2 Par3';
    TPluginParameter.Seq1Clock:               result.Name := 'Seq1 Clock';
    TPluginParameter.Seq1Direction:           result.Name := 'Seq1 Direction';
    TPluginParameter.Seq1Length:              result.Name := 'Seq1 Length';
    TPluginParameter.Seq2Clock:               result.Name := 'Seq2 Clock';
    TPluginParameter.Seq2Direction:           result.Name := 'Seq2 Direction';
    TPluginParameter.Seq2Length:              result.Name := 'Seq2 Length';
    TPluginParameter.PreviewVolume:           result.Name := '';
    TPluginParameter.Preview:                 result.Name := '';
    TPluginParameter.PadX1:                   result.ShortName := 'Pad X1';
    TPluginParameter.PadY1:                   result.ShortName := 'Pad Y1';
    TPluginParameter.PadX2:                   result.ShortName := 'Pad X2';
    TPluginParameter.PadY2:                   result.ShortName := 'Pad Y2';
    TPluginParameter.PadX3:                   result.ShortName := 'Pad X3';
    TPluginParameter.PadY3:                   result.ShortName := 'Pad Y3';
    TPluginParameter.PadX4:                   result.ShortName := 'Pad X4';
    TPluginParameter.PadY4:                   result.ShortName := 'Pad Y4';
  else
    raise Exception.Create('Type not handled.');
  end;


  // Short Parameter Name... (I think this needs to be 8 charactors or less.
  case Par of
    TPluginParameter.VoiceMode:               result.ShortName := 'Voice Md';
    TPluginParameter.VoiceGlide:              result.ShortName := 'Voice Gd';
    TPluginParameter.PitchTracking:           result.ShortName := 'Pitch Tk';
    TPluginParameter.SamplePlaybackType:      result.ShortName := '';
    TPluginParameter.SampleResetClockSource:  result.ShortName := 'Smp Rst';
    TPluginParameter.SamplerLoopBounds:       result.ShortName := 'SmpLpBnd';
    TPluginParameter.SamplerTriggerMode:         result.ShortName := 'SmpLpMd';
    TPluginParameter.OutputGain:              result.ShortName := 'Out Gain';
    TPluginParameter.OutputPan:               result.ShortName := 'Out Pan';
    TPluginParameter.VoicePitchOne:           result.ShortName := 'Tune';
    TPluginParameter.VoicePitchTwo:           result.ShortName := 'Fine';
    TPluginParameter.SampleStart:             result.ShortName := 'Smp Strt';
    TPluginParameter.SampleEnd:               result.ShortName := 'Smp End';
    TPluginParameter.LoopStart:               result.ShortName := 'Lp Strt';
    TPluginParameter.LoopEnd:                 result.ShortName := 'Lp End';
    TPluginParameter.AmpAttack:               result.ShortName := 'Amp A';
    TPluginParameter.AmpHold:                 result.ShortName := 'Amp H';
    TPluginParameter.AmpDecay:                result.ShortName := 'Amp D';
    TPluginParameter.AmpSustain:              result.ShortName := 'Amp S';
    TPluginParameter.AmpRelease:              result.ShortName := 'Amp R';
    TPluginParameter.AmpVelocity:             result.ShortName := 'Amp Vel';
    TPluginParameter.AmpEnvSnap:              result.ShortName := 'Amp Snap';
    TPluginParameter.ModAttack:               result.ShortName := 'Mod A';
    TPluginParameter.ModHold:                 result.ShortName := 'Mod H';
    TPluginParameter.ModDecay:                result.ShortName := 'Mod D';
    TPluginParameter.ModSustain:              result.ShortName := 'Mod S';
    TPluginParameter.ModRelease:              result.ShortName := 'Mod R';
    TPluginParameter.ModVelocity:             result.ShortName := 'Mod Vel';
    TPluginParameter.ModEnvSnap:              result.ShortName := 'Mod Snap';
    TPluginParameter.FilterRouting:           result.ShortName := 'Flt Rout';
    TPluginParameter.FilterOutputBlend:       result.ShortName := 'Flt Blnd';
    TPluginParameter.Filter1Type:             result.ShortName := 'Flt1 Typ';
    TPluginParameter.Filter2Type:             result.ShortName := 'Flt2 Typ';
    TPluginParameter.Filter1KeyFollow:        result.ShortName := 'Flt1 Key';
    TPluginParameter.Filter2KeyFollow:        result.ShortName := 'Flt2 Key';
    TPluginParameter.Filter1Par1:             result.ShortName := 'Flt1 P1';
    TPluginParameter.Filter1Par2:             result.ShortName := 'Flt1 P2';
    TPluginParameter.Filter1Par3:             result.ShortName := 'Flt1 P3';
    TPluginParameter.Filter1Par4:             result.ShortName := 'Flt1 P4';
    TPluginParameter.Filter2Par1:             result.ShortName := 'Flt2 P1';
    TPluginParameter.Filter2Par2:             result.ShortName := 'Flt2 P2';
    TPluginParameter.Filter2Par3:             result.ShortName := 'Flt2 P3';
    TPluginParameter.Filter2Par4:             result.ShortName := 'Flt2 P4';
    TPluginParameter.Lfo1Shape:               result.ShortName := 'Lfo1 Shp';
    TPluginParameter.Lfo2Shape:               result.ShortName := 'Lfo2 Shp';
    TPluginParameter.Lfo1FreqMode:            result.ShortName := 'Lfo1Mode';
    TPluginParameter.Lfo2FreqMode:            result.ShortName := 'Lfo2Mode';
    TPluginParameter.Lfo1Par1:                result.ShortName := 'Lfo1 P1';
    TPluginParameter.Lfo1Par2:                result.ShortName := 'Lfo1 P2';
    TPluginParameter.Lfo1Par3:                result.ShortName := 'Lfo1 P3';
    TPluginParameter.Lfo2Par1:                result.ShortName := 'Lfo2 P1';
    TPluginParameter.Lfo2Par2:                result.ShortName := 'Lfo2 P2';
    TPluginParameter.Lfo2Par3:                result.ShortName := 'Lfo2 P3';
    TPluginParameter.Seq1Clock:               result.ShortName := 'Seq1 Clk';
    TPluginParameter.Seq1Direction:           result.ShortName := 'Seq1 Dir';
    TPluginParameter.Seq1Length:              result.ShortName := 'Seq1 Len';
    TPluginParameter.Seq2Clock:               result.ShortName := 'Seq2 Clk';
    TPluginParameter.Seq2Direction:           result.ShortName := 'Seq2 Dir';
    TPluginParameter.Seq2Length:              result.ShortName := 'Seq2 Len';
    TPluginParameter.PreviewVolume:           result.ShortName := '';
    TPluginParameter.Preview:                 result.ShortName := '';
    TPluginParameter.PadX1:                   result.ShortName := 'Pad X1';
    TPluginParameter.PadY1:                   result.ShortName := 'Pad Y1';
    TPluginParameter.PadX2:                   result.ShortName := 'Pad X2';
    TPluginParameter.PadY2:                   result.ShortName := 'Pad Y2';
    TPluginParameter.PadX3:                   result.ShortName := 'Pad X3';
    TPluginParameter.PadY3:                   result.ShortName := 'Pad Y3';
    TPluginParameter.PadX4:                   result.ShortName := 'Pad X4';
    TPluginParameter.PadY4:                   result.ShortName := 'Pad Y4';
  else
    raise Exception.Create('Type not handled.');
  end;

end;

class procedure TPluginParameterController.SendMessages(const aPlugin: TObject; const Par: TPluginParameter; const ParValue: single);
var
  Plugin : TeePlugin;
begin
  // NOTE: Some parameter changes require messages to be sent afterwards so
  // the GUI (or other things can be updated. This set up is kind of the result
  // of some decisions made in older code. It might be possible to have
  // these changes sent from the source of the changes.

  // TODO: at the moment all messages are destined for the GUI.
  // perhaps this method should promptly exit if the GUI isn't open.
  // TODO: perhaps the messages here need to be thottled.

  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  if IsModPar(Par) then exit;

  // TODO:HIGH try to have all these MsgVclTS() calls removed. the GUI should send these messages at the calling site
  // where ever possible.
  case Par of
    TPluginParameter.FilterRouting:
    begin
      Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.FilterChanged, nil);
      Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateScope, nil);
    end;
    TPluginParameter.PitchTracking:       Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateControlVisibility, nil);
    TPluginParameter.SamplerLoopBounds:   Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateSampleInfo, nil);
    TPluginParameter.SamplePlaybackType:  Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleOscTypeChanged, nil);
    TPluginParameter.Filter1Type:         Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.FilterChanged, nil);
    TPluginParameter.Filter2Type:         Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.FilterChanged, nil);
    TPluginParameter.Lfo1Shape:           Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged, nil);
    TPluginParameter.Lfo2Shape:           Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged, nil);
    TPluginParameter.Lfo1FreqMode:        Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged, nil);
    TPluginParameter.Lfo2FreqMode:        Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged, nil);
    TPluginParameter.SamplerTriggerMode:  Plugin.Globals.MotherShip.MsgAudio(TLucidMsgID.AudioCommand_QuickReleaseAllNotes);
    TPluginParameter.VoiceMode:           Plugin.Globals.MotherShip.MsgAudio(TLucidMsgID.AudioCommand_QuickReleaseAllNotes);
  end;
end;


class procedure TPluginParameterController.ResetKeyGroupParameters(const KeyGroup: IKeyGroup);
var
  c1, c2: Integer;
  Par : TPluginParameter;
  ParValue : single;
  ModParIndex : integer;
begin
  assert(assigned(KeyGroup));

  for c1 := 0 to TPluginParameterHelper.GetEnumTypeCount-1 do
  begin
    Par := TPluginParameterHelper.ToEnum(c1);

    //=== set all parameters to their default value ==
    if IsGlobalPluginPar(Par) = false then
    begin
      ParValue := GetPluginParInfo(Par).DefaultValue;
      ApplyPluginParToKeyGroup(KeyGroup, Par, ParValue, false);
    end;

    //=== set all mod amount values to 0 ==
    if IsModPar(Par) then
    begin
      ModParIndex := GetModParIndex(Par);
      for c2 := 0 to kModSlotCount-1 do
      begin
        KeyGroup.SetModParModAmount(ModParIndex, c2, 0);
      end;
    end;

    //=== set all modulation slots to default values?? ====
  end;
end;








end.
