unit Lucidity.PluginParameterController;

interface

uses
  VamLib.Utils,
  eePublishedVstParameters,
  Lucidity.Interfaces,
  Lucidity.PluginParameters,
  Lucidity.Types;


type
  TPluginParameterController = class
  private
    class procedure SendMessages(const aPlugin : TObject; const Par : TPluginParameter; const ParValue : single); static;
  public
    class procedure SetPluginParameter(const aPlugin : TObject; const Scope : TParChangeScope; const KeyGroupName : string; const ParName : string; const ParValue : single); static; inline;
    class function GetPluginParameter(const aPlugin : TObject; const ParName : string):single; static; inline;

    class procedure SetParameterModAmount(const aPlugin : TObject; const Scope : TParChangeScope; const ParName : string; const ModSlot : integer; const ModAmount : single); static; inline;
    class function GetParameterModAmount(const aPlugin : TObject; const ParName : string; const ModSlot : integer):single; static; inline;

    class procedure GetModParModMinMax(const aPlugin : TObject; const ParName : string; out ModMin, ModMax:single); static; inline;

    class function GetParameterInfo(const aPlugin : TObject; const ParName : string) : TPluginParameterInfo; static; inline;

    class function GetPluginParameterVstInfo(const aPlugin : TObject; const ParName : string):TVstParameterInfo; static; inline;

    class procedure ApplyPluginParToKeyGroup(const KeyGroup : IKeyGroup; const Par : TPluginParameter; const ParValue : single); static;

    class procedure ResetKeyGroupParameters(const KeyGroup : IKeyGroup); static;
  end;

  // TODO: This class will contain all the code required to set/get the parameter values
  // to the internal audio engine.

implementation

uses
  uKeyGroupManager,
  uConstants,
  SysUtils,
  soLucidityVoiceParameterWrapper,
  Lucidity.KeyGroup,
  uLucidityEnums,
  eePlugin;

{ TPluginParameterController }

class function TPluginParameterController.GetPluginParameter(const aPlugin : TObject; const ParName: string): single;
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

  Par := PluginParFromName(ParName);


  //======= Plugin scope Parameters ======================
  if IsGlobalPluginPar(Par) then
  begin
    case Par of
      TPluginParameter.VoiceMode:     result := TVoiceModeHelper.ToSingle(Plugin.VoiceMode);
      TPluginParameter.VoiceGlide:    result := Plugin.VoiceGlide;
      TPluginParameter.PreviewVolume: result := Plugin.PreviewVolume;
      TPluginParameter.Preview:       result := BooleanToFloat(Plugin.IsPreviewEnabled);
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
      TPluginParameter.SamplerLoopMode:        result := TKeyGroupTriggerModeHelper.ToSingle(VoicePar.SamplerLoopMode);
      TPluginParameter.AmpVelocity:            result := TEnvVelocityDepthHelper.ToSingle(VoicePar.AmpVelocityDepth);
      TPluginParameter.FilterVelocity:         result := TEnvVelocityDepthHelper.ToSingle(VoicePar.FilterVelocityDepth);
      TPluginParameter.FilterRouting:          result := TFilterRoutingHelper.ToSingle(VoicePar.FilterRouting);
      TPluginParameter.Filter1Type:            result := TFilterTypeHelper.ToSingle(VoicePar.Filter1Type);
      TPluginParameter.Filter2Type:            result := TFilterTypeHelper.ToSingle(VoicePar.Filter2Type);
      TPluginParameter.Filter1KeyFollow:       result := VoicePar.Filter1KeyFollow;
      TPluginParameter.Filter2KeyFollow:       result := VoicePar.Filter2KeyFollow;
      TPluginParameter.Lfo1Shape:              result := TLfoShapeHelper.ToSingle(VoicePar.LfoShape1);
      TPluginParameter.Lfo2Shape:              result := TLfoShapeHelper.ToSingle(VoicePar.LfoShape2);
      TPluginParameter.Lfo1FreqMode:           result := TLfoFreqModeHelper.ToSingle(VoicePar.LfoFreqMode1);
      TPluginParameter.Lfo2FreqMode:           result := TLfoFreqModeHelper.ToSingle(VoicePar.LfoFreqMode2);
      TPluginParameter.Lfo1Range:              result := TLfoRangeHelper.ToSingle(VoicePar.LfoRange1);
      TPluginParameter.Lfo2Range:              result := TLfoRangeHelper.ToSingle(VoicePar.LfoRange2);
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
    const ParName: string;
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

  Par := PluginParFromName(ParName);

  //======= Plugin scope Parameters ======================
  if IsGlobalPluginPar(Par) then
  begin
    case Par of
      TPluginParameter.VoiceMode:
      begin
        Plugin.VoiceMode  := TVoiceModeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_UpdateControlVisibility);
      end;
      TPluginParameter.VoiceGlide:    Plugin.VoiceGlide := ParValue;
      TPluginParameter.PreviewVolume: Plugin.PreviewVolume := ParValue;
      TPluginParameter.Preview:       Plugin.IsPreviewEnabled := FloatToBoolean(ParValue);
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

    psFocusedKeyGroup:
    begin
      kg :=  Plugin.ActiveKeyGroup;
      if assigned(kg) then
      begin
        ApplyPluginParToKeyGroup(kg, Par, ParValue);
        SendMessages(Plugin, Par, ParValue);
      end;
    end;

    psKeyGroup:
    begin
      kg := Plugin.KeyGroups.FindSampleGroup(KeyGroupName);
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
  const ParValue: single);
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
      TPluginParameter.SamplerLoopMode:        VoicePar.SamplerLoopMode     := TKeyGroupTriggerModeHelper.ToEnum(ParValue);
      TPluginParameter.AmpVelocity:            VoicePar.AmpVelocityDepth    := TEnvVelocityDepthHelper.ToEnum(ParValue);
      TPluginParameter.FilterVelocity:         VoicePar.FilterVelocityDepth := TEnvVelocityDepthHelper.ToEnum(ParValue);
      TPluginParameter.FilterRouting:          VoicePar.FilterRouting       := TFilterRoutingHelper.ToEnum(ParValue);
      TPluginParameter.Filter1Type:            VoicePar.Filter1Type         := TFilterTypeHelper.ToEnum(ParValue);
      TPluginParameter.Filter2Type:            VoicePar.Filter2Type         := TFilterTypeHelper.ToEnum(ParValue);
      TPluginParameter.Filter1KeyFollow:       VoicePar.Filter1KeyFollow    := ParValue;
      TPluginParameter.Filter2KeyFollow:       VoicePar.Filter2KeyFollow    := ParValue;
      TPluginParameter.Lfo1Shape:              VoicePar.LfoShape1           := TLfoShapeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo2Shape:              VoicePar.LfoShape2           := TLfoShapeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo1FreqMode:           VoicePar.LfoFreqMode1        := TLfoFreqModeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo2FreqMode:           VoicePar.LfoFreqMode2        := TLfoFreqModeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo1Range:              VoicePar.LfoRange1           := TLfoRangeHelper.ToEnum(ParValue);
      TPluginParameter.Lfo2Range:              VoicePar.LfoRange2           := TLfoRangeHelper.ToEnum(ParValue);
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

    psFocusedKeyGroup:
    begin
      kg :=  Plugin.ActiveKeyGroup;
      if assigned(kg)
        then kg.SetModParModAmount(ModParIndex, ModSlot, ModAmount);
    end;

    psKeyGroup:
    begin
      assert(false, 'TODO');
      {
      kg := Plugin.KeyGroups.FindSampleGroup(KeyGroupName);
      if assigned(kg)
        then kg.SetModParModAmount(ModParIndex, ModSlot, ModAmount);
      }
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
begin
  result.Name := ParName;
  result.ShortName := ParName;
  result.Display := '';
  result.Units   := '';
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

  case Par of
    TPluginParameter.FilterRouting:
    begin
      Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.FilterChanged);
      Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateScope);
    end;
    TPluginParameter.SamplerLoopBounds:  Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateSampleInfo);
    TPluginParameter.SamplePlaybackType: Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleOscTypeChanged);
    TPluginParameter.Filter1Type:        Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.FilterChanged);
    TPluginParameter.Filter2Type:        Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.FilterChanged);
    TPluginParameter.Lfo1Shape:          Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged);
    TPluginParameter.Lfo2Shape:          Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged);
    TPluginParameter.Lfo1FreqMode:       Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged);
    TPluginParameter.Lfo2FreqMode:       Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged);
    TPluginParameter.Lfo1Range:          Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged);
    TPluginParameter.Lfo2Range:          Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.LfoChanged);
    TPluginParameter.SamplerLoopMode:    Plugin.Globals.MotherShip.MsgAudio(TLucidMsgID.AudioCommand_QuickReleaseAllNotes);
    TPluginParameter.VoiceMode:          Plugin.Globals.MotherShip.MsgAudio(TLucidMsgID.AudioCommand_QuickReleaseAllNotes);
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
      ApplyPluginParToKeyGroup(KeyGroup, Par, ParValue);
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
