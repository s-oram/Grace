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
    class procedure ApplyPluginParToKeyGroup(const aPlugin : TObject; const KeyGroup : IKeyGroup; const Par : TPluginParameter; const ParValue : single); static; inline;

  public
    class procedure SetPluginParameter(const aPlugin : TObject; const Scope : TParChangeScope; const KeyGroupName : string; const ParName : string; const ParValue : single); static; inline;
    class function GetPluginParameter(const aPlugin : TObject; const ParName : string):single; static; inline;

    class procedure SetParameterModAmount(const aPlugin : TObject; const Scope : TParChangeScope; const ParName : string; const ModSlot : integer; const ModAmount : single); static; inline;
    class function GetParameterModAmount(const aPlugin : TObject; const ParName : string; const ModSlot : integer):single; static; inline;

    class procedure GetModParModMinMax(const aPlugin : TObject; const ParName : string; out ModMin, ModMax:single); static; inline;

    class function GetParameterInfo(const aPlugin : TObject; const ParName : string) : TPluginParameterInfo; static; inline;

    class function GetPluginParameterVstInfo(const aPlugin : TObject; const ParName : string):TVstParameterInfo; static; inline;
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
      TPluginParameter.PitchTracking:
      begin
        result := TPitchTrackingHelper.ToSingle(VoicePar.PitchTracking);
      end;

      TPluginParameter.SamplePlaybackType:
      begin
        result := TSamplePlaybackTypeHelper.ToSingle(VoicePar.SamplePlaybackType);
      end;

      TPluginParameter.SampleResetClockSource:
      begin
        result := TClockSourceHelper.ToSingle(VoicePar.SampleReset);
      end;

      TPluginParameter.SamplerLoopBounds:
      begin
        result := TSamplerLoopBoundsHelper.ToSingle(VoicePar.SamplerLoopBounds);
      end;

      TPluginParameter.SamplerLoopMode:
      begin
        result := TSamplerLoopModeHelper.ToSingle(VoicePar.SamplerLoopMode);
      end;

      TPluginParameter.AmpVelocity:
      begin
        result := TEnvVelocityDepthHelper.ToSingle(VoicePar.AmpVelocityDepth);
      end;

      TPluginParameter.FilterVelocity:
      begin
        result := TEnvVelocityDepthHelper.ToSingle(VoicePar.FilterVelocityDepth);
      end;

      TPluginParameter.FilterRouting:
      begin
        result := TFilterRoutingHelper.ToSingle(VoicePar.FilterRouting);
      end;

      TPluginParameter.Filter1Type:
      begin
        result := TFilterTypeHelper.ToSingle(VoicePar.Filter1Type);
      end;

      TPluginParameter.Filter2Type:
      begin
        result := TFilterTypeHelper.ToSingle(VoicePar.Filter2Type);
      end;

      TPluginParameter.Filter1KeyFollow:
      begin
        result := VoicePar.Filter1KeyFollow;
      end;

      TPluginParameter.Filter2KeyFollow:
      begin
        result := VoicePar.Filter2KeyFollow;
      end;

      TPluginParameter.Lfo1Shape:
      begin
        result := TLfoShapeHelper.ToSingle(VoicePar.LfoShape1);
      end;

      TPluginParameter.Lfo2Shape:
      begin
        result := TLfoShapeHelper.ToSingle(VoicePar.LfoShape2);
      end;

      TPluginParameter.Lfo1FreqMode:
      begin
        result := TLfoFreqModeHelper.ToSingle(VoicePar.LfoFreqMode1);
      end;

      TPluginParameter.Lfo2FreqMode:
      begin
        result := TLfoFreqModeHelper.ToSingle(VoicePar.LfoFreqMode2);
      end;

      TPluginParameter.Lfo1Range:
      begin
        result := TLfoRangeHelper.ToSingle(VoicePar.LfoRange1);
      end;

      TPluginParameter.Lfo2Range:
      begin
        result := TLfoRangeHelper.ToSingle(VoicePar.LfoRange2);
      end;

      TPluginParameter.Seq1Clock:
      begin
        result := TSequencerClockHelper.ToSingle(VoicePar.Seq1Clock);
      end;

      TPluginParameter.Seq1Direction:
      begin
        result := TStepSequencerDirectionHelper.ToSingle(VoicePar.Seq1Direction);
      end;

      TPluginParameter.Seq1Length:
      begin
        result := TStepSequencerLengthHelper.ToSingle(VoicePar.StepSeq1Length);
      end;

      TPluginParameter.Seq2Clock:
      begin
        result := TSequencerClockHelper.ToSingle(VoicePar.Seq2Clock);
      end;

      TPluginParameter.Seq2Direction:
      begin
        result := TStepSequencerDirectionHelper.ToSingle(VoicePar.Seq2Direction);
      end;

      TPluginParameter.Seq2Length:
      begin
        result := TStepSequencerLengthHelper.ToSingle(VoicePar.StepSeq2Length);
      end;


      //TPluginParameter.PreviewVolume: ;
      //TPluginParameter.Preview: ;
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
        ApplyPluginParToKeyGroup(Plugin, kg, Par, ParValue);
      end;
    end;

    psFocusedKeyGroup:
    begin
      kg :=  Plugin.ActiveKeyGroup;
      ApplyPluginParToKeyGroup(Plugin, kg, Par, ParValue);
    end;

    psKeyGroup:
    begin
      kg := Plugin.KeyGroups.FindSampleGroup(KeyGroupName);
      if assigned(kg)
        then ApplyPluginParToKeyGroup(Plugin, kg, Par, ParValue);
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

end;

class procedure TPluginParameterController.ApplyPluginParToKeyGroup(
  const aPlugin : TObject;
  const KeyGroup: IKeyGroup;
  const Par: TPluginParameter;
  const ParValue: single);
var
  Plugin : TeePlugin;
  VoicePar : TLucidityVoiceParameterWrapper;
  ModParIndex : integer;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  if IsModPar(Par) then
  begin
    ModParIndex := GetModParIndex(Par);
    KeyGroup.SetModParValue(ModParIndex, ParValue);
  end else
  begin
    VoicePar := (KeyGroup.GetObject as TKeyGroup).VoiceParameters;


    case Par of
      TPluginParameter.PitchTracking:
      begin
        VoicePar.PitchTracking := TPitchTrackingHelper.ToEnum(ParValue);
      end;

      TPluginParameter.SamplePlaybackType:
      begin
        VoicePar.SamplePlaybackType   := TSamplePlaybackTypeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleOscTypeChanged);
      end;

      TPluginParameter.SampleResetClockSource:
      begin
        VoicePar.SampleReset := TClockSourceHelper.ToEnum(ParValue);
      end;

      TPluginParameter.SamplerLoopBounds:
      begin
        VoicePar.SamplerLoopBounds := TSamplerLoopBoundsHelper.ToEnum(ParValue);

      end;

      TPluginParameter.SamplerLoopMode:
      begin
        VoicePar.SamplerLoopMode := TSamplerLoopModeHelper.ToEnum(ParValue);
      end;

      TPluginParameter.AmpVelocity:
      begin
        VoicePar.AmpVelocityDepth := TEnvVelocityDepthHelper.ToEnum(ParValue);
      end;

      TPluginParameter.FilterVelocity:
      begin
        VoicePar.FilterVelocityDepth := TEnvVelocityDepthHelper.ToEnum(ParValue);
      end;

      TPluginParameter.FilterRouting:
      begin
        VoicePar.FilterRouting := TFilterRoutingHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.FilterChanged);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_UpdateScope);
      end;

      TPluginParameter.Filter1Type:
      begin
        VoicePar.Filter1Type := TFilterTypeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.FilterChanged);
      end;

      TPluginParameter.Filter2Type:
      begin
        VoicePar.Filter2Type := TFilterTypeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.FilterChanged);
      end;

      TPluginParameter.Filter1KeyFollow:
      begin
        VoicePar.Filter1KeyFollow := ParValue;
      end;

      TPluginParameter.Filter2KeyFollow:
      begin
        VoicePar.Filter2KeyFollow := ParValue;
      end;

      TPluginParameter.Lfo1Shape:
      begin
        VoicePar.LfoShape1 := TLfoShapeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
      end;

      TPluginParameter.Lfo2Shape:
      begin
        VoicePar.LfoShape2 := TLfoShapeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
      end;

      TPluginParameter.Lfo1FreqMode:
      begin
        VoicePar.LfoFreqMode1 := TLfoFreqModeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
      end;

      TPluginParameter.Lfo2FreqMode:
      begin
        VoicePar.LfoFreqMode2 := TLfoFreqModeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
      end;

      TPluginParameter.Lfo1Range:
      begin
        VoicePar.LfoRange1 := TLfoRangeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
      end;

      TPluginParameter.Lfo2Range:
      begin
        VoicePar.LfoRange2 := TLfoRangeHelper.ToEnum(ParValue);
        Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
      end;

      TPluginParameter.Seq1Clock:
      begin
        VoicePar.Seq1Clock := TSequencerClockHelper.ToEnum(ParValue);
      end;

      TPluginParameter.Seq1Direction:
      begin
        VoicePar.Seq1Direction := TStepSequencerDirectionHelper.ToEnum(ParValue);
      end;

      TPluginParameter.Seq1Length:
      begin
        VoicePar.StepSeq1Length := TStepSequencerLengthHelper.ToEnum(ParValue);
      end;

      TPluginParameter.Seq2Clock:
      begin
        VoicePar.Seq2Clock := TSequencerClockHelper.ToEnum(ParValue);
      end;

      TPluginParameter.Seq2Direction:
      begin
        VoicePar.Seq2Direction := TStepSequencerDirectionHelper.ToEnum(ParValue);
      end;

      TPluginParameter.Seq2Length:
      begin
        VoicePar.StepSeq2Length := TStepSequencerLengthHelper.ToEnum(ParValue);
      end;

      //TPluginParameter.PreviewVolume: ;
      //TPluginParameter.Preview: ;
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

  kg.GetModParModMinMax(ModParIndex, ModMin, ModMax);
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
  result := kg.GetModParModAmount(ModParIndex, ModSlot);
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
      kg.SetModParModAmount(ModParIndex, ModSlot, ModAmount);
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





end.
