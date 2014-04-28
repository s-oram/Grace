unit Lucidity.PluginParameterController;

interface

uses
  Lucidity.Interfaces,
  Lucidity.PluginParameters,
  Lucidity.Types;


type
  TPluginParameterController = class
  private
    class procedure ApplyPluginParToKeyGroup(const aPlugin : TObject; const KeyGroup : IKeyGroup; const Par : TPluginParameter; const ParValue : single);

  public
    class procedure SetPluginParameter(const aPlugin : TObject; const Scope : TParChangeScope; const KeyGroupName : string; const ParName : string; const ParValue : single); static; inline;
    class function GetPluginParameter(const aPlugin : TObject; const ParName : string):single; static; inline;
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
begin
  result := 0;
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

  case Scope of
    psGlobal:
    begin
      Par := PluginParFromName(ParName);
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
      Par := PluginParFromName(ParName);
      ApplyPluginParToKeyGroup(Plugin, kg, Par, ParValue);
    end;

    psKeyGroup:
    begin
      kg := Plugin.KeyGroups.FindSampleGroup(KeyGroupName);
      if assigned(kg) then
      begin
        Par := PluginParFromName(ParName);
        ApplyPluginParToKeyGroup(Plugin, kg, Par, ParValue);
      end;
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
        Plugin.ActiveVoicePar.FilterRouting := TFilterRoutingHelper.ToEnum(ParValue);
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



end.
