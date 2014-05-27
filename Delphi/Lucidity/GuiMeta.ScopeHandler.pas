unit GuiMeta.ScopeHandler;

interface

uses
  eePlugin,
  Lucidity.PluginParameters,
  VamLib.ZeroObject,
  eeGlobals,
  Classes,
  Controls,
  VamLib.UniqueID,
  LucidityGUI.Scope;

type
  {$SCOPEDENUMS ON}
  TScopeFocus = (
    None,
    AmpEnv,
    ModEnv,
    Lfo1,
    Lfo2,
    Filter1,
    Filter2,
    FilterBlend
  );

  TScopeHandler = class(TZeroObject)
  private
    fScopeControl: TLucidityScope;
  protected
    Plugin  : TeePlugin;
    Globals : TGlobals;

    IsParFocusActive : boolean;

    //CurrentParFocus : TPluginParameter;
    CurrentParFocus : string; // this is a parameter name.

    FocusedControl : TControl;
    ScopeFocus : TScopeFocus;

    procedure ParameterEnter(const ParName : string);
    procedure ParameterLeave(const ParName : string);
    procedure ParameterChanged(const ParName : string);

    procedure UpdateScope;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(aPlugin : TeePlugin);
    destructor Destroy; override;

    property ScopeControl : TLucidityScope read fScopeControl write fScopeControl;
  end;

implementation

uses
  SysUtils,
  VamLib.Throttler,
  uLucidityEnums,
  VamQuery,
  VamWinControl,
  uConstants,
  VamKnob,
  VamTextBox,
  VamButton,
  VamSliderSwitch;


function FindScopeFocus_NEW(const ParName : string):TScopeFocus;
var
  Par : TPluginParameter;
begin
  if ParName = ''
    then exit(TScopeFocus.None);

  Par := PluginParFromName(ParName);

  case Par of
    TPluginParameter.VoiceMode:                 result := TScopeFocus.None;
    TPluginParameter.VoiceGlide:                result := TScopeFocus.None;
    TPluginParameter.PitchTracking:             result := TScopeFocus.None;
    TPluginParameter.SamplePlaybackType:        result := TScopeFocus.None;
    TPluginParameter.SampleResetClockSource:    result := TScopeFocus.None;
    TPluginParameter.SamplerLoopBounds:         result := TScopeFocus.None;
    TPluginParameter.SamplerLoopMode:           result := TScopeFocus.None;
    TPluginParameter.OutputGain:                result := TScopeFocus.None;
    TPluginParameter.OutputPan:                 result := TScopeFocus.None;
    TPluginParameter.VoicePitchOne:             result := TScopeFocus.None;
    TPluginParameter.VoicePitchTwo:             result := TScopeFocus.None;
    TPluginParameter.SampleStart:               result := TScopeFocus.None;
    TPluginParameter.SampleEnd:                 result := TScopeFocus.None;
    TPluginParameter.LoopStart:                 result := TScopeFocus.None;
    TPluginParameter.LoopEnd:                   result := TScopeFocus.None;
    TPluginParameter.AmpAttack:         result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpHold:           result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpDecay:          result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpSustain:        result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpRelease:        result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpVelocity:       result := TScopeFocus.AmpEnv;
    TPluginParameter.ModAttack:         result := TScopeFocus.ModEnv;
    TPluginParameter.ModHold:           result := TScopeFocus.ModEnv;
    TPluginParameter.ModDecay:          result := TScopeFocus.ModEnv;
    TPluginParameter.ModSustain:        result := TScopeFocus.ModEnv;
    TPluginParameter.ModRelease:        result := TScopeFocus.ModEnv;
    TPluginParameter.ModVelocity:       result := TScopeFocus.ModEnv;
    TPluginParameter.FilterRouting:     result := TScopeFocus.FilterBlend;
    TPluginParameter.FilterOutputBlend: result := TScopeFocus.FilterBlend;
    TPluginParameter.Filter1Type:       result := TScopeFocus.Filter1;
    TPluginParameter.Filter1KeyFollow:  result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par1:       result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par2:       result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par3:       result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par4:       result := TScopeFocus.Filter1;
    TPluginParameter.Filter2Type:       result := TScopeFocus.Filter2;
    TPluginParameter.Filter2KeyFollow:  result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par1:       result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par2:       result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par3:       result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par4:       result := TScopeFocus.Filter2;
    TPluginParameter.Lfo1Shape:         result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1FreqMode:      result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1Range:         result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1Par1:          result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1Par2:          result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1Par3:          result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo2Shape:         result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2FreqMode:      result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2Range:         result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2Par1:          result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2Par2:          result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2Par3:          result := TScopeFocus.Lfo2;
    TPluginParameter.Seq1Clock:         result := TScopeFocus.None;
    TPluginParameter.Seq1Direction:     result := TScopeFocus.None;
    TPluginParameter.Seq1Length:        result := TScopeFocus.None;
    TPluginParameter.Seq2Clock:         result := TScopeFocus.None;
    TPluginParameter.Seq2Direction:     result := TScopeFocus.None;
    TPluginParameter.Seq2Length:        result := TScopeFocus.None;
    TPluginParameter.PreviewVolume:     result := TScopeFocus.None;
    TPluginParameter.Preview:           result := TScopeFocus.None;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

{ TScopeHandler }

constructor TScopeHandler.Create(aPlugin : TeePlugin);
begin
  Plugin  := aPlugin;
  Globals := Plugin.Globals;
  IsParFocusActive := false;
end;

destructor TScopeHandler.Destroy;
begin

  inherited;
end;

procedure TScopeHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  s : string;
begin
  inherited;

  if MsgID = TLucidMsgID.LfoChanged then
  begin
    UpdateScope;
  end;


  if MsgID = TLucidMsgID.Command_UpdateScope then
  begin
    UpdateScope;
  end;


  if MsgID = TLucidMsgID.OnParControlEnter then
  begin
    s := string(Data^);
    ParameterEnter(s);
  end;

  if MsgID = TLucidMsgID.OnParControlLeave then
  begin
    s := string(Data^);
    ParameterLeave(s);
  end;

  if MsgID = TLucidMsgID.OnParControlChanged then
  begin
    s := string(Data^);
    ParameterChanged(s);
  end;

end;


procedure TScopeHandler.ParameterEnter(const ParName: string);
begin
  if (ParName <> CurrentParFocus) or (IsParFocusActive = false) then
  begin
    IsParFocusActive := true;
    CurrentParFocus := ParName;
    UpdateScope;
  end;
end;

procedure TScopeHandler.ParameterLeave(const ParName: string);
begin
  if ParName = CurrentParFocus then
  begin
    CurrentParFocus := '';
    IsParFocusActive := false;
    UpdateScope;
  end;
end;

procedure TScopeHandler.ParameterChanged(const ParName: string);
begin
  if ParName = CurrentParFocus then
  begin
    UpdateScope;
  end;
end;

procedure TScopeHandler.UpdateScope;
var
  ScopeFocus : TScopeFocus;
  ParValue : single;
begin
  if IsParFocusActive then
  begin
    ScopeFocus := FindScopeFocus_New(CurrentParFocus);
  end else
  begin
    ScopeFocus := TScopeFocus.None;
  end;


  case ScopeFocus of
    TScopeFocus.None:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.DisplayOff;
    end;

    TScopeFocus.AmpEnv:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.ADSR;

      ScopeControl.AdsrValues.Attack  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.AmpAttack));
      ScopeControl.AdsrValues.Hold    := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.AmpHold));
      ScopeControl.AdsrValues.Decay   := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.AmpDecay));
      ScopeControl.AdsrValues.Sustain := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.AmpSustain));
      ScopeControl.AdsrValues.Release := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.AmpRelease));
    end;

    TScopeFocus.ModEnv:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.ADSR;

      ScopeControl.AdsrValues.Attack  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.ModAttack));
      ScopeControl.AdsrValues.Hold    := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.ModHold));
      ScopeControl.AdsrValues.Decay   := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.ModDecay));
      ScopeControl.AdsrValues.Sustain := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.ModSustain));
      ScopeControl.AdsrValues.Release := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.ModRelease));
    end;

    TScopeFocus.Lfo1:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.LFO;

      ParValue := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo1Shape));
      ScopeControl.LfoValues.Shape := TLfoShapeHelper.ToEnum(ParValue);

      ScopeControl.LfoValues.Par1  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo1Par1));
      ScopeControl.LfoValues.Par2  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo1Par2));
      ScopeControl.LfoValues.Par3  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo1Par3));
    end;

    TScopeFocus.Lfo2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.LFO;

      ParValue := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo2Shape));
      ScopeControl.LfoValues.Shape := TLfoShapeHelper.ToEnum(ParValue);
      ScopeControl.LfoValues.Par1  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo2Par1));
      ScopeControl.LfoValues.Par2  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo2Par2));
      ScopeControl.LfoValues.Par3  := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Lfo2Par3));
    end;

    TScopeFocus.Filter1:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ParValue := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter1Type));
      ScopeControl.FilterValues.FilterType := TFilterTypeHelper.ToEnum(ParValue);

      ScopeControl.FilterValues.Par1 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter1Par1));
      ScopeControl.FilterValues.Par2 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter1Par2));
      ScopeControl.FilterValues.Par3 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter1Par3));
      ScopeControl.FilterValues.Par4 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter1Par4));
    end;

    TScopeFocus.Filter2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ParValue := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter2Type));
      ScopeControl.FilterValues.FilterType := TFilterTypeHelper.ToEnum(ParValue);

      ScopeControl.FilterValues.Par1 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter2Par1));
      ScopeControl.FilterValues.Par2 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter2Par2));
      ScopeControl.FilterValues.Par3 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter2Par3));
      ScopeControl.FilterValues.Par4 := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.Filter2Par4));
    end;

    TScopeFocus.FilterBlend:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.FilterBlend;

      ParValue := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.FilterRouting));
      ScopeControl.FilterBlendValues.FilterRouting := TFilterRoutingHelper.ToEnum(ParValue);
      ScopeControl.FilterBlendValues.BlendAmt      := Plugin.GetPluginParameter(PluginParToName(TPluginparameter.FilterOutputBlend));
    end;
  end;

  ScopeControl.Invalidate;
end;










end.
