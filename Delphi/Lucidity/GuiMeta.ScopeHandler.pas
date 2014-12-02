unit GuiMeta.ScopeHandler;

interface

{$INCLUDE Defines.inc}

uses
  ExtCtrls,
  VamLib.Debouncer,
  VamLib.ZeroObject,
  eePlugin,
  Lucidity.PluginParameters,
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

    UpdateTimer : TTimer;

    ScopeState : record
      IsParFocusActive : boolean;
      CurrentParFocus : string; // this is a parameter name.
    end;

    FocusedControl : TControl;
    LastScopeFocus : TScopeFocus;

    ScopeFocusDebounceID : TUniqueID;

    procedure UpdateScope; overload;
    procedure UpdateScope(const ScopeFocus : TScopeFocus); overload;
    procedure UpdateScope(const FocusParName : string); overload;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface);  override;

    procedure HandleUpdateTimer(Sender : TObject);
  public
    constructor Create(aPlugin : TeePlugin);
    destructor Destroy; override;

    property ScopeControl : TLucidityScope read fScopeControl write fScopeControl;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils,
  Lucidity.Enums,
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
  if (ParName = '') then exit(TScopeFocus.None);

  Par := PluginParFromName(ParName);

  case Par of
    TPluginParameter.VoiceMode:              result := TScopeFocus.None;
    TPluginParameter.VoiceGlide:             result := TScopeFocus.None;
    TPluginParameter.PitchTracking:          result := TScopeFocus.None;
    TPluginParameter.SamplePlaybackType:     result := TScopeFocus.None;
    TPluginParameter.SampleResetClockSource: result := TScopeFocus.None;
    TPluginParameter.SamplerLoopBounds:      result := TScopeFocus.None;
    TPluginParameter.SamplerTriggerMode:     result := TScopeFocus.None;
    TPluginParameter.OutputGain:             result := TScopeFocus.None;
    TPluginParameter.OutputPan:              result := TScopeFocus.None;
    TPluginParameter.VoicePitchOne:          result := TScopeFocus.None;
    TPluginParameter.VoicePitchTwo:          result := TScopeFocus.None;
    TPluginParameter.SampleStart:            result := TScopeFocus.None;
    TPluginParameter.SampleEnd:              result := TScopeFocus.None;
    TPluginParameter.LoopStart:              result := TScopeFocus.None;
    TPluginParameter.LoopEnd:                result := TScopeFocus.None;
    TPluginParameter.AmpAttack:              result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpHold:                result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpDecay:               result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpSustain:             result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpRelease:             result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpVelocity:            result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpEnvSnap:             result := TScopeFocus.AmpEnv;
    TPluginParameter.ModAttack:              result := TScopeFocus.ModEnv;
    TPluginParameter.ModHold:                result := TScopeFocus.ModEnv;
    TPluginParameter.ModDecay:               result := TScopeFocus.ModEnv;
    TPluginParameter.ModSustain:             result := TScopeFocus.ModEnv;
    TPluginParameter.ModRelease:             result := TScopeFocus.ModEnv;
    TPluginParameter.ModVelocity:            result := TScopeFocus.ModEnv;
    TPluginParameter.ModEnvSnap:             result := TScopeFocus.ModEnv;
    TPluginParameter.FilterRouting:          result := TScopeFocus.FilterBlend;
    TPluginParameter.FilterOutputBlend:      result := TScopeFocus.FilterBlend;
    TPluginParameter.Filter1Type:            result := TScopeFocus.Filter1;
    TPluginParameter.Filter1KeyFollow:       result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par1:            result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par2:            result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par3:            result := TScopeFocus.Filter1;
    TPluginParameter.Filter1Par4:            result := TScopeFocus.Filter1;
    TPluginParameter.Filter2Type:            result := TScopeFocus.Filter2;
    TPluginParameter.Filter2KeyFollow:       result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par1:            result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par2:            result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par3:            result := TScopeFocus.Filter2;
    TPluginParameter.Filter2Par4:            result := TScopeFocus.Filter2;
    TPluginParameter.Lfo1Shape:              result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1FreqMode:           result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1Par1:               result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1Par2:               result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo1Par3:               result := TScopeFocus.Lfo1;
    TPluginParameter.Lfo2Shape:              result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2FreqMode:           result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2Par1:               result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2Par2:               result := TScopeFocus.Lfo2;
    TPluginParameter.Lfo2Par3:               result := TScopeFocus.Lfo2;
    TPluginParameter.Seq1Clock:              result := TScopeFocus.None;
    TPluginParameter.Seq1Direction:          result := TScopeFocus.None;
    TPluginParameter.Seq1Length:             result := TScopeFocus.None;
    TPluginParameter.Seq2Clock:              result := TScopeFocus.None;
    TPluginParameter.Seq2Direction:          result := TScopeFocus.None;
    TPluginParameter.Seq2Length:             result := TScopeFocus.None;
    TPluginParameter.PreviewVolume:          result := TScopeFocus.None;
    TPluginParameter.Preview:                result := TScopeFocus.None;
    TPluginParameter.PadX1:                  result := TScopeFocus.None;
    TPluginParameter.PadY1:                  result := TScopeFocus.None;
    TPluginParameter.PadX2:                  result := TScopeFocus.None;
    TPluginParameter.PadY2:                  result := TScopeFocus.None;
    TPluginParameter.PadX3:                  result := TScopeFocus.None;
    TPluginParameter.PadY3:                  result := TScopeFocus.None;
    TPluginParameter.PadX4:                  result := TScopeFocus.None;
    TPluginParameter.PadY4:                  result := TScopeFocus.None;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

{ TScopeHandler }

constructor TScopeHandler.Create(aPlugin : TeePlugin);
begin
  Plugin  := aPlugin;
  Globals := Plugin.Globals;
  ScopeState.IsParFocusActive := false;

  LastScopeFocus := TScopeFocus.None;

  ScopeFocusDebounceID.Init;

  UpdateTimer := TTimer.Create(nil);
  UpdateTimer.Interval := 50;
  UpdateTimer.OnTimer := HandleUpdateTimer;
  UpdateTimer.Enabled := true;
end;

destructor TScopeHandler.Destroy;
begin
  UpdateTimer.Free;
  DebounceCancel(ScopeFocusDebounceID);
  inherited;
end;

procedure TScopeHandler.HandleUpdateTimer(Sender: TObject);
begin
  if LastScopeFocus <> TScopeFocus.None
    then UpdateScope(LastScopeFocus);
end;

procedure TScopeHandler.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB:IInterface);
const
  DebounceTime = 200; //milliseconds,
var
  ParName : string;
  s : string;
  dbf : TProc;
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

  if MsgID = TLucidMsgID.VstParameterChanged then
  begin
    UpdateScope;
    //LogMain.LogMessage('VstParameter Changed Called.');
  end;


  if MsgID = TLucidMsgID.OnParControlEnter then
  begin
    s := string(DataA^);
    ParName := string(DataA^);

    if (ParName <> ScopeState.CurrentParFocus) or (ScopeState.IsParFocusActive = false) then
    begin
      ScopeState.IsParFocusActive := true;
      ScopeState.CurrentParFocus := ParName;
    end;

    dbf := procedure
    begin
      UpdateScope(ScopeState.CurrentParFocus);
    end;

    Debounce(ScopeFocusDebounceID, DebounceTime, TDebounceEdge.deTrailing, dbf);
  end;

  if MsgID = TLucidMsgID.OnParControlLeave then
  begin
    ParName := string(DataA^);

    if (ParName = ScopeState.CurrentParFocus) then
    begin
      ScopeState.CurrentParFocus := '';
      ScopeState.IsParFocusActive := false;
    end;

    dbf := procedure
    begin
      UpdateScope(ScopeState.CurrentParFocus);
    end;

    Debounce(ScopeFocusDebounceID, DebounceTime, TDebounceEdge.deTrailing, dbf);
  end;

  if MsgID = TLucidMsgID.OnParControlChanged then
  begin
    ParName := string(DataA^);

    if ParName = ScopeState.CurrentParFocus then
    begin
      UpdateScope(ScopeState.CurrentParFocus);
    end;
  end;
end;

procedure TScopeHandler.UpdateScope;
begin
  UpdateScope(LastScopeFocus);
end;

procedure TScopeHandler.UpdateScope(const FocusParName : string);
var
  ScopeFocus : TScopeFocus;
begin
  if FocusParName <> '' then
  begin
    ScopeFocus := FindScopeFocus_New(FocusParName);
  end else
  begin
    ScopeFocus := TScopeFocus.None;
  end;

  UpdateScope(ScopeFocus);
end;

procedure TScopeHandler.UpdateScope(const ScopeFocus: TScopeFocus);
var
  ParValue : single;
begin
  LastScopeFocus := ScopeFocus;

  case ScopeFocus of
    TScopeFocus.None:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.DisplayOff;
    end;

    TScopeFocus.AmpEnv:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.ADSR;

      ScopeControl.AdsrValues.Attack  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.AmpAttack));
      ScopeControl.AdsrValues.Hold    := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.AmpHold));
      ScopeControl.AdsrValues.Decay   := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.AmpDecay));
      ScopeControl.AdsrValues.Sustain := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.AmpSustain));
      ScopeControl.AdsrValues.Release := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.AmpRelease));
    end;

    TScopeFocus.ModEnv:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.ADSR;

      ScopeControl.AdsrValues.Attack  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.ModAttack));
      ScopeControl.AdsrValues.Hold    := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.ModHold));
      ScopeControl.AdsrValues.Decay   := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.ModDecay));
      ScopeControl.AdsrValues.Sustain := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.ModSustain));
      ScopeControl.AdsrValues.Release := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.ModRelease));
    end;

    TScopeFocus.Lfo1:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.LFO;

      ParValue := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo1Shape));
      ScopeControl.LfoValues.Shape := TLfoShapeHelper.ToEnum(ParValue);

      ScopeControl.LfoValues.Par1  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo1Par1));
      ScopeControl.LfoValues.Par2  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo1Par2));
      ScopeControl.LfoValues.Par3  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo1Par3));
    end;

    TScopeFocus.Lfo2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.LFO;

      ParValue := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo2Shape));
      ScopeControl.LfoValues.Shape := TLfoShapeHelper.ToEnum(ParValue);
      ScopeControl.LfoValues.Par1  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo2Par1));
      ScopeControl.LfoValues.Par2  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo2Par2));
      ScopeControl.LfoValues.Par3  := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Lfo2Par3));
    end;

    TScopeFocus.Filter1:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ParValue := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter1Type));
      ScopeControl.FilterValues.FilterType := TFilterTypeHelper.ToEnum(ParValue);

      ScopeControl.FilterValues.Par1 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter1Par1));
      ScopeControl.FilterValues.Par2 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter1Par2));
      ScopeControl.FilterValues.Par3 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter1Par3));
      ScopeControl.FilterValues.Par4 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter1Par4));
    end;

    TScopeFocus.Filter2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ParValue := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter2Type));
      ScopeControl.FilterValues.FilterType := TFilterTypeHelper.ToEnum(ParValue);

      ScopeControl.FilterValues.Par1 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter2Par1));
      ScopeControl.FilterValues.Par2 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter2Par2));
      ScopeControl.FilterValues.Par3 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter2Par3));
      ScopeControl.FilterValues.Par4 := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.Filter2Par4));
    end;

    TScopeFocus.FilterBlend:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.FilterBlend;

      ParValue := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.FilterRouting));
      ScopeControl.FilterBlendValues.FilterRouting := TFilterRoutingHelper.ToEnum(ParValue);
      ScopeControl.FilterBlendValues.BlendAmt      := Plugin.GetPluginParameter(PluginParToID(TPluginparameter.FilterOutputBlend));
    end;
  end;

  ScopeControl.Invalidate;
end;









end.
