unit GuiMeta.ScopeHandler;

interface

uses
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
    Globals : TGlobals;

    IsParFocusActive : boolean;
    CurrentParFocus : TPluginParameter;

    FocusedControl : TControl;
    ScopeFocus : TScopeFocus;
    ThrottleHandle : TUniqueID;



    procedure ParameterEnter(const ParName : string);
    procedure ParameterLeave(const ParName : string);
    procedure ParameterChanged(const ParName : string);

    procedure UpdateScope;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(aGlobals : TGlobals);
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


function FindScopeFocus_NEW(const Par : TPluginParameter):TScopeFocus;
begin
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
    TPluginParameter.AmpAttack:   result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpHold:     result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpDecay:    result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpSustain:  result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpRelease:  result := TScopeFocus.AmpEnv;
    TPluginParameter.AmpVelocity: result := TScopeFocus.AmpEnv;
    TPluginParameter.FilterAttack:    result := TScopeFocus.ModEnv;
    TPluginParameter.FilterHold:      result := TScopeFocus.ModEnv;
    TPluginParameter.FilterDecay:     result := TScopeFocus.ModEnv;
    TPluginParameter.FilterSustain:   result := TScopeFocus.ModEnv;
    TPluginParameter.FilterRelease:   result := TScopeFocus.ModEnv;
    TPluginParameter.FilterVelocity:  result := TScopeFocus.ModEnv;
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

constructor TScopeHandler.Create(aGlobals : TGlobals);
begin
  Globals := aGlobals;

  ThrottleHandle.Init;
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
    // NOTE:
    // The scope needs to be updated when the LFO selector is changed by the USER
    // on the GUI.
    if (ScopeFocus = TScopeFocus.Lfo1) or (ScopeFocus = TScopeFocus.Lfo2) then
    begin
      case Globals.SelectedLfo of
        0: ScopeFocus := TScopeFocus.Lfo1;
        1: ScopeFocus := TScopeFocus.Lfo2;
      else
        raise Exception.Create('Type not handled.');
      end;
    end;

    //TODO: update scope here.
  end;


  if MsgID = TLucidMsgID.Command_UpdateScope then
  begin
    //TODO: update scope here.
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
var
  Par : TPluginParameter;
begin
  Par := PluginParFromName(ParName);
  if (Par <> CurrentParFocus) or (IsParFocusActive = false) then
  begin
    IsParFocusActive := true;
    CurrentParFocus := Par;
    UpdateScope;
  end;
end;

procedure TScopeHandler.ParameterLeave(const ParName: string);
var
  Par : TPluginParameter;
begin
  Par := PluginParFromName(ParName);
  if Par = CurrentParFocus then
  begin
    IsParFocusActive := false;
    UpdateScope;
  end;
end;

procedure TScopeHandler.ParameterChanged(const ParName: string);
var
  Par : TPluginParameter;
begin
  Par := PluginParFromName(ParName);
  if Par = CurrentParFocus then
  begin
    UpdateScope;
  end;
end;

procedure TScopeHandler.UpdateScope;
var
  ScopeFocus : TScopeFocus;
begin
  if IsParFocusActive
    then ScopeFocus := FindScopeFocus_New(CurrentParFocus)
    else ScopeFocus := TScopeFocus.None;

  case ScopeFocus of
    TScopeFocus.None:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.DisplayOff;
    end;

    TScopeFocus.AmpEnv:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.ADSR;

      ScopeControl.AdsrValues.Attack  := Globals.VstParameters.FindParameter(TParName.AmpAttack).ValueVST;
      ScopeControl.AdsrValues.Hold    := Globals.VstParameters.FindParameter(TParName.AmpHold).ValueVST;
      ScopeControl.AdsrValues.Decay   := Globals.VstParameters.FindParameter(TParName.AmpDecay).ValueVST;
      ScopeControl.AdsrValues.Sustain := Globals.VstParameters.FindParameter(TParName.AmpSustain).ValueVST;
      ScopeControl.AdsrValues.Release := Globals.VstParameters.FindParameter(TParName.AmpRelease).ValueVST;
    end;

    TScopeFocus.ModEnv:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.ADSR;

      ScopeControl.AdsrValues.Attack  := Globals.VstParameters.FindParameter(TParName.FilterAttack).ValueVST;
      ScopeControl.AdsrValues.Hold    := Globals.VstParameters.FindParameter(TParName.FilterHold).ValueVST;
      ScopeControl.AdsrValues.Decay   := Globals.VstParameters.FindParameter(TParName.FilterDecay).ValueVST;
      ScopeControl.AdsrValues.Sustain := Globals.VstParameters.FindParameter(TParName.FilterSustain).ValueVST;
      ScopeControl.AdsrValues.Release := Globals.VstParameters.FindParameter(TParName.FilterRelease).ValueVST;
    end;

    TScopeFocus.Lfo1:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.LFO;

      ScopeControl.LfoValues.Shape := Globals.VstParameters.FindParameter(TParName.Lfo1Shape).ValueAsEnum<TLfoShape>;
      ScopeControl.LfoValues.Par1  := Globals.VstParameters.FindParameter(TParName.Lfo1Par1).ValueVST;
      ScopeControl.LfoValues.Par2  := Globals.VstParameters.FindParameter(TParName.Lfo1Par2).ValueVST;
      ScopeControl.LfoValues.Par3  := Globals.VstParameters.FindParameter(TParName.Lfo1Par3).ValueVST;
    end;

    TScopeFocus.Lfo2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.LFO;

      ScopeControl.LfoValues.Shape := Globals.VstParameters.FindParameter(TParName.Lfo2Shape).ValueAsEnum<TLfoShape>;
      ScopeControl.LfoValues.Par1  := Globals.VstParameters.FindParameter(TParName.Lfo2Par1).ValueVST;
      ScopeControl.LfoValues.Par2  := Globals.VstParameters.FindParameter(TParName.Lfo2Par2).ValueVST;
      ScopeControl.LfoValues.Par3  := Globals.VstParameters.FindParameter(TParName.Lfo2Par3).ValueVST;
    end;

    TScopeFocus.Filter1:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ScopeControl.FilterValues.FilterType := Globals.VstParameters.FindParameter(TParName.Filter1Type).ValueAsEnum<TFilterType>;
      ScopeControl.FilterValues.Par1 := Globals.VstParameters.FindParameter(TParName.Filter1Par1).ValueVST;
      ScopeControl.FilterValues.Par2 := Globals.VstParameters.FindParameter(TParName.Filter1Par2).ValueVST;
      ScopeControl.FilterValues.Par3 := Globals.VstParameters.FindParameter(TParName.Filter1Par3).ValueVST;
      ScopeControl.FilterValues.Par4 := Globals.VstParameters.FindParameter(TParName.Filter1Par4).ValueVST;
    end;

    TScopeFocus.Filter2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ScopeControl.FilterValues.FilterType := Globals.VstParameters.FindParameter(TParName.Filter2Type).ValueAsEnum<TFilterType>;
      ScopeControl.FilterValues.Par1 := Globals.VstParameters.FindParameter(TParName.Filter2Par1).ValueVST;
      ScopeControl.FilterValues.Par2 := Globals.VstParameters.FindParameter(TParName.Filter2Par2).ValueVST;
      ScopeControl.FilterValues.Par3 := Globals.VstParameters.FindParameter(TParName.Filter2Par3).ValueVST;
      ScopeControl.FilterValues.Par4 := Globals.VstParameters.FindParameter(TParName.Filter2Par4).ValueVST;
    end;

    TScopeFocus.FilterBlend:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.FilterBlend;

      ScopeControl.FilterBlendValues.FilterRouting := Globals.VstParameters.FindParameter(TParName.FilterRouting).ValueAsEnum<TFilterRouting>;
      ScopeControl.FilterBlendValues.BlendAmt      := Globals.VstParameters.FindParameter(TParName.FilterOutputBlend).ValueVST;
    end;
  end;

  ScopeControl.Invalidate;
end;










end.
