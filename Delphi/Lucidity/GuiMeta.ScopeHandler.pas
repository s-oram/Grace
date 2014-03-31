unit GuiMeta.ScopeHandler;

interface

uses
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
    FocusedControl : TControl;
    ScopeFocus : TScopeFocus;

    ThrottleHandle : TUniqueID;
    procedure KnobMouseEnter(Sender : TObject);
    procedure KnobMouseLeave(Sender : TObject);
    procedure KnobChanged(Sender : TObject);

    procedure ControlChanged(c : TControl);

    function FindScopeFocus(c : TControl):TScopeFocus;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(aGlobals : TGlobals);
    destructor Destroy; override;

    procedure RegisterControl(c : TControl);

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

procedure TScopeHandler.RegisterControl(c: TControl);
begin
  if (c is TVamKnob) then
  begin
    (c as TVamKnob).MouseEnterMultiEvent.Add(KnobMouseEnter);
    (c as TVamKnob).MouseLeaveMultiEvent.Add(KnobMouseLeave);
    (c as TVamKnob).ChangedMultiEvent.Add(KnobChanged);
  end;

  if (c is TVamTextBox) then
  begin
    (c as TVamTextBox).MouseEnterMultiEvent.Add(KnobMouseEnter);
    (c as TVamTextBox).MouseLeaveMultiEvent.Add(KnobMouseLeave);
    (c as TVamTextBox).ChangedMultiEvent.Add(KnobChanged);
  end;

  if (c is TVamButton) then
  begin
    (c as TVamButton).MouseEnterMultiEvent.Add(KnobMouseEnter);
    (c as TVamButton).MouseLeaveMultiEvent.Add(KnobMouseLeave);
    (c as TVamButton).ChangedMultiEvent.Add(KnobChanged);
  end;

  if (c is TVamSliderSwitch) then
  begin
    (c as TVamSliderSwitch).MouseEnterMultiEvent.Add(KnobMouseEnter);
    (c as TVamSliderSwitch).MouseLeaveMultiEvent.Add(KnobMouseLeave);
    (c as TVamSliderSwitch).ChangedMultiEvent.Add(KnobChanged);
  end;



end;

procedure TScopeHandler.KnobMouseEnter(Sender: TObject);
begin
  assert(Sender is TControl);
  FocusedControl := Sender as TControl;

  ScopeFocus := FindScopeFocus(FocusedControl);

  ControlChanged(Sender as TControl);
end;

procedure TScopeHandler.KnobChanged(Sender: TObject);
begin
  //TODO: this event here needs to be throttled to slow GUI updates.

  Throttle(ThrottleHandle, 25,
  procedure
  begin
    ControlChanged(Sender as TControl);
  end);


end;

procedure TScopeHandler.KnobMouseLeave(Sender: TObject);
begin
  // TODO: it would be better to put the leave action here
  // on a timer and trigger it after some amount of delay.
  if (assigned(Sender)) and (Sender = FocusedControl) then
  begin
    FocusedControl := nil;
    ScopeControl.Text := '';
    ScopeFocus := TScopeFocus.None;
    ControlChanged(nil);
  end;

end;

procedure TScopeHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
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

    ControlChanged(nil);
  end;

end;

procedure TScopeHandler.ControlChanged(c: TControl);
var
  Knob : TVamKnob;
  ParIndex : integer;
  Text : string;
begin
  if (assigned(c)) and (c is TVamKnob) then
  begin
    Knob := (c as TVamKnob);
    ParIndex := Knob.ParameterIndex;
    if (ParIndex >= 0) and (ParIndex < kParameterCount) then
    begin
      Text := Globals.VstParameters[ParIndex].ParInfo;
      ScopeControl.Text := Text;
    end;
  end;


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

function TScopeHandler.FindScopeFocus(c: TControl): TScopeFocus;
begin
  if (c is TVamWinControl) then
  begin
    if HasDisplayClass(c, TScopeFocusID.AmpEnv)      then exit(TScopeFocus.AmpEnv);
    if HasDisplayClass(c, TScopeFocusID.ModEnv)      then exit(TScopeFocus.ModEnv);
    if HasDisplayClass(c, TScopeFocusID.Lfo1)        then exit(TScopeFocus.Lfo1);
    if HasDisplayClass(c, TScopeFocusID.Lfo2)        then exit(TScopeFocus.Lfo2);
    if HasDisplayClass(c, TScopeFocusID.Filter1)     then exit(TScopeFocus.Filter1);
    if HasDisplayClass(c, TScopeFocusID.Filter2)     then exit(TScopeFocus.Filter2);
    if HasDisplayClass(c, TScopeFocusID.FilterBlend) then exit(TScopeFocus.FilterBlend);

    if HasDisplayClass(c, LfoControlDisplayClass) then
    begin
      case Globals.SelectedLfo of
        0: exit(TScopeFocus.Lfo1);
        1: exit(TScopeFocus.Lfo2);
      else
        raise Exception.Create('Selected LFO value not handled.');
      end;
    end;
  end;

  // if we've made it this far...
  result := TScopeFocus.None;
end;









end.
