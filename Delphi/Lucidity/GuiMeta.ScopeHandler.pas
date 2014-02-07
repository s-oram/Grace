unit GuiMeta.ScopeHandler;

interface

uses
  eeGlobals,
  Classes,
  Controls,
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

  TScopeHandler = class
  private
    fScopeControl: TLucidityScope;
  protected
    Globals : TGlobals;
    FocusedControl : TControl;
    ScopeFocus : TScopeFocus;
    procedure KnobMouseEnter(Sender : TObject);
    procedure KnobMouseLeave(Sender : TObject);
    procedure KnobChanged(Sender : TObject);

    procedure ControlChanged(c : TControl);

    function FindScopeFocus(c : TControl):TScopeFocus;
  public
    constructor Create(aGlobals : TGlobals);
    destructor Destroy; override;

    procedure RegisterControl(c : TControl);

    property ScopeControl : TLucidityScope read fScopeControl write fScopeControl;
  end;

implementation

uses
  VamQuery,
  VamWinControl,
  uConstants,
  VamKnob;

{ TScopeHandler }

constructor TScopeHandler.Create(aGlobals : TGlobals);
begin
  Globals := aGlobals;
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
  ControlChanged(Sender as TControl);
end;

procedure TScopeHandler.KnobMouseLeave(Sender: TObject);
begin
  // TODO: it would be better to put the leave action here
  // on a timer and trigger it after some amount of delay.
  if (assigned(Sender)) and (Sender = FocusedControl) then
  begin
    FocusedControl := nil;
    ScopeControl.Text := '';
  end;

end;

procedure TScopeHandler.ControlChanged(c: TControl);
var
  Knob : TVamKnob;
  ParIndex : integer;
  Text : string;
begin
  if (c is TVamKnob) then
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

      ScopeControl.LfoValues.Par1 := Globals.VstParameters.FindParameter(TParName.Lfo1Par1).ValueVST;
      ScopeControl.LfoValues.Par2 := Globals.VstParameters.FindParameter(TParName.Lfo1Par2).ValueVST;
      ScopeControl.LfoValues.Par3 := Globals.VstParameters.FindParameter(TParName.Lfo1Par3).ValueVST;
    end;

    TScopeFocus.Lfo2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.LFO;

      ScopeControl.LfoValues.Par1 := Globals.VstParameters.FindParameter(TParName.Lfo2Par1).ValueVST;
      ScopeControl.LfoValues.Par2 := Globals.VstParameters.FindParameter(TParName.Lfo2Par2).ValueVST;
      ScopeControl.LfoValues.Par3 := Globals.VstParameters.FindParameter(TParName.Lfo2Par3).ValueVST;
    end;

    TScopeFocus.Filter1:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ScopeControl.FilterValues.Par1 := Globals.VstParameters.FindParameter(TParName.Filter1Par1).ValueVST;
      ScopeControl.FilterValues.Par2 := Globals.VstParameters.FindParameter(TParName.Filter1Par2).ValueVST;
      ScopeControl.FilterValues.Par3 := Globals.VstParameters.FindParameter(TParName.Filter1Par3).ValueVST;
      ScopeControl.FilterValues.Par4 := Globals.VstParameters.FindParameter(TParName.Filter1Par4).ValueVST;
    end;

    TScopeFocus.Filter2:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.Filter;

      ScopeControl.FilterValues.Par1 := Globals.VstParameters.FindParameter(TParName.Filter2Par1).ValueVST;
      ScopeControl.FilterValues.Par2 := Globals.VstParameters.FindParameter(TParName.Filter2Par2).ValueVST;
      ScopeControl.FilterValues.Par3 := Globals.VstParameters.FindParameter(TParName.Filter2Par3).ValueVST;
      ScopeControl.FilterValues.Par4 := Globals.VstParameters.FindParameter(TParName.Filter2Par4).ValueVST;
    end;

    TScopeFocus.FilterBlend:
    begin
      ScopeControl.ScopeMode := TScopeDisplayMode.FilterBlend;

      //TODO: add link to filter blend amount!
      //ScopeControl.FilterBlendValues.BlendAmt := Globals.VstParameters.FindParameter(TParName.fiFilter1Par1).ValueVST;
    end;
  end;
end;

function TScopeHandler.FindScopeFocus(c: TControl): TScopeFocus;
var
  vc : TVamWinControl;
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
  end;


end;









end.
