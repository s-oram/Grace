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
var
  ScopeFocus : TScopeFocus;
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
