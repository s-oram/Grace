unit GuiMeta.ScopeHandler;

interface

uses
  eeGlobals,
  Classes,
  Controls,
  LucidityGUI.Scope;

type
  TScopeHandler = class
  private
    fScopeControl: TLucidityScope;
  protected
    Globals : TGlobals;
    procedure KnobMouseEnter(Sender : TObject);
    procedure KnobMouseLeave(Sender : TObject);
    procedure KnobChanged(Sender : TObject);
  public
    constructor Create(aGlobals : TGlobals);
    destructor Destroy; override;

    procedure RegisterControl(c : TControl);

    property ScopeControl : TLucidityScope read fScopeControl write fScopeControl;
  end;

implementation

uses
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
    (c as TVamKnob).ChangedMultiEvent.Add(KnobMouseLeave);
  end;

end;

procedure TScopeHandler.KnobMouseEnter(Sender: TObject);
var
  c : TVamKnob;
  ParIndex : integer;
  Text : string;
begin
  if (Sender is TVamKnob) then
  begin
    c := (Sender as TVamKnob);
    ParIndex := c.ParameterIndex;

    if (ParIndex >= 0) and (ParIndex < kParameterCount) then
    begin
      Text := Globals.VstParameters[ParIndex].ParInfo;
      ScopeControl.Text := Text;
    end;



  end;
end;

procedure TScopeHandler.KnobChanged(Sender: TObject);
begin

end;

procedure TScopeHandler.KnobMouseLeave(Sender: TObject);
begin

end;





end.
