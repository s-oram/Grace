unit GuiMeta.ScopeHandler;

interface

uses
  Classes,
  Controls,
  LucidityGUI.Scope;

type
  TScopeHandler = class
  private
    fScopeControl: TLucidityScope;
  protected
    procedure KnobMouseEnter(Sender : TObject);
    procedure KnobMouseLeave(Sender : TObject);
    procedure KnobChanged(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterControl(c : TControl);

    property ScopeControl : TLucidityScope read fScopeControl write fScopeControl;
  end;

implementation

uses
  VamKnob;

{ TScopeHandler }

constructor TScopeHandler.Create;
begin

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
begin

end;

procedure TScopeHandler.KnobChanged(Sender: TObject);
begin

end;

procedure TScopeHandler.KnobMouseLeave(Sender: TObject);
begin

end;





end.
