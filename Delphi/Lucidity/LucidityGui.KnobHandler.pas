unit LucidityGui.KnobHandler;

interface

uses
  eePlugin,
  VamLib.ZeroObject,
  eeGuiStandardv2;

type
  TKnobHandler = class(TRefCountedZeroObject, IStandardControlHandler)
  private
  protected
    Plugin : TeePlugin;
    procedure UpdateControl(const c : TObject);
    procedure SetupControl(const c : TObject);
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

{ TKnobHandler }

constructor TKnobHandler.Create(const aPlugin : TeePlugin);
begin
  Plugin := aPlugin;
end;

destructor TKnobHandler.Destroy;
begin

  inherited;
end;

procedure TKnobHandler.SetupControl(const c: TObject);
begin

end;

procedure TKnobHandler.UpdateControl(const c: TObject);
begin

end;

end.
