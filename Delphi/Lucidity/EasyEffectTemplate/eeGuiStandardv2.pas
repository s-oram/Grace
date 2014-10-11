unit eeGuiStandardv2;

interface

uses
  LucidityGui.MenuButtonHandler,
  LucidityGui.KnobHandler,
  eeTemp,
  Contnrs,
  VamLib.ZeroObject;

type
  TGuiStandard = class(TZeroObject)
  private
    fKnobHandler: TKnobHandler;
    fMenuHandler: TMenuButtonHandler;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterControl(const HandlerName : string; const c : TObject);
    procedure DeregisterControl(const c : TObject);

    procedure UpdateControls;

    property KnobHandler : TKnobHandler          read fKnobHandler;
    property MenuHandler : TMenuButtonHandler    read fMenuHandler;
  end;

implementation

uses
  SysUtils;

{ TGuiStandard }

constructor TGuiStandard.Create;
begin

end;

destructor TGuiStandard.Destroy;
begin

  inherited;
end;

procedure TGuiStandard.RegisterControl(const HandlerName : string; const c : TObject);

begin

end;

procedure TGuiStandard.DeregisterControl(const c: TObject);
begin

end;

procedure TGuiStandard.UpdateControls;
begin

end;



end.
