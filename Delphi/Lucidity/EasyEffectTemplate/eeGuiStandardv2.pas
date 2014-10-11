unit eeGuiStandardv2;

interface

uses
  eePlugin,
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
  protected
    Plugin : TeePlugin;
  public
    constructor Create(const aPlugin : TeePlugin);
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

constructor TGuiStandard.Create(const aPlugin : TeePlugin);
begin
  fKnobHandler := TKnobHandler.Create(aPlugin);
  fMenuHandler := TMenuButtonHandler.Create(aPlugin);

  aPlugin.Globals.MotherShip.RegisterZeroObject(fKnobHandler, TZeroObjectRank.VCL);
  aPlugin.Globals.MotherShip.RegisterZeroObject(fMenuHandler, TZeroObjectRank.VCL);
end;

destructor TGuiStandard.Destroy;
begin
  fKnobHandler.Free;
  fMenuHandler.Free;
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
