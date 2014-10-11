unit eeGuiStandardv2;

interface

uses
  eePlugin,
  LucidityGui.MenuButtonHandler,
  LucidityGui.KnobHandler,
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
  FreeAndNil(fKnobHandler);
  FreeAndNil(fMenuHandler);
  inherited;
end;

procedure TGuiStandard.UpdateControls;
begin
  KnobHandler.UpdateAllControls;
  MenuHandler.UpdateAllControls;
end;

end.
