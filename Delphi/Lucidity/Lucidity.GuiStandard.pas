unit Lucidity.GuiStandard;

interface

uses
  Contnrs,
  VamLib.ZeroObject,
  eePlugin,
  LucidityGui.MenuButtonHandler,
  LucidityGui.KnobHandler,
  LucidityGUI.XYPadHandler;

type
  TGuiStandard = class(TZeroObject)
  private
    fKnobHandler: TKnobHandler;
    fMenuHandler: TMenuButtonHandler;
    fXyPadHandler: TXyPadHandler;
  protected
    Plugin : TeePlugin;
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;

    procedure UpdateControls;

    property KnobHandler  : TKnobHandler          read fKnobHandler;
    property MenuHandler  : TMenuButtonHandler    read fMenuHandler;
    property XyPadHandler : TXyPadHandler         read fXyPadHandler;
  end;

implementation

uses
  SysUtils;

{ TGuiStandard }

constructor TGuiStandard.Create(const aPlugin : TeePlugin);
begin
  fKnobHandler  := TKnobHandler.Create(aPlugin);
  fMenuHandler  := TMenuButtonHandler.Create(aPlugin);
  fXyPadHandler := TXyPadHandler.Create(aPlugin);

  aPlugin.Globals.MotherShip.RegisterZeroObject(fKnobHandler, TZeroObjectRank.VCL);
  aPlugin.Globals.MotherShip.RegisterZeroObject(fMenuHandler, TZeroObjectRank.VCL);
  aPlugin.Globals.MotherShip.RegisterZeroObject(fXyPadHandler, TZeroObjectRank.VCL);
end;

destructor TGuiStandard.Destroy;
begin
  FreeAndNil(fKnobHandler);
  FreeAndNil(fMenuHandler);
  FreeAndNil(fXyPadHandler);
  inherited;
end;

procedure TGuiStandard.UpdateControls;
begin
  KnobHandler.UpdateAllControls;
  MenuHandler.UpdateAllControls;
  XYPadHandler.UpdateAllControls;
end;

end.
