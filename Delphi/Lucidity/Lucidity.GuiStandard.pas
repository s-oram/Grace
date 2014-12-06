unit Lucidity.GuiStandard;

interface

uses
  Classes,
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
    constructor Create(AGuiOwner: TComponent; const aPlugin : TeePlugin);
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

constructor TGuiStandard.Create(AGuiOwner: TComponent; const aPlugin : TeePlugin);
begin
  fKnobHandler  := TKnobHandler.Create(AGuiOwner, aPlugin);
  fMenuHandler  := TMenuButtonHandler.Create(AGuiOwner, aPlugin);
  fXyPadHandler := TXyPadHandler.Create(AGuiOwner, aPlugin);

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
