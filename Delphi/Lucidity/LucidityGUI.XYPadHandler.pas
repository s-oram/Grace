unit LucidityGUI.XYPadHandler;

interface

{$INCLUDE Defines.inc}

uses
  Vcl.Menus,
  Menu.CustomPopupMenu,
  VamLib.UniqueID,
  Contnrs,
  Controls,
  Classes,
  eePlugin,
  VamLib.ZeroObject,
  eeMidiAutomationV2,
  Lucidity.PluginParameters;

type
  TXYPadHandler = class(TZeroObject)
  private
  protected
    ControlList : TObjectList;
    ThrottleHandle : TUniqueID;
    Plugin : TeePlugin;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);  override;
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;

    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);
    procedure UpdateAllControls;
  end;

implementation

{ TXYPadHandler }

constructor TXYPadHandler.Create(const aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;
  ThrottleHandle.Init;
end;

destructor TXYPadHandler.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TXYPadHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB: IInterface);
begin
  inherited;

end;

procedure TXYPadHandler.RegisterControl(const c: TObject);
begin

end;

procedure TXYPadHandler.DeregisterControl(const c: TObject);
begin

end;

procedure TXYPadHandler.UpdateAllControls;
begin

end;

end.
