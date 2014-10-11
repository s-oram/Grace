unit LucidityGUI.XYPadHandler;

interface

{$INCLUDE Defines.inc}

uses
  Duck,
  Contnrs,
  Controls,
  Classes,
  Vcl.Menus,
  Menu.CustomPopupMenu,
  VamLib.UniqueID,
  VamLib.ZeroObject,
  eePlugin,
  eeMidiAutomationV2,
  Lucidity.PluginParameters;

{+M}

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
  published
    procedure EventHandle_XYPadChanged(Sender: TObject);
    procedure EventHandle_XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

implementation

uses
  Vcl.Dialogs,
  TypInfo,
  System.Rtti;


type
  PObject = ^TObject;

function GetEventHandler(obj : PObject; MethodName : string):TMethod;
var
  m : TMethod;
begin
  m.Data := obj;
  m.Code := obj^.MethodAddress(MethodName);
  result := m;
end;

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
var
  v : TValue;
  Prop: TRttiProperty;
  RttiContext : TRtticontext;
begin
  if ControlList.IndexOf(c) = -1
    then ControlList.Add(c);

  SetMethodProp(c, 'OnChanged', GetEventHandler(@self, 'EventHandle_XYPadChanged'));
end;

procedure TXYPadHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TXYPadHandler.UpdateAllControls;
begin

end;



procedure TXYPadHandler.EventHandle_XYPadChanged(Sender: TObject);
begin
  showMessage('bang');
end;

procedure TXYPadHandler.EventHandle_XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;



end.
