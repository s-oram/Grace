unit LucidityGUI.XYPadHandler;

interface

{$INCLUDE Defines.inc}

{
  TODO: The other handlers all use interfaces, but it turns out my code
  has become very Lucidity centric, rather than the general purpose code i
  wanted it to be. Because of that I'm going to change tack with this handler.
  Rather than interfaces, I'll use duck typing to interact with the
  registered controls. This will allow similar flexibility to using interfaces,
  but the code will be in one less place. (Control + Handler) instead
  of (Control + Interface + Hander).
}

// TODO:HIGH
// The XY Pads don't broadcast BeginEdit, EndEdit and SetParameterAutomated
// so they can't be linked into the host automation. This needs to change.


uses
  VamLib.DuckType,
  Contnrs,
  Controls,
  Classes,
  Vcl.Menus,
  Menu.CustomPopupMenu,
  VamLib.UniqueID,
  VamLib.ZeroObject,
  Menu.XYPadContextMenu,
  eePlugin,
  eeMidiAutomationV2,
  Lucidity.PluginParameters;

{+M}

type
  TXYPadHandler = class(TZeroObject)
  private
  protected
    Plugin : TeePlugin;
    PadContextMenu : TXYPadContextMenu;
    ControlList : TObjectList;
    ThrottleHandle : TUniqueID;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);  override;
    procedure UpdateControl(const c : TObject);
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;

    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);
    procedure UpdateAllControls;

  published
    // publish the event handlers so they can be accessed using the RTTI.
    procedure EventHandle_XYPadChanged(Sender: TObject);
    procedure EventHandle_XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

implementation

uses
  eeTypes,
  VamXYPad,
  SysUtils,
  VamLib.PatchUtils,
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
  PadContextMenu := TXYPadContextMenu.Create;
  PadContextMenu.Plugin := aPlugin;
end;

destructor TXYPadHandler.Destroy;
begin
  ControlList.Free;
  PadContextMenu.Free;
  inherited;
end;

procedure TXYPadHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB: IInterface);
begin
  inherited;

end;

type
  TNotifyEventReference = reference to procedure(Sender : TObject);

procedure TXYPadHandler.RegisterControl(const c: TObject);
var
  v : TValue;
  Prop: TRttiProperty;
  RttiContext : TRtticontext;

  p : TNotifyEventReference;
  Proc : pointer;
  m :TMethod;
begin
  if ControlList.IndexOf(c) = -1
    then ControlList.Add(c);

  //==== check for requirements ====
  c.Duck.RequireTarget.SetEvent('OnChanged', self, 'EventHandle_XYPadChanged');
  c.Duck.RequireTarget.SetEvent('OnMouseDown', self, 'EventHandle_XYPadMouseDown');
end;

procedure TXYPadHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TXYPadHandler.UpdateAllControls;
var
  c1: Integer;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    UpdateControl(ControlList[c1]);
  end;
end;

procedure TXYPadHandler.UpdateControl(const c: TObject);
var
  Pad : TVamXYPad;
  PadX, PadY : single;
begin
  assert(c is TVamXyPad);
  Pad := (c as TVamXyPad);

  case Pad.Tag of
    1:
    begin
      PadX := Plugin.XYPads.PadX1;
      PadY := Plugin.XYPads.PadY1;
    end;

    2:
    begin
      PadX := Plugin.XYPads.PadX2;
      PadY := Plugin.XYPads.PadY2;
    end;

    3:
    begin
      PadX := Plugin.XYPads.PadX3;
      PadY := Plugin.XYPads.PadY3;
    end;

    4:
    begin
      PadX := Plugin.XYPads.PadX4;
      PadY := Plugin.XYPads.PadY4;
    end;
  else
    raise Exception.Create('Index not handled.');
  end;

  Pad.PosX := PadX;
  Pad.PosY := PadY;
end;

procedure TXYPadHandler.EventHandle_XYPadChanged(Sender: TObject);
var
  Tag : integer;
  PadX, PadY : single;
begin
  assert(assigned(Plugin));

  Tag := (Sender as TVamXYPad).Tag;

  PadX := (Sender as TVamXYPad).PosX;
  PadY := (Sender as TVamXYPad).PosY;

  case Tag of
    1:
    begin
      Plugin.XYPads.PadX1 := PadX;
      Plugin.XYPads.PadY1 := PadY;
    end;

    2:
    begin
      Plugin.XYPads.PadX2 := PadX;
      Plugin.XYPads.PadY2 := PadY;
    end;

    3:
    begin
      Plugin.XYPads.PadX3 := PadX;
      Plugin.XYPads.PadY3 := PadY;
    end;

    4:
    begin
      Plugin.XYPads.PadX4 := PadX;
      Plugin.XYPads.PadY4 := PadY;
    end;
  else
    raise Exception.Create('Index not handled.');
  end;
end;

procedure TXYPadHandler.EventHandle_XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;
  Name1, Name2 : string;
  ptr : Pointer;
begin
  assert(assigned(Plugin));

  if (Button = TMouseButton.mbLeft) and (ssCtrl in Shift) then
  begin
    Tag := (Sender as TVamXYPad).Tag;

    case Tag of
      1:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX1);
        Name2 := PluginParToName(TPluginParameter.PadY1);
      end;

      2:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX2);
        Name2 := PluginParToName(TPluginParameter.PadY2);
      end;

      3:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX3);
        Name2 := PluginParToName(TPluginParameter.PadY3);
      end;

      4:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX4);
        Name2 := PluginParToName(TPluginParameter.PadY4);
      end;
    else
      Name1 := '';
      Name2 := '';
      raise Exception.Create('Tag not handled.');
    end;

    Plugin.ResetPluginParameter(TParChangeScope.psFocused, Name1);
    Plugin.ResetPluginParameter(TParChangeScope.psFocused, Name2);
  end;

  if Button = TMouseButton.mbRight then
  begin
    Tag := (Sender as TVamXYPad).Tag;
    PadContextMenu.TargetXYPadIndex := Tag - 1; //NOTE: The (Tag-1) is because the pads are numbered 1 to 4.
    PadContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;



end.
