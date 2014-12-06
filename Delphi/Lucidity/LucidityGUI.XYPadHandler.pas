unit LucidityGUI.XYPadHandler;

interface

{$INCLUDE Defines.inc}

uses
  Lucidity.CustomControlHandler,
  VamLib.DuckType,
  Contnrs,
  Controls,
  Classes,
  Vcl.Menus,
  Menu.CustomPopupMenu,
  VamLib.ZeroObject,
  Menu.XYPadContextMenu,
  eePlugin,
  eeMidiAutomationV2,
  Lucidity.PluginParameters;

{+M}

type
  TXYPadHandler = class(TCustomControlHandler)
  private
  protected
    PadContextMenu : TXYPadContextMenu;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface);  override;
    procedure UpdateControl(const c : TObject); override;
  public
    constructor Create(AGuiOwner: TComponent; const aPlugin : TeePlugin); override;
    destructor Destroy; override;

    procedure RegisterControl(const c : TObject); override;
  published
    // publish the event handlers so they can be accessed using the RTTI.
    procedure EventHandle_XYPadChanged(Sender: TObject);
    procedure EventHandle_XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EventHandle_XYPadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

implementation

uses
  eeTypes,
  VamXYPad,
  SysUtils,
  VamLib.PatchUtils,
  Lucidity.Enums,
  Vcl.Dialogs;

{ TXYPadHandler }

constructor TXYPadHandler.Create(AGuiOwner: TComponent; const aPlugin: TeePlugin);
begin
  inherited;
  PadContextMenu := TXYPadContextMenu.Create(AGuiOwner);
  PadContextMenu.Plugin := aPlugin;
end;

destructor TXYPadHandler.Destroy;
begin
  PadContextMenu.Free;
  inherited;
end;

procedure TXYPadHandler.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB: IInterface);
begin
  inherited;

end;

type
  TNotifyEventReference = reference to procedure(Sender : TObject);

procedure TXYPadHandler.RegisterControl(const c: TObject);
begin
  inherited;

  // requirement
  assert(c is TVamXYPad);

  //==== assign event handlers ====
  c.Duck.RequireTarget.SetEvent('OnChanged', self, 'EventHandle_XYPadChanged');
  c.Duck.RequireTarget.SetEvent('OnMouseDown', self, 'EventHandle_XYPadMouseDown');
  c.Duck.RequireTarget.SetEvent('OnMouseUp', self, 'EventHandle_XYPadMouseUp');
end;

procedure TXYPadHandler.UpdateControl(const c: TObject);
var
  Pad : TVamXYPad;
  PadX, PadY : single;
begin
  inherited;

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
  PadX, PadY : single;
  Name1, Name2 : string;
begin
  assert(assigned(Plugin));

  Name1 := (Sender as TVamXYPad).ParameterNameX;
  Name2 := (Sender as TVamXYPad).ParameterNameY;

  PadX := (Sender as TVamXYPad).PosX;
  PadY := (Sender as TVamXYPad).PosY;

  PluginParameterChanged((Sender as TVamXYPad).ParameterNameX, PadX);
  PluginParameterChanged((Sender as TVamXYPad).ParameterNameY, PadY);
end;

procedure TXYPadHandler.EventHandle_XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;
  Name1, Name2 : string;
begin
  assert(assigned(Plugin));

  Name1 := (Sender as TVamXYPad).ParameterNameX;
  Name2 := (Sender as TVamXYPad).ParameterNameY;

  Plugin.Globals.GuiState.HotkeyContext := THotKeyContext.None;

  if (Button = TMouseButton.mbLeft) then
  begin
    PluginParameterBeginEdit(Name1, Name2);
    if (ssCtrl in Shift) then
    begin
      PluginParameterReset(Name1);
      PluginParameterReset(Name2);
    end;
  end;

  if Button = TMouseButton.mbRight then
  begin
    Tag := (Sender as TVamXYPad).Tag;
    PadContextMenu.TargetXYPadIndex := Tag - 1; //NOTE: The (Tag-1) is because the pads are numbered 1 to 4.
    PadContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TXYPadHandler.EventHandle_XYPadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Name1, Name2 : string;
begin
  if (Button = TMouseButton.mbLeft) then
  begin
    Name1 := (Sender as TVamXYPad).ParameterNameX;
    Name2 := (Sender as TVamXYPad).ParameterNameY;
    PluginParameterEndEdit(Name1, Name2);
  end;
end;

end.
