unit LucidityGui.MenuButtonHandler;

interface

uses
  eeGuiStandardv2_MenuBuilder,
  Contnrs,
  Controls,
  Classes,
  eePlugin,
  VamLib.ZeroObject,
  eeGuiStandardv2;

type
  TMenuButtonHandler = class(TRefCountedZeroObject, IStandardControlHandler)
  private
  protected
    ControlList : TObjectList;

    Plugin : TeePlugin;

    MenuBuilder             : TGuiMenuBuilder;

    procedure UpdateAllControls;

    procedure UpdateControl(const c : TObject);
    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  eeEnumHelper,
  Lucidity.PluginParameters,
  Lucidity.Types,
  uConstants,
  uGuiUtils,
  VamTextBox;

{ TMenuButtonHandler }

constructor TMenuButtonHandler.Create(const aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;

  MenuBuilder := TGuiMenuBuilder.Create;
end;

destructor TMenuButtonHandler.Destroy;
begin
  ControlList.Free;
  MenuBuilder.Free;
  inherited;
end;

procedure TMenuButtonHandler.RegisterControl(const c: TObject);
var
  tb : TVamTextBox;
begin
  assert(c is TVamTextBox);
  tb := c as TVamTextBox;

  tb.OnMouseEnter := self.Handle_MouseEnter;
  tb.OnMouseLeave := self.Handle_MouseLeave;
  tb.OnMouseDown  := self.Handle_MouseDown;
  tb.OnMouseUp    := self.Handle_MouseUp;

  if ControlList.IndexOf(c) = -1
    then ControlList.Add(c);
end;

procedure TMenuButtonHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TMenuButtonHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  inherited;

end;

procedure TMenuButtonHandler.UpdateAllControls;
var
  c1: Integer;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    UpdateControl(ControlList[c1]);
  end;
end;

procedure TMenuButtonHandler.UpdateControl(const c: TObject);
var
  tb : TVamTextBox;
  Par : TPluginParameter;
  ParName  : string;
  ParValue : single;
  EnumHelper : TCustomEnumHelperClass;
  TextValue : string;
begin
  assert(c is TVamTextBox);
  tb := (c as TVamTextBox);

  ParName  := tb.ParameterName;
  Par := PluginParFromName(Parname);

  ParValue := Plugin.GetPluginParameter(ParName);

  assert(ParValue >= 0);
  assert(ParValue <= 1);

  EnumHelper := uGuiUtils.FindMenuHelperForParameter(Par);
  TextValue := EnumHelper.ToShortGuiString(ParValue);
  if tb.Text <> TextValue then tb.Text := TextValue;
end;

procedure TMenuButtonHandler.Handle_MouseEnter(Sender: TObject);
var
  tb : TVamTextBox;
  ParName  : string;
begin
  assert(Sender is TVamTextBox);
  tb := (Sender as TVamTextBox);
  ParName  := tb.ParameterName;
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnParControlEnter, @ParName);
end;

procedure TMenuButtonHandler.Handle_MouseLeave(Sender: TObject);
var
  tb : TVamTextBox;
  ParName  : string;
begin
  assert(Sender is TVamTextBox);
  tb := (Sender as TVamTextBox);
  ParName  := tb.ParameterName;
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnParControlLeave, @ParName);
end;

procedure TMenuButtonHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tb : TVamTextBox;
  Par : TPluginParameter;
  ParName  : string;
  ParValue : single;
  EnumHelper : TCustomEnumHelperClass;
  ParValueAsInt : integer;
begin
  assert(Sender is TVamTextBox);
  tb := (Sender as TVamTextBox);

  if (Button = mbRight) then
  begin
    //===== Increment Enumerated Vst Parameter =================
    ParName  := tb.ParameterName;
    ParValue := Plugin.GetPluginParameter(ParName);
    Par := PluginParFromName(Parname);
    EnumHelper := uGuiUtils.FindMenuHelperForParameter(Par);

    ParValueAsInt := EnumHelper.ToInteger(ParValue);
    inc(ParValueAsInt);
    if ParValueAsInt >= EnumHelper.GetEnumTypeCount
      then ParValueAsInt := 0;

    ParValue := EnumHelper.ToSingle(ParValueAsInt);

    // TODO: Should check if the parameter is a published vst parameter here.
    Plugin.SetPluginParameter(TParChangeScope.psFocusedKeyGroup, '', ParName, ParValue);

    UpdateControl(Sender);
  end;
end;

procedure TMenuButtonHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemSelectedCallback : TMenuItemSelectedCallback;
  ParValueAsInt : integer;
  EnumHelper : TCustomEnumHelperClass;
  tb : TVamTextBox;
  Par : TPluginParameter;
  ParName  : string;
  ParValue : single;
begin
  assert(Sender is TVamTextBox);
  tb := (Sender as TVamTextBox);

  ParName  := tb.ParameterName;
  ParValue := Plugin.GetPluginParameter(ParName);
  Par := PluginParFromName(Parname);
  EnumHelper := uGuiUtils.FindMenuHelperForParameter(Par);
  ParValueAsInt := EnumHelper.ToInteger(ParValue);

  ItemSelectedCallback := procedure(SelectedItemIndex : integer)
  var
    NewParValue : single;
  begin
    NewParValue := EnumHelper.ToSingle(SelectedItemIndex);

    // TODO: Should check if the parameter is a published vst parameter here.
    Plugin.SetPluginParameter(TParChangeScope.psFocusedKeyGroup, '', ParName, NewParValue);

    UpdateControl(Sender);
  end;

  if (Button = mbLeft) then
  begin
    MenuBuilder.ShowMenuForVstParameter(ItemSelectedCallback, Mouse.CursorPos.X, Mouse.CursorPos.Y, ParValueAsInt, EnumHelper);
  end;


end;


end.
