unit LucidityGui.MenuButtonHandler;

interface

uses
  VamGuiControlInterfaces,
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
    MenuBuilder  : TGuiMenuBuilder;

    procedure UpdateAllControls;

    procedure UpdateControl(const c : TObject);
    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);  override;
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Menus,
  eeTypes,
  eeEnumHelper,
  Lucidity.PluginParameters,
  Lucidity.Types,
  uConstants,
  uGuiUtils;

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
  mc : IMenuControl;
begin
  if Supports(c, IMenuControl, mc)  then
  begin
    mc.SetOnMouseEnter(Handle_MouseEnter);
    mc.SetOnMouseLeave(Handle_MouseLeave);
    mc.SetOnMouseDown(Handle_MouseDown);
    mc.SetOnMouseUp(Handle_MouseUp);

    if ControlList.IndexOf(c) = -1
      then ControlList.Add(c);
  end;
end;

procedure TMenuButtonHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TMenuButtonHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IInterface);
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
  Par : TPluginParameter;
  ParName  : string;
  ParID : TPluginParameterID;
  ParValue : single;
  EnumHelper : TCustomEnumHelperClass;
  TextValue : string;
  mc : IMenuControl;
begin
  if Supports(c, IMenuControl, mc)  then
  begin
    ParName  := mc.GetParameterName;
    Par := PluginParFromName(Parname);
    ParID := PluginParToID(Par);

    ParValue := Plugin.GetPluginParameter(ParID);

    assert(ParValue >= 0);
    assert(ParValue <= 1);

    EnumHelper := uGuiUtils.FindMenuHelperForParameter(Par);
    TextValue := EnumHelper.ToShortGuiString(ParValue);
    mc.SetMenuText(TextValue);
  end;
end;

procedure TMenuButtonHandler.Handle_MouseEnter(Sender: TObject);
var
  ParName  : string;
  mc : IMenuControl;
begin
  if Supports(Sender, IMenuControl, mc)  then
  begin
    ParName  := mc.GetParameterName;
    Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnParControlEnter, @ParName);
  end;
end;

procedure TMenuButtonHandler.Handle_MouseLeave(Sender: TObject);
var
  mc : IMenuControl;
  ParName  : string;
begin
  if Supports(Sender, IMenuControl, mc)  then
  begin
    ParName  := mc.GetParameterName;
    Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnParControlLeave, @ParName);
  end;
end;

procedure TMenuButtonHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  mc : IMenuControl;
  Par : TPluginParameter;
  ParName  : string;
  ParID    : TPluginParameterID;
  ParValue : single;
  EnumHelper : TCustomEnumHelperClass;
  ParValueAsInt : integer;
begin
  if Supports(Sender, IMenuControl, mc)  then
  begin
    if (Button = mbRight) then
    begin
      //===== Increment Enumerated Vst Parameter =================
      ParName  := mc.GetParameterName;
      ParID    := PluginParNameToID(ParName);
      ParValue := Plugin.GetPluginParameter(ParID);
      Par := PluginParFromName(Parname);
      EnumHelper := uGuiUtils.FindMenuHelperForParameter(Par);

      ParValueAsInt := EnumHelper.ToInteger(ParValue);
      inc(ParValueAsInt);
      if ParValueAsInt >= EnumHelper.GetEnumTypeCount
        then ParValueAsInt := 0;

      ParValue := EnumHelper.ToSingle(ParValueAsInt);

      // TODO:HIGH Should check if the parameter is a published vst parameter here.
      Plugin.SetPluginParameter(ParID, ParValue, TParChangeScope.psFocused);

      UpdateControl(Sender);
    end;
  end;
end;

procedure TMenuButtonHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemSelectedCallback : TMenuItemSelectedCallback;
  ParValueAsInt : integer;
  EnumHelper : TCustomEnumHelperClass;
  mc : IMenuControl;
  Par : TPluginParameter;
  ParName  : string;
  ParID    : TPluginParameterID;
  ParValue : single;
  ShowMenuCallback : TShowMenuCallback;
  CustomCallback : TNotifyEvent;
begin
  if Supports(Sender, IMenuControl, mc)  then
  begin
    ParName  := mc.GetParameterName;
    ParID    := PluginParNameToID(ParName);
    ParValue := Plugin.GetPluginParameter(ParID);
    Par := PluginParFromName(Parname);
    EnumHelper := uGuiUtils.FindMenuHelperForParameter(Par);
    ParValueAsInt := EnumHelper.ToInteger(ParValue);

    ItemSelectedCallback := procedure(SelectedItemIndex : integer)
    var
      NewParValue : single;
    begin
      NewParValue := EnumHelper.ToSingle(SelectedItemIndex);

      // TODO:HIGH Should check if the parameter is a published vst parameter here.
      Plugin.SetPluginParameter(ParID, NewParValue, TParChangeScope.psFocused);

      CustomCallback := mc.GetMenuItemSelectedCallback;
      if assigned(CustomCallback)
        then CustomCallback(self);

      UpdateControl(Sender);
    end;

    ShowMenuCallback := procedure(aMenu : TMenu)
    var
      Data : TMsgData_ShowMenu;
    begin
      Data.MenuName := ParName;
      Data.Menu     := @aMenu;

      Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnShowMenu, @Data);
    end;

    if (Button = mbLeft) then
    begin
      MenuBuilder.ShowMenuForVstParameter(ItemSelectedCallback, Mouse.CursorPos.X, Mouse.CursorPos.Y, ParValueAsInt, EnumHelper, ShowMenuCallback);
    end;
  end;
end;


end.
