unit LucidityGui.KnobHandler;

interface

uses
  VamLib.UniqueID,
  Contnrs,
  Controls,
  Classes,
  eePlugin,
  VamLib.ZeroObject,
  eeGuiStandardv2;

type
  TKnobHandler = class(TRefCountedZeroObject, IStandardControlHandler)
  private
  protected
    ControlList : TObjectList;

    Plugin : TeePlugin;

    ThrottleHandle : TUniqueID;

    procedure UpdateAllControls;
    procedure UpdateModulation(const c : TObject);

    procedure UpdateControl(const c : TObject);
    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_KnobPosChanged(Sender: TObject);
    procedure Handle_ModAmountChanged(Sender: TObject);

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  VamLib.Throttler,
  Lucidity.PluginParameters,
  VamKnob,
  Lucidity.Types,
  uConstants,
  uGuiUtils;

{ TKnobHandler }

constructor TKnobHandler.Create(const aPlugin : TeePlugin);
begin
  Plugin := aPlugin;
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;
  ThrottleHandle.Init;
end;

destructor TKnobHandler.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TKnobHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  c1: Integer;
begin
  inherited;

  if MsgID = TLucidMsgID.ModSlotChanged then
  begin
    for c1 := 0 to ControlList.Count-1 do
    begin
      UpdateModulation(ControlList[c1]);
    end;
  end;

  if MsgID = TLucidMsgID.OnPostCreateFinished then
  begin
    for c1 := 0 to ControlList.Count-1 do
    begin
      UpdateControl(ControlList[c1]);
      UpdateModulation(ControlList[c1]);
    end;
  end;



end;



procedure TKnobHandler.RegisterControl(const c: TObject);
var
  Knob : TVamKnob;
begin
  assert(c is TVamKnob);
  Knob := c as TVamKnob;

  Knob.OnMouseEnter       := Handle_MouseEnter;
  Knob.OnMouseLeave       := Handle_MouseLeave;
  Knob.OnMouseDown        := Handle_MouseDown;
  Knob.OnMouseUp          := Handle_MouseUp;
  Knob.OnKnobPosChanged   := Handle_KnobPosChanged;
  Knob.OnModAmountChanged := Handle_ModAmountChanged;

  if ControlList.IndexOf(c) = -1
    then ControlList.Add(c);
end;

procedure TKnobHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TKnobHandler.UpdateControl(const c: TObject);
var
  Knob : TVamKnob;
  Par : TPluginParameter;
  ParName  : string;
  ParValue : single;
  ModIndex       : integer;
  ModAmountValue : single;
begin
  assert(c is TVamKnob);
  Knob := c as TVamKnob;

  ParName  := Knob.ParameterName;
  Par := PluginParFromName(Parname);
  // TODO: It might be handy to have a IsParNameValid() function here
  // to assert that parameter names are correct.
  ParValue := Plugin.GetPluginParameter(ParName);

  assert(ParValue >= 0);
  assert(ParValue <= 1);

  Knob.Pos := ParValue;

  if IsModPar(Par) then
  begin
    if Plugin.Globals.IsMouseOverModSlot
      then ModIndex := Plugin.Globals.MouseOverModSlot
      else ModIndex := Plugin.Globals.SelectedModSlot;

    if (ModIndex <> -1) then
    begin
      ModAmountValue := Plugin.GetPluginParameterModAmount(ParName, ModIndex);
      Knob.ModAmount := ModAmountValue;
    end;
  end;


  {
  if IsModPar(Par) = false then
  begin
    assert(Knob.KnobMode = TKnobMode.PositionEdit);
  end else
  begin
    if Plugin.Globals.IsMouseOverModSlot
      then ModIndex := Plugin.Globals.MouseOverModSlot
      else ModIndex := Plugin.Globals.SelectedModSlot;

    if (ModIndex <> -1) then
    begin
      ModAmountValue := Plugin.GetPluginParameterModAmount(ParName, ModIndex);
      Knob.ModAmount := ModAmountValue;
      Knob.KnobMode := TKnobMode.ModEdit;
      Knob.ModLineColor := kModLineColorB;
    end else
    begin
      Knob.KnobMode := TKnobMode.PositionEdit;
      Knob.ModLineColor := kModLineColorA;

      Plugin.GetModParModMinMax(ParName, ModMin, ModMax);
      Knob.MinModDepth := ModMin;
      Knob.MaxModDepth := ModMax;
    end;
  end;
  }
end;

procedure TKnobHandler.UpdateModulation(const c: TObject);
var
  Knob : TVamKnob;
  Par : TPluginParameter;
  ParName  : string;
  ModIndex       : integer;
  ModAmountValue : single;
  ModMin, ModMax : single;
begin
  assert(c is TVamKnob);
  Knob := c as TVamKnob;

  ParName  := Knob.ParameterName;
  Par := PluginParFromName(Parname);

  if IsModPar(Par) = false then
  begin
    assert(Knob.KnobMode = TKnobMode.PositionEdit);
  end else
  begin
    if Plugin.Globals.IsMouseOverModSlot
      then ModIndex := Plugin.Globals.MouseOverModSlot
      else ModIndex := Plugin.Globals.SelectedModSlot;

    if (ModIndex <> -1) then
    begin
      ModAmountValue := Plugin.GetPluginParameterModAmount(ParName, ModIndex);
      Knob.ModAmount := ModAmountValue;
      Knob.KnobMode := TKnobMode.ModEdit;
      Knob.ModLineColor := kModLineColorB;
    end else
    begin
      Knob.KnobMode := TKnobMode.PositionEdit;
      Knob.ModLineColor := kModLineColorA;

      Plugin.GetModParModMinMax(ParName, ModMin, ModMax);

      Knob.MinModDepth := ModMin;
      Knob.MaxModDepth := ModMax;
    end;
  end;
end;



procedure TKnobHandler.UpdateAllControls;
var
  c1: Integer;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    UpdateControl(ControlList[c1]);
  end;
end;



procedure TKnobHandler.Handle_MouseEnter(Sender: TObject);
var
  Knob : TVamKnob;
  ParName  : string;
begin
  assert(Sender is TVamKnob);
  Knob := Sender as TVamKnob;
  ParName  := Knob.ParameterName;
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnParControlEnter, @ParName);
end;

procedure TKnobHandler.Handle_MouseLeave(Sender: TObject);
var
  Knob : TVamKnob;
  ParName  : string;
begin
  assert(Sender is TVamKnob);
  Knob := Sender as TVamKnob;
  ParName  := Knob.ParameterName;
  Plugin.Globals.MotherShip.MsgMain(TLucidMsgID.OnParControlLeave, @ParName);
end;


procedure TKnobHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // TODO: need to have BeginEdit() called here for Publised VST parameter.

  // TODO: the last eeGuiStandard had an "Active Controls" list. Active Controls
  // aren't updated in the UpdateControl method.
end;

procedure TKnobHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // TODO: need to have BeginEdit() called here for Publised VST parameter.

  // TODO: the last eeGuiStandard had an "Active Controls" list. Active Controls
  // aren't updated in the UpdateControl method.
end;

procedure TKnobHandler.Handle_KnobPosChanged(Sender: TObject);
var
  Knob : TVamKnob;
  ParName  : string;
  ParValue : single;
begin
  assert(Sender is TVamKnob);
  Knob := Sender as TVamKnob;

  ParName  := Knob.ParameterName;
  ParValue := Knob.Pos;

  // TODO: Check if the parameter is a published vst parameter,
  // Send parameter change via the published VST parameter route if so,
  // otherwise set parameter value directly in plugin.
  Plugin.SetPluginParameter(TParChangeScope.psFocusedKeyGroup, '', ParName, ParValue);

  Throttle(ThrottleHandle, 25,
  procedure
  begin
    Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.Command_UpdateScope);
  end);
end;

procedure TKnobHandler.Handle_ModAmountChanged(Sender: TObject);
var
  Knob : TVamKnob;
  ParName  : string;
  ModIndex       : integer;
  ModAmountValue : single;
begin
  assert(Sender is TVamKnob);
  Knob := Sender as TVamKnob;

  ParName  := Knob.ParameterName;
  ModAmountValue := Knob.ModAmount;
  ModIndex := Plugin.Globals.SelectedModSlot;

  if ModIndex <> -1 then
  begin
    Plugin.SetPluginParameterModAmount(TParChangeScope.psFocusedKeyGroup, ParName, ModIndex, ModAmountValue);
  end;

end;










end.
