unit LucidityGui.KnobHandler;

interface

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
  eeGuiStandardv2;

type
  TKnobContextMenu = class(TCustomPopupMenu)
  private
    Menu : TPopUpMenu;
    fTargetParameterName: string;
  protected
    procedure Handle_RemoveCurrentModulation(Sender:TObject);
    procedure Handle_RemoveAllModulation(Sender:TObject);
    procedure Handle_RemoveModulationFromModSlot(Sender : TObject);
    procedure Handle_MidiLearn(Sender:TObject);
    procedure Handle_MidiUnlearn(Sender:TObject);
    procedure Handle_SetMidiCC(Sender:TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);

    property TargetParameterName : string read fTargetParameterName write fTargetParameterName;
  end;

  TKnobHandler = class(TRefCountedZeroObject, IStandardControlHandler)
  private
  protected
    ControlList : TObjectList;
    ThrottleHandle : TUniqueID;
    KnobContextMenu : TKnobContextMenu;
    Plugin : TeePlugin;

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

    procedure ShowControlContextMenu(const X, Y : integer; const ParName : string);
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Dialogs,
  Effect.MidiAutomation,
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

  KnobContextMenu := TKnobContextMenu.Create;
  KnobContextMenu.Initialize(aPlugin, nil);
end;

destructor TKnobHandler.Destroy;
begin
  ControlList.Free;
  KnobContextMenu.Free;
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
  Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.OnParControlEnter, @ParName);
end;

procedure TKnobHandler.Handle_MouseLeave(Sender: TObject);
var
  Knob : TVamKnob;
  ParName  : string;
begin
  assert(Sender is TVamKnob);
  Knob := Sender as TVamKnob;
  ParName  := Knob.ParameterName;
  Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.OnParControlLeave, @ParName);
end;


procedure TKnobHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Knob : TVamKnob;
  ParName  : string;
  //ParValue : single;
begin
  assert(Sender is TVamKnob);
  Knob := Sender as TVamKnob;

  ParName  := Knob.ParameterName;
  //ParValue := Knob.Pos;

  // TODO: need to have BeginEdit() called here for Publised VST parameter.

  // TODO: the last eeGuiStandard had an "Active Controls" list. Active Controls
  // aren't updated in the UpdateControl method.

  if (Button = TMouseButton.mbRight) then
  begin
    ShowControlContextMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y, ParName);
  end;
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
    Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_UpdateScope);
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

  Throttle(ThrottleHandle, 25,
  procedure
  begin
    Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.ModAmountChanged);
  end);

end;


procedure TKnobHandler.ShowControlContextMenu(const X, Y: integer; const ParName: string);
begin
  KnobContextMenu.TargetParameterName := ParName;
  KnobContextMenu.Popup(x, y);
end;




{ TKnobContextMenu }

constructor TKnobContextMenu.Create;
begin
  Menu := TPopUpMenu.Create(nil);
end;

destructor TKnobContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TKnobContextMenu.Popup(const x, y: integer);
var
  c1 : integer;
  mi : TMenuItem;
  miMidiLearn : TMenuItem;
  MidiBinding : IMidiBinding;
  MidiCC : integer;
  Text : string;
  ModSlotInfo : string;
begin
  Menu.Items.Clear;




  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Remove Current Modulation';
  mi.OnClick := Handle_RemoveCurrentModulation;
  if (Plugin.Globals.SelectedModSlot <> -1) and (Command.IsParameterModulated(Plugin, TargetParameterName))
    then mi.Enabled := true
    else mi.Enabled := false;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Remove All Modulation';
  mi.OnClick := Handle_RemoveAllModulation;
  if (Command.IsParameterModulated(Plugin, TargetParameterName))
    then mi.Enabled := true
    else mi.Enabled := false;
  Menu.Items.Add(mi);


  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);

  for c1 := 0 to kModSlotCount-1 do
  begin
    if Command.IsParameterModulated(Plugin, TargetParameterName, c1) then
    begin
      ModSlotInfo := 'Mod Slot ' + IntToStr(c1+1) + ' - ';
      mi := TMenuItem.Create(Menu);
      mi.Caption := 'Remove Modulation: ' + ModSlotInfo + Command.GetModSlotSource(Plugin, c1);
      mi.Tag := c1;
      mi.OnClick := Handle_RemoveModulationFromModSlot;
      Menu.Items.Add(mi);
    end;
  end;



  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);



    // Rebuild the context menu before showing it.
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'MIDI Learn';
  mi.OnClick := Handle_MidiLearn;
  Menu.Items.Add(mi);
  miMidiLearn := mi;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'MIDI Unlearn';
  mi.OnClick := Handle_MidiUnlearn;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Set MIDI CC...';
  mi.OnClick := Handle_SetMidiCC;
  Menu.Items.Add(mi);


  MidiBinding := Plugin.MidiAutomation.FindBinding(TargetParameterName);

  if assigned(MidiBinding)
    then MidiCC := MidiBinding.GetMidiCC
    else MidiCC := -1;

  if MidiCC <> -1
    then Text := 'MIDI Learn  [CC: ' + IntToStr(MidiCC) + ']'
    else Text := 'MIDI Learn  [CC: --]';

  miMidiLearn.Caption := Text;







  {
  //=== Update MIDI Learn menu item with current control midi binding. =====
  ParIndex := Globals.VstParameters.FindParameterIndex(ControlLinks[LinkIndex].LinkedParameter);
  MidiCC := Globals.VstMethods^.GetCurrentMidiBiding(ParIndex, ttVstParameter);

  if MidiCC <> -1
    then Text := 'MIDI Learn  [CC: ' + IntToStr(MidiCC) + ']'
    else Text := 'MIDI Learn  [CC: --]';

  miMidiLearn.Caption := Text;
  }

  //Show the controls context menu.
  Menu.Popup(X, Y);

end;

procedure TKnobContextMenu.Handle_MidiLearn(Sender: TObject);
var
  TargetBinding : TMidiBinding;
begin
  TargetBinding := TMidiBinding.Create;
  TargetBinding.ParName := TargetParameterName;
  Plugin.MidiAutomation.ActivateMidiLearn(TargetBinding);
end;

procedure TKnobContextMenu.Handle_MidiUnlearn(Sender: TObject);
begin
  Plugin.MidiAutomation.ClearBinding(TargetParametername);
end;

procedure TKnobContextMenu.Handle_SetMidiCC(Sender: TObject);
var
  Value : string;
  MidiCC : integer;
  Error : boolean;
  ErrorMessage : string;
  MidiBinding : IMidiBinding;
begin
  assert(Sender is TMenuItem);

  Error := false;

  Value := InputBox('Set MIDI CC', 'Choose a MIDI CC# (0-127)', '');

  if Value <> '' then
  begin
    // 2: Check for valid MIDI CC index,
    try
      MidiCC := StrToInt(Value);
    except
      //Catch all exceptions. Assume an invalid integer value was entered.
      Error := true;
      ErrorMessage := '"' + Value + '" isn''t a valid integer.';
      MidiCC := -1;
    end;

    if (Error = false) and (MidiCC < 0) then
    begin
      Error := true;
      ErrorMessage := 'The MIDI CC index you entered is too small.';
    end;

    if (Error = false) and (MidiCC > 127) then
    begin
      Error := true;
      ErrorMessage := 'The MIDI CC index you entered is too big.';
    end;

    if (Error = false) and (MidiCC >= 0) and (MidiCC <= 127) then
    begin
      // Set the midi binding for the current parameter.
      Plugin.MidiAutomation.ClearBinding(TargetParameterName);

      MidiBinding := TMidiBinding.Create;
      MidiBinding.SetParName(TargetParameterName);
      MidiBinding.SetMidiCC(MidiCC);

      Plugin.MidiAutomation.AddBinding(MidiBinding);
    end;
  end;

  if (Error = true) then
  begin
    ShowMessage('Error: ' + ErrorMessage);
  end;
end;

procedure TKnobContextMenu.Handle_RemoveAllModulation(Sender: TObject);
begin
  Command.ClearAllModulationForParameter(Plugin, TargetParameterName);
end;

procedure TKnobContextMenu.Handle_RemoveCurrentModulation(Sender: TObject);
begin
  Command.ClearCurrentModulationForParameter(Plugin, TargetParameterName);
end;

procedure TKnobContextMenu.Handle_RemoveModulationFromModSlot(Sender: TObject);
var
  Tag : integer;
begin
  Tag := (Sender as TMenuItem).Tag;
  Command.ClearModulationForParameter(Plugin, TargetParameterName, Tag);
end;

end.
