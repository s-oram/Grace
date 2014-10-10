unit LucidityGui.KnobHandler;

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
  eeGuiStandardv2,
  Lucidity.PluginParameters;

type
  TKnobContextMenu = class(TCustomPopupMenu)
  private
    fTargetParameterName: string;
  protected
    procedure Handle_RemoveCurrentModulation(Sender:TObject);
    procedure Handle_RemoveAllModulation(Sender:TObject);
    procedure Handle_RemoveModulationFromModSlot(Sender : TObject);
    procedure Handle_MidiLearn(Sender:TObject);
    procedure Handle_MidiUnlearn(Sender:TObject);
    procedure Handle_SetMidiCC(Sender:TObject);
  public
    constructor Create; override;
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

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);  override;

    procedure ShowControlContextMenu(const X, Y : integer; const ParName : string);
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils,
  eeTypes,
  Effect.MidiAutomation,
  VamLib.Throttler,
  VamKnob,
  VamGuiControlInterfaces,
  uLucidityEnums,
  Lucidity.Types,
  uConstants,
  Lucidity.GuiUtils;

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

procedure TKnobHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IInterface);
var
  c1: Integer;
begin
  inherited;

  if MsgID = TLucidMsgID.ModAmountChanged then
  begin
    for c1 := 0 to ControlList.Count-1 do
    begin
      UpdateModulation(ControlList[c1]);
    end;
  end;

  if MsgID = TLucidMsgID.ModSlotChanged then
  begin
    for c1 := 0 to ControlList.Count-1 do
    begin
      UpdateModulation(ControlList[c1]);
    end;
  end;

  if (MsgID = TLucidMsgID.OnPostCreateFinished) or (MsgID = TLucidMsgID.NewPatchLoaded) then
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
  KnobControl : IKnobControl;
begin
  assert(Supports(c, IKnobControl), 'Control doesn''t support IKnobControl.');

  if Supports(c, IKnobControl, KnobControl) then
  begin
    KnobControl.SetOnMouseEnter(Handle_MouseEnter);
    KnobControl.SetOnMouseLeave(Handle_MouseLeave);
    KnobControl.SetOnMouseDown(Handle_MouseDown);
    KnobControl.SetOnMouseUp(Handle_MouseUp);
    KnobControl.SetOnKnobPosChanged(Handle_KnobPosChanged);
    KnobControl.SetOnModAmountChanged(Handle_ModAmountChanged);
  end;

  if ControlList.IndexOf(c) = -1
    then ControlList.Add(c);
end;

procedure TKnobHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TKnobHandler.UpdateControl(const c: TObject);
var
  KnobControl : IKnobControl;
  Par : TPluginParameter;
  ParName  : string;
  ParID : TPluginParameterID;
  ParValue : single;
  ModIndex       : integer;
  ModAmountValue : single;
begin
  if Supports(c, IKnobControl, KnobControl) then
  begin
    //TODO:HIGH Instead of getting the parameter name from the knob,
    // we need to get a parameter ID.
    ParName  := KnobControl.GetParameterName;
    Par := PluginParFromName(Parname);
    ParID := PluginParToID(Par);

    // TODO:MED It might be handy to have a IsParNameValid() function here
    // to assert that parameter names are correct.
    ParValue := Plugin.GetPluginParameter(ParID);

    assert(ParValue >= 0);
    assert(ParValue <= 1);

    KnobControl.SetKnobValue(ParValue);

    if IsModPar(Par) then
    begin
      if Plugin.Globals.IsMouseOverModSlot
        then ModIndex := Plugin.Globals.MouseOverModSlot
        else ModIndex := Plugin.Globals.SelectedModSlot;

      if (ModIndex <> -1) then
      begin
        ModAmountValue := Plugin.GetPluginParameterModAmount(ParName, ModIndex);
        KnobControl.SetModAmountValue(ModAmountValue);
      end;
    end;
  end;
end;

procedure TKnobHandler.UpdateModulation(const c: TObject);
var
  KnobControl : IKnobControl;
  Par : TPluginParameter;
  ParName  : string;
  ModIndex       : integer;
  ModAmountValue : single;
  ModMin, ModMax : single;
begin
  if Supports(c, IKnobControl, KnobControl) then
  begin
    ParName  := KnobControl.GetParameterName;
    Par := PluginParFromName(Parname);

    if IsModPar(Par) = false then
    begin
      assert(KnobControl.GetKnobMode = TKnobMode.PositionEdit);
    end else
    begin
      if Plugin.Globals.IsMouseOverModSlot
        then ModIndex := Plugin.Globals.MouseOverModSlot
        else ModIndex := Plugin.Globals.SelectedModSlot;

      if (ModIndex <> -1) then
      begin
        ModAmountValue := Plugin.GetPluginParameterModAmount(ParName, ModIndex);
        KnobControl.SetModAmountValue(ModAmountValue);
        KnobControl.SetKnobMode(TKnobMode.ModEdit);

        if (c is TVamKnob) then
        begin
          (c as TVamKnob).ModLineColor := kModLineColorB;
        end;
      end else
      begin
        KnobControl.SetKnobMode(TKnobMode.PositionEdit);

        if (c is TVamKnob) then
        begin
          (c as TVamKnob).ModLineColor := kModLineColorA;

          Plugin.GetModParModMinMax(ParName, ModMin, ModMax);
          (c as TVamKnob).MinModDepth := ModMin;
          (c as TVamKnob).MaxModDepth := ModMax;
        end;
      end;
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
  KnobControl : IKnobControl;
  ParName  : string;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    ParName  := KnobControl.GetParameterName;
    Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.OnParControlEnter, @ParName, nil);
  end;


  // HACK: The mod slot sends a message when the mouse is not mousing over it.
  // This message is sent with a small delay. The delay avoids flickering when quickly
  // mousing between mod slots. The problem is when quickly mousing over a knob.
  // The knob will show a modulation display as if the moused over mod slot
  // was actually selected. This looks pretty crap and unprofessional so i've
  // added this hack here to ensure the mouse over knob behaviour is correct.
  Plugin.Globals.IsMouseOverModSlot := false;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModSlotChanged);
end;

procedure TKnobHandler.Handle_MouseLeave(Sender: TObject);
var
  KnobControl : IKnobControl;
  ParName  : string;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    ParName  := KnobControl.GetParameterName;
    Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.OnParControlLeave, @ParName, nil);
  end;
end;


procedure TKnobHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  KnobControl : IKnobControl;
  ParName  : string;
  Par : TPluginParameterClass;
  ParID    : TPluginParameterID;
begin
  Plugin.Globals.GuiState.HotkeyContext := THotKeyContext.None;

  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    ParName  := KnobControl.GetParameterName;
    ParID    := PluginParNameToID(ParName);
    Par := Plugin.PluginParameters.FindByName(ParName);
    assert(assigned(Par));

    Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.OnParControlEnter, @ParName, nil);

    if (Button = TMouseButton.mbLeft) then
    begin
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_ShowParChangeInfo, @ParID, nil);
    end;

    if (Button = TMouseButton.mbLeft) and (Par.IsPublishedVstParameter) then
    begin
      Plugin.Globals.GuiState.ActiveVstPluginParameterID := PluginParNameToID(ParName);
      Command.VstPar_BeginEdit(Plugin, Par.VstParameterIndex);
      //LogMain.LogMessage('Begin Edit ' + IntToStr(Par.VstParameterIndex));
    end;


    if (Button = TMouseButton.mbLeft) and (ssCtrl in Shift) then
    begin
      Plugin.ResetPluginParameter(TParChangeScope.psFocused, ParName);
    end;


    if (Button = TMouseButton.mbRight) and (Sender is TVamKnob) then
    begin
      // NOTE: Only show context menus for TVamKnob here as that is the only
      // requirement. Numeric knobs don't use a context menu at the moment.
      ShowControlContextMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y, ParName);
    end;
  end;
end;

procedure TKnobHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  KnobControl : IKnobControl;
  ParName  : string;
  Par : TPluginParameterClass;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    ParName  := KnobControl.GetParameterName;
    Par := Plugin.PluginParameters.FindByName(ParName);
    assert(assigned(Par));

    if (Button = TMouseButton.mbLeft) and (Par.IsPublishedVstParameter) then
    begin
      Command.VstPar_EndEdit(Plugin, Par.VstParameterIndex);
      Plugin.Globals.GuiState.ActiveVstPluginParameterID := -1;
      //LogMain.LogMessage('End Edit ' + IntToStr(Par.VstParameterIndex));
    end;

    if (Button = TMouseButton.mbLeft) then
    begin
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_HideParChangeInfo);
    end;
  end;

  // TODO:MED the last eeGuiStandard had an "Active Controls" list. Active Controls
  // aren't updated in the UpdateControl method.
end;

procedure TKnobHandler.Handle_KnobPosChanged(Sender: TObject);
var
  KnobControl : IKnobControl;
  ParName  : string;
  ParID    : TPluginParameterID;
  ParValue : single;
  Par : TPluginParameterClass;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    ParName  := KnobControl.GetParameterName;
    ParID    := PluginParNameToID(ParName);
    ParValue := KnobControl.GetKnobValue;

    assert(ParValue >= 0);
    assert(ParValue <= 1);

    Par := Plugin.PluginParameters.FindByName(ParName);
    assert(assigned(Par));

    if Par.IsQuantised then
    begin
      ParValue := QuantiseParameterValue(ParValue, Par.QuantisedMin, Par.QuantisedMax);
    end;

    if Par.IsPublishedVstParameter then
    begin
      Command.VstPar_SetParameterAutomated(Plugin, Par.VstParameterIndex, ParValue);
      Plugin.SetPluginParameter(ParID, ParValue, TParChangeScope.psFocused);
    end else
    begin
      Plugin.SetPluginParameter(ParID, ParValue, TParChangeScope.psFocused);
    end;

    Throttle(ThrottleHandle, 25,
    procedure
    begin
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_UpdateParChangeInfo, @ParID, nil);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_UpdateScope);
    end);
  end;
end;

procedure TKnobHandler.Handle_ModAmountChanged(Sender: TObject);
var
  KnobControl : IKnobControl;
  ParName  : string;
  ModIndex       : integer;
  ModAmountValue : single;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    ParName  := KnobControl.GetParameterName;
    ModAmountValue := KnobControl.GetModAmountValue;
    ModIndex := Plugin.Globals.SelectedModSlot;

    if ModIndex <> -1 then
    begin
      Plugin.SetPluginParameterModAmount(TParChangeScope.psFocused, ParName, ModIndex, ModAmountValue);
    end;

    Throttle(ThrottleHandle, 25,
    procedure
    begin
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.ModAmountChanged, nil, nil);
    end);
  end;
end;


procedure TKnobHandler.ShowControlContextMenu(const X, Y: integer; const ParName: string);
begin
  KnobContextMenu.TargetParameterName := ParName;
  KnobContextMenu.Popup(x, y);
end;




{ TKnobContextMenu }

constructor TKnobContextMenu.Create;
begin
  inherited;
end;

destructor TKnobContextMenu.Destroy;
begin

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
  TargetBinding.ParID   := PluginParNameToID(TargetParameterName);
  Plugin.MidiAutomation.ActivateMidiLearn(TargetBinding);
end;

procedure TKnobContextMenu.Handle_MidiUnlearn(Sender: TObject);
begin
  Plugin.MidiAutomation.ClearBindingByName(TargetParametername);
end;

procedure TKnobContextMenu.Handle_SetMidiCC(Sender: TObject);
begin
  Command.SetMidiCCForParameter(Plugin, TargetParameterName);
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
