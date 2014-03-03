unit eeGuiStandard.RedFoxKnob;

interface

uses
  Controls,
  Menus, eePlugin,
  VamLib.Collections.Lists,
  VamLib.Collections.RecordArray,
  VamGuiControlInterfaces,
  eeVstParameter,
  eeVstParameterEx,
  eeGlobals,
  Classes, Types, Contnrs;

type
  PControlInfo = ^TControlInfo;
  TControlInfo = record
    Control         : TControl;
    KnobControl     : IKnobControl;
    LinkedParameter : TVstParameterEx;
  end;

  TControlInfoList = class(TSimpleRecordList<TControlInfo>)
  public
    function FindByControl(c : TControl):PControlInfo;
    function FindIndex(c: TControl): integer;
  end;

  TRedFoxKnobHandler = class
  private
    fGlobals: TGlobals;
    ActiveControls          : TObjectList;
    ControlLinks            : TControlInfoList;
    ControlContextMenu      : TPopupMenu;
    IsManualGuiUpdateActive : boolean;
    fPlugin: TeePlugin;

    function FindIndexOfControl(c:TControl):integer;

    procedure BeginParameterEdit(const ControlLinkIndex : integer);
    procedure EndParameterEdit(const ControlLinkIndex : integer);
    procedure SetParameterToDefaut(const ControlLinkIndex : integer);
    procedure SetParameterValue(const ControlLinkIndex : integer; const Value : single);
    procedure SetModAmount(const ControlLinkIndex : integer; const Value : single);

    procedure ShowControlContextMenu(const X, Y, ControlLinkIndex : integer);

    procedure Handle_MidiLearn(Sender:TObject);
    procedure Handle_MidiUnlearn(Sender:TObject);
    procedure Handle_SetMidiCC(Sender:TObject);

    procedure Handle_ClearCurrentModulation(Sender:TObject);
    procedure Handle_ClearAllModulation(Sender:TObject);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_KnobPosChanged(Sender: TObject);
    procedure Handle_ModAmountChanged(Sender: TObject);
  protected
    property Globals : TGlobals read fGlobals;
    property Plugin  : TeePlugin read fPlugin;

    procedure UpdateControl(Index : integer);
  public
    constructor Create(aPlugin:TeePlugin;  aGlobals : TGlobals);
    destructor Destroy; override;

    procedure RegisterControl(c : TControl; aLinkedParameter : TVstParameter);
    procedure DeregisterControl(c : TControl);

    //Update the registered controls to match parameter values.
    procedure UpdateControls;

  end;

implementation

uses
  uConstants,
  LucidityModConnections,
  uLucidityKeyGroupInterface,
  SysUtils,
  Vcl.Dialogs,
  eeMidiMap,
  TypInfo,
  uGuiUtils;

function ShowClearCurrentModulationCommand(const Plugin : TeePlugin; Par: TVstParameterEx):boolean;
var
  ModParIndex : integer;
  ModSlotIndex : integer;
begin
  if Plugin.Globals.SelectedModSlot = -1 then exit(false);

  if Par.HasModLink = false then exit(false);

  ModParIndex := Par.ModLinkIndex;
  ModSlotIndex := Plugin.Globals.SelectedModSlot;

  if Plugin.ActiveKeyGroup.GetModulatedParameters^[ModParIndex].ModAmount[ModSlotIndex] <> 0
    then result := true
    else result := false;
end;



function ShowClearAllModulationCommand(const Plugin : TeePlugin; Par: TVstParameterEx):boolean;
var
  ModParIndex : integer;
begin
  if Par.HasModLink = false then exit(false);
  ModParIndex := Par.ModLinkIndex;
  result := Plugin.ActiveKeyGroup.GetModulatedParameters^[ModParIndex].IsModulated;
end;

{ TControlInfoList }

function TControlInfoList.FindByControl(c: TControl): PControlInfo;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if Raw[c1].Control = c
      then exit(@Raw[c1]);
  end;

  //If we've made it this far, we've not found anything.
  result := nil;
end;

function TControlInfoList.FindIndex(c: TControl): integer;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if Raw[c1].Control = c
      then exit(c1);
  end;

  //If we've made it this far, we've not found anything.
  result := -1;
end;

{ TRedFoxKnobHandler }

constructor TRedFoxKnobHandler.Create(aPlugin:TeePlugin;  aGlobals: TGlobals);
begin
  fPlugin  := aPlugin;
  fGlobals := aGlobals;

  ControlContextMenu := TPopupMenu.Create(nil);

  ControlLinks := TControlInfoList.Create;

  ActiveControls := TObjectList.Create;
  ActiveControls.OwnsObjects := false;
  //ActiveControls.AllowDuplicates := false;
end;

destructor TRedFoxKnobHandler.Destroy;
begin
  ControlContextMenu.Free;
  ControlLinks.Free;
  ActiveControls.Free;
  inherited;
end;

procedure TRedFoxKnobHandler.RegisterControl(c: TControl; aLinkedParameter: TVstParameter);
var
  ci : PControlInfo;
  ControlLinkIndex : integer;
  ParIndex : integer;
begin
  assert(assigned(c));
  assert(assigned(aLinkedParameter));
  assert(Supports(c, IKnobControl));

  ci := ControlLinks.FindByControl(c);
  if not assigned(ci) then ci := ControlLinks.New;

  ci^.Control     := c;
  if Supports(c, IKnobControl, ci^.KnobControl) = false then raise Exception.Create('Control doesn''t support IKnobControlInterface.');
  ci^.LinkedParameter := aLinkedParameter as TVstParameterEx;
  ParIndex := Globals.VstParameters.FindParameterIndex(aLinkedParameter);
  ci^.KnobControl.SetParameterIndex(ParIndex);

  ci^.KnobControl.SetOnMouseEnter(self.Handle_MouseEnter);
  ci^.KnobControl.SetOnMouseLeave(self.Handle_MouseLeave);
  ci^.KnobControl.SetOnMouseDown(self.Handle_MouseDown);
  ci^.KnobControl.SetOnMouseUp(self.Handle_MouseUp);
  ci^.KnobControl.SetOnKnobPosChanged(self.Handle_KnobPosChanged);
  ci^.KnobControl.SetOnModAmountChanged(self.Handle_ModAmountChanged);

  //== finally ==
  ControlLinkIndex := ControlLinks.FindIndex(c);
  UpdateControl(ControlLinkIndex);
end;

procedure TRedFoxKnobHandler.DeregisterControl(c: TControl);
var
  Index : integer;
begin
  Index := FindIndexOfControl(c);
  if Index <> -1 then
  begin
    ControlLinks.Delete(Index);
  end;
end;

function TRedFoxKnobHandler.FindIndexOfControl(c:TControl): integer;
var
  c1: Integer;
begin
  for c1 := 0 to ControlLinks.Count-1 do
  begin
    if ControlLinks[c1].Control = c
      then exit(c1);
  end;

  result := -1;
end;

procedure TRedFoxKnobHandler.Handle_MouseEnter(Sender: TObject);
var
  Index : integer;
begin
  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  Plugin.Globals.MotherShip.SendMessage(TLucidMsgID.OnControlEnter, @Sender);
end;

procedure TRedFoxKnobHandler.Handle_MouseLeave(Sender: TObject);
var
  Index : integer;
begin
  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  Plugin.Globals.MotherShip.SendMessage(TLucidMsgID.OnControlLeave, @Sender);
end;



procedure TRedFoxKnobHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Value : single;
  Block : boolean;
  Index : integer;
begin
  assert(Supports(Sender, IKnobControl));

  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  // I don't think this is needed.
  //if IsManualGuiUpdateActive then exit;

  Value := ControlLinks[Index].KnobControl.GetKnobValue;

  if (Button = mbLeft) then
  begin
    ActiveControls.Add(ControlLinks[Index].Control);

    BeginParameterEdit(Index);

    if (ssCtrl in Shift)
      then SetParameterToDefaut(Index)
      else SetParameterValue(Index, Value);
  end else
  if (Button = mbRight) then
  begin
    ShowControlContextMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y, Index);
  end;

end;


procedure TRedFoxKnobHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
  Value : single;
begin
  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  Value := ControlLinks[Index].KnobControl.GetKnobValue;

  if (Button = mbLeft) then
  begin
    if not (ssCtrl in Shift)
      then SetParameterValue(Index, Value);

    EndParameterEdit(Index);

    ActiveControls.Remove(ControlLinks[Index].Control);
  end;
end;

procedure TRedFoxKnobHandler.Handle_KnobPosChanged(Sender: TObject);
var
  Index : integer;
  Value : single;
begin
  if IsManualGuiUpdateActive then exit;

  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);
  Value := ControlLinks[Index].KnobControl.GetKnobValue;

  SetParameterValue(Index, Value);

  Plugin.Globals.MotherShip.SendMessage(TLucidMsgID.ModAmountChanged);
end;

procedure TRedFoxKnobHandler.Handle_ModAmountChanged(Sender: TObject);
var
  Index : integer;
  Value : single;
  msg : string;
begin
  if IsManualGuiUpdateActive then exit;

  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);
  Value := ControlLinks[Index].KnobControl.GetModAmountValue;

  SetModAmount(Index, Value);

  msg := 'Mod Amount: ' + IntToStr(Round(Value * 100)) + '%';

  Plugin.Globals.MotherShip.SendMessage(TLucidMsgID.ModAmountChanged);
end;



procedure TRedFoxKnobHandler.BeginParameterEdit(const ControlLinkIndex: integer);
var
  Tag : integer;
begin
  if ControlLinks[ControlLinkIndex].LinkedParameter.IsPublished then
  begin
    Tag := ControlLinks[ControlLinkIndex].LinkedParameter.PublishedVSTParameterIndex;
    Globals.VstMethods^.BeginParameterEdit(Tag);
  end;
end;

procedure TRedFoxKnobHandler.EndParameterEdit(const ControlLinkIndex: integer);
var
  Tag : integer;
begin
  if ControlLinks[ControlLinkIndex].LinkedParameter.IsPublished then
  begin
    Tag := ControlLinks[ControlLinkIndex].LinkedParameter.PublishedVSTParameterIndex;
    Globals.VstMethods^.EndParameterEdit(Tag);
  end;
end;

procedure TRedFoxKnobHandler.SetModAmount(const ControlLinkIndex: integer; const Value: single);
var
  ModSlot : integer;
  ModLinkIndex : integer;
  kg : IKeyGroup;
  ModConnections : TModConnections;
begin
  ModSlot := Globals.SelectedModSlot;

  if (ControlLinks[ControlLinkIndex].LinkedParameter.HasModLink) and (ModSlot >= 0) then
  begin
    ModLinkIndex := ControlLinks[ControlLinkIndex].LinkedParameter.ModLinkIndex;

    kg := Plugin.ActiveKeyGroup;

    kg.SetModParModAmount(ModLinkIndex, ModSlot, Value);
  end;

end;

procedure TRedFoxKnobHandler.SetParameterToDefaut(const ControlLinkIndex: integer);
var
  Tag : integer;
  dv : single;
begin
  dv := ControlLinks[ControlLinkIndex].LinkedParameter.DefaultVST;

  if ControlLinks[ControlLinkIndex].LinkedParameter.IsPublished then
  begin
    Tag := ControlLinks[ControlLinkIndex].LinkedParameter.PublishedVSTParameterIndex;
    Globals.VstMethods^.SetParameterAutomated(Tag, dv);
  end else
  begin
    ControlLinks[ControlLinkIndex].LinkedParameter.ValueVST := dv;
  end;
end;


procedure TRedFoxKnobHandler.SetParameterValue(const ControlLinkIndex: integer; const Value: single);
var
  Tag : integer;
begin
  if ControlLinks[ControlLinkIndex].LinkedParameter.IsPublished then
  begin
    Tag := ControlLinks[ControlLinkIndex].LinkedParameter.PublishedVSTParameterIndex;
    Globals.VstMethods^.SetParameterAutomated(Tag, Value);
  end else
  begin
    ControlLinks[ControlLinkIndex].LinkedParameter.ValueVST := Value;
  end;
end;

procedure TRedFoxKnobHandler.ShowControlContextMenu(const X, Y, ControlLinkIndex: integer);
var
  mi : TMenuItem;
  MidiCC : integer;
  Text   : string;
  miMidiLearn : TMenuItem;

  ParIndex : integer;
  LinkIndex : integer absolute ControlLinkIndex;

  Par : TVstParameterEx;
begin
  Par := ControlLinks[ControlLinkIndex].LinkedParameter;

  // Clear the menu
  ControlContextMenu.Items.Clear;




  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Clear Current Modulation';
  mi.OnClick := Handle_ClearCurrentModulation;
  mi.Tag     := ControlLinkIndex;
  if ShowClearCurrentModulationCommand(Plugin, Par)
    then mi.Enabled := true
    else mi.Enabled := false;
  ControlContextMenu.Items.Add(mi);
  miMidiLearn := mi;

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Clear All Modulation';
  mi.OnClick := Handle_ClearAllModulation;
  mi.Tag     := ControlLinkIndex;
  if ShowClearAllModulationCommand(Plugin, Par)
    then mi.Enabled := true
    else mi.Enabled := false;
  ControlContextMenu.Items.Add(mi);
  miMidiLearn := mi;


  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := '-';
  ControlContextMenu.Items.Add(mi);




  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'MIDI Learn';
  mi.OnClick := Handle_MidiLearn;
  mi.Tag     := ControlLinkIndex;
  ControlContextMenu.Items.Add(mi);
  miMidiLearn := mi;

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'MIDI Unlearn';
  mi.OnClick := Handle_MidiUnlearn;
  mi.Tag     := ControlLinkIndex;
  ControlContextMenu.Items.Add(mi);

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Set MIDI CC...';
  mi.OnClick := Handle_SetMidiCC;
  mi.Tag     := ControlLinkIndex;
  ControlContextMenu.Items.Add(mi);




  //=== Update MIDI Learn menu item with current control midi binding. =====
  ParIndex := Globals.VstParameters.FindParameterIndex(ControlLinks[LinkIndex].LinkedParameter);
  MidiCC := Globals.VstMethods^.GetCurrentMidiBiding(ParIndex, ttVstParameter);

  if MidiCC <> -1
    then Text := 'MIDI Learn  [CC: ' + IntToStr(MidiCC) + ']'
    else Text := 'MIDI Learn  [CC: --]';

  miMidiLearn.Caption := Text;

  //Show the controls context menu.
  ControlContextMenu.Popup(X, Y);
end;

procedure TRedFoxKnobHandler.UpdateControl(Index : integer);
var
  parValue : single;
  c : TControl;
begin
  c := ControlLinks[Index].Control;

  if ActiveControls.IndexOf(c) = -1 then
  begin
    parValue := ControlLinks[Index].LinkedParameter.ValueVST;
    ControlLinks[Index].KnobControl.SetKnobValue(ParValue);
  end;
end;

procedure TRedFoxKnobHandler.UpdateControls;
var
  c1: Integer;
begin
  IsManualGuiUpdateActive := true;
  try
    for c1 := 0 to ControlLinks.Count-1
      do UpdateControl(c1);
  finally
    IsManualGuiUpdateActive := false;
  end;
end;

procedure TRedFoxKnobHandler.Handle_MidiLearn(Sender: TObject);
var
  LinkIndex : integer;
  ParIndex : integer;
begin
  assert(Sender is TMenuItem);
  LinkIndex := (Sender as TMenuItem).Tag;

  ParIndex := Globals.VstParameters.FindParameterIndex(ControlLinks[LinkIndex].LinkedParameter);

  Globals.VstMethods^.EnableMidiLearn(ParIndex, ttVstParameter);
end;

procedure TRedFoxKnobHandler.Handle_MidiUnlearn(Sender: TObject);
var
  LinkIndex : integer;
  ParIndex : integer;
begin
  assert(Sender is TMenuItem);
  LinkIndex := (Sender as TMenuItem).Tag;

  ParIndex := Globals.VstParameters.FindParameterIndex(ControlLinks[LinkIndex].LinkedParameter);

  Globals.VstMethods^.RemoveMidiBinding(ParIndex, ttVstParameter);
end;

procedure TRedFoxKnobHandler.Handle_SetMidiCC(Sender: TObject);
var
  LinkIndex : integer;
  ParIndex : integer;
  Value : string;
  MidiCC : integer;
  Error : boolean;
  ErrorMessage : string;
begin
  assert(Sender is TMenuItem);
  LinkIndex := (Sender as TMenuItem).Tag;

  ParIndex := Globals.VstParameters.FindParameterIndex(ControlLinks[LinkIndex].LinkedParameter);

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
      Globals.VstMethods^.SetMidiBinding(ParIndex, ttVstParameter, MidiCC);
    end;
  end;

  if (Error = true) then
  begin
    ShowMessage('Error: ' + ErrorMessage);
  end;
end;

procedure TRedFoxKnobHandler.Handle_ClearAllModulation(Sender: TObject);
var
  c1 : integer;
  ControlLinkIndex : integer;
  Par : TVstParameterEx;
  ModParIndex : integer;
  ModSlotIndex : integer;
begin
  ControlLinkIndex := (Sender as TMenuItem).Tag;

  Par := ControlLinks[ControlLinkIndex].LinkedParameter;

  if Par.HasModLink = false then exit;

  ModParIndex  := Par.ModLinkIndex;

  for c1 := 0 to kModSlotCount-1 do
  begin
    Plugin.ActiveKeyGroup.SetModParModAmount(ModParIndex, c1, 0);
  end;

  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
end;

procedure TRedFoxKnobHandler.Handle_ClearCurrentModulation(Sender: TObject);
var
  c1 : integer;
  ControlLinkIndex : integer;
  Par : TVstParameterEx;
  ModParIndex : integer;
  ModSlotIndex : integer;
begin
  ControlLinkIndex := (Sender as TMenuItem).Tag;

  Par := ControlLinks[ControlLinkIndex].LinkedParameter;

  if Par.HasModLink = false then exit;

  ModParIndex  := Par.ModLinkIndex;
  ModSlotIndex := Plugin.Globals.SelectedModSlot;

  Plugin.ActiveKeyGroup.SetModParModAmount(ModParIndex, ModSlotIndex, 0);

  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
end;







end.
