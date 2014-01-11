{
  TGuiStandard

  TGuiStandard provides ways to programmatically attach default handling to
  VST GUI controls. At the moment it is being used to attach handling to
  GUI's made at design time. In future it might be possible to attach handling
  to controls made at run-time.

  So far TGuiStandard is proving useful because:
  - it saves time. Using GUI standard only requires are few lines to
    attach default handling to controls in a GUI's setup method. Previously
    all controls had their event handling manually programmed.
  - Control's handling becomes more consistent because all controls share the
    same handling routines. (More noticable between different plugins).

  TGuiStandard is a work in progress and I think there is still room for
  improvement.


  - A possible future improvement. Currently GUI standard checks to
    a components 'TAG' property (generally) to find the VST Parameter Index
    which is used to update the relative VST parameter.

    The downside of this approach is that the number of VST parameters can't
    be changed once the VST GUI is created as the number of parameters and their
    associated TAG/INDEX values will possibily change.

    Instead each control could have a VST Parameter Name property that would
    be used. When needing to update a parameter TGuiStandard would use the
    Name property to find the correct VST parameter index.



}

unit eeGuiStandard;

interface


{$TYPEINFO ON}
{$SCOPEDENUMS ON}

uses
  eeGlobals, eeEnumHelper,
  Menus, Controls, Classes,
  TypInfo, Contnrs,
  eeTypes, eeMidiAutomation, eeMidiMap,
  eeGuiStandard_Types, eeGuiStandard_MenuController,
  eeGuiStandard_MenuBuilder,
  eeGuiStandard.RedFoxKnob;


type
  TMenuCallback = eeGuiStandard_MenuBuilder.TMenuCallback;
  TShowMenu     = eeGuiStandard_MenuController.TShowMenu;

  TControlMouseDownEvent = procedure(Sender: TObject; const Target:TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Block : boolean) of object;
  TControlMouseUpEvent   = procedure(Sender: TObject; const Target:TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TGuiStandard = class
  private
    fIsManualGuiUpdateActive : boolean;
    fGlobals: TGlobals;
    fOnControlMouseDown: TControlMouseDownEvent;
    fOnControlMouseUp: TControlMouseUpEvent;
    fRedFoxKnobHandler : TRedFoxKnobHandler;
  protected
    MenuController : TMenuController;

    AutoUpdateControlList : TObjectList;
    GrabbedControlsList   : TObjectList;
    ControlContextMenu    : TPopupMenu;



    function  BeginParameterEdit(Index:longint):boolean;
    function  EndParameterEdit(Index:longint):boolean;
    procedure SetParameterAutomated(Index:longint; Value:single); inline;
    procedure SetParameterToDefault(Index:longint);

    procedure Handle_MidiLearn(Sender:TObject);
    procedure Handle_MidiUnlearn(Sender:TObject);
    procedure Handle_SetMidiCC(Sender:TObject);

    //Set to true when manually changing the value of GUI controls. This prevents the controls sending parameter changes to VST plugin class.
    property IsManualGuiUpdateActive : boolean                read fIsManualGuiUpdateActive write fIsManualGuiUpdateActive;
  public
    constructor Create(aGlobals : TGlobals);
    destructor Destroy; override;

    property RedFoxKnobHandler : TRedFoxKnobHandler read fRedFoxKnobHandler;

    // procedure RegisterControlAsMenuControl()
    // WARNING: This method modifies aControl. It will assign values to:
    // - aControl.Tag
    // - aControl.OnMouseDown
    // - aControl.OnMouseUp
    procedure RegisterControlAsMenuControl(const aControl : TControl; const ParIndex:integer; const EnumHelper:TCustomEnumHelperClass; ShowMenu   : TShowMenu; PopupCallBack : TMenuCallback = nil);

    procedure RegisterControlForAutoUpdate(aControl:TObject; AddEventHandling:boolean = false);

    //Update control values registered with the RegisterControlForAutoUpdate() method.
    procedure UpdateControls;

    //Links a control to event handlers...
    procedure AssignEventHandlers(c:TObject);


    //Shows a control's context menu with options for MIDI Learn and Midi Unlearn.
    procedure ShowControlContextMenu(const X, Y, ParameterIndex : integer);
    procedure ShowXYPadContextMenu(const X, Y, ParameterIndex1, ParameterIndex2 : integer);

    //The globals property must be set so that this class can get/set parameter values and MIDI parameter mappings.
    property Globals : TGlobals read fGlobals;

  published
    //== RedFox / Vam Control event handlers ===================
    // for knobs...
    procedure RedFoxKnobMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RedFoxKnobMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RedFoxKnobChanged(Sender: TObject);

    // for xy pads..
    procedure RedFoxXYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RedFoxXYPadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RedFoxXYPadChanged(Sender: TObject);

    property OnControlMouseDown : TControlMouseDownEvent read fOnControlMouseDown write fOnControlMouseDown;
    property OnControlMouseUp   : TControlMouseUpEvent   read fOnControlMouseUp   write fOnControlMouseUp;
  end;


  


implementation

uses
  SysUtils, eeFunctions, VCL.Dialogs;

type
  EGuiStandardException = Exception;

{ TGuiStandard }

constructor TGuiStandard.Create(aGlobals : TGlobals);
begin
  fGlobals := aGlobals;

  MenuController := TMenuController.Create;
  MenuController.Globals := Globals;

  ControlContextMenu := TPopupMenu.Create(nil);

  AutoUpdateControlList := TObjectList.Create;
  AutoUpdateControlList.OwnsObjects := false;

  GrabbedControlsList   := TObjectList.Create;
  GrabbedControlsList.OwnsObjects := false;

  fRedFoxKnobHandler := TRedFoxKnobHandler.Create(aGlobals);
end;

destructor TGuiStandard.Destroy;
begin
  fRedFoxKnobHandler.Free;
  ControlContextMenu.Free;
  AutoUpdateControlList.Free;
  GrabbedControlsList.Free;
  MenuController.Free;
  inherited;
end;


procedure TGuiStandard.ShowControlContextMenu(const X, Y, ParameterIndex: integer);
var
  mi : TMenuItem;
  MidiCC : integer;
  Text   : string;
  miMidiLearn : TMenuItem;
begin
  // Clear the menu
  ControlContextMenu.Items.Clear;

  // Rebuild the context menu before showing it.
  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'MIDI Learn';
  mi.OnClick := Handle_MidiLearn;
  mi.Tag     := ParameterIndex;
  ControlContextMenu.Items.Add(mi);
  miMidiLearn := mi;

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'MIDI Unlearn';
  mi.OnClick := Handle_MidiUnlearn;
  mi.Tag     := ParameterIndex;
  ControlContextMenu.Items.Add(mi);

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Set MIDI CC...';
  mi.OnClick := Handle_SetMidiCC;
  mi.Tag     := ParameterIndex;
  ControlContextMenu.Items.Add(mi);

  //=== Update MIDI Learn menu item with current control midi binding. =====
  MidiCC := Globals.VstMethods^.GetCurrentMidiBiding(ParameterIndex, ttVstParameter);

  if MidiCC <> -1
    then Text := 'MIDI Learn  [CC: ' + IntToStr(MidiCC) + ']'
    else Text := 'MIDI Learn  [CC: --]';

  miMidiLearn.Caption := Text;

  //Show the controls context menu.
  ControlContextMenu.Popup(X, Y);
end;


procedure TGuiStandard.ShowXYPadContextMenu(const X, Y, ParameterIndex1, ParameterIndex2: integer);
var
  mi : TMenuItem;
  MidiCC : integer;
  Text   : string;
  miMidiLearnX : TMenuItem;
  miMidiLearnY : TMenuItem;
begin
  // Clear the menu
  ControlContextMenu.Items.Clear;

  // Rebuild the context menu before showing it.
  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'X-Axis MIDI Learn';
  mi.OnClick := Handle_MidiLearn;
  mi.Tag     := ParameterIndex1;
  ControlContextMenu.Items.Add(mi);
  miMidiLearnX := mi;

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Y-Axis MIDI Learn';
  mi.OnClick := Handle_MidiLearn;
  mi.Tag     := ParameterIndex2;
  ControlContextMenu.Items.Add(mi);
  miMidiLearnY := mi;

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := '-'; //Drawn as spacer.
  ControlContextMenu.Items.Add(mi);


  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'X-Axis MIDI Unlearn';
  mi.OnClick := Handle_MidiUnlearn;
  mi.Tag     := ParameterIndex1;
  ControlContextMenu.Items.Add(mi);

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Y-Axis MIDI Unlearn';
  mi.OnClick := Handle_MidiUnlearn;
  mi.Tag     := ParameterIndex2;
  ControlContextMenu.Items.Add(mi);

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := '-'; //Drawn as spacer.
  ControlContextMenu.Items.Add(mi);


  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'X-Axis Set MIDI CC...';
  mi.OnClick := Handle_SetMidiCC;
  mi.Tag     := ParameterIndex1;
  ControlContextMenu.Items.Add(mi);

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Y-Axis Set MIDI CC...';
  mi.OnClick := Handle_SetMidiCC;
  mi.Tag     := ParameterIndex2;
  ControlContextMenu.Items.Add(mi);

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := '-'; //Drawn as spacer.
  ControlContextMenu.Items.Add(mi);


  //=== Update MIDI Learn menu item with current control midi binding. =====
  MidiCC := Globals.VstMethods^.GetCurrentMidiBiding(ParameterIndex1, ttVstParameter);
  if MidiCC <> -1
    then Text := 'X-Axis MIDI Learn  [CC: ' + IntToStr(MidiCC) + ']'
    else Text := 'X-Axis MIDI Learn  [CC: --]';
  miMidiLearnX.Caption := Text;


  MidiCC := Globals.VstMethods^.GetCurrentMidiBiding(ParameterIndex2, ttVstParameter);
  if MidiCC <> -1
    then Text := 'Y-Axis MIDI Learn  [CC: ' + IntToStr(MidiCC) + ']'
    else Text := 'Y-Axis MIDI Learn  [CC: --]';
  miMidiLearnY.Caption := Text;



  //Show the controls context menu.
  ControlContextMenu.Popup(X, Y);
end;

//========= RedFox / Vam Control Event Handlers ==============================

procedure TGuiStandard.RedFoxKnobChanged(Sender: TObject);
var
  Index : integer;
  Value : single;
begin
  assert(Sender.ClassName = 'TVamKnob');

  if IsManualGuiUpdateActive then exit;

  Index := GetPropValue(Sender, 'Tag');
  Value := GetPropValue(Sender, 'Pos');

  SetParameterAutomated(Index, Value);
end;

procedure TGuiStandard.RedFoxKnobMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;
  Value : single;
  Block : boolean;
begin
  assert(Sender.ClassName = 'TVamKnob');
  assert(Sender is TControl);

  if assigned(OnControlMouseDown) then
  begin
    Block := false;
    OnControlMouseDown(self, (Sender as TControl), Button, Shift, X, Y, Block);
    if Block = true then
    begin
      assert(false, 'TODO');
      exit; //===========================================>> exit >>=====>>
    end;
  end;

  if IsManualGuiUpdateActive then exit;

  Tag   := GetPropValue(Sender, 'Tag');
  Value := GetPropValue(Sender, 'Pos');

  if (Button = mbLeft) and (not(ssCtrl in Shift)) then
  begin
    BeginParameterEdit(Tag);
    SetParameterAutomated(Tag, Value);
  end;

  if (Button = mbLeft) and ((ssCtrl in Shift)) then
  begin
    BeginParameterEdit(Tag);
    SetParameterToDefault(Tag);
  end;

  if Button = mbRight then
  begin
    ShowControlContextMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y, Tag);
  end;
end;

procedure TGuiStandard.RedFoxKnobMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;
begin
  assert(Sender.ClassName = 'TVamKnob');
  assert(Sender is TControl);

  if assigned(OnControlMouseUp) then
  begin
    OnControlMouseUp(self, (Sender as TControl), Button, Shift, X, Y);
  end;

  Tag := GetPropValue(Sender, 'Tag');
  if Button = mbLeft then EndParameterEdit(Tag);
end;

procedure TGuiStandard.RedFoxXYPadChanged(Sender: TObject);
var
  Index1 : integer;
  Index2 : integer;
  Value1 : single;
  Value2 : single;
begin
  assert(Sender.ClassName = 'TVamXYPad');

  if IsManualGuiUpdateActive then exit;

  Index1 := GetPropValue(Sender, 'PadX_VstParameterIndex');
  Value1 := GetPropValue(Sender, 'PosX');

  Index2 := GetPropValue(Sender, 'PadY_VstParameterIndex');
  Value2 := GetPropValue(Sender, 'PosY');

  SetParameterAutomated(Index1, Value1);
  SetParameterAutomated(Index2, Value2);
end;

procedure TGuiStandard.RedFoxXYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index1 : integer;
  Index2 : integer;
  Value1 : single;
  Value2 : single;
begin
  assert(Sender.ClassName = 'TVamXYPad');

  if IsManualGuiUpdateActive then exit;

  Index1 := GetPropValue(Sender, 'PadX_VstParameterIndex');
  Value1 := GetPropValue(Sender, 'PosX');

  Index2 := GetPropValue(Sender, 'PadY_VstParameterIndex');
  Value2 := GetPropValue(Sender, 'PosY');

  if (Button = mbLeft) and (not(ssCtrl in Shift)) then
  begin
    BeginParameterEdit(Index1);
    BeginParameterEdit(Index2);

    SetParameterAutomated(Index1, Value1);
    SetParameterAutomated(Index2, Value2);
  end;

  if (Button = mbLeft) and ((ssCtrl in Shift)) then
  begin
    BeginParameterEdit(Index1);
    BeginParameterEdit(Index2);

    SetParameterToDefault(Index1);
    SetParameterToDefault(Index2);
  end;

  if Button = mbRight then
  begin
    ShowXYPadContextMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y, Index1, Index2);
  end;

end;

procedure TGuiStandard.RedFoxXYPadMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index1 : integer;
  Index2 : integer;
  Value1 : single;
  Value2 : single;
begin
  assert(Sender.ClassName = 'TVamXYPad');

  if IsManualGuiUpdateActive then exit;

  Index1 := GetPropValue(Sender, 'PadX_VstParameterIndex');
  Value1 := GetPropValue(Sender, 'PosX');

  Index2 := GetPropValue(Sender, 'PadY_VstParameterIndex');
  Value2 := GetPropValue(Sender, 'PosY');

  if Button = mbLeft then
  begin
    SetParameterAutomated(Index1, Value1);
    SetParameterAutomated(Index2, Value2);

    EndParameterEdit(Index1);
    EndParameterEdit(Index2);
  end;
end;


procedure TGuiStandard.SetParameterAutomated(Index: Integer; Value: single);
var
  PublishedParameterIndex : integer;
begin
  if Globals.VstParameters.Parameter[Index].IsPublished then
  begin
    PublishedParameterIndex := Globals.VstParameters.Parameter[Index].PublishedVSTParameterIndex;
    Globals.VstMethods^.SetParameterAutoMated(PublishedParameterIndex, Value);
  end else
  begin
    Globals.VstParameters.Parameter[Index].ValueVST := Value;
  end;
end;

function TGuiStandard.BeginParameterEdit(Index: Integer): boolean;
var
  PublishedParameterIndex : integer;
begin
  if Globals.VstParameters.Parameter[Index].IsPublished then
  begin
    PublishedParameterIndex := Globals.VstParameters.Parameter[Index].PublishedVSTParameterIndex;
    result := Globals.VstMethods^.BeginParameterEdit(PublishedParameterIndex);
  end else
  begin
    result := false;
  end;
end;

function TGuiStandard.EndParameterEdit(Index: Integer): boolean;
var
  PublishedParameterIndex : integer;
begin
  if Globals.VstParameters.Parameter[Index].IsPublished then
  begin
    PublishedParameterIndex := Globals.VstParameters.Parameter[Index].PublishedVSTParameterIndex;
    result := Globals.VstMethods^.EndParameterEdit(PublishedParameterIndex);
  end else
  begin
    result := false;
  end;
end;

procedure TGuiStandard.SetParameterToDefault(Index: Integer);
var
  PublishedParameterIndex : integer;
  DefaultValue : single;
begin
  if Globals.VstParameters.Parameter[Index].IsPublished then
  begin
    PublishedParameterIndex := Globals.VstParameters.Parameter[Index].PublishedVSTParameterIndex;
    DefaultValue            := Globals.VstParameters.Parameter[Index].DefaultVST;
    Globals.VstMethods^.SetParameterAutoMated(PublishedParameterIndex, DefaultValue);
  end else
  begin
    Globals.VstParameters.Parameter[Index].ResetToDefault;
  end;
end;



procedure TGuiStandard.Handle_MidiLearn(Sender: TObject);
var
  ParameterIndex : integer;
begin
  assert(Sender is TMenuItem);
  ParameterIndex := (Sender as TMenuItem).Tag;

  Globals.VstMethods^.EnableMidiLearn(ParameterIndex, ttVstParameter);
end;

procedure TGuiStandard.Handle_MidiUnlearn(Sender: TObject);
var
  ParameterIndex : integer;
begin
  assert(Sender is TMenuItem);
  ParameterIndex := (Sender as TMenuItem).Tag;
  Globals.VstMethods^.RemoveMidiBinding(ParameterIndex, ttVstParameter);
end;

procedure TGuiStandard.Handle_SetMidiCC(Sender: TObject);
var
  Value : string;
  MidiCC : integer;
  Error : boolean;
  ErrorMessage : string;
  ParameterIndex : integer;
begin
  //assert(assigned(fOnSetMidiBinding));
  assert(Sender is TMenuItem);
  ParameterIndex := (Sender as TMenuItem).Tag;

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
      Globals.VstMethods^.SetMidiBinding(ParameterIndex, ttVstParameter, MidiCC);
    end;
  end;

  if (Error = true) then
  begin
    ShowMessage('Error: ' + ErrorMessage);
  end;
end;

procedure TGuiStandard.RegisterControlForAutoUpdate(aControl: TObject; AddEventHandling:boolean = false);
begin
  AutoUpdateControlList.Add(aControl);
  if AddEventHandling then self.AssignEventHandlers(aControl);
end;

procedure TGuiStandard.UpdateControls;
var
  c1: Integer;
  c : TObject;
  cType : string;
  cTag  : integer;
  parValue  : single;
  PropValue : single;
  s : string;
  ControlName : string;
begin
  RedFoxKnobHandler.UpdateControls;

  IsManualGuiUpdateActive := true;

  for c1 := 0 to AutoUpdateControlList.Count - 1 do
  begin
    c := AutoUpdateControlList[c1];
    ControlName := (c as TControl).Name;

    //Only update if the control hasn't been grabbed by the user.
    if GrabbedControlsList.IndexOf(c) = -1 then
    begin
      cType   := c.ClassName;

      //==== VGScene ====
      if cType = 'TvgVamBitmapButton' then
      begin
        assert(false, 'TODO');
        cTag    := GetPropValue(c, 'Tag');
        parValue := Globals.VstMethods^.GetParameter(cTag);
        SetPropValue(c, 'IsOn', FloatToBoolean(parValue));
      end;

      if cType = 'TvgVamBitmapKnob' then
      begin
        assert(false, 'TODO');
        cTag    := GetPropValue(c, 'Tag');
        parValue := Globals.VstMethods^.GetParameter(cTag);
        SetPropValue(c, 'Pos', parValue);
      end;

      //==== Vst Controls ====
      if cType = 'TVstKnob' then
      begin
        assert(false, 'TODO');
        cTag    := GetPropValue(c, 'Tag');
        parValue := Globals.VstMethods^.GetParameter(cTag);
        SetPropValue(c, 'Pos', parValue);
      end;

      if cType = 'TVstLabel' then
      begin
        assert(false, 'TODO');
        cTag    := GetPropValue(c, 'Tag');
        s := Globals.VstMethods^.GetParameterDisplay(cTag) + Globals.VstMethods^.GetParameterLabel(cTag);
        SetPropValue(c, 'Caption', s);
      end;

      //===== RedFox / Vam Controls ========
      if cType = 'TVamKnob' then
      begin
        cTag    := GetPropValue(c, 'Tag');
        if (cTag = -1) then raise EGuiStandardException.Create('Tag for control (' + ControlName + ') is -1.');
        parValue := Globals.VstParameters[cTag].ValueVST;
        SetPropValue(c, 'Pos', parValue);
      end;

      if cType = 'TVamXYPad' then
      begin
        cTag    := GetPropValue(c, 'PadX_VstParameterIndex');
        if (cTag = -1) then raise EGuiStandardException.Create('Tag for control (' + ControlName + ') is -1.');
        parValue := Globals.VstParameters[cTag].ValueVST;
        PropValue := GetPropValue(c, 'PosX');
        if ParValue <> PropValue then
        begin
          SetPropValue(c, 'PosX', parValue);
        end;


        cTag    := GetPropValue(c, 'PadY_VstParameterIndex');
        if (cTag = -1) then raise EGuiStandardException.Create('Tag for control (' + ControlName + ') is -1.');
        parValue := Globals.VstParameters[cTag].ValueVST;
        PropValue := GetPropValue(c, 'PosY');
        if ParValue <> PropValue then
        begin
          SetPropValue(c, 'PosY', parValue);
        end;
      end;

    end;
  end;


  IsManualGuiUpdateActive := false;
end;

procedure TGuiStandard.AssignEventHandlers(c : TObject);
// NOTE: This code doesn't seem to work in  Delphi 2007 because the methods can not be found
// using the RTTI. AFAIK it should be possible. Perhaps leave it unitl after upgrading to Delphi XE.
var
  cType : string;
  m     : TMethod;
begin
  cType := c.ClassName;


  //=================================================================
  //================== RedFox / Vam Controls ========================
  //=================================================================
  if cType = 'TVamKnob' then
  begin
    m.Data := Pointer(self);

    // Knob Mouse-Down
    m.Code := Self.MethodAddress('RedFoxKnobMouseDown');
    SetMethodProp(c, 'OnMouseDown', m);

    // Knob Mouse-Up
    m.Code := Self.MethodAddress('RedFoxKnobMouseUp');
    SetMethodProp(c, 'OnMouseUp', m);

    // Knob Change
    m.Code := Self.MethodAddress('RedFoxKnobChanged');
    SetMethodProp(c, 'OnChanged', m);
  end;


  if cType = 'TVamXYPad' then
  begin
    m.Data := Pointer(self);

    // XY Pad Mouse-Down
    m.Code := Self.MethodAddress('RedFoxXYPadMouseDown');
    SetMethodProp(c, 'OnMouseDown', m);

    // XY Pad Mouse-Up
    m.Code := Self.MethodAddress('RedFoxXYPadMouseUp');
    SetMethodProp(c, 'OnMouseUp', m);

    // XY Pad Changed
    m.Code := Self.MethodAddress('RedFoxXYPadChanged');
    SetMethodProp(c, 'OnChanged', m);
  end;

end;

procedure TGuiStandard.RegisterControlAsMenuControl(const aControl: TControl; const ParIndex: integer; const EnumHelper: TCustomEnumHelperClass; ShowMenu   : TShowMenu; PopupCallBack : TMenuCallback);
begin
  if ParIndex = -1 then raise EGuiStandardException.Create('aControl (' + aControl.Name + ') ParIndex is -1');

  MenuController.RegisterControlAsMenuControl(aControl, ParIndex, EnumHelper, ShowMenu, PopupCallBack);
end;



end.
