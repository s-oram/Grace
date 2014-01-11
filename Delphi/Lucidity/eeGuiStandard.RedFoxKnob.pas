unit eeGuiStandard.RedFoxKnob;

interface

uses
  Menus,
  VamLib.Collections.RecordArray,
  VamGuiControlInterfaces,
  eeVstParameter,
  Classes, Controls, eeGlobals;

type
  PControlInfo = ^TControlInfo;
  TControlInfo = record
    Control         : TControl;
    KnobControl     : IKnobControl;
    LinkedParameter : TVstParameter;
  end;

  TRedFoxKnobHandler = class
  private
    fGlobals: TGlobals;
    ControlLinks : TRecordArray<TControlInfo>;
    ControlContextMenu      : TPopupMenu;
    IsManualGuiUpdateActive : boolean;

    function FindIndexOfControl(c:TControl):integer;

    procedure BeginParameterEdit(const ControlLinkIndex : integer);
    procedure EndParameterEdit(const ControlLinkIndex : integer);
    procedure SetParameterToDefaut(const ControlLinkIndex : integer);
    procedure SetParameterValue(const ControlLinkIndex : integer; const Value : single);

    procedure ShowControlContextMenu(const X, Y, ControlLinkIndex : integer);

    procedure Handle_MidiLearn(Sender:TObject);
    procedure Handle_MidiUnlearn(Sender:TObject);
    procedure Handle_SetMidiCC(Sender:TObject);

    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_Changed(Sender: TObject);
  protected
    property Globals : TGlobals read fGlobals;

  public
    constructor Create(aGlobals : TGlobals);
    destructor Destroy; override;

    procedure RegisterControl(c : TControl; aLinkedParameter : TVstParameter);
    procedure DeregisterControl(c : TControl);

    //Update the registered controls to match parameter values.
    procedure UpdateControls;

  end;

implementation

uses
  SysUtils,
  Vcl.Dialogs,
  eeMidiMap,
  TypInfo;

{ TRedFoxKnobHandler }

constructor TRedFoxKnobHandler.Create(aGlobals: TGlobals);
begin
  fGlobals := aGlobals;

  ControlContextMenu := TPopupMenu.Create(nil);
end;

destructor TRedFoxKnobHandler.Destroy;
begin
  ControlContextMenu.Free;
  inherited;
end;

procedure TRedFoxKnobHandler.RegisterControl(c: TControl; aLinkedParameter: TVstParameter);
var
  ci : TControlInfo;
  cType : string;
  m     : TMethod;
  kc : IKnobControl;
begin
  assert(Supports(c, IKnobControl));

  ci.Control     := c;
  if Supports(c, IKnobControl, ci.KnobControl) = false then raise Exception.Create('Control doesn''t support IKnobControlInterface.');
  ci.LinkedParameter := aLinkedParameter;
  ControlLinks.Append(ci);

  ci.KnobControl.SetOnMouseDown(self.Handle_MouseDown);
  ci.KnobControl.SetOnMouseUp(self.Handle_MouseUp);
  ci.KnobControl.SetOnChanged(self.Handle_Changed);
end;

procedure TRedFoxKnobHandler.DeregisterControl(c: TControl);
var
  Index : integer;
begin
  Index := FindIndexOfControl(c);
  assert(Index <> -1);
  ControlLinks.Delete(Index);
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

procedure TRedFoxKnobHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Value : single;
  Block : boolean;
  Index : integer;
begin
  assert(Supports(Sender, IKnobControl));

  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);


  {
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
  }

  // I don't think this is needed.
  //if IsManualGuiUpdateActive then exit;

  Value := ControlLinks[Index].KnobControl.GetKnobValue;

  if (Button = mbLeft) then
  begin
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
  end;
end;

procedure TRedFoxKnobHandler.Handle_Changed(Sender: TObject);
var
  Index : integer;
  Value : single;
begin
  if IsManualGuiUpdateActive then exit;

  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);
  Value := ControlLinks[Index].KnobControl.GetKnobValue;

  SetParameterValue(Index, Value);
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
begin
  // Clear the menu
  ControlContextMenu.Items.Clear;

  // Rebuild the context menu before showing it.
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

procedure TRedFoxKnobHandler.UpdateControls;
var
  c1: Integer;
  parValue : single;
  c : TControl;
begin
  IsManualGuiUpdateActive := true;

  for c1 := 0 to ControlLinks.Count-1 do
  begin
    c := ControlLinks[c1].Control;
    parValue := ControlLinks[c1].LinkedParameter.ValueVST;
    ControlLinks[c1].KnobControl.SetKnobValue(ParValue);
  end;

  IsManualGuiUpdateActive := false;
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



end.
