unit eeGuiStandard.RedFoxMenu;

interface

uses
  Menus,
  VamLib.Collections.RecordArray,
  VamGuiControlInterfaces,
  eeVstParameter,
  Classes, Controls, eeGlobals,
  eeEnumHelper,
  eeGuiStandard_MenuBuilder;

type
  PControlInfo = ^TControlInfo;
  TControlInfo = record
    Control         : TControl;
    MenuControl     : IMenuControl;
    LinkedParameter : TVstParameter;
    EnumHelper      : TCustomEnumHelperClass;
    PopupCallBack   : TMenuCallback;
  end;

  TRedFoxMenuHandler = class
  private
    fGlobals: TGlobals;
    ControlLinks            : TRecordArray<TControlInfo>;
    ControlContextMenu      : TPopupMenu;
    IsManualGuiUpdateActive : boolean;

    function FindIndexOfControl(c:TControl):integer;

    procedure BeginParameterEdit(const ControlLinkIndex : integer);
    procedure EndParameterEdit(const ControlLinkIndex : integer);
    procedure SetParameterToDefaut(const ControlLinkIndex : integer);
    procedure SetParameterValue(const ControlLinkIndex : integer; const Value : single);

    procedure ShowControlContextMenu(const X, Y, ControlLinkIndex : integer);

    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    property Globals : TGlobals read fGlobals;

  public
    constructor Create(aGlobals : TGlobals);
    destructor Destroy; override;

    procedure RegisterControl(c : TControl; aLinkedParameter : TVstParameter; const EnumHelper:TCustomEnumHelperClass; PopupCallBack : TMenuCallback = nil);
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

constructor TRedFoxMenuHandler.Create(aGlobals: TGlobals);
begin
  fGlobals := aGlobals;

  ControlContextMenu := TPopupMenu.Create(nil);
end;

destructor TRedFoxMenuHandler.Destroy;
begin
  ControlContextMenu.Free;
  inherited;
end;

procedure TRedFoxMenuHandler.RegisterControl(c: TControl; aLinkedParameter: TVstParameter; const EnumHelper:TCustomEnumHelperClass; PopupCallBack : TMenuCallback = nil);
var
  ci : TControlInfo;
  cType : string;
  m     : TMethod;
  kc : IMenuControl;
begin
  assert(Supports(c, IMenuControl));

  ci.Control := c;
  if Supports(c, IMenuControl, ci.MenuControl) = false then raise Exception.Create('Control doesn''t support IMenuControlInterface.');
  ci.LinkedParameter  := aLinkedParameter;
  ci.EnumHelper       := EnumHelper;
  ci.PopupCallBack    := PopupCallback;
  ControlLinks.Append(ci);

  ci.MenuControl.SetOnMouseDown(self.Handle_MouseDown);
  ci.MenuControl.SetOnMouseUp(self.Handle_MouseUp);
end;

procedure TRedFoxMenuHandler.DeregisterControl(c: TControl);
var
  Index : integer;
begin
  Index := FindIndexOfControl(c);
  assert(Index <> -1);
  ControlLinks.Delete(Index);
end;

function TRedFoxMenuHandler.FindIndexOfControl(c:TControl): integer;
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

procedure TRedFoxMenuHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Value : single;
  Block : boolean;
  Index : integer;
begin
  assert(Supports(Sender, IMenuControl));

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

  //TODO
  //Value := ControlLinks[Index].KnobControl.GetKnobValue;

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


procedure TRedFoxMenuHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
  Value : single;
begin
  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  //TODO
  //Value := ControlLinks[Index].KnobControl.GetKnobValue;

  if (Button = mbLeft) then
  begin
    if not (ssCtrl in Shift)
      then SetParameterValue(Index, Value);

    EndParameterEdit(Index);
  end;
end;


procedure TRedFoxMenuHandler.BeginParameterEdit(const ControlLinkIndex: integer);
var
  Tag : integer;
begin
  if ControlLinks[ControlLinkIndex].LinkedParameter.IsPublished then
  begin
    Tag := ControlLinks[ControlLinkIndex].LinkedParameter.PublishedVSTParameterIndex;
    Globals.VstMethods^.BeginParameterEdit(Tag);
  end;
end;

procedure TRedFoxMenuHandler.EndParameterEdit(const ControlLinkIndex: integer);
var
  Tag : integer;
begin
  if ControlLinks[ControlLinkIndex].LinkedParameter.IsPublished then
  begin
    Tag := ControlLinks[ControlLinkIndex].LinkedParameter.PublishedVSTParameterIndex;
    Globals.VstMethods^.EndParameterEdit(Tag);
  end;
end;

procedure TRedFoxMenuHandler.SetParameterToDefaut(const ControlLinkIndex: integer);
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


procedure TRedFoxMenuHandler.SetParameterValue(const ControlLinkIndex: integer; const Value: single);
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

procedure TRedFoxMenuHandler.ShowControlContextMenu(const X, Y, ControlLinkIndex: integer);
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
  //mi.OnClick := Handle_MidiLearn;
  mi.Tag     := ControlLinkIndex;
  ControlContextMenu.Items.Add(mi);
  miMidiLearn := mi;

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'MIDI Unlearn';
  //mi.OnClick := Handle_MidiUnlearn;
  mi.Tag     := ControlLinkIndex;
  ControlContextMenu.Items.Add(mi);

  mi := TMenuItem.Create(ControlContextMenu);
  mi.Caption := 'Set MIDI CC...';
  //mi.OnClick := Handle_SetMidiCC;
  mi.Tag     := ControlLinkIndex;
  ControlContextMenu.Items.Add(mi);



  //Show the controls context menu.
  ControlContextMenu.Popup(X, Y);
end;

procedure TRedFoxMenuHandler.UpdateControls;
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
    //TODO
    //ControlLinks[c1].KnobControl.SetKnobValue(ParValue);
  end;

  IsManualGuiUpdateActive := false;
end;


end.
