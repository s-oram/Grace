unit eeGuiStandard.RedFoxMenu;

interface

uses
  Menus,
  VamLib.Collections.Lists,
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
    ParIndex        : integer;
  end;

  TControlInfoList = class(TSimpleRecordList<TControlInfo>)
  private
  public
    function FindByControl(c : TControl):PControlInfo;
  end;

  TRedFoxMenuHandler = class
  private
    fGlobals: TGlobals;
    ControlLinks            : TControlInfoList;
    IsManualGuiUpdateActive : boolean;
    MenuBuilder             : TGuiMenuBuilder;

    function FindIndexOfControl(c:TControl):integer;

    procedure BeginParameterEdit(const ControlLinkIndex : integer);
    procedure EndParameterEdit(const ControlLinkIndex : integer);
    procedure SetParameterToDefaut(const ControlLinkIndex : integer);
    procedure SetParameterValue(const ControlLinkIndex : integer; const Value : single);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
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

  MenuBuilder := TGuiMenuBuilder.Create;
  MenuBuilder.Globals := aGlobals;

  ControlLinks := TControlInfoList.Create;
end;

destructor TRedFoxMenuHandler.Destroy;
begin
  ControlLinks.Free;
  MenuBuilder.Free;
  inherited;
end;

procedure TRedFoxMenuHandler.RegisterControl(c: TControl; aLinkedParameter: TVstParameter; const EnumHelper:TCustomEnumHelperClass; PopupCallBack : TMenuCallback = nil);
var
  ci : PControlInfo;

  cType : string;
  m     : TMethod;
  kc : IMenuControl;
begin
  assert(Supports(c, IMenuControl));

  ci := ControlLinks.FindByControl(c);
  if not assigned(ci) then ci := ControlLinks.New;

  ci^.Control := c;
  if Supports(c, IMenuControl, ci^.MenuControl) = false then raise Exception.Create('Control doesn''t support IMenuControlInterface.');
  ci^.LinkedParameter  := aLinkedParameter;
  ci^.EnumHelper       := EnumHelper;
  ci^.PopupCallBack    := PopupCallback;
  ci^.ParIndex := Globals.VstParameters.FindParameterIndex(aLinkedParameter);
  ControlLinks.Add(ci^);

  ci^.MenuControl.SetOnMouseEnter(self.Handle_MouseEnter);
  ci^.MenuControl.SetOnMouseLeave(self.Handle_MouseLeave);
  ci^.MenuControl.SetOnMouseDown(self.Handle_MouseDown);
  ci^.MenuControl.SetOnMouseUp(self.Handle_MouseUp);
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

procedure TRedFoxMenuHandler.Handle_MouseEnter(Sender: TObject);
var
  Index : integer;
begin
  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  Globals.InfoBarReceiver.EnterControl(Sender);
  Globals.InfoBarReceiver.SendControlMessage(Sender, ControlLinks[Index].LinkedParameter.ParInfo);
end;

procedure TRedFoxMenuHandler.Handle_MouseLeave(Sender: TObject);
var
  Index : integer;
begin
  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  Globals.InfoBarReceiver.LeaveControl(Sender);
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


  if (Button = mbRight) then
  begin
    MenuBuilder.IncrementEnumeratedVstParameter(ControlLinks[Index].ParIndex, ControlLinks[Index].EnumHelper);
  end;
end;

procedure TRedFoxMenuHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
  Value : single;
begin
  Index := FindIndexOfControl(Sender as TControl);
  assert(Index <> -1);

  if (Button = mbLeft) then
  begin
    MenuBuilder.ShowMenuForVstParameter(Mouse.CursorPos.X, Mouse.CursorPos.Y, ControlLinks[Index].ParIndex, ControlLinks[Index].EnumHelper, ControlLinks[Index].PopupCallBack);
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

procedure TRedFoxMenuHandler.UpdateControls;
var
  c1: Integer;
  parValue : single;
  TextValue : string;
  c : TControl;
begin
  IsManualGuiUpdateActive := true;
  try
    for c1 := 0 to ControlLinks.Count-1 do
    begin
      c := ControlLinks[c1].Control;
      parValue := ControlLinks[c1].LinkedParameter.ValueVST;

      TextValue := ControlLinks[c1].EnumHelper.ToShortGuiString(parValue);
      ControlLinks[c1].MenuControl.SetMenuText(TextValue);
    end;
  finally
    IsManualGuiUpdateActive := false;
  end;
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

end.
