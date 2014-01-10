unit eeGuiStandard.RedFoxKnob;

interface

uses
  VamLib.Collections.RecordArray,
  eeVstParameter,
  Classes, Controls, eeGlobals;

type
  PControlInfo = ^TControlInfo;
  TControlInfo = record
    Control : TControl;
    LinkedParameter : TVstParameter;
  end;

  TRedFoxKnobHandler = class
  private
    fGlobals: TGlobals;
    ControlLinks : TRecordArray<TControlInfo>;

    function FindIndexOfControl(c:TControl):integer;

    procedure BeginParameterEdit(const ControlLinkIndex : integer);
    procedure EndParameterEdit(const ControlLinkIndex : integer);
    procedure SetParameterToDefaut(const ControlLinkIndex : integer);
    procedure SetParameterValue(const ControlLinkIndex : integer; const Value : single);
  public
    constructor Create(aGlobals : TGlobals);

    property Globals : TGlobals read fGlobals;

    procedure RegisterControl(c : TControl; aLinkedParameter : TVstParameter);
    procedure DeregisterControl(c : TControl);
  published
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Changed(Sender: TObject);
  end;

implementation

uses
  TypInfo;

{ TRedFoxKnobHandler }

constructor TRedFoxKnobHandler.Create(aGlobals: TGlobals);
begin
  fGlobals := aGlobals;
end;

procedure TRedFoxKnobHandler.RegisterControl(c: TControl; aLinkedParameter: TVstParameter);
var
  ci : TControlInfo;
  cType : string;
  m     : TMethod;
begin
  ci.Control := c;
  ci.LinkedParameter := aLinkedParameter;
  ControlLinks.Append(ci);


  // assign the controls event handlers to the common handler..

  cType := c.ClassName;

  if cType = 'TVamKnob' then
  begin
    m.Data := Pointer(self);

    // Knob Mouse-Down
    m.Code := Self.MethodAddress('MouseDown');
    SetMethodProp(c, 'OnMouseDown', m);

    // Knob Mouse-Up
    m.Code := Self.MethodAddress('MouseUp');
    SetMethodProp(c, 'OnMouseUp', m);

    // Knob Change
    m.Code := Self.MethodAddress('Changed');
    SetMethodProp(c, 'OnChanged', m);
  end;


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

procedure TRedFoxKnobHandler.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Value : single;
  Block : boolean;
  Index : integer;
begin
  assert(Sender.ClassName = 'TVamKnob');
  assert(Sender is TControl);

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



  Value := GetPropValue(Sender, 'Pos');


  if (Button = mbLeft) then
  begin
    BeginParameterEdit(Index);
    if (ssCtrl in Shift)
      then SetParameterToDefaut(Index)
      else SetParameterValue(Index, Value);

  end else
  if (Button = mbRight) then
  begin
    // TODO
    //ShowControlContextMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y, Tag);
  end;




  {
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
  }
end;


procedure TRedFoxKnobHandler.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TRedFoxKnobHandler.Changed(Sender: TObject);
begin

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




end.
