unit FarScape.UserInteraction;

interface

// NOTE: FarScape has a couple interaction differences to VCL controls.
// - All FarScape controls capture the mouse when clicked.
// - Mouse capture will occur with all button clicks (left, right, middle etc).

uses
  Classes,
  Controls,
  FarScape.PureInterfacedObject,
  FarScape.CustomControl,
  FarScape.Scene;

type
  TFarScapeControlEvent = procedure(Sender : TObject; const Control : TFarScapeControl) of object;

  TUserInteraction = class(TPureInterfacedObject, IFarScapeUserInteraction)
  private
    FOnHoverChanged: TFarScapeControlEvent;
    FMouseHover: TFarScapeControl;
    FMouseFocus: TFarScapeControl;
    procedure SetMouseFocus(const NewTarget: TFarScapeControl);
    procedure SetMouseHover(const NewTarget: TFarScapeControl);
  protected
    Scene : TScene;
    MouseButtonCount : integer;
    property MouseFocus : TFarScapeControl read FMouseFocus write SetMouseFocus;
    property MouseHover : TFarScapeControl read FMouseHover write SetMouseHover;
  public
    constructor Create(const aScene : TScene);

    procedure MouseEnter;
    procedure MouseLeave;
    procedure MouseDown(const Button: TMouseButton; const Shift: TShiftState; const X, Y: Integer);
    procedure MouseMove(const Shift: TShiftState; const X, Y: Integer);
    procedure MouseUp(const Button: TMouseButton; const Shift: TShiftState; const X, Y: Integer);

    property OnHoverChanged : TFarScapeControlEvent read FOnHoverChanged write FOnHoverChanged;
  end;



implementation

uses
  Types,
  FarScape.Assistant.UserInteraction;

type
  TProtectedControlHack = class(TFarScapeControl);

{ TUserInteraction }

constructor TUserInteraction.Create(const aScene: TScene);
begin
  assert(assigned(aScene));
  Scene := aScene;
  MouseButtonCount := 0;
  FMouseFocus := nil;
  FMouseHover := nil;
end;

procedure TUserInteraction.MouseEnter;
begin
  MouseFocus := nil;
  MouseHover := nil;
  MouseButtonCount := 0;
end;

procedure TUserInteraction.MouseLeave;
begin
  MouseFocus := nil;
  MouseHover := nil;
  MouseButtonCount := 0;
end;

procedure TUserInteraction.MouseDown(const Button: TMouseButton; const Shift: TShiftState; const X, Y: Integer);
var
  cX, cY : integer;
  el : TSceneElement;
  abr : TRect;
begin
  // 1) Update mouse hover
  if (MouseButtonCount = 0) then
  begin
    el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);
    assert( (not assigned(el)) or ((assigned(el)) and (assigned(el.Control))) );
    if assigned(el)
      then MouseHover := el.Control
      else MouseHover := nil;
  end;

  // 2) Update Mouse focus
  if (MouseButtonCount = 0) then
  begin
    el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);
    assert( (not assigned(el)) or ((assigned(el)) and (assigned(el.Control))) );
    if assigned(el)
      then MouseFocus := el.Control
      else MouseFocus := nil;
  end;

  // 3) Fire the event
  if assigned(MouseFocus) then
  begin
    abr := MouseFocus.GetAbsoluteRect;
    cX := X - abr.Left;
    cY := Y - abr.Top;
    TProtectedControlHack(MouseFocus).MouseDown(Button, Shift, cX, cY);
  end;

  // 4) finally, update the button count
  inc(MouseButtonCount);
end;

procedure TUserInteraction.MouseMove(const Shift: TShiftState; const X, Y: Integer);
var
  TargetControl : TFarScapeControl;
  abr : TRect;
  el : TSceneElement;
  cX, cY : integer;
begin

  // 1) Find where the mouse event should be directed.
  if assigned(MouseFocus) then
  begin
    TargetControl := MouseFocus;
  end else
  begin
    el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);
    if assigned(el) then
    begin
      assert(assigned(el.Control));
      TargetControl := el.Control;
    end else
    begin
      TargetControl := nil;
    end;
  end;

  // 2) Update Mouse hover..
  if MouseHover <> TargetControl then MouseHover := TargetControl;


  // 3) Fire the mouse move event.
  if assigned(TargetControl) then
  begin
    abr := TargetControl.GetAbsoluteRect;
    cX := X - abr.Left;
    cY := Y - abr.Top;
    TProtectedControlHack(TargetControl).MouseMove(Shift, cX, cY);
  end;
end;

procedure TUserInteraction.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const X, Y: Integer);
var
  TargetControl : TFarScapeControl;
  abr : TRect;
  el : TSceneElement;
  cX, cY : integer;
begin

  // 1) Find where the mouse event should be directed.
  if assigned(MouseFocus) then
  begin
    TargetControl := MouseFocus;
  end else
  begin
    el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);
    if assigned(el) then
    begin
      assert(assigned(el.Control));
      TargetControl := el.Control;
    end else
    begin
      TargetControl := nil;
    end;
  end;

  // 2) Fire the event
  if assigned(TargetControl) then
  begin
    abr := TargetControl.GetAbsoluteRect;
    cX := X - abr.Left;
    cY := Y - abr.Top;
    TProtectedControlHack(TargetControl).MouseUp(Button, Shift, cX, cY);
  end;

  // 3) Button count processing
  if MouseButtonCount > 1 then
  begin
    dec(MouseButtonCount);
  end else
  begin
    MouseButtonCount := 0;
    MouseFocus := nil;
  end;

  // 4) Update Mouse Hover
  if (MouseButtonCount = 0) then
  begin
    el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);
    assert( (not assigned(el)) or ((assigned(el)) and (assigned(el.Control))) );
    if assigned(el)
      then MouseHover := el.Control
      else MouseHover := nil;
  end;
end;

procedure TUserInteraction.SetMouseFocus(const NewTarget: TFarScapeControl);
begin
  FMouseFocus := NewTarget;
end;

procedure TUserInteraction.SetMouseHover(const NewTarget: TFarScapeControl);
begin
  if NewTarget <> FMouseHover then
  begin
    if assigned(FMouseHover)
      then TProtectedControlHack(FMouseHover).MouseLeave;

    if assigned(NewTarget)
      then TProtectedControlHack(NewTarget).MouseEnter;

    FMouseHover := NewTarget;

    if assigned(OnHoverChanged) then OnHoverChanged(self, FMouseHover);
  end;
end;



end.
