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
  protected
    Scene : TScene;
    MouseHover : TSceneElement;
    MouseFocus : TSceneElement;
    MouseButtonCount : integer;
    procedure UpdateMouseHover(const NewTarget : TSceneElement);
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
  FarScape.Assistant.UserInteraction;

type
  TProtectedControlHack = class(TFarScapeControl);

{ TUserInteraction }

constructor TUserInteraction.Create(const aScene: TScene);
begin
  assert(assigned(aScene));
  Scene := aScene;
  MouseFocus := nil;
  MouseButtonCount := 0;
  MouseHover := nil;
end;

procedure TUserInteraction.MouseEnter;
begin
  MouseFocus := nil;
  MouseButtonCount := 0;
  if MouseHover <> nil then UpdateMouseHover(nil);
end;

procedure TUserInteraction.MouseLeave;
begin
  MouseFocus := nil;
  MouseButtonCount := 0;
  if MouseHover <> nil then UpdateMouseHover(nil);
end;

procedure TUserInteraction.MouseDown(const Button: TMouseButton; const Shift: TShiftState; const X, Y: Integer);
var
  cX, cY : integer;
begin
  // 1) Update MouseFocus if needed.
  if (MouseButtonCount = 0)
    then MouseFocus := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);
  inc(MouseButtonCount);

  // 2) Update Mouse hover if needed.
  if MouseFocus <> MouseHover then UpdateMouseHover(MouseFocus);

  // 3) Trigger the mouse event
  if assigned(MouseFocus) then
  begin
    cX := X - MouseFocus.AbsoluteBoundsRect.Left;
    cY := Y - MouseFocus.AbsoluteBoundsRect.Top;
    TProtectedControlHack(MouseFocus.Control).MouseDown(Button, Shift, cX, cY);
  end;

end;

procedure TUserInteraction.MouseUp(const Button: TMouseButton; const Shift: TShiftState; const X, Y: Integer);
var
  el : TSceneElement;
  cX, cY : integer;
begin
  // 1) Find where the mouse event should be directed.
  if assigned(MouseFocus)
    then el := MouseFocus
    else el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);

  // 2) Update Mouse hover if needed.
  if el <> MouseHover then UpdateMouseHover(el);

  // 3) Trigger the mouse move event.
  if assigned(el) then
  begin
    cX := X - el.AbsoluteBoundsRect.Left;
    cY := Y - el.AbsoluteBoundsRect.Top;
    TProtectedControlHack(el.Control).MouseUp(Button, Shift, cX, cY);
  end;

  // 4) Mouse button up processing...
  if MouseButtonCount > 1 then
  begin
    dec(MouseButtonCount);
  end else
  begin
    MouseButtonCount := 0;
    MouseFocus := nil;
    el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);
    if el <> MouseHover then UpdateMouseHover(el);
  end;

end;

procedure TUserInteraction.MouseMove(const Shift: TShiftState; const X, Y: Integer);
var
  el : TSceneElement;
  cX, cY : integer;
begin
  // 1) Find where the mouse event should be directed.
  if assigned(MouseFocus)
    then el := MouseFocus
    else el := UserInteractionAssistant.GetInteractionTargetAt(Scene, X, Y);

  // 2) Update Mouse hover if needed.
  if el <> MouseHover
    then UpdateMouseHover(el);

  // 3) Trigger the mouse move event.
  if assigned(el) then
  begin
    cX := X - el.AbsoluteBoundsRect.Left;
    cY := Y - el.AbsoluteBoundsRect.Top;
    TProtectedControlHack(el.Control).MouseMove(Shift, cX, cY);
  end;
end;


procedure TUserInteraction.UpdateMouseHover(const NewTarget: TSceneElement);
begin
  assert(NewTarget <> MouseHover);

  if assigned(MouseHover)
    then TProtectedControlHack(MouseHover.Control).MouseLeave;

  if assigned(NewTarget)
    then TProtectedControlHack(NewTarget.Control).MouseEnter;

  MouseHover := NewTarget;

  if assigned(OnHoverChanged) then
  begin
    if assigned(MouseHover)
      then OnHoverChanged(self, MouseHover.Control)
      else OnHoverChanged(self, nil);
  end;


end;





end.
