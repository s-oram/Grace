{
  GUI controls will sometimes be invalidated many times in a short period. For example, a user moves a scroll bar.

  If a control is repeatably invalidated at too great a rate, each Invalidate() call will arrive before
  the screen has finished drawing. This can have two side effects.
  - sometimes a control may appear frozen until the final Invalidate() call.
  - sometimes other controls will be blocked from invalidating.
  Additionally it is a waste of resouces to redraw a control at a faster rate then the eye can perceive.

  To avoid these problems I've created a laggy invalidator. All RedFox Invalidate() calls are redirected
  to the Invalidator. The Invalidator limits the maximum rate a control can be redrawn.
}

unit RedFoxInvalidator;

interface

uses
  Controls;

procedure LaggyInvalidate(TargetControl:TControl);
procedure SetLaggyInvalidateUpdateInterval(UpdateInterval:cardinal); //UpdateInterval in milliseconds.
procedure CancelInvalidateRequests(TargetControl:TControl);

implementation

uses
  RedFoxContainer,
  Contnrs, SysUtils, Types, Classes, WinApi.Windows, ExtCtrls;

type
  TControlHack = class(TControl)
  strict private
    procedure InvalidateControl(IsVisible, IsOpaque: Boolean);
  public
    procedure InvalidateHack;
  end;

  TRedFoxInvalidator = class
  strict private
    Timer           : TTimer;
    UpdateQueue     : TObjectList;
    RecentlyUpdated : TObjectList;
  strict protected
    procedure HandleTimerEvent(Sender:Tobject);
    procedure DoInvalidate(Target:TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LaggyInvalidateControl(Target:TControl);
    procedure SetUpdateInterval(Interval : cardinal);
    procedure CancelInvalidateRequests(TargetControl:TControl);
  end;

{ TControlHack }

procedure TControlHack.InvalidateControl(IsVisible, IsOpaque: Boolean);
var
  Rect: TRect;
begin
  if (IsVisible or ((csDesigning in ComponentState) and
    not (csDesignerHide in ControlState)) and
    not (csNoDesignVisible in ControlStyle)) and (Parent <> nil) and
    Parent.HandleAllocated then
  begin
    Rect := BoundsRect;
    InvalidateRect(Parent.Handle, Rect, not (IsOpaque or (csOpaque in Parent.ControlStyle)));
  end;
end;

procedure TControlHack.InvalidateHack;
begin
  InvalidateControl(Visible, csOpaque in ControlStyle);
end;

{ TRedFoxInvalidator }

constructor TRedFoxInvalidator.Create;
begin
  UpdateQueue     := TObjectList.Create;
  UpdateQueue.OwnsObjects := false;

  RecentlyUpdated := TObjectList.Create;
  RecentlyUpdated.OwnsObjects := false;

  Timer := TTimer.Create(nil);
  Timer.OnTimer := HandleTimerEvent;
  Timer.Interval := 1000 div 60;
  Timer.Enabled  := true;
end;

destructor TRedFoxInvalidator.Destroy;
begin
  Timer.Enabled := false;

  UpdateQueue.Free;
  RecentlyUpdated.Free;
  Timer.Free;
  inherited;
end;


procedure TRedFoxInvalidator.DoInvalidate(Target: TObject);
var
  aVisibleControl : IRedFoxVisibleControl;
begin
  if (Supports(Target, IRedFoxVisibleControl, aVisibleControl)) and (aVisibleControl.GetIsShowing)  then
  begin
    aVisibleControl.MarkAsInvalidateRequired;
  end;

  if Target is TWinControl then
  begin
    (Target as TWinControl).Perform(CM_INVALIDATE, 0, 0);
  end else
  if Target is TControl then
  begin
    TControlHack(Target).InvalidateHack;
  end;




end;

procedure TRedFoxInvalidator.HandleTimerEvent(Sender: Tobject);
var
  c1 : integer;
  obj : TObject;
  Target : TObject;
begin
  //remove any RecentlyUpdated controls from the list if they aren't in the UpdateQueue.
  for c1 := RecentlyUpdated.Count-1 downto 0 do
  begin
    obj := RecentlyUpdated[c1];
    if UpdateQueue.IndexOf(obj) = -1 then RecentlyUpdated.Delete(c1);
  end;

  while UpdateQueue.Count > 0 do
  begin
    obj := UpdateQueue[0];
    if RecentlyUpdated.IndexOf(obj) = -1 then RecentlyUpdated.Add(obj);
    UpdateQueue.Delete(0);

    Target := obj;
    DoInvalidate(Target);
  end;

end;

procedure TRedFoxInvalidator.LaggyInvalidateControl(Target: TControl);
begin
  if (UpdateQueue.IndexOf(Target) = -1) then
  begin
    if RecentlyUpdated.IndexOf(Target) <> -1 then
    begin
      UpdateQueue.Add(Target);
    end else
    begin
      RecentlyUpdated.Add(Target);
      DoInvalidate(Target);
    end;
  end;
end;

procedure TRedFoxInvalidator.CancelInvalidateRequests(TargetControl: TControl);
begin
  UpdateQueue.Remove(TargetControl);
  RecentlyUpdated.Remove(TargetControl);
end;

procedure TRedFoxInvalidator.SetUpdateInterval(Interval: cardinal);
begin
  Timer.Interval := Interval;
end;

//==============================================================================
var
  GlobalInvalidator : TRedFoxInvalidator;

procedure LaggyInvalidate(TargetControl:TControl);
begin
  if not assigned(GlobalInvalidator) then GlobalInvalidator := TRedFoxInvalidator.Create;
  GlobalInvalidator.LaggyInvalidateControl(TargetControl);
end;

procedure SetLaggyInvalidateUpdateInterval(UpdateInterval:cardinal);
begin
  if not assigned(GlobalInvalidator) then GlobalInvalidator := TRedFoxInvalidator.Create;
  GlobalInvalidator.SetUpdateInterval(UpdateInterval);
end;

procedure CancelInvalidateRequests(TargetControl:TControl);
begin
  if not assigned(GlobalInvalidator) then exit;
  GlobalInvalidator.CancelInvalidateRequests(TargetControl);

end;

initialization

finalization
  if assigned(GlobalInvalidator) then FreeAndNil(GlobalInvalidator);

end.
