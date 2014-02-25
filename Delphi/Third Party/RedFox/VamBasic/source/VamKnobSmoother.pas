unit VamKnobSmoother;

interface

// TODO: It might be good to make a high speed timer based around
// the omni thread library functionality.

uses
  VamLib.HighSpeedTimer,
  SysUtils,
  Generics.Collections,
  ExtCtrls;

type
  EKnobSmootherException = class(Exception);

  TApplyValueMethod = reference to procedure(NewValue : single);

  TSmoothAction = class
  public
    LinkedObject : TObject;
    CurrentValue : single;
    TargetValue  : single;
    IsActive     : boolean;
    KnobMove : TApplyValueMethod;
    KnobUp   : TApplyValueMethod;
  end;

  TSmoothActionList = class(TDictionary<TObject, TSmoothAction>);

  TKnobSmoother = class
  private
    ActionList  : TSmoothActionList;
    fSlewStepSize      : single;
    fChangeCoefficient : single;

    Timer : THighSpeedTimer;

    procedure ProcessAction(Action : TSmoothAction);
    procedure ProcessKnobActionsB;

    procedure ClearActionList;

    procedure HandleTimerEvent(Sender : TObject);

  public
    constructor Create;
    destructor Destroy; override;

    property SlewStepSize : single read fSlewStepSize write fSlewStepSize;

    procedure KnobDown(const Obj : TObject; CurrentValue : single; ApplyValue : TApplyValueMethod);
    procedure KnobMove(const Obj : TObject; TargetValue : single; ApplyValue : TApplyValueMethod);
    procedure KnobUp(const Obj : TObject; TargetValue : single; ApplyValue : TApplyValueMethod);
    procedure FinaliseKnob(const Obj : TObject);
  end;


function KnobSmoother : TKnobSmoother;

implementation

uses
  Math;

var
  FGlobalKnobSmoother : TKnobSmoother;

function KnobSmoother : TKnobSmoother;
begin
  if not assigned(FGlobalKnobSmoother)
    then FGlobalKnobSmoother := TKnobSmoother.Create;

  result := FGlobalKnobSmoother;
end;

{ TKnobSmoother }

constructor TKnobSmoother.Create;
begin
  ActionList  := TSmoothActionList.Create(0);
  fSlewStepSize := 0.003;
  fChangeCoefficient := 0.15;

  Timer := THighSpeedTimer.Create;
  Timer.Interval := 15;
  Timer.OnTimer := self.HandleTimerEvent;
end;

destructor TKnobSmoother.Destroy;
begin
  if FGlobalKnobSmoother = self
    then FGlobalKnobSmoother := nil;

  Timer.Free;

  ClearActionList;
  ActionList.Free;

  inherited;
end;

procedure TKnobSmoother.ClearActionList;
var
  c1 : integer;
  Action : TSmoothAction;
begin
  for c1 := ActionList.Count-1 downto 0 do
  begin
    Action := ActionList.ToArray[c1].Value;
    ActionList.Remove(Action.LinkedObject);
    Action.Free;
  end;
end;



procedure TKnobSmoother.KnobDown(const Obj: TObject; CurrentValue: single; ApplyValue: TApplyValueMethod);
var
  Action : TSmoothAction;
begin
  if ActionList.TryGetValue(Obj, Action) = false then
  begin
    Action := TSmoothAction.Create;
    Action.LinkedObject := obj;
    Action.CurrentValue := CurrentValue;
    Action.TargetValue  := CurrentValue;

    Action.KnobMove := nil;
    Action.KnobUp   := nil;
    Action.IsActive := true;
    if assigned(ApplyValue) then ApplyValue(CurrentValue);

    ActionList.Add(Obj, Action);
  end else
  begin
    Action.IsActive := true;
    Action.CurrentValue := CurrentValue;
    Action.TargetValue  := CurrentValue;

    Action.KnobMove := nil;
    Action.KnobUp   := nil;
    Action.IsActive := true;
    if assigned(ApplyValue) then ApplyValue(CurrentValue);
  end;

  if Timer.Enabled = false
    then Timer.Enabled := true;
end;

procedure TKnobSmoother.KnobMove(const Obj: TObject; TargetValue: single; ApplyValue: TApplyValueMethod);
var
  Action : TSmoothAction;
begin
  if ActionList.TryGetValue(Obj, Action) then
  begin
    Action.TargetValue := TargetValue;
    Action.KnobMove := ApplyValue;
  end;
end;

procedure TKnobSmoother.KnobUp(const Obj: TObject; TargetValue: single; ApplyValue: TApplyValueMethod);
var
  Action : TSmoothAction;
begin
  if ActionList.TryGetValue(Obj, Action) then
  begin
    Action.TargetValue := TargetValue;
    Action.KnobUp := ApplyValue;
  end;
end;

procedure TKnobSmoother.FinaliseKnob(const Obj: TObject);
var
  Action : TSmoothAction;
begin
  if ActionList.TryGetValue(Obj, Action) then
  begin
    Action.CurrentValue := Action.TargetValue;
    if assigned(Action.KnobMove)
      then Action.KnobMove(Action.CurrentValue);

    if assigned(Action.KnobUp)
      then Action.KnobUp(Action.CurrentValue);

    Action.IsActive := false;
  end;
end;

procedure TKnobSmoother.HandleTimerEvent(Sender: TObject);
begin
  ProcessKnobActionsB;
end;

procedure TKnobSmoother.ProcessAction(Action: TSmoothAction);
var
  Dist : single;
begin
  if (Action.IsActive) then
  begin
    if (Action.CurrentValue <> Action.TargetValue) and (assigned(Action.KnobMove)) then
    begin
      Dist := Action.TargetValue - Action.CurrentValue;

      if abs(Dist) <= SlewStepSize then
      begin
        Action.CurrentValue := Action.TargetValue
      end else
      if Dist > 0 then
      begin
        Action.CurrentValue := Action.CurrentValue + fChangeCoefficient * Dist + SlewStepSize;
        if Action.CurrentValue > Action.TargetValue
          then Action.CurrentValue := Action.TargetValue;
      end else
      begin
        Action.CurrentValue := Action.CurrentValue + fChangeCoefficient * Dist - SlewStepSize;
        if Action.CurrentValue < Action.TargetValue
          then Action.CurrentValue := Action.TargetValue;
      end;

      try
        Action.KnobMove(Action.CurrentValue);
      except
        raise EKnobSmootherException.Create('ERROR');
      end;
    end;


    if (Action.CurrentValue = Action.TargetValue) and (assigned(Action.KnobUp)) then
    begin
      try
        Action.KnobUp(Action.CurrentValue);
      except
        raise EKnobSmootherException.Create('ERROR');
      end;
      Action.IsActive := false;
    end;
  end;

end;

procedure TKnobSmoother.ProcessKnobActionsB;
var
  c1 : integer;
  Action : TSmoothAction;
begin
  for c1 := ActionList.Count-1 downto 0 do
  begin
    Action := ActionList.ToArray[c1].Value;
    ProcessAction(Action);
  end;

  for c1 := ActionList.Count-1 downto 0 do
  begin
    Action := ActionList.ToArray[c1].Value;
    if Action.IsActive = false then
    begin
      ActionList.Remove(Action.LinkedObject);
      Action.Free;
    end;
  end;

  if ActionList.Count = 0 then Timer.Enabled := false;
end;




initialization

finalization
  if assigned(FGlobalKnobSmoother)
    then FGlobalKnobSmoother.Free;

end.
