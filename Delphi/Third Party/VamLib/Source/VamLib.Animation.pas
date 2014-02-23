unit VamLib.Animation;

interface

uses
  VamLib.UniqueID,
  VamLib.ZeroObject,
  ExtCtrls,
  Contnrs, Generics.Collections;


type
  //==============================================================================
  //               Low level details
  //==============================================================================

  TCustomAnimation     = class;
  TAnimateAction       = class;
  TAnimateActionList   = class;
  TAnimateController   = class;

  TApplyAnimationMethod = reference to procedure(AniObj : TCustomAnimation);

  TCustomAnimation = class
  private
    fID          : TUniqueID;
    fTime        : integer;
    FApplyMethod : TApplyAnimationMethod;
  protected
    function RunStep(const FramePos : single):boolean; virtual; abstract;
  public
    property ID   : TUniqueID read fID   write fID;
    property Time : integer  read fTime write fTime; //milliseconds;
  end;

  TAnimateAction = class
  private
  public
    ID         : TUniqueID;
    StartTime  : TDateTime;
    ActiveTime : integer; //milliseconds
    IsActive   : boolean;
    IsExpired  : boolean;
    Animation  : TCustomAnimation;

    destructor Destroy; override;
  end;

  TAnimateActionList = class(TDictionary<TUniqueID, TAnimateAction>);

  TAnimateController = class(TZeroObject)
  private
    FrameTimer : TTimer;
    ActionList : TAnimateActionList;
    procedure Handle_FrameTimerEvent(Sender : TObject);
  protected
    procedure MakeActive(Action : TAnimateAction);

    procedure AddAction(Action : TAnimateAction);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Animate(Animation : TCustomAnimation);

    procedure Clear;
  end;

  //==============================================================================
  //               Animation Types
  //==============================================================================
  TSingleAnimation = class(TCustomAnimation)
  private
    FStartValue   : single;
    FEndValue     : single;
    FCurrentValue : single;
  protected
    function RunStep(const FramePos : single):boolean; override;
  public
    constructor Create(const ID : TUniqueID; const StartValue, EndValue : single; Time : integer; ApplyMethod : TApplyAnimationMethod);
    property CurrentValue : single read fCurrentValue;
  end;

  TByteAnimation = class(TCustomAnimation)
  private
    FStartValue   : byte;
    FEndValue     : byte;
    FCurrentValue : byte;
  protected
    function RunStep(const FramePos : single):boolean; override;
  public
    constructor Create(const ID : TUniqueID; const StartValue, EndValue : byte; Time : integer; ApplyMethod : TApplyAnimationMethod);
    property CurrentValue : byte read fCurrentValue;
  end;


  //==============================================================================
  //               Global Stuff
  //==============================================================================

function GlobalAnimator:TAnimateController;

implementation

uses
  SysUtils,
  DateUtils;

var
  FGlobalAnimator : TAnimateController;


function GlobalAnimator:TAnimateController;
begin
  if not assigned(FGlobalAnimator) then
  begin
    FGlobalAnimator := TAnimateController.Create;
  end;

  result := FGlobalAnimator;
end;

function CalcFramePos(Action : TAnimateAction):single;
var
  ms : single;
begin
  ms := MillisecondsBetween(Now, Action.StartTime);
  result := ms / Action.ActiveTime;
end;


{ TAnimationController }

constructor TAnimateController.Create;
begin
  ActionList := TAnimateActionList.Create;
  FrameTimer := TTimer.Create(nil);
  FrameTimer.Enabled  := false;
  FrameTimer.Interval := 1;
  FrameTimer.OnTimer  := Handle_FrameTimerEvent;
end;

destructor TAnimateController.Destroy;
begin
  //TODO:
  // Check for any animations. all animations should be cleared before exiting.

  FrameTimer.Enabled := false;
  FrameTimer.Free;

  Clear;
  ActionList.Free;


  inherited;
end;

procedure TAnimateController.Clear;
var
  c1: Integer;
  Action : TAnimateAction;
begin
  for c1 := ActionList.Count-1 downto 0 do
  begin
    Action := ActionList.ToArray[c1].Value;
    Action.Free;
  end;

  ActionList.Clear;
end;



procedure TAnimateController.MakeActive(Action: TAnimateAction);
begin
  Action.StartTime := Now;
  Action.IsActive  := true;
end;

procedure TAnimateController.AddAction(Action: TAnimateAction);
var
  OldAction : TAnimateAction;
begin
  if ActionList.TryGetValue(Action.ID, OldAction) then
  begin
    ActionList.Remove(Action.ID);
    OldAction.Free;
  end;
  ActionList.Add(Action.ID, Action);
end;

procedure TAnimateController.Animate(Animation: TCustomAnimation);
var
  Action : TAnimateAction;
begin
  Action := TAnimateAction.Create;
  Action.IsActive   := false;
  Action.IsExpired  := false;
  Action.ID         := Animation.ID;
  Action.ActiveTime := Animation.Time;
  Action.Animation  := Animation;

  AddAction(Action);
  MakeActive(Action);
  FrameTimer.Enabled := true;
end;

procedure TAnimateController.Handle_FrameTimerEvent(Sender: TObject);
var
  c1: Integer;
  Action : TAnimateAction;
  FramePos : single;
begin
  for c1 := 0 to ActionList.Count-1 do
  begin
    Action := ActionList.ToArray[c1].Value;

    if Action.IsActive then
    begin
      FramePos := CalcFramePos(Action);
      if FramePos >= 1 then
      begin
        Action.Animation.RunStep(1);
        Action.IsActive  := false;
        Action.IsExpired := true;
      end else
      begin
        Action.Animation.RunStep(FramePos);
      end;
    end;
  end;


  for c1 := ActionList.Count-1 downto 0 do
  begin
    Action := ActionList.ToArray[c1].Value;

    if (Action.IsExpired) then
    begin
      ActionList.Remove(Action.ID);
      Action.Free;
    end;
  end;

end;






{ TAnimateAction }

destructor TAnimateAction.Destroy;
begin
  if assigned(Animation) then Animation.Free;

  inherited;
end;

{ TSingleAnimation }

constructor TSingleAnimation.Create(const ID: TUniqueID; const StartValue, EndValue: single; Time: integer; ApplyMethod: TApplyAnimationMethod);
begin
  self.fID           := ID;
  self.fTime         := Time; //Animation running time in milliseconds.
  self.FApplyMethod  := ApplyMethod;
  self.FCurrentValue := StartValue;
  self.FStartValue   := StartValue;
  self.FEndValue     := EndValue;
end;

function TSingleAnimation.RunStep(const FramePos: single): boolean;
begin
  assert(FramePos >= 0);
  assert(FramePos <= 1);
  assert(Assigned(FApplyMethod));

  FCurrentValue := FStartValue * (1 - FramePos) + FEndValue * FramePos;

  FApplyMethod(self);
end;

{ TByteAnimation }

constructor TByteAnimation.Create(const ID: TUniqueID; const StartValue, EndValue: byte; Time: integer; ApplyMethod: TApplyAnimationMethod);
begin
  self.fID           := ID;
  self.fTime         := Time; //Animation running time in milliseconds.
  self.FApplyMethod  := ApplyMethod;
  self.FCurrentValue := StartValue;
  self.FStartValue   := StartValue;
  self.FEndValue     := EndValue;
end;

function TByteAnimation.RunStep(const FramePos: single): boolean;
begin
  assert(FramePos >= 0);
  assert(FramePos <= 1);
  assert(Assigned(FApplyMethod));

  FCurrentValue := round(FStartValue * (1 - FramePos) + FEndValue * FramePos);

  FApplyMethod(self);
end;

initialization

finalization
  if assigned(FGlobalAnimator)
    then FGlobalAnimator.Free;

end.
