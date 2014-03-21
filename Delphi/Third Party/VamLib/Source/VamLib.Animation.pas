unit VamLib.Animation;

interface

uses
  SysUtils,
  VamLib.UniqueID,
  VamLib.ZeroObject,
  ExtCtrls,
  Contnrs, Generics.Collections;


type
  // TODO: It would be awesome to exten the animations with easing curves.
  // For a list of different curves.
  //   http://easings.net/

  //==============================================================================
  //               Low level details
  //==============================================================================

  TCustomAnimation     = class;
  TAnimateAction       = class;
  TAnimateActionList   = class;
  TAnimateController   = class;

  ICustomAnimation = interface
    ['{57E7F1DA-74E3-4B8D-8ADA-42815A05F02B}']
    procedure RunStep(const FramePos : single);
    function GetRunTime: integer;
    procedure SetRunTime(const Value: integer);
  end;

  TCustomAnimation = class(TRefCountedZeroObject, ICustomAnimation)
  private
    fTime        : integer;
    function GetRunTime: integer;
    procedure SetRunTime(const Value: integer);
  protected
    procedure RunStep(const FramePos : single); virtual; abstract;
    property RunTime : integer  read GetRunTime write SetRunTime; //milliseconds;
  public
  end;

  TAnimateAction = class
  private
  public
    ID         : TUniqueID;
    StartTime  : TDateTime;
    RunTime    : integer; //milliseconds
    IsActive   : boolean;
    IsExpired  : boolean;
    Animation  : ICustomAnimation;

    destructor Destroy; override;
  end;

  TAnimateActionList = class(TDictionary<TUniqueID, TAnimateAction>);

  TAnimateController = class(TZeroObject)
  private
    //TODO: Use high speed timer here.
    FrameTimer : TTimer;
    ActionList : TAnimateActionList;
    procedure Handle_FrameTimerEvent(Sender : TObject);
  protected
    procedure MakeActive(Action : TAnimateAction);
    procedure AddAction(Action : TAnimateAction);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StopAnimation(const ID : TUniqueID);
    procedure Animate(const ID : TUniqueID; Animation : ICustomAnimation);
    procedure Clear;
  end;

  //==============================================================================
  //               Animation Types
  //==============================================================================
  TGenericAnimation<T> = class(TCustomAnimation)
  private
    FStartValue   : T;
    FEndValue     : T;
    FCurrentValue : T;
    procedure SetEndValue(const Value: T);
    procedure SetStartValue(const Value: T);
  protected type
    TApplyAnimationMethod = reference to procedure(CurrentValue:T);
  protected
    FApplyMethod : TApplyAnimationMethod;
    property CurrentValue : T read fCurrentValue;
  public
    constructor Create;

    property RunTime     : integer write SetRunTime;  //Animation running time in milliseconds.
    property StartValue  : T       write SetStartValue;
    property EndValue    : T       write SetEndValue;
    property ApplyMethod : TApplyAnimationMethod write FApplyMethod;
  end;

  TSingleAnimation = class(TGenericAnimation<Single>)
  protected
    procedure RunStep(const FramePos : single); override;
  public
  end;

  TByteAnimation = class(TGenericAnimation<Byte>)
  private
  protected
    procedure RunStep(const FramePos : single); override;
  public
  end;

  TIntegerAnimation = class(TGenericAnimation<Integer>)
  protected
    procedure RunStep(const FramePos : single); override;
  public
  end;


  //==============================================================================
  //               Global Stuff
  //==============================================================================

function GlobalAnimator:TAnimateController;

type
  EAnimationException = class(Exception);

implementation

uses
  DateUtils;

var
  FGlobalAnimator : TAnimateController; //singleton object reference.


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
  result := ms / Action.RunTime;
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
  if FGlobalAnimator = self
    then FGlobalAnimator := nil;

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

procedure TAnimateController.Animate(const ID : TUniqueID; Animation: ICustomAnimation);
var
  Action : TAnimateAction;
begin
  assert(Animation.GetRunTime > 0);

  Action            := TAnimateAction.Create;
  Action.IsActive   := false;
  Action.IsExpired  := false;
  Action.ID         := ID;
  Action.RunTime    := Animation.GetRunTime;
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
  DoException : boolean;
begin
  DoException := false;

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
        // TODO: I might need to put a try..except block around here
        // and kill the animation
        try
          Action.Animation.RunStep(FramePos);
        except
          Action.IsActive  := false;
          Action.IsExpired := true;
          DoException := true;
          Break; //====================>> break >>======>>
        end;
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

  if DoException then
  begin
    // TODO: this exception should probably not be raised in release builds. Perhaps
    // try to log the exception instead. It would be good to raise the exception
    // in beta builds. (Fail fast!)
    raise EAnimationException.Create('There was an animation error.');
  end;
end;

procedure TAnimateController.StopAnimation(const ID: TUniqueID);
var
  Action : TAnimateAction;
begin
  if ActionList.TryGetValue(ID, Action) then
  begin
    Action.IsActive  := false;
    Action.IsExpired := true;
  end;
end;






{ TAnimateAction }

destructor TAnimateAction.Destroy;
begin
  Animation := nil;
  inherited;
end;


{ TCustomAnimation }

function TCustomAnimation.GetRunTime: integer;
begin
  result := FTime;
end;

procedure TCustomAnimation.SetRunTime(const Value: integer);
begin
  FTime := Value;
end;

{ TGenericAnimation<T> }
constructor TGenericAnimation<T>.Create;
begin
  self.fTime         := 0; //Animation running time in milliseconds.
  self.FApplyMethod  := nil;
end;

procedure TGenericAnimation<T>.SetEndValue(const Value: T);
begin
  FEndValue := Value;
end;

procedure TGenericAnimation<T>.SetStartValue(const Value: T);
begin
  FStartValue := Value;
end;

{ TSingleAnimation }
procedure TSingleAnimation.RunStep(const FramePos: single);
begin
  assert(FramePos >= 0);
  assert(FramePos <= 1);
  assert(Assigned(FApplyMethod));

  FCurrentValue := FStartValue * (1 - FramePos) + FEndValue * FramePos;

  FApplyMethod(FCurrentValue);
end;

{ TByteAnimation }
procedure TByteAnimation.RunStep(const FramePos: single);
begin
  assert(FramePos >= 0);
  assert(FramePos <= 1);
  assert(Assigned(FApplyMethod));

  FCurrentValue := round(FStartValue * (1 - FramePos) + FEndValue * FramePos);

  FApplyMethod(FCurrentValue);
end;

{ TIntegerAnimation }

procedure TIntegerAnimation.RunStep(const FramePos: single);
begin
  assert(FramePos >= 0);
  assert(FramePos <= 1);
  assert(Assigned(FApplyMethod));

  FCurrentValue := round(FStartValue * (1 - FramePos) + FEndValue * FramePos);

  FApplyMethod(FCurrentValue);
end;

initialization

finalization
  if assigned(FGlobalAnimator)
    then FGlobalAnimator.Free;

end.
