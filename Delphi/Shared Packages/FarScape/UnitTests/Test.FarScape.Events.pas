unit Test.FarScape.Events;

interface

uses
  TestMocks.FarScapeControls,
  FarScape.CustomControl,
  FarScape.Event,
  FarScape.Events,
  FarScape.EventDispatcher,
  WatchTower;


type
  TFarScapeEventsTest = class(TWatchTowerTest)
  private
    ControlA, ControlB, ControlC : TFarScapeControl;
    EventACount : integer;
    EventBCount : integer;
    EventCCount : integer;
    procedure HandleEventA(const ev : TFarScapeEvent);
    procedure HandleEventB(const ev : TFarScapeEvent);
    procedure HandleMouseEnterEvents(const ev : TFarScapeEvent);
  public
    procedure Setup;
    procedure TearDown;

    [Test]
    procedure ConfirmEventCalls;

    [Test]
    procedure ConfirmEventType;

    [Test]
    procedure TestB;
  end;

  TFarScapeControlEvents = class(TWatchTowerTest)
  protected
  public
  end;

implementation

uses
  UITypes,
  Classes,
  WatchTower.Confirm;


{ TFarScapeEventsTest }

procedure TFarScapeEventsTest.HandleEventA(const ev: TFarScapeEvent);
begin
  inc(EventACount);
end;

procedure TFarScapeEventsTest.HandleEventB(const ev: TFarScapeEvent);
begin
  inc(EventBCount);
end;

procedure TFarScapeEventsTest.Setup;
begin
  EventACount := 0;
  EventBCount := 0;
  EventCCount := 0;

  ControlA := TFarScapeControl.Create;
  ControlB := TFarScapeControl.Create;
  ControlC := TFarScapeControl.Create;
end;

procedure TFarScapeEventsTest.TearDown;
begin
  ControlA.Free;
  ControlB.Free;
  ControlC.Free;
end;

procedure TFarScapeEventsTest.ConfirmEventCalls;
var
  evEnter : TMouseEnterEvent;
  evLeave : TMouseLeaveEvent;
begin
  //evEnter := TMouseEnterEvent.Create(ControlA);
  //evLeave := TMouseLeaveEvent.Create(ControlA);

  evEnter := TMouseEnterEvent.Create(ControlA);
  ControlA.TriggerEvent(TFarScapeEvent(evEnter));
  Confirm.IsTrue(EventACount = 0);
  Confirm.IsTrue(EventBCount = 0);

  ControlA.AddEventListener([TMouseEnterEvent], self.HandleEventA);
  evEnter := TMouseEnterEvent.Create(ControlA);
  ControlA.TriggerEvent(TFarScapeEvent(evEnter));
  Confirm.IsTrue(EventACount = 1);
  Confirm.IsTrue(EventBCount = 0);


  evLeave := TMouseLeaveEvent.Create(ControlA);
  ControlA.TriggerEvent(TFarScapeEvent(evLeave));
  Confirm.IsTrue(EventACount = 1);
  Confirm.IsTrue(EventBCount = 0);
end;

procedure TFarScapeEventsTest.TestB;
var
  ev : TMouseEnterEvent;
  s : string;
begin
  assert(assigned(ControlA));

  ev := TMouseEnterEvent.Create(ControlA);
  ControlA.TriggerEvent(TFarScapeEvent(ev));
end;

procedure TFarScapeEventsTest.ConfirmEventType;
var
  ev : TMouseEnterEvent;
  s : string;
begin
  assert(assigned(ControlA));

  ControlA.AddEventListener([TMouseEnterEvent], HandleMouseEnterEvents);

  ev := TMouseEnterEvent.Create(ControlA);
  ControlA.TriggerEvent(TFarScapeEvent(ev));

  Confirm.IsTrue(EventCCount = 1);
end;

procedure TFarScapeEventsTest.HandleMouseEnterEvents(const ev: TFarScapeEvent);
begin
  Confirm.IsTrue(  assigned(ev)  );
  Confirm.IsTrue(  ev.Target = ControlA  );
  Confirm.IsTrue(  ev.EventType = 'TMouseEnterEvent'  );
  Confirm.IsTrue(  ev.ClassType = TMouseEnterEvent    );

  inc(EventCCount);
end;




end.
