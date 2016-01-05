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
    Dispatcher : TEventDispatcher;
    EventCountA, EventCountB, EventCountC : integer;

    procedure HandleEventA(const ev : TEventData);
    procedure HandleEventB(const ev : TEventData);
    procedure HandleEventC(const ev : TEventData);
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure TestEventCreation;

    [Test]
    procedure TestEventDispatcher_TestA;

    [Test]
    procedure TestEventDispatcher_TestB;

    [Test]
    procedure TestEventDispatcher_TestC;
  end;

  TFarScapeControlEvents = class(TWatchTowerTest)
  protected
    mA : TMockControl;
    mB : TMockControl;
    mC : TMockControl;
    Root : TMockRootControl;
    EventCountA, EventCountB, EventCountC : integer;
    procedure HandleEventAOnly(const ev : TEventData);
    procedure HandleEventA(const ev : TEventData);
    procedure HandleEventB(const ev : TEventData);
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure NewChildEvents;

    [Test]
    procedure RemoveAllListeners;
  end;

implementation

uses
  UITypes,
  Classes,
  WatchTower.Confirm;

{ TFarScapeEventsTest }

procedure TFarScapeEventsTest.Setup;
begin
  inherited;

  EventCountA := 0;
  EventCountB := 0;
  EventCountC := 0;

  Dispatcher := TEventDispatcher.Create;
end;

procedure TFarScapeEventsTest.TearDown;
begin
  Dispatcher.Free;

  inherited;
end;

procedure TFarScapeEventsTest.HandleEventA(const ev: TEventData);
begin
  inc(EventCountA);
end;

procedure TFarScapeEventsTest.HandleEventB(const ev: TEventData);
begin
  inc(EventCountB);
end;

procedure TFarScapeEventsTest.HandleEventC(const ev: TEventData);
begin
  inc(EventCountC);
end;



procedure TFarScapeEventsTest.TestEventCreation;
var
  ev : TEventData;
begin
  ev := TMouseDownEvent.Create(self, TMouseButton.mbLeft, [ssLeft], 10,15);
  Confirm.IsTrue(  ev.EventClass = TMouseDownEvent );
  Confirm.IsTrue(  ev.Target     = self );
  Confirm.IsTrue(  ev.Target is TFarScapeEventsTest );
  Confirm.IsTrue(  TMouseDownEvent(ev).Button = TMouseButton.mbLeft  );
  Confirm.IsTrue(  TMouseDownEvent(ev).Shift  = [ssLeft] );
  Confirm.IsTrue(  TMouseDownEvent(ev).X      = 10 );
  Confirm.IsTrue(  TMouseDownEvent(ev).Y      = 15 );
  Confirm.IsTrue(  ev.Propagate   );
  Confirm.IsFalse( ev.IsHandled   );
  ev.Release;

  ev := TMouseEnterEvent.Create(self);
  Confirm.IsTrue(  ev.EventClass = TMouseEnterEvent );
  ev.Release;

  ev := TMouseLeaveEvent.Create(self);
  Confirm.IsTrue(  ev.EventClass = TMouseLeaveEvent );
  ev.Release;
end;

procedure TFarScapeEventsTest.TestEventDispatcher_TestA;
begin
  Dispatcher.AddListener([TMouseEnterEvent], HandleEventA, TListenerDuration.ldAll);
  Dispatcher.AddListener([TMouseLeaveEvent], HandleEventB, TListenerDuration.ldAll);
  Dispatcher.AddListener([TMouseDownEvent],  HandleEventC, TListenerDuration.ldAll);

  Dispatcher.Broadcast( TMouseEnterEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseLeaveEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseDownEvent.Create(self, TMouseButton.mbRight, [ssRight], 35, -1)  );

  Confirm.IsTrue(  EventCountA = 1  );
  Confirm.IsTrue(  EventCountB = 1  );
  Confirm.IsTrue(  EventCountC = 1  );
end;

procedure TFarScapeEventsTest.TestEventDispatcher_TestB;
begin
  Dispatcher.AddListener([TMouseEnterEvent], HandleEventA, TListenerDuration.ldAll);
  Dispatcher.AddListener([TMouseLeaveEvent], HandleEventB, TListenerDuration.ldAll);
  Dispatcher.AddListener([TMouseDownEvent],  HandleEventC, TListenerDuration.ldAll);

  Dispatcher.Broadcast( TMouseEnterEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseLeaveEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseDownEvent.Create(self, TMouseButton.mbRight, [ssRight], 35, -1)  );

  Dispatcher.RemoveListener(HandleEventA);

  Dispatcher.Broadcast( TMouseEnterEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseLeaveEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseDownEvent.Create(self, TMouseButton.mbRight, [ssRight], 35, -1)  );

  Confirm.IsTrue(  EventCountA = 1  );
  Confirm.IsTrue(  EventCountB = 2  );
  Confirm.IsTrue(  EventCountC = 2  );
end;

procedure TFarScapeEventsTest.TestEventDispatcher_TestC;
begin
  Dispatcher.AddListener([], HandleEventA, TListenerDuration.ldAll);
  Dispatcher.AddListener([TMouseLeaveEvent], HandleEventB, TListenerDuration.ldAll);
  Dispatcher.AddListener([TMouseDownEvent],  HandleEventC, TListenerDuration.ldAll);


  Dispatcher.Broadcast( TMouseEnterEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseLeaveEvent.Create(self)  );
  Dispatcher.Broadcast( TMouseDownEvent.Create(self, TMouseButton.mbRight, [ssRight], 35, -1)  );

  Confirm.IsTrue(  EventCountA = 3  );
  Confirm.IsTrue(  EventCountB = 1  );
  Confirm.IsTrue(  EventCountC = 1  );
end;

{ TFarScapeControlEvents }

procedure TFarScapeControlEvents.HandleEventAOnly(const ev: TEventData);
begin
  inc(EventCountA);
  Confirm.IsTrue(  ev.EventClass = TChildAddedEvent   );
  Confirm.IsTrue(  ev.Target = mA   );
end;

procedure TFarScapeControlEvents.HandleEventA(const ev: TEventData);
begin
  inc(EventCountA);
end;

procedure TFarScapeControlEvents.HandleEventB(const ev: TEventData);
begin
  inc(EventCountB);
end;

procedure TFarScapeControlEvents.Setup;
begin
  inherited;

  EventCountA := 0;
  EventCountB := 0;
  EventCountC := 0;

  Root := TMockRootControl.Create;
end;

procedure TFarScapeControlEvents.TearDown;
begin
  Root.Free;
  inherited;
end;

procedure TFarScapeControlEvents.NewChildEvents;
begin
  Root.AddEventListener([TChildAddedEvent], HandleEventAOnly);
  mA := TMockControl.Create;
  mA.Parent := Root;
  Confirm.IsTrue(  EventCountA = 1  );
end;

procedure TFarScapeControlEvents.RemoveAllListeners;
begin
  Root.AddEventListener([TChildAddedEvent], HandleEventA);
  Root.AddEventListener([TValueChangedEvent], HandleEventB);

  mA := TMockControl.Create;
  mA.Parent := Root;

  mB := TMockControl.Create;
  mB.Parent := mA;

  mC := TMockControl.Create;
  mC.Parent := mB;

  Root.RemoveEventListener(HandleEventA);
  Root.RemoveAllEventListeners(true);


end;

end.
