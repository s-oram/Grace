unit Test.Helm.Dispatcher;

interface

uses
  WatchTower,
  Helm.Message,
  Helm.Dispatcher;

type
  TEventDispatcherTests = class(TWatchTowerTest)
  private
    Dispatcher : THelmDispatcher;
    EventCountA, EventCountB, EventCountC : integer;
    procedure EventHandlerA(const Msg : THelmMessage);
    procedure EventHandlerB(const Msg : THelmMessage);
    procedure EventHandlerC(const Msg : THelmMessage);
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure AddEventHandlersAndSendEvents;

    [Test]
    procedure RemoveEventHandlers;

    [Test]
    procedure SingleEventCatch;
  end;



implementation

uses
  TestMocks.Helm,
  WatchTower.MemLogger,
  WatchTower.Confirm;

{ TEventDispatcherTests }

procedure TEventDispatcherTests.Setup;
begin
  inherited;

  Dispatcher := THelmDispatcher.Create;

  EventCountA := 0;
  EventCountB := 0;
  EventCountC := 0;
end;

procedure TEventDispatcherTests.TearDown;
begin
  Dispatcher.Free;
  inherited;
end;

procedure TEventDispatcherTests.EventHandlerA(const Msg: THelmMessage);
begin
  inc(EventCountA);
end;

procedure TEventDispatcherTests.EventHandlerB(const Msg: THelmMessage);
begin
  inc(EventCountB);
end;

procedure TEventDispatcherTests.EventHandlerC(const Msg: THelmMessage);
begin
  inc(EventCountC);
end;

procedure TEventDispatcherTests.AddEventHandlersAndSendEvents;
begin
  Dispatcher.AddListener([], EventHandlerA, ldAll);
  Dispatcher.AddListener([TMsgB], EventHandlerB, ldAll);
  Dispatcher.AddListener([TMsgC], EventHandlerC, ldAll);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Confirm.IsTrue(  EventCountA = 3  );
  Confirm.IsTrue(  EventCountB = 1  );
  Confirm.IsTrue(  EventCountC = 1  );
end;

procedure TEventDispatcherTests.RemoveEventHandlers;
begin
  //== Step 1 ==

  Dispatcher.AddListener([], EventHandlerA, ldAll);
  Dispatcher.AddListener([TMsgB], EventHandlerB, ldAll);
  Dispatcher.AddListener([TMsgC], EventHandlerC, ldAll);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Confirm.IsTrue(  EventCountA = 3  );
  Confirm.IsTrue(  EventCountB = 1  );
  Confirm.IsTrue(  EventCountC = 1  );

  //== Step 2 ==

  Dispatcher.RemoveListener(EventHandlerA);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Confirm.IsTrue(  EventCountA = 3  );
  Confirm.IsTrue(  EventCountB = 2  );
  Confirm.IsTrue(  EventCountC = 2  );

  //== Step 3 ==

  Dispatcher.RemoveListener(EventHandlerB);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Confirm.IsTrue(  EventCountA = 3  );
  Confirm.IsTrue(  EventCountB = 2  );
  Confirm.IsTrue(  EventCountC = 3  );


  //== Step 4 ==

  Dispatcher.RemoveListener(EventHandlerC);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Confirm.IsTrue(  EventCountA = 3  );
  Confirm.IsTrue(  EventCountB = 2  );
  Confirm.IsTrue(  EventCountC = 3  );
end;

procedure TEventDispatcherTests.SingleEventCatch;
begin
  Dispatcher.AddListener([], EventHandlerA, ldAll);
  Dispatcher.AddListener([TMsgB], EventHandlerB, ldOnce);
  Dispatcher.AddListener([TMsgC], EventHandlerC, ldAll);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Dispatcher.Broadcast(TMsgA.CreateMessage, true);
  Dispatcher.Broadcast(TMsgB.CreateMessage, true);
  Dispatcher.Broadcast(TMsgC.CreateMessage, true);

  Confirm.IsTrue(  EventCountA = 9  );
  Confirm.IsTrue(  EventCountB = 1  );
  Confirm.IsTrue(  EventCountC = 3  );
end;



end.
