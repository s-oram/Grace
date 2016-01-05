unit Test.VamLib.MultiCastEvents;

interface

uses
  WatchTower,
  VamLib.MultiCastEvents;

type
  TMultiCastEventTest = class(TWatchTowerTest)
  private
    mc : TMultiCastNotifyEvent;
    ACount : integer;
    BCount : integer;
    CCount : integer;
    procedure EventHandlerA(Sender : TObject);
    procedure EventHandlerB(Sender : TObject);
    procedure EventHandlerC(Sender : TObject);
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure AddMethods;
  end;

implementation

uses
  WatchTower.Confirm;

{ TMultiCastEventTest }

procedure TMultiCastEventTest.EventHandlerA(Sender: TObject);
begin
  Confirm.IsTrue(  Sender = self  );
  inc(ACount);
end;

procedure TMultiCastEventTest.EventHandlerB(Sender: TObject);
begin
  Confirm.IsTrue(  Sender = self  );
  inc(BCount);
end;

procedure TMultiCastEventTest.EventHandlerC(Sender: TObject);
begin
  Confirm.IsTrue(  Sender = self  );
  inc(CCount);
end;

procedure TMultiCastEventTest.Setup;
begin
  inherited;
  mc := TMultiCastNotifyEvent.Create;

  ACount := 0;
  BCount := 0;
  CCount := 0;
end;

procedure TMultiCastEventTest.TearDown;
begin
  mc.Free;
  inherited;
end;

procedure TMultiCastEventTest.AddMethods;
var
  x : integer;
begin
  mc.AddWatcher(EventHandlerA);
  Confirm.IsTrue(  mc.WatcherCount = 1  );

  // Adding event handlers multiple times shouldn't result in multiple handlers being registered.
  mc.AddWatcher(EventHandlerA);
  Confirm.IsTrue(  mc.WatcherCount = 1  );

  mc.AddWatcher(EventHandlerB);
  Confirm.IsTrue(  mc.WatcherCount = 2  );

  mc.AddWatcher(EventHandlerC);
  Confirm.IsTrue(  mc.WatcherCount = 3  );



  mc.TriggerAll(self);

  Confirm.IsTrue(  ACount = 1  );
  Confirm.IsTrue(  BCount = 1  );
  Confirm.IsTrue(  CCount = 1  );

  mc.RemoveWatcher(EventHandlerA);



  mc.TriggerAll(self);

  Confirm.IsTrue(  ACount = 1  );
  Confirm.IsTrue(  BCount = 2  );
  Confirm.IsTrue(  CCount = 2  );


  mc.RemoveWatcher(EventHandlerA);
  mc.RemoveWatcher(EventHandlerB);
  mc.RemoveWatcher(EventHandlerC);

  Confirm.IsTrue(  mc.WatcherCount = 0  );

  x := SizeOf(mc);
end;

end.
