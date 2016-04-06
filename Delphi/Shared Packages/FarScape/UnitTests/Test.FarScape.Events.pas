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
    procedure HandleEventA(const ev : TFarScapeEvent);
    procedure HandleEventB(const ev : TFarScapeEvent);
  public
    procedure Setup;
    procedure TearDown;

    [Test]
    procedure TestA;
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

procedure TFarScapeEventsTest.TestA;
var
  evEnter : TMouseEnterEvent;
  evLeave : TMouseLeaveEvent;
begin
  evEnter := TMouseEnterEvent.Create(ControlA);
  evLeave := TMouseLeaveEvent.Create(ControlA);
  try
    ControlA.TriggerEvent(evEnter);
    Confirm.IsTrue(EventACount = 0);
    Confirm.IsTrue(EventBCount = 0);

    ControlA.AddEventListener([TMouseEnterEvent], self.HandleEventA);
    ControlA.TriggerEvent(evEnter);
    Confirm.IsTrue(EventACount = 1);
    Confirm.IsTrue(EventBCount = 0);

    ControlA.TriggerEvent(evLeave);
    Confirm.IsTrue(EventACount = 1);
    Confirm.IsTrue(EventBCount = 0);


  finally
    evEnter.Free;
    evLeave.Free;
  end;
end;

end.
