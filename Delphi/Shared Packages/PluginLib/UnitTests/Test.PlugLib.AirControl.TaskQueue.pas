unit Test.PlugLib.AirControl.TaskQueue;

interface

uses
  WatchTower,
  PlugLib.AirControl.TaskQueue;

type
  TTaskQueueTest = class(TWatchTowerTest)
  private
    procedure MakeWorkMethodA;
    procedure MakeWorkMethodB;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure Basics;

    [Test]
    procedure AddAndRemoveItemsA;

    [Test]
    procedure AddAndRemoveItemsB;
  end;



implementation

uses
  SysUtils,
  Classes,
  WatchTower.Confirm;

{ TTaskQueueTest }

procedure TTaskQueueTest.Setup;
begin
  inherited;

end;

procedure TTaskQueueTest.TearDown;
begin
  inherited;

end;

procedure TTaskQueueTest.MakeWorkMethodA;
begin

end;

procedure TTaskQueueTest.MakeWorkMethodB;
begin

end;



procedure TTaskQueueTest.Basics;
var
  TQ : TTaskQueue;
begin
  TQ := TTaskQueue.Create(10,20,100);
  try
    Confirm.IsTrue(  TQ.Capacity     = 10  );
    Confirm.IsTrue(  TQ.GrowBy       = 20  );
    Confirm.IsTrue(  TQ.MaxCapacity  = 100 );
    Confirm.IsTrue(  TQ.Count        = 0   );
  finally
    TQ.Free;
  end;
end;

procedure TTaskQueueTest.AddAndRemoveItemsA;
var
  TQ : TTaskQueue;
  Task : TThreadProcedure;
  pr : boolean;
begin
  TQ := TTaskQueue.Create(10,20,100);
  try
    Task := MakeWorkMethodA;
    TQ.Push(Task);
    Task := nil;

    pr := TQ.Pop(Task);
    Confirm.IsTrue(  pr = true       );
    Confirm.IsTrue(  assigned(Task)  );

    pr := TQ.Pop(Task);
    Confirm.IsTrue(  pr = false       );
    Confirm.IsTrue(  not assigned(Task)  );

  finally
    TQ.Free;
  end;
end;

procedure TTaskQueueTest.AddAndRemoveItemsB;
var
  TQ : TTaskQueue;
  Task : TThreadProcedure;
  c1: Integer;
  Count : integer;
begin
  TQ := TTaskQueue.Create(10,20,100);
  try
    for c1 := 0 to 30 do
    begin
      Task := MakeWorkMethodA;
      TQ.Push(Task);
    end;

    Count := 0;
    Task := nil;
    while TQ.Pop(Task) do
    begin
      inc(Count);
    end;

    Confirm.IsTrue(  Count = 31  );
  finally
    TQ.Free;
  end;
end;




end.
