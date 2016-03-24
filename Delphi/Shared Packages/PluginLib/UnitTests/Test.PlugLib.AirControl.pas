unit Test.PlugLib.AirControl;

interface

uses
  WatchTower,
  PlugLib.AirControl;

type
  TAirControlTest = class(TWatchTowerTest)
  private
    BackgroundTaskCount : integer;
    AudioTaskCount      : integer;
    GuiTaskCount        : integer;
    procedure BackgroundTask;
    procedure AudioTask;
    procedure GuiTask;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure Basics;

    [Test]
    procedure ExecuteBackgroundTask;

    [Test]
    procedure ExecuteBackgroundTaskWithCallbackTasks;
  end;



implementation

uses
  SysUtils,
  Classes,
  WatchTower.Confirm;

{ TTaskQueueTest }

procedure TAirControlTest.Setup;
begin
  inherited;

  BackgroundTaskCount  := 0;
  AudioTaskCount := 0;
  GuiTaskCount   := 0;
end;

procedure TAirControlTest.TearDown;
begin
  inherited;

end;

procedure TAirControlTest.GuiTask;
begin
  inc(GuiTaskCount);
end;

procedure TAirControlTest.BackgroundTask;
begin
  inc(BackgroundTaskCount);
end;

procedure TAirControlTest.AudioTask;
begin
  inc(AudioTaskCount);
end;



procedure TAirControlTest.Basics;
var
  Worker : TAirControl;
begin
  Worker := TAirControl.Create(10,10,100);
  Sleep(100);
  Worker.Free;
end;



procedure TAirControlTest.ExecuteBackgroundTask;
var
  Worker : TAirControl;
begin
  Worker := TAirControl.Create(10,10,100);
  try
    Worker.AddTask(BackgroundTask, nil, nil);
    Sleep(100);
    Confirm.IsTrue(  BackgroundTaskCount = 1  );

    Worker.AddTask(BackgroundTask, nil, nil);
    Sleep(100);
    Confirm.IsTrue(  BackgroundTaskCount = 2  );

    Worker.AddTask(BackgroundTask, nil, nil);
    Sleep(100);
    Confirm.IsTrue(  BackgroundTaskCount = 3  );

  finally
    Worker.Free;
  end;
end;

procedure TAirControlTest.ExecuteBackgroundTaskWithCallbackTasks;
var
  Worker : TAirControl;
begin
  Worker := TAirControl.Create(10,10,100);
  try
    Worker.AddTask(BackgroundTask, nil, nil);
    Sleep(100);
    Worker.ProcessAudioSync;
    Worker.ProcessGuiSync;
    Confirm.IsTrue(  BackgroundTaskCount = 1  );
    Confirm.IsTrue(  AudioTaskCount = 0       );
    Confirm.IsTrue(  GuiTaskCount   = 0       );

    Worker.AddTask(BackgroundTask, AudioTask, nil);
    Sleep(100);
    Worker.ProcessAudioSync;
    Worker.ProcessGuiSync;
    Confirm.IsTrue(  BackgroundTaskCount = 2  );
    Confirm.IsTrue(  AudioTaskCount = 1       );
    Confirm.IsTrue(  GuiTaskCount   = 0       );

    Worker.AddTask(BackgroundTask, nil, GuiTask);
    Sleep(100);
    Worker.ProcessAudioSync;
    Worker.ProcessGuiSync;
    Confirm.IsTrue(  BackgroundTaskCount = 3  );
    Confirm.IsTrue(  AudioTaskCount = 1       );
    Confirm.IsTrue(  GuiTaskCount   = 1       );

  finally
    Worker.Free;
  end;
end;

end.
