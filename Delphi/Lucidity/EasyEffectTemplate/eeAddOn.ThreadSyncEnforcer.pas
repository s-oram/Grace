unit eeAddOn.ThreadSyncEnforcer;

///  ThreadSyncEnforcer is a singleton class to Classes.CheckSynchronize() to run
///  in a DLL based application. Without polling the Class.CheckSynchronize()
///  function any threads using the Queue() or Synchroize() function will fail.
///

interface

type
  IThreadSyncEnforcer = interface
    ['{CB574A00-04B4-4EFD-981A-82D8BE490187}']
    procedure Activate;
    procedure Deactivate;
  end;

function ThreadSyncEnforcer:IThreadSyncEnforcer;

implementation

uses
  VamLib.Types,
  SyncObjs,
  Classes,
  ExtCtrls,
  SysUtils;

type
  TThreadEnforcer = class(TInterfacedObject, IThreadSyncEnforcer)
  private
    cst : TTimer;
    procedure EventHandle_SyncTimer(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Activate;
    procedure Deactivate;
  end;


{ TThreadEnforcer }

constructor TThreadEnforcer.Create;
begin
  cst := TTimer.Create(nil);
  cst.OnTimer := EventHandle_SyncTimer;
  cst.Interval := 50;
end;

destructor TThreadEnforcer.Destroy;
begin
  cst.Free;
  inherited;
end;

procedure TThreadEnforcer.Activate;
begin
  cst.Enabled := true;
end;

procedure TThreadEnforcer.Deactivate;
begin
  cst.Enabled := false;
end;

procedure TThreadEnforcer.EventHandle_SyncTimer(Sender: TObject);
begin
  // It's important to call CheckSynchronize for any TThreads that want to
  // use their Synchroize() or Queue() methods. CheckSynchronize isn't called
  // in DLLs by default. Perhaps this could be called from the VST GUI idle function.
  // dunno.
  // TODO:HIGH this works for now but seems to be a heavy handed way.
  Classes.CheckSynchronize(10);

  // TODO:HIGH look at the comment for Classes.WakeMainThread variable.
  // this seems to suggest I need to have a global meta GUI class that
  // is dedicated to calling  Classes.CheckSynchronize(). It would
  // also assign a method to the Classes.WakeMainThread() variable.
  // There is also the WaitForMultipleObjects reference that suggests calling
  // CheckSynchronize() every 50ms is not the most efficient way to all this.
end;


var
  GlobalEnforcer : IThreadSyncEnforcer;
  CreateLock     : TFixedCriticalSection;

function ThreadSyncEnforcer:IThreadSyncEnforcer;
begin
  CreateLock.Enter;
  try
    if not assigned(GlobalEnforcer) then GlobalEnforcer := TThreadEnforcer.Create;
  finally
    CreateLock.Leave;
  end;
  result := GlobalEnforcer;
end;

initialization
  CreateLock := TFixedCriticalSection.Create;
finalization
  GlobalEnforcer := nil;
  CreateLock.Free;

end.
