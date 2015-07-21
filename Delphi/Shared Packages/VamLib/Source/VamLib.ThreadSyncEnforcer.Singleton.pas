unit VamLib.ThreadSyncEnforcer.Singleton;

interface

uses
  VamLib.ThreadSyncEnforcer;

implementation

uses
  VamLib.Types;

var
  GlobalEnforcer : IThreadSyncEnforcer;
  CreateLock     : TFixedCriticalSection;

function SyncEnforcerSingleton:IThreadSyncEnforcer;
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

  SyncEnforcerSingleton.Activate;

finalization
  GlobalEnforcer := nil;
  CreateLock.Free;


end.
