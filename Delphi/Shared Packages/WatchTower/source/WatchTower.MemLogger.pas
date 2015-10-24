unit WatchTower.MemLogger;

interface

type
  // Use MemLogger to monitor memory activity.
  MemLogger = record
  public
    class procedure Install; static;
    class procedure Uninstall; static;

    class procedure Reset; static;
    class procedure GetActivity(out _GetMemCount, _FreeMemCount, _ReallocMemCount, _AllocMemCount : integer); static;
    class function GetTotalActivity : integer; static;
  end;

implementation

uses
  System.SyncObjs,
  StrUtils,
  SysUtils;

var
  IsInstalled : integer;
  OldMemMgr : TMemoryManagerEx;
  NewMemMgr : TMemoryManagerEx;
  GetMemCount : integer;
  FreeMemCount : integer;
  ReallocMemCount : integer;
  AllocMemCount : integer;


function Logging_GetMem(Size: NativeInt): Pointer;
begin
  result := OldMemMgr.GetMem(Size);
  TInterlocked.Increment(GetMemCount);
end;

function Logging_FreeMem(P: Pointer): Integer;
begin
  result := OldMemMgr.FreeMem(p);
  TInterlocked.Increment(FreeMemCount);
end;

function Logging_ReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  result := OldMemMgr.ReallocMem(P, Size);
  TInterlocked.Increment(ReallocMemCount);
end;

function Logging_AllocMem(Size: NativeInt): Pointer;
begin
  result := OldMemMgr.AllocMem(Size);
  TInterlocked.Increment(AllocMemCount);
end;

function Logging_RegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  result := OldMemMgr.RegisterExpectedMemoryLeak(P);
end;

function Logging_UnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  result := OldMemMgr.UnregisterExpectedMemoryLeak(P);
end;

{ LoggingMemoryManager }

class procedure MemLogger.Reset;
begin
  GetMemCount := 0;
  FreeMemCount := 0;
  ReallocMemCount := 0;
  AllocMemCount := 0;
end;

class procedure MemLogger.GetActivity(out _GetMemCount, _FreeMemCount, _ReallocMemCount, _AllocMemCount: integer);
begin
  _GetMemCount     := GetMemCount;
  _GetMemCount     := GetMemCount;
  _FreeMemCount    := FreeMemCount;
  _ReallocMemCount := ReallocMemCount;
  _AllocMemCount   := AllocMemCount;
end;

class function MemLogger.GetTotalActivity: integer;
begin
  result := GetMemCount + FreeMemCount + ReallocMemCount + AllocMemCount;
end;

class procedure MemLogger.Install;
begin
  if TInterLocked.Increment(IsInstalled) = 1 then
  begin
    GetMemCount := 0;
    FreeMemCount := 0;
    ReallocMemCount := 0;
    AllocMemCount := 0;

    NewMemMgr.GetMem := Logging_GetMem;
    NewMemMgr.FreeMem := Logging_FreeMem;
    NewMemMgr.ReallocMem := Logging_ReallocMem;
    NewMemMgr.AllocMem := Logging_AllocMem;
    NewMemMgr.RegisterExpectedmemoryLeak :=   Logging_RegisterExpectedMemoryLeak;
    NewMemMgr.UnregisterExpectedmemoryLeak := Logging_UnregisterExpectedMemoryLeak;

    GetMemoryManager(OldMemMgr);
    SetMemoryManager(NewMemMgr);
  end;
end;

class procedure MemLogger.Uninstall;
begin
  if TInterLocked.Decrement(IsInstalled) = 0 then
  begin
    SetMemoryManager(OldMemMgr);
  end;
end;

initialization
  IsInstalled := 0;

end.
