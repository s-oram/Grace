unit CpuCycleTimer;

interface

function StartTimer:integer;
function StopTimer(StartTime:integer):integer;

function StartTimerMS:Int64;
function StopTimerMS(StartTime:Int64):single;

type
  TCycleTimer = class
  private
    StartTime:Int64;
  public
    procedure Start;
    function Stop:single;
  end;


implementation

uses
  Windows;

function RDTSC: Int64; assembler;
asm
  RDTSC  // result Int64 in EAX and EDX
end;

function StartTimer:integer;
begin
  result := RDTSC;
end;

function StopTimer(StartTime:integer):integer;
begin
  result := RDTSC - StartTime - 92;
end;


function StartTimerMS:Int64;
var
  pc:Int64;
begin
  QueryPerformanceCounter(pc);
  result := pc;
end;


function StopTimerMS(StartTime:Int64):single;
var
  pc:Int64;
  freq:Int64;
begin
  QueryPerformanceCounter(pc);
  QueryPerformanceFrequency(freq);

  result := (pc - startTime) * 1000 / freq;

end;






{ TCycleTimer }

procedure TCycleTimer.Start;
begin
  QueryPerformanceCounter(StartTime);
end;

function TCycleTimer.Stop: single;
var
  StopTime:Int64;
  Freq:Int64;
begin
  QueryPerformanceCounter(StopTime);
  QueryPerformanceFrequency(Freq);

  result := (StopTime - StartTime) * 1000 / Freq;
end;

end.
