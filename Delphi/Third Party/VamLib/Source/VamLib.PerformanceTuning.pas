unit VamLib.PerformanceTuning;

interface

uses
  SysUtils;


type
  TPerformanceTimer = record
  strict private
    InitialTime : Int64;
    fLastResult : single;
  public
    procedure Start;
    function Stop:single;
    function LastResult : single;
  end;


function StartTimer:integer;
function StopTimer(StartTime:integer):integer;

function StartTimerMS:Int64;
function StopTimerMS(StartTime:Int64):single;

function CalcPerformanceDifference(WorkA, WorkB, ResetMethod : TProc):string;


implementation

uses
  WinApi.Windows;

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



{ TPerformanceTimer }

function TPerformanceTimer.LastResult: single;
begin
  result := fLastResult;
end;

procedure TPerformanceTimer.Start;
begin
  QueryPerformanceCounter(InitialTime);
end;

function TPerformanceTimer.Stop: single;
var
  CurrentTime:Int64;
  freq:Int64;
begin
  QueryPerformanceCounter(CurrentTime);
  QueryPerformanceFrequency(freq);

  result := (CurrentTime - InitialTime) * 1000 / freq;
  fLastResult := result;
end;



function CalcPerformanceDifference(WorkA, WorkB, ResetMethod : TProc):string;
var
  CounterA, CounterB : TPerformanceTimer;
  c1: Integer;
begin
  if assigned(ResetMethod) then ResetMethod;
  WorkA;
  WorkB;



  if assigned(ResetMethod) then ResetMethod;
  CounterA.Start;
  WorkA;
  CounterA.Stop;

  if assigned(ResetMethod) then ResetMethod;
  CounterB.Start;
  WorkB;
  CounterB.Stop;

  if CounterA.LastResult = CounterB.LastResult then
  begin
    result := 'WorkA and WorkB take the same amount of time.';
  end else
  if CounterA.LastResult < CounterB.LastResult then
  begin
    if CounterA.LastResult <> 0
      then result := 'WorkA is ' + FloatToStr(CounterB.LastResult / CounterA.LastResult) + ' times faster.'
      else result := 'WorkA is unmeasurably faster.';
  end else
  begin
    if CounterB.LastResult <> 0
      then result := 'WorkB is ' + FloatToStr(CounterA.LastResult / CounterB.LastResult) + ' times faster.'
      else result := 'WorkB is unmeasurably faster.';
  end;
end;

end.
