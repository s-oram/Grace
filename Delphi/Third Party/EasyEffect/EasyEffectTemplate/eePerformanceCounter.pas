unit eePerformanceCounter;

interface

type
  TPerformanceCounter = class
  private
  public
    constructor Create;
	  destructor Destroy; override;

    function Start:Int64;
    function Stop(StartTime:Int64):single;
  end;

var
  GlobalPerformanceCounter:TPerformanceCounter;

implementation

uses
  Windows, eePerformanceCounterForm, Forms;

var
  PcForm:TPerformanceCounterForm;

{ TPerformanceCounter }

constructor TPerformanceCounter.Create;
begin

end;

destructor TPerformanceCounter.Destroy;
begin

  inherited;
end;

function TPerformanceCounter.Start: Int64;
var
  pc:Int64;
begin
  QueryPerformanceCounter(pc);
  result := pc;
end;

function TPerformanceCounter.Stop(StartTime: Int64): single;
var
  pc:Int64;
  freq:Int64;
  Time:single;
begin
  QueryPerformanceCounter(pc);
  QueryPerformanceFrequency(freq);

  Time := (pc - startTime) * 1000 / freq;

  PcForm.LastTime := Time;
  if Time > PcForm.MaxTime then PcForm.MaxTime := Time;  
end;




initialization
  PcForm := TPerformanceCounterForm.Create(nil);
  GlobalPerformanceCounter := TPerformanceCounter.Create;



  PcForm.Show;
  PcForm.Left := Screen.Width - PcForm.Width;
  PcForm.Top  := 0;


finalization
  GlobalPerformanceCounter.Free;
  pcForm.Free;
end.
