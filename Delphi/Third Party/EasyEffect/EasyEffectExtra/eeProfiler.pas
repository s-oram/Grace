unit eeProfiler;

interface

uses
  Classes,
  Generics.Collections;

{.$DEFINE ProfilerEnabled}


{$SCOPEDENUMS ON}

type
  TSectionTime = record
    SectionName : string;
    CallCount   : integer;
    TotalTime   : single; //in milliseconds.
  end;

  TSectionTimeList = class(TList<TSectionTime>)
  private
  public
    function FindSection(aSectionName : string):integer;
  end;

  TProfiler = class
  private
  protected
    SectionTimeList : TSectionTimeList;

    xSectionRootName : string;
    xSectionName : string;
    xSectionCount : integer;
    xStart : Int64;
    xStop  : Int64;
    xActive : boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure StartTimer(aSectionName : string); overload;
    procedure Starttimer; overload;
    procedure StopTimer;

    procedure SaveReportToFile(aFileName : string);

    procedure GetReport(var aReport : TStringList);
  end;





//==== High Level Methods for Singleton Profiler Instance =====
//procedure GlobalProfiler_Open;
//procedure GlobalProfiler_Close(aReportFileName : string);
//procedure GlobalProfiler_StartTimer(aSectionName : string);
//procedure GlobalProfiler_StopTimer(aSectionName : string);
//=============================================================


type
  GlobalProfiler = class
  private
  protected
    class var fProfiler : TProfiler;
    class var fProfilerCount : integer;
  public
    class procedure Open;
    class procedure Close(aReportFileName : string);
    class procedure StartTimer(aSectionName : string); overload;
    class procedure Starttimer; overload;
    class procedure StopTimer;
  end;


implementation


uses
  SysUtils,
  Windows, VamLib.Utils;

{ TProfiler }

procedure TProfiler.Clear;
begin
  SectionTimeList.Clear;
end;

constructor TProfiler.Create;
begin
  SectionTimeList := TSectionTimeList.Create;
  xActive := false;
end;

destructor TProfiler.Destroy;
begin
  SectionTimeList.Free;
  inherited;
end;



procedure TProfiler.StartTimer(aSectionName: string);
begin
  if xActive then StopTimer;

  xSectionRootName := aSectionName;
  xSectionCount := 1;
  xSectionName := xSectionRootName + '#' + IntToStr(xSectionCount);
  QueryPerformanceCounter(xStart);

  xActive := true;
end;

procedure TProfiler.StartTimer;
begin
  if not (xActive) then raise Exception.Create('Time not active.');

  StopTimer;
  inc(xSectionCount);
  xSectionName := xSectionRootName + '#' + IntToStr(xSectionCount);
  QueryPerformanceCounter(xStart);

  xActive := true;

end;

procedure TProfiler.StopTimer;
var
  Index : integer;
  st : TSectionTime;
  freq:Int64;
begin
  if not (xActive) then raise Exception.Create('Time not active.');


  QueryPerformanceCounter(xStop);
  QueryPerformanceFrequency(freq);

  Index := SectionTimeList.FindSection(xSectionName);
  st := SectionTimeList[Index];

  st.CallCount := st.CallCount + 1;
  st.TotalTime := st.TotalTime + ((xStop - xStart) * 1000 / freq);


  SectionTimeList[Index] := st;

  xActive := false;
end;


procedure TProfiler.SaveReportToFile(aFileName: string);
var
  c1: Integer;
  TotalTime : single;
  Report : TStringList;
  s : string;
begin
  Report := TStringList.Create;
  AutoFree(@Report);

  TotalTime := 0;
  for c1 := 0 to SectionTimeList.Count-1 do
  begin
    TotalTime := TotalTime + SectionTimeList[c1].TotalTime;
  end;


  s := '==== Profiler Data ====';
  Report.Add(s);


  for c1 := 0 to SectionTimeList.Count-1 do
  begin
    s := 'Section: ' + SectionTimeList[c1].SectionName;
    Report.Add(s);

    s := 'Call Count: ' + IntToStr(SectionTimeList[c1].CallCount);
    Report.Add(s);

    s := 'Time Per Call: ' + FloatToStrF(SectionTimeList[c1].TotalTime / SectionTimeList[c1].CallCount, TFloatFormat.ffFixed, 12, 10) + 'ms';
    Report.Add(s);

    s := 'Total Time Used: ' + FloatToStrF(SectionTimeList[c1].TotalTime / TotalTime * 100, TFloatFormat.ffFixed, 5, 2) + '%';
    Report.Add(s);

    s := '   ';
    Report.Add(s);
    Report.Add(s);
  end;

  Report.SaveToFile(aFileName);
end;

procedure TProfiler.GetReport(var aReport: TStringList);
begin
  //TODO:
end;





{ TSectionTimeList }

function TSectionTimeList.FindSection(aSectionName: string): integer;
var
  c1: Integer;
  aSection : TSectionTime;

begin
  for c1 := 0 to self.Count-1 do
  begin
    if self[c1].SectionName = aSectionName then
    begin
      exit(c1);
    end;
  end;

  //If we've made it this far, create a new section,
  aSection.SectionName := aSectionName;
  aSection.CallCount   := 0;
  aSection.TotalTime   := 0;

  self.Add(aSection);
  result := Count-1;

end;





{ TGlobalProfiler }

class procedure GlobalProfiler.Open;
begin
{$IFDEF ProfilerEnabled}
  if not (assigned(fProfiler)) then
  begin
    fProfiler := TProfiler.Create;
    fProfilerCount := 1;
  end else
  begin
    inc(fProfilerCount);
  end;
{$ENDIF}
end;

class procedure GlobalProfiler.Close(aReportFileName: string);
begin
{$IFDEF ProfilerEnabled}
  if fProfilerCount = 1 then
  begin
    if aReportFileName <> ''
      then fProfiler.SaveReportToFile(aReportFileName);
  end;
  dec(fProfilerCount);
  assert(fProfilerCount >= 0);
{$ENDIF}
end;

class procedure GlobalProfiler.StartTimer(aSectionName: string);
begin
{$IFDEF ProfilerEnabled}
  fProfiler.StartTimer(aSectionName);
{$ENDIF}
end;

class procedure GlobalProfiler.StartTimer;
begin
{$IFDEF ProfilerEnabled}
  fProfiler.StartTimer;
{$ENDIF}
end;

class procedure GlobalProfiler.StopTimer;
begin
{$IFDEF ProfilerEnabled}
  fProfiler.StopTimer;
{$ENDIF}
end;

initialization

finalization

end.
