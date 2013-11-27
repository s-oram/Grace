unit eeProfilerV2;

interface

uses
  Classes,
  Generics.Collections,
  eeSyncObjects,
  eeProfilerV2.Form,
  B2.Filter.CriticallyDampedLowpass;

{.$DEFINE ProfilerEnabled}


{$SCOPEDENUMS ON}


type
  //=== forward declarations ==
  TProfilerBrain = class;


  // TProfiler is the main profiling class.
  TProfiler = class
  private
  protected
    class var fProfiler : TProfilerBrain;
    class var fProfilerCount : integer;
  public
    class procedure Open;
    class procedure Close; overload;
    class procedure Close(aReportFileName : string); overload;

    class procedure StartTimer(aSectionName : string);
    class procedure StopTimer(aSectionName : string);
  end;



  //============================================================================
  //    Private Internal Use Only
  //============================================================================
  TSectionTime = class
  strict private
    fSectionName: string;
    fCallCount: integer;
    fTotalTime: single;
    fIsActive: boolean;
    fStartTime: Int64;
    fStopTime: Int64;
    fMaxtime: single;
    fMintime: single;
    fSmoothedTime: single;
  private

  public
    LowPassFilter : TCriticallyDampedLowpass;

    constructor Create;
    destructor Destroy; override;

    property SectionName : string    read fSectionName write fSectionName;
    property CallCount   : integer   read fCallCount   write fCallCount;
    property TotalTime   : single    read fTotalTime   write fTotalTime;  //in milliseconds.

    property MinTime : single read fMintime write fMinTime;
    property MaxTime : single read fMaxtime write fMaxTime;
    property SmoothedTime : single read fSmoothedTime write fSmoothedTime;

    property IsActive  : boolean read fIsActive  write fIsActive; //is this timer currently active.
    property StartTime : Int64   read fStartTime write fStartTime;
    property StopTime  : Int64   read fStopTime  write fStopTime;
  end;

  TSectionTimeList = class(TObjectList<TSectionTime>)
  private
  public
    function FindSection(aSectionName : string; CreateSection : boolean):TSectionTime;
  end;

  TProfilerBrain = class
  private
  protected
    SectionTimeList : TSectionTimeList;
    ListLock : TFixedCriticalSection;
    Form : TProfillerForm;
    procedure EventHandle_GetReportData(Sender : TObject; var Data:TStringList);
    procedure EventHandle_Reset(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure StartTimer(aSectionName : string);
    procedure StopTimer(aSectionName : string);

    procedure SaveReportToFile(aFileName : string);

    procedure GetReport(var aReport : TStringList);
  end;









implementation


uses
  SysUtils, Math,
  Windows, uAutoFree;

{ TProfiler }

procedure TProfilerBrain.Clear;
begin
  SectionTimeList.Clear;
end;

constructor TProfilerBrain.Create;
begin
  ListLock := TFixedCriticalSection.Create;

  SectionTimeList := TSectionTimeList.Create;
  Form := TProfillerForm.Create(nil);
  Form.OnGetReportData := self.EventHandle_GetReportData;
  Form.OnReset         := self.EventHandle_Reset;
  Form.Show;
end;

destructor TProfilerBrain.Destroy;
begin
  ListLock.Free;
  SectionTimeList.Free;
  Form.Free;
  inherited;
end;

procedure TProfilerBrain.EventHandle_GetReportData(Sender: TObject; var Data: TStringList);
begin
  self.GetReport(Data);
end;

procedure TProfilerBrain.EventHandle_Reset(Sender: TObject);
begin
  ListLock.Acquire;
  try
    SectionTimeList.Clear;
  finally
    ListLock.Release;
  end;
end;

procedure TProfilerBrain.StartTimer(aSectionName: string);
var
  st : TSectionTime;
  xStart : Int64;
begin
  ListLock.Acquire;
  try
    st := SectionTimeList.FindSection(aSectionName, true);
    if st.IsActive then raise Exception.Create('This Timer is already active (' + aSectionName + ').');

    QueryPerformanceCounter(xStart);
    st.StartTime := xStart;
    st.IsActive  := true;
  finally
    ListLock.Release;
  end;
end;

procedure TProfilerBrain.StopTimer(aSectionName : string);
var
  Index : integer;
  st : TSectionTime;
  freq:Int64;
  xStop : Int64;
  xSectionTime : single;
begin
  ListLock.Acquire;
  try
    QueryPerformanceCounter(xStop);
    QueryPerformanceFrequency(freq);

    st := SectionTimeList.FindSection(aSectionName, false);
    if not assigned(st) then exit;
    if not st.IsActive then raise Exception.Create('This Timer is not active (' + aSectionName + ').');

    xSectionTime := ((xStop - st.StartTime) * 1000 / freq);

    if st.CallCount = 0 then
    begin
      st.MinTime := xSectionTime;
      st.MaxTime := xSectionTime;
    end else
    begin
      st.MinTime := Min(st.MinTime, xSectionTime);
      st.MaxTime := Max(st.MaxTime, xSectionTime);
    end;

    st.SmoothedTime := st.LowPassFilter.Step(xSectionTime);

    st.CallCount := st.CallCount + 1;
    st.TotalTime := st.TotalTime + xSectionTime;
    st.IsActive  := false;
  finally
    ListLock.Release;
  end;
end;


procedure TProfilerBrain.SaveReportToFile(aFileName: string);
var
  Report : TStringList;
begin
  Report := TStringList.Create;
  AutoFree(@Report);

  GetReport(Report);

  Report.SaveToFile(aFileName);
end;

procedure TProfilerBrain.GetReport(var aReport: TStringList);
var
  c1: Integer;
  TotalTime : single;
  s : string;
  AverageTime : single;
  MinPercent, MaxPercent : single;
  SmoothedPercent : single;
  st : TSectionTime;
begin
  TotalTime := 0;
  for c1 := 0 to SectionTimeList.Count-1 do
  begin
    TotalTime := TotalTime + SectionTimeList[c1].TotalTime;
  end;


  s := '==== Profiler Data ====';
  aReport.Add(s);


  for c1 := 0 to SectionTimeList.Count-1 do
  begin
    st := SectionTimeList[c1];



    MinPercent := (st.MinTime / st.SmoothedTime);
    MaxPercent := (st.MaxTime / st.SmoothedTime);


    s := 'Section: ' + st.SectionName;
    aReport.Add(s);

    s := 'Call Count: ' + IntToStr(st.CallCount);
    aReport.Add(s);

    s := 'Min Time: ' + FloatToStrF(MinPercent, TFloatFormat.ffFixed, 12, 10) + ' ms';
    aReport.Add(s);

    s := 'Max Time: ' + FloatToStrF(MaxPercent, TFloatFormat.ffFixed, 12, 10);
    aReport.Add(s);

    s := 'Smoothed Time: ' + FloatToStrF(st.SmoothedTime, TFloatFormat.ffFixed, 12, 10);
    aReport.Add(s);









    {
    AverageTime := st.TotalTime / st.CallCount;
    MinPercent := (st.MinTime / AverageTime) * 100;
    MaxPercent := (st.MaxTime / AverageTime) * 100;
    SmoothedPercent := (st.SmoothedTime / AverageTime) * 100;


    s := 'Section: ' + st.SectionName;
    aReport.Add(s);

    s := 'Call Count: ' + IntToStr(st.CallCount);
    aReport.Add(s);

    s := 'Time Per Call: ' + FloatToStrF(AverageTime, TFloatFormat.ffFixed, 12, 10) + ' ms';
    aReport.Add(s);

    s := 'Min Time: ' + FloatToStrF(MinPercent, TFloatFormat.ffFixed, 12, 10) + '%';
    aReport.Add(s);

    s := 'Max Time: ' + FloatToStrF(MaxPercent, TFloatFormat.ffFixed, 12, 10) + '%';
    aReport.Add(s);

    s := 'Smoothed Time: ' + FloatToStrF(SmoothedPercent, TFloatFormat.ffFixed, 12, 10) + '%';
    aReport.Add(s);

    s := 'Total Time Used: ' + FloatToStrF(st.TotalTime / TotalTime * 100, TFloatFormat.ffFixed, 5, 2) + '%';
    aReport.Add(s);
    }


    s := '   ';
    aReport.Add(s);
    aReport.Add(s);
  end;
end;





{ TSectionTimeList }

function TSectionTimeList.FindSection(aSectionName: string; CreateSection : boolean): TSectionTime;
var
  c1: Integer;
  aSection : TSectionTime;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if self[c1].SectionName = aSectionName then
    begin
      result := self[c1];
      exit; //==================================exit==========>>
    end;
  end;



  // the section hasn't been found if we've made it this far.
  if CreateSection then
  begin
    aSection := TSectionTime.Create;
    aSection.SectionName := aSectionName;
    aSection.CallCount   := 0;
    aSection.TotalTime   := 0;

    Add(aSection);

    result := aSection;
  end else
  begin
    result := nil;
  end;




end;





{ TGlobalProfiler }

class procedure TProfiler.Open;
begin
{$IFDEF ProfilerEnabled}
  if not (assigned(fProfiler)) then
  begin
    fProfiler := TProfilerBrain.Create;
    fProfilerCount := 1;
  end else
  begin
    inc(fProfilerCount);
  end;
{$ENDIF}
end;

class procedure TProfiler.Close;
begin
{$IFDEF ProfilerEnabled}
  if fProfilerCount = 1 then
  begin
    FreeAndNil(fProfiler);
  end;
  dec(fProfilerCount);
  assert(fProfilerCount >= 0);
{$ENDIF}
end;



class procedure TProfiler.Close(aReportFileName: string);
begin
{$IFDEF ProfilerEnabled}
  if fProfilerCount = 1 then
  begin
    assert(aReportFileName <> '');
    fProfiler.SaveReportToFile(aReportFileName);
    FreeAndNil(fProfiler);
  end;
  dec(fProfilerCount);
  assert(fProfilerCount >= 0);
{$ENDIF}
end;

class procedure TProfiler.StartTimer(aSectionName: string);
begin
{$IFDEF ProfilerEnabled}
  fProfiler.StartTimer(aSectionName);
{$ENDIF}
end;

class procedure TProfiler.StopTimer(aSectionName : string);
begin
{$IFDEF ProfilerEnabled}
  fProfiler.StopTimer(aSectionName);
{$ENDIF}
end;

{ TSectionTime }

constructor TSectionTime.Create;
begin
  LowPassFilter := TCriticallyDampedLowpass.Create;
  LowPassFilter.SetCutoff(0.01, 2);
  SmoothedTime := 0;
end;

destructor TSectionTime.Destroy;
begin
  LowPassFilter.Free;
  inherited;
end;

initialization

finalization

end.
