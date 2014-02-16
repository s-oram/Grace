unit VamLib.Debouncer;

interface

uses
  VamLib.Types,
  VamLib.Utils,
  VamLib.UniqueID,
  VamLib.Collections.Lists,
  SysUtils,
  Vcl.ExtCtrls;

type
  TProcedure = procedure;
  TProcedureOfObject = procedure of object;

  TLastCallType = (ctNil, ctProcedure, ctProcedureOfObject, ctAnonymousMethod);
  TDebounceEdge = (deLeading, deTrailing, deBoth);

  PDebounceInfo = ^TDebounceInfo;
  TDebounceInfo = record
    ID       : TUniqueID;
    Delay    : integer; //milliseconds debouncing delay time.
    CallTime : TDateTime;
    CallRefA : TProcedure;
    CallRefB : TProcedureOfObject;
    CallRefC : TProc;
    CallType : TLastCallType;
    Edge     : TDebounceEdge;
    IsExpired : boolean;
  end;

  TDebounceInfoList = class(TSimpleRecordList<TDebounceInfo>)
  public
    function FindOrCreate(ID : TUniqueID):PDebounceInfo;
    function Find(ID : TUniqueID):PDebounceInfo;
  end;

  TDebounceController = class
  private
  protected
    Timer : TTimer;
    InfoList : TDebounceInfoList;

    procedure ProcessDebounceRequests(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedure); overload;
    procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedureOfObject); overload;
    procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProc); overload;
  end;


  TDebouncer = class
  private type
    TLastCall = record
      p1 : TProcedure;
      p2 : TProcedureOfObject;
    end;

  private var
    Timer         : TTimer;
    fDebounceTime : cardinal;

    IsActive      : boolean;
    LastCall      : TLastCall;
    LastCallType  : TLastCallType;

    cs : TFixedCriticalSection;

    procedure SetDebounceTime(const Value: cardinal);
    procedure Reset(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Debounce(p : TProcedure); overload;
    procedure Debounce(p : TProcedureOfObject); overload;

    property DebounceTime : cardinal read fDebounceTime write SetDebounceTime;
  end;



implementation

uses
  DateUtils;

{ TDebouncer }

constructor TDebouncer.Create;
begin
  Timer := TTimer.Create(nil);
  Timer.Enabled := false;
  fDebounceTime  := 100;
  Timer.Interval := fDebounceTime;
  Timer.OnTimer := Reset;

  cs := TFixedCriticalSection.Create;
end;

destructor TDebouncer.Destroy;
begin
  Timer.Free;
  cs.Free;
  inherited;
end;

procedure TDebouncer.SetDebounceTime(const Value: cardinal);
begin
  if value <> fDebounceTime then
  begin
    fDebounceTime  := Value;
    Timer.Interval := Value;
  end;
end;

procedure TDebouncer.Debounce(p: TProcedure);
begin
  cs.Acquire;
  try
    if IsActive = false then
    begin
      IsActive := true;
      p;
      LastCallType := ctNil;
      Timer.Enabled := true;
    end else
    begin
      assert(IsActive = true);
      LastCallType := ctProcedure;
      LastCall.p1 := p;
    end;
  finally
    cs.Release;
  end;

end;

procedure TDebouncer.Debounce(p: TProcedureOfObject);
begin
  cs.Acquire;
  try
    if IsActive = false then
    begin
      IsActive := true;
      p;
      LastCallType := ctNil;
      Timer.Enabled := true;
    end else
    begin
      assert(IsActive = true);
      LastCallType := ctProcedureOfObject;
      LastCall.p2 := p;
    end;
  finally
    cs.Release;
  end;
end;

procedure TDebouncer.Reset(Sender: TObject);
begin
  cs.Acquire;
  try
    case LastCallType of
      ctNil:
      begin
        Timer.Enabled := false;
        IsActive := false;
      end;

      ctProcedure:
      begin
        Timer.Enabled := false;
        LastCall.p1;
        LastCall.p1 := nil;
        LastCallType := ctNil;
        Timer.Enabled := true;
      end;

      ctProcedureOfObject:
      begin
        Timer.Enabled := false;
        LastCall.p2;
        LastCall.p2 := nil;
        LastCallType := ctNil;
        Timer.Enabled := true;
      end;

      ctAnonymousMethod:
      begin

      end;
    else
      raise Exception.Create('Type not handled.');
    end;
  finally
    cs.Release;
  end;
end;



{ TDebounceInfoInfo }

function TDebounceInfoList.Find(ID: TUniqueID): PDebounceInfo;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if self[c1].ID = ID
      then exit(@self.Raw[c1]);
  end;

  result := nil;
end;

function TDebounceInfoList.FindOrCreate(ID: TUniqueID): PDebounceInfo;
var
  c1: Integer;
  ptr : PDebounceInfo;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if self[c1].ID = ID
      then exit(@self.Raw[c1]);
  end;

  ptr := New;
  ptr^.ID := ID;
  result := ptr;
end;

{ TDebounceController }

constructor TDebounceController.Create;
begin
  Timer := TTimer.Create(nil);
  Timer.Interval := 1;
  Timer.Enabled := false;
  Timer.OnTimer := ProcessDebounceRequests;
  InfoList := TDebounceInfoList.Create;
end;

destructor TDebounceController.Destroy;
begin
  Timer.Free;
  InfoList.Free;

  inherited;
end;

procedure TDebounceController.Debounce(ID: TUniqueID; Time: integer; Edge: TDebounceEdge; p: TProcedure);
var
  Info : PDebounceInfo;
begin
  Info := InfoList.Find(ID);

  if assigned(Info) then
  begin
    Info.ID       := ID;
    Info.CallTime := Now;
    Info.Delay    := Time;
    Info.CallRefA := p;
    Info.CallType := ctProcedure;
    Info.Edge     := Edge;
    Info.IsExpired := false;
  end else
  begin
    Info          := InfoList.FindOrCreate(ID);
    Info.ID       := ID;
    Info.CallTime := Now;
    Info.Delay    := Time;
    Info.CallRefA := p;
    Info.CallType := ctProcedure;
    Info.Edge     := Edge;
    Info.IsExpired := false;

    if (Edge = deLeading) or (Edge = deBoth) then
    begin
      p();
    end;
  end;

  if not Timer.Enabled then Timer.Enabled := true;
end;

procedure TDebounceController.Debounce(ID: TUniqueID; Time: integer; Edge: TDebounceEdge; p: TProcedureOfObject);
var
  Info : PDebounceInfo;
begin
  Info := InfoList.Find(ID);

  if assigned(Info) then
  begin
    Info.ID       := ID;
    Info.CallTime := Now;
    Info.Delay    := Time;
    Info.CallRefB := p;
    Info.CallType := ctProcedureOfObject;
    Info.Edge     := Edge;
    Info.IsExpired := false;
  end else
  begin
    Info          := InfoList.FindOrCreate(ID);
    Info.ID       := ID;
    Info.CallTime := Now;
    Info.Delay    := Time;
    Info.CallRefB := p;
    Info.CallType := ctProcedureOfObject;
    Info.Edge     := Edge;
    Info.IsExpired := false;

    if (Edge = deLeading) or (Edge = deBoth) then
    begin
      p();
    end;
  end;

  if not Timer.Enabled then Timer.Enabled := true;
end;

procedure TDebounceController.Debounce(ID: TUniqueID; Time: integer; Edge: TDebounceEdge; p: TProc);
var
  Info : PDebounceInfo;
begin
  Info := InfoList.Find(ID);

  if assigned(Info) then
  begin
    Info.ID       := ID;
    Info.CallTime := Now;
    Info.Delay    := Time;
    Info.CallRefC := p;
    Info.CallType := ctAnonymousMethod;
    Info.Edge     := Edge;
    Info.IsExpired := false;
  end else
  begin
    Info          := InfoList.FindOrCreate(ID);
    Info.ID       := ID;
    Info.CallTime := Now;
    Info.Delay    := Time;
    Info.CallRefC := p;
    Info.CallType := ctAnonymousMethod;
    Info.Edge     := Edge;
    Info.IsExpired := false;

    if (Edge = deLeading) or (Edge = deBoth) then
    begin
      p();
    end;
  end;

  if not Timer.Enabled then Timer.Enabled := true;
end;

procedure TDebounceController.ProcessDebounceRequests(Sender: TObject);
var
  c1: Integer;
  Info : PDebounceInfo;
  ms : integer;
begin
  for c1 := 0 to InfoList.Count-1 do
  begin
    Info := @InfoList.Raw[c1];

    ms := MilliSecondsBetween(Now, Info^.CallTime);

    if (Info^.Delay < ms) then
    begin
      if (Info^.Edge = deTrailing) or (Info^.Edge = deBoth) then
      begin
        case Info^.CallType of
          ctProcedure:         Info^.CallRefA();
          ctProcedureOfObject: Info^.CallRefB();
          ctAnonymousMethod:   Info^.CallRefC();
        end;
      end;

      Info^.IsExpired := true;
    end;
  end;

  for c1 := InfoList.Count-1 downto 0 do
  begin
    InfoList.Delete(c1);
  end;
end;




end.
