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
  //============================================================================
  //    low level implementation details.
  //============================================================================

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
    //TODO: Use high speed timer here.
    Timer : TTimer;
    InfoList : TDebounceInfoList;

    procedure ProcessDebounceRequests(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedure); overload;
    procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedureOfObject); overload;
    procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProc); overload;

    procedure DebounceCancel(const ID : TUniqueID);
  end;





//============================================================================
//    Global Highlevel functions
//============================================================================

procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedure); overload;
procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedureOfObject); overload;
procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProc); overload;

procedure DebounceCancel(const ID : TUniqueID);

//============================================================================



implementation

uses
  DateUtils;

var
  GlobalDebouncer : TDebounceController;

procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedure);
begin
  if not assigned(GlobalDebouncer) then GlobalDebouncer := TDebounceController.Create;

  GlobalDebouncer.Debounce(ID, Time, Edge, p);
end;

procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProcedureOfObject);
begin
  if not assigned(GlobalDebouncer) then GlobalDebouncer := TDebounceController.Create;

  GlobalDebouncer.Debounce(ID, Time, Edge, p);
end;

procedure Debounce(ID : TUniqueID; Time : integer; Edge : TDebounceEdge; p : TProc);
begin
  if not assigned(GlobalDebouncer) then GlobalDebouncer := TDebounceController.Create;

  GlobalDebouncer.Debounce(ID, Time, Edge, p);
end;

procedure DebounceCancel(const ID : TUniqueID);
begin
  if not assigned(GlobalDebouncer) then exit;

  GlobalDebouncer.DebounceCancel(ID);
end;






{ TDebounceInfoList }

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

procedure TDebounceController.DebounceCancel(const ID: TUniqueID);
var
  c1 : integer;
begin
  for c1 := InfoList.Count-1 downto 0 do
  begin
    if InfoList[c1].ID = ID
      then InfoList.Delete(c1);
  end;
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
    if InfoList[c1].IsExpired
      then InfoList.Delete(c1);
  end;

  if InfoList.Count = 0 then
  begin
    Timer.Enabled := false;
  end;

end;



initialization

finalization
  if assigned(GlobalDebouncer) then GlobalDebouncer.Free;
  


end.
