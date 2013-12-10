unit VamLib.Debouncer;

interface

uses
  VamLib.Utils,
  SysUtils,
  Vcl.ExtCtrls;

type
  TProcedure = procedure;
  TProcedureOfObject = procedure of object;


  TDebouncer = class
  private type
    TLastCall = record
      p1 : TProcedure;
      p2 : TProcedureOfObject;
    end;
    TLastCallType = (ctNil, ctProcedure, ctProcedureOfObject, ctAnonymousMethod);
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



end.
