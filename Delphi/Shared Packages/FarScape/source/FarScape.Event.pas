unit FarScape.Event;

interface

type
  TCustomEvent = class;
  TEventClass = class of TCustomEvent;

  TDataByte = record
  case Byte of
    0: (VInteger : integer);
    1: (VBoolean : boolean);
    2: (VPointer : pointer);
    3: (VSingle  : single);
  end;

  TEventBase = class(TObject)
  protected
    FEventClass : TEventClass;
    FTarget     : TObject;
    FIsHandled  : boolean;
    FPropagate  : boolean;
    FData       : array[0..3] of TDataByte;
  public
    property EventClass : TEventClass read FEventClass; // The type of event. Important for reading the data value.
    property Target     : TObject     read FTarget;
    property IsHandled  : boolean     read FIsHandled;
    property Propagate  : boolean     read FPropagate;
  end;

  // All Farscape events descend from TCustomEvent.
  TCustomEvent = class(TEventBase)
  private
  protected
    function GetDataAsObject(const Index: Integer): TObject;
    procedure SetDataAsObject(const Index : integer; const Value: TObject);

    function GetDataAsPointer(const Index: Integer): Pointer;
    procedure SetDataAsPointer(const Index : integer; const Value: Pointer);

    function GetDataAsInteger(const Index: Integer): integer;
    procedure SetDataAsInteger(const Index, Value: integer);

    function GetDataAsSingle(const Index: Integer): single;
    procedure SetDataAsSingle(const Index: integer; const Value: single);

    function GetDataAsBoolean(const Index: Integer): boolean;
    procedure SetDataAsBoolean(const Index : integer; const Value: boolean);

    function GetDataAsPString(const Index: Integer): PString;
    procedure SetDataAsPString(const Index : integer; const Value: PString);
  protected
  public
  end;

  // Events will be passed around as TEventData. They will need to be cast to the right TCustomEvent descendant class by the
  // application event handlers.
  TEventData = class(TEventBase)
  private
    FRefCount: Integer;
  protected
  public
    constructor Create(const aMsgClass : TEventClass);
    destructor Destroy; override; final;
    function Release:integer; // Reduce reference count. Release to global stack when RefCount is 0.
    function AddRef:integer;  // Add to reference count.
    property RefCount: Integer read FRefCount; // Reference count will automatically be set to 1 when a new event is created.

    class function NewInstance: TObject; override;
  end;




implementation

uses
  SysUtils,
  System.SyncObjs,
  FarScape.EventStack;

{ TEventData }

constructor TEventData.Create(const aMsgClass : TEventClass);
begin
  FRefCount := 1;
  FEventClass := aMsgClass;

  FIsHandled := false;
  FPropagate := true;

  self.FData[0].VInteger := 0;
  self.FData[1].VInteger := 0;
  self.FData[2].VInteger := 0;
  self.FData[3].VInteger := 0;
end;

destructor TEventData.Destroy;
begin
  if assigned(FEventClass) then
  begin
    FEventClass := nil;
  end;
  inherited;
end;

class function TEventData.NewInstance: TObject;
var
  msg : TEventData;
begin
  msg := GlobalEventStack.NewEvent;
  if assigned(msg)
    then result := msg
    else result := inherited NewInstance;
end;

function TEventData.AddRef: integer;
begin
  Result := TInterLocked.Increment(FRefCount);
end;

function TEventData.Release: integer;
begin
  Result := TInterLocked.Decrement(FRefCount);
  if Result = 0 then
  begin
    if assigned(FEventClass) then
    begin
      FEventClass := nil;
    end;
    GlobalEventStack.ReleaseEvent(self);
  end;
end;

{ TCustomEvent }

function TCustomEvent.GetDataAsSingle(const Index: Integer): single;
begin
  result := FData[Index].VSingle;
end;

function TCustomEvent.GetDataAsBoolean(const Index: Integer): boolean;
begin
  result := FData[Index].VBoolean;
end;

function TCustomEvent.GetDataAsInteger(const Index: Integer): integer;
begin
  result := FData[Index].VInteger;
end;

function TCustomEvent.GetDataAsObject(const Index: Integer): TObject;
begin
  result := TObject(FData[Index].VPointer);
end;

function TCustomEvent.GetDataAsPointer(const Index: Integer): Pointer;
begin
  result := FData[Index].VPointer;
end;

function TCustomEvent.GetDataAsPString(const Index: Integer): PString;
begin
  result := PString(FData[Index].VPointer);
end;

procedure TCustomEvent.SetDataAsBoolean(const Index : integer; const Value: boolean);
begin
  FData[Index].VBoolean := Value;
end;

procedure TCustomEvent.SetDataAsInteger(const Index, Value: integer);
begin
  FData[Index].VInteger := Value;
end;

procedure TCustomEvent.SetDataAsObject(const Index: integer; const Value: TObject);
begin
  TObject(FData[Index].VPointer) := Value;
end;

procedure TCustomEvent.SetDataAsPointer(const Index: integer; const Value: Pointer);
begin
  FData[Index].VPointer := Value;
end;

procedure TCustomEvent.SetDataAsPString(const Index: integer; const Value: PString);
begin
  PString(FData[Index].VPointer) := Value;
end;

procedure TCustomEvent.SetDataAsSingle(const Index : integer; const Value: single);
begin
  FData[Index].VSingle := Value;
end;



end.
