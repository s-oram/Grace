unit FarScape.Event;

interface

type
  TFarScapeEvent = class;
  TFarScapeEventClass = class of TFarScapeEvent;

  TDataByte = record
  case Byte of
    0: (VInteger : integer);
    1: (VBoolean : boolean);
    2: (VPointer : pointer);
    3: (VSingle  : single);
  end;

  TEventBase = class(TObject)
  protected
    FEventClass : TFarScapeEventClass;
    FTarget     : TObject;
    FIsHandled  : boolean;
    FPropagate  : boolean;
    FData       : array[0..3] of TDataByte;
  public
    property EventClass : TFarScapeEventClass read FEventClass; // The type of event. Important for reading the data value.
    property Target     : TObject     read FTarget;
    property IsHandled  : boolean     read FIsHandled write FIsHandled;
    property Propagate  : boolean     read FPropagate write FPropagate;
  end;

  // All Farscape events descend from TCustomEvent.
  TFarScapeEvent = class(TEventBase)
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
    constructor Create(const Target : TObject); virtual;
  end;



implementation

uses
  SysUtils,
  System.SyncObjs;

{ TFarScapeEvent }

constructor TFarScapeEvent.Create(const Target: TObject);
begin
  self.FTarget := Target;
  self.Propagate := true;
  self.IsHandled := false;
end;

function TFarScapeEvent.GetDataAsSingle(const Index: Integer): single;
begin
  result := FData[Index].VSingle;
end;

function TFarScapeEvent.GetDataAsBoolean(const Index: Integer): boolean;
begin
  result := FData[Index].VBoolean;
end;

function TFarScapeEvent.GetDataAsInteger(const Index: Integer): integer;
begin
  result := FData[Index].VInteger;
end;

function TFarScapeEvent.GetDataAsObject(const Index: Integer): TObject;
begin
  result := TObject(FData[Index].VPointer);
end;

function TFarScapeEvent.GetDataAsPointer(const Index: Integer): Pointer;
begin
  result := FData[Index].VPointer;
end;

function TFarScapeEvent.GetDataAsPString(const Index: Integer): PString;
begin
  result := PString(FData[Index].VPointer);
end;

procedure TFarScapeEvent.SetDataAsBoolean(const Index : integer; const Value: boolean);
begin
  FData[Index].VBoolean := Value;
end;

procedure TFarScapeEvent.SetDataAsInteger(const Index, Value: integer);
begin
  FData[Index].VInteger := Value;
end;

procedure TFarScapeEvent.SetDataAsObject(const Index: integer; const Value: TObject);
begin
  TObject(FData[Index].VPointer) := Value;
end;

procedure TFarScapeEvent.SetDataAsPointer(const Index: integer; const Value: Pointer);
begin
  FData[Index].VPointer := Value;
end;

procedure TFarScapeEvent.SetDataAsPString(const Index: integer; const Value: PString);
begin
  PString(FData[Index].VPointer) := Value;
end;

procedure TFarScapeEvent.SetDataAsSingle(const Index : integer; const Value: single);
begin
  FData[Index].VSingle := Value;
end;



end.
