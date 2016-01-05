unit Helm.Message;

interface

type
  THelmMessage = class;
  TAppMessage = class;
  TAppMessageClass = class of TAppMessage;
  TAppMessageClasses = array of TAppMessageClass;

  TDataByte = record
  case Byte of
    0: (VInteger : integer);
    1: (VBoolean : boolean);
    2: (VPointer : pointer);
    3: (VSingle  : single);
  end;

  TMessageBase = class(TObject)
  protected
    FMsgClass : TAppMessageClass;
    FMsgData  : array[0..3] of TDataByte;
  end;

  // All application messages should descend from TCustomAppMessage.
  TAppMessage = class(TMessageBase)
  private
  protected
    function GetDataAsPointer(const Index: Integer): Pointer;
    procedure SetDataAsPointer(const Index : integer; const Value: Pointer);

    function GetDataAsInteger(const Index: Integer): integer;
    procedure SetDataAsInteger(const Index, Value: integer);

    function GetDataAsSingle(const Index: Integer): single;
    procedure SetDataAsSingle(const Index: integer; const Value: single);

    function GetDataAsBoolean(const Index: Integer): boolean;
    procedure SetDataAsBoolean(const Index : integer; const Value: boolean);
  protected
    // If a custom message class needs to initialise some memory for
    // storing message data, it may do so in MessageSetup().
    // The reserved memory should be released in MessageTearDown().
    // Where ever possible, try to avoid using extra memory.
    class procedure MessageSetup(var Msg : THelmMessage); virtual;
    class procedure MessageTearDown(var Msg : THelmMessage); virtual;
  public
  end;

  // Messages will be passed around as THelmMessage. They'll need to be cast
  // to the appropiate "app" message type to be read.
  THelmMessage = class(TMessageBase)
  private
    FRefCount: Integer;
  protected
  public
    constructor Create(const aMsgClass : TAppMessageClass);
    destructor Destroy; override; final;
    function Release:integer; // Reduce reference count. Release to global stack when RefCount is 0.
    function AddRef:integer;  // Add to reference count.
    property RefCount: Integer read FRefCount; // Reference count will automatically be set to 1 when a new message is created.

    class function NewInstance: TObject; override;

    property MsgClass : TAppMessageClass read FMsgClass; // The type of message. Important for reading the data value.
  end;




implementation

uses
  SysUtils,
  System.SyncObjs,
  Helm.GlobalMessageStack;

{ THelmMessage }

constructor THelmMessage.Create(const aMsgClass : TAppMessageClass);
begin
  FRefCount := 1;
  FMsgClass := aMsgClass;

  self.FMsgData[0].VInteger := 0;
  self.FMsgData[1].VInteger := 0;
  self.FMsgData[2].VInteger := 0;
  self.FMsgData[3].VInteger := 0;

  if assigned(FMsgClass) then FMsgClass.MessageSetup(self);
end;

destructor THelmMessage.Destroy;
begin
  if assigned(FMsgClass) then
  begin
    FMsgClass.MessageTearDown(self);
    FMsgClass := nil;
  end;
  inherited;
end;

class function THelmMessage.NewInstance: TObject;
var
  msg : THelmMessage;
begin
  msg := GlobalMessageStack.NewMessage;
  if assigned(msg)
    then result := msg
    else result := inherited NewInstance;
end;

function THelmMessage.AddRef: integer;
begin
  Result := TInterLocked.Increment(FRefCount);
end;

function THelmMessage.Release: integer;
begin
  Result := TInterLocked.Decrement(FRefCount);
  if Result = 0 then
  begin
    if assigned(FMsgClass) then
    begin
      FMsgClass.MessageTearDown(self);
      FMsgClass := nil;
    end;
    GlobalMessageStack.ReleaseMessage(self);
  end;
end;

{ TCustomAppMessage }

class procedure TAppMessage.MessageSetup(var Msg: THelmMessage);
begin

end;

class procedure TAppMessage.MessageTearDown(var Msg: THelmMessage);
begin

end;

function TAppMessage.GetDataAsSingle(const Index: Integer): single;
begin
  result := FMsgData[Index].VSingle;
end;

function TAppMessage.GetDataAsBoolean(const Index: Integer): boolean;
begin
  result := FMsgData[Index].VBoolean;
end;

function TAppMessage.GetDataAsInteger(const Index: Integer): integer;
begin
  result := FMsgData[Index].VInteger;
end;

function TAppMessage.GetDataAsPointer(const Index: Integer): Pointer;
begin
  result := FMsgData[Index].VPointer;
end;

procedure TAppMessage.SetDataAsBoolean(const Index : integer; const Value: boolean);
begin
  FMsgData[Index].VBoolean := Value;
end;

procedure TAppMessage.SetDataAsInteger(const Index, Value: integer);
begin
  FMsgData[Index].VInteger := Value;
end;

procedure TAppMessage.SetDataAsPointer(const Index: integer; const Value: Pointer);
begin
  FMsgData[Index].VPointer := Value;
end;

procedure TAppMessage.SetDataAsSingle(const Index : integer; const Value: single);
begin
  FMsgData[Index].VSingle := Value;
end;

end.
