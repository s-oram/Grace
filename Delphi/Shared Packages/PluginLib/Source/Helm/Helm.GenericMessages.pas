unit Helm.GenericMessages;

interface

uses
  Helm.Message;

type
  TIntegerMessage = class(TAppMessage)
  private
  public
    class function Create(const MsgID, IntA, IntB, IntC : integer):THelmMessage;
    property MsgID : integer index 0 read GetDataAsInteger;
    property IntA  : integer index 1 read GetDataAsInteger;
    property IntB  : integer index 2 read GetDataAsInteger;
    property IntC  : integer index 3 read GetDataAsInteger;
  end;

  TFloatMessage = class(TAppMessage)
  private
  public
    class function Create(const MsgID: integer; const FloatA, FloatB, FloatC : single):THelmMessage;
    property MsgID  : integer index 0 read GetDataAsInteger;
    property FloatA : single  index 1 read GetDataAsSingle;
    property FloatB : single  index 2 read GetDataAsSingle;
    property FloatC : single  index 3 read GetDataAsSingle;
  end;

  TBooleanMessage = class(TAppMessage)
  private
  public
    class function Create(const MsgID: integer; const BooleanA, BooleanB, BooleanC : boolean):THelmMessage;
    property MsgID    : integer index 0 read GetDataAsInteger;
    property BooleanA : boolean index 1 read GetDataAsBoolean;
    property BooleanB : boolean index 2 read GetDataAsBoolean;
    property BooleanC : boolean index 3 read GetDataAsBoolean;
  end;

  TPointerMessage = class(TAppMessage)
  private
  public
    class function Create(const MsgID: integer; const PointerA, PointerB, PointerC : Pointer):THelmMessage;
    property MsgID    : integer index 0 read GetDataAsInteger;
    property PointerA : Pointer index 1 read GetDataAsPointer;
    property PointerB : Pointer index 2 read GetDataAsPointer;
    property PointerC : Pointer index 3 read GetDataAsPointer;
  end;

implementation

type
  TProtectedMessageHack = class(TAppMessage);

{ TIntegerMessage }

class function TIntegerMessage.Create(const MsgID, IntA, IntB, IntC: integer): THelmMessage;
var
  msg : THelmMessage;
begin
  msg := THelmMessage.Create(TIntegerMessage);

  TProtectedMessageHack(msg).SetDataAsInteger(0, MsgID);
  TProtectedMessageHack(msg).SetDataAsInteger(1, IntA);
  TProtectedMessageHack(msg).SetDataAsInteger(2, IntB);
  TProtectedMessageHack(msg).SetDataAsInteger(3, IntC);

  result := msg;
end;

{ TFloatMessage }

class function TFloatMessage.Create(const MsgID: integer; const FloatA, FloatB, FloatC: single): THelmMessage;
var
  msg : THelmMessage;
begin
  msg := THelmMessage.Create(TFloatMessage);

  TProtectedMessageHack(msg).SetDataAsInteger(0, MsgID);
  TProtectedMessageHack(msg).SetDataAsSingle(1, FloatA);
  TProtectedMessageHack(msg).SetDataAsSingle(2, FloatB);
  TProtectedMessageHack(msg).SetDataAsSingle(3, FloatC);

  result := msg;
end;


{ TBooleanMessage }

class function TBooleanMessage.Create(const MsgID: integer; const BooleanA, BooleanB, BooleanC: boolean): THelmMessage;
var
  msg : THelmMessage;
begin
  msg := THelmMessage.Create(TBooleanMessage);

  TProtectedMessageHack(msg).SetDataAsInteger(0, MsgID);
  TProtectedMessageHack(msg).SetDataAsBoolean(1, BooleanA);
  TProtectedMessageHack(msg).SetDataAsBoolean(2, BooleanB);
  TProtectedMessageHack(msg).SetDataAsBoolean(3, BooleanC);

  result := msg;
end;

{ TPointerMessage }

class function TPointerMessage.Create(const MsgID: integer; const PointerA, PointerB, PointerC: Pointer): THelmMessage;
var
  msg : THelmMessage;
begin
  msg := THelmMessage.Create(TPointerMessage);

  TProtectedMessageHack(msg).SetDataAsInteger(0, MsgID);
  TProtectedMessageHack(msg).SetDataAsPointer(1, PointerA);
  TProtectedMessageHack(msg).SetDataAsPointer(2, PointerB);
  TProtectedMessageHack(msg).SetDataAsPointer(3, PointerC);

  result := msg;
end;

end.
