unit Test.Helm.Message;

interface

uses
  WatchTower,
  Helm.Message,
  Helm.GenericMessages;

type
  THelmMessageTests = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure CreateMessage;

    [Test]
    procedure CreateMessageFromGlobalStack;
  end;


implementation

uses
  WatchTower.MemLogger,
  WatchTower.Confirm;

{ TAnyValueTests }


procedure THelmMessageTests.CreateMessage;
var
  msg : THelmMessage;
  x1, x2, x3, x4 : integer;
begin
  //== Integer message type ==
  msg := TIntegerMessage.Create(42,101,102,103);
  confirm.IsTrue(  msg.MsgClass = TIntegerMessage  );
  confirm.IsTrue( TIntegerMessage(msg).MsgID = 42 );
  confirm.IsTrue( TIntegerMessage(msg).IntA  = 101 );
  confirm.IsTrue( TIntegerMessage(msg).IntB  = 102 );
  confirm.IsTrue( TIntegerMessage(msg).IntC  = 103 );
  msg.Free;

  //== float message type ==
  msg := TFloatMessage.Create(42,101,102,103);
  confirm.IsTrue(  msg.MsgClass = TFloatMessage  );
  confirm.IsTrue( TFloatMessage(msg).MsgID = 42 );
  confirm.IsTrue( TFloatMessage(msg).FloatA  = 101 );
  confirm.IsTrue( TFloatMessage(msg).FloatB  = 102 );
  confirm.IsTrue( TFloatMessage(msg).FloatC  = 103 );
  msg.Free;

  //== bool message type ==
  msg := TBooleanMessage.Create(42,true,false,true);
  confirm.IsTrue(  msg.MsgClass = TBooleanMessage  );
  confirm.IsTrue( TBooleanMessage(msg).MsgID = 42 );
  confirm.IsTrue( TBooleanMessage(msg).BooleanA  = true );
  confirm.IsTrue( TBooleanMessage(msg).BooleanB  = false );
  confirm.IsTrue( TBooleanMessage(msg).BooleanC  = true );
  msg.Free;

    //== pointer message type ==
  msg := TPointerMessage.Create(42,@x1, @x2, @x3);
  confirm.IsTrue(  msg.MsgClass = TPointerMessage  );
  confirm.IsTrue( TPointerMessage(msg).MsgID = 42 );
  confirm.IsTrue( TPointerMessage(msg).PointerA = addr(x1) );
  confirm.IsTrue( TPointerMessage(msg).PointerB = addr(x2) );
  confirm.IsTrue( TPointerMessage(msg).PointerC = addr(x3) );
  msg.Free;


end;

procedure THelmMessageTests.CreateMessageFromGlobalStack;
var
  msg : THelmMessage;
begin
  //=== Create a message from the stack ===
  msg := TIntegerMessage.Create(42,0,0,0);
  confirm.IsTrue( TIntegerMessage(msg).MsgID = 42 );
  msg.Release;

  //=== Create another message from the stack ===
  // Confirm there is no heap memory activity when
  // creating a message from the global stack.
  MemLogger.Reset;

  msg := TIntegerMessage.Create(43,0,0,0);
  confirm.IsTrue( TIntegerMessage(msg).MsgID = 43 );
  msg.Release;

  Confirm.IsTrue(  MemLogger.GetTotalActivity = 0  );
end;


end.
