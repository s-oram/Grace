unit Test.Helm.AnyValue;

interface

uses
  WatchTower,
  Helm.AnyValue;

type
  TAnyValueTests = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure BasicTests;
  end;






implementation

uses
  WatchTower.MemLogger,
  WatchTower.Confirm;

{ TAnyValueTests }

const
  kText : string = 'Pizza';



procedure TAnyValueTests.BasicTests;

var
  av1 : TAnyValue;
  kx : single;
begin
  av1.AsInteger := 10;
  Confirm.IsTrue( av1.IsInteger  );
  Confirm.IsTrue( av1.AsInteger = 10 );

  av1.AsString := 'Tom';
  Confirm.IsTrue( av1.IsString  );
  Confirm.IsTrue( av1.AsString = 'Tom' );

  kx := 3.14;
  av1.AsSingle := kx;
  Confirm.IsTrue( av1.IsSingle  );
  Confirm.IsTrue( av1.AsSingle = kx );

  av1.AsBoolean := true;
  Confirm.IsTrue( av1.IsBoolean  );
  Confirm.IsTrue( av1.AsBoolean  );

  av1.AsPointer := @kx;
  Confirm.IsTrue( av1.IsPointer  );
  Confirm.IsTrue( single(av1.AsPointer^) = kx );


  MemLogger.Reset;

  av1.AsPString := @kText;
  Confirm.IsTrue( av1.AsPString^ = kText );

  Confirm.IsTrue( MemLogger.GetTotalActivity = 0 );
end;


end.
