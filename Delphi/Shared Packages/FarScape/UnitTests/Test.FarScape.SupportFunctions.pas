unit Test.FarScape.SupportFunctions;

interface

uses
  FarScape.SupportFunctions,
  WatchTower;

type
  TSupportFunctionsTests = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure BitAccess_SetBit;

    [Test]
    procedure BitAccess_GetBit;
  end;

implementation

uses
  SysUtils,
  Types,
  WatchTower.Confirm;

{ TSupportFunctionsTests }

procedure TSupportFunctionsTests.BitAccess_SetBit;
var
  x : integer;
begin
  x := 0;
  TBitAccess(x).SetBit(0,true);
  Confirm.IsTrue(  x = 1  );

  x := 0;
  TBitAccess(x).SetBit(1,true);
  Confirm.IsTrue(  x = 2  );

  x := 0;
  TBitAccess(x).SetBit(2,true);
  Confirm.IsTrue(  x = 4  );

  x := 0;
  TBitAccess(x).SetBit(3,true);
  Confirm.IsTrue(  x = 8  );
end;

procedure TSupportFunctionsTests.BitAccess_GetBit;
var
  x : integer;
begin
  x := 0;
  TBitAccess(x).SetBit(0,true);
  Confirm.IsTrue(   TBitAccess(x).GetBit(0)  );
  Confirm.IsFalse(  TBitAccess(x).GetBit(1)  );

  x := 0;
  TBitAccess(x).SetBit(4,true);
  Confirm.IsTrue(   TBitAccess(x).GetBit(4)  );
  Confirm.IsFalse(  TBitAccess(x).GetBit(3)  );
  Confirm.IsFalse(  TBitAccess(x).GetBit(5)  );



end;



end.
