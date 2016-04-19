unit Test.VamLib.Collection.List;

interface

uses
  WatchTower,
  VamLib.Collection.List;

type
  TRecListTest = class(TWatchTowerTest)
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure Basics;

    [Test]
    procedure PushAndPop;
  end;

implementation

uses
  SysUtils,
  WatchTower.Confirm;

{ TRecListTest }

procedure TRecListTest.Setup;
begin
  inherited;

end;

procedure TRecListTest.TearDown;
begin
  inherited;

end;

procedure TRecListTest.Basics;
var
  List : TRecList;
  Data : array[0..20] of integer;
  px : PInteger;
  c1: Integer;
  PreCount : integer;
  PostCount : integer;
begin
  List := TRecList.Create(10,10,30);

  for c1 := 0 to 20 do
  begin
    Data[c1] := c1;
  end;

  // Add items to list.
  for c1 := 0 to 20 do
  begin
    px := @Data[c1];
    List.Add(px);
  end;

  // confirm one of each item is in the list.
  for c1 := 0 to 20 do
  begin
    px := @Data[c1];
    Confirm.IsTrue(  List.CountOf(px) = 1  );
  end;

  // remove some items.
  for c1 := 0 to 9 do
  begin
    px := @Data[c1];
    List.Remove(px);
  end;

  // check that compress actually compresses.
  PreCount := List.Count;
  List.Compress;
  PostCount := List.Count;

  Confirm.IsTrue(  PreCount <> PostCount  );


  // Check removed items are not in list.
  for c1 := 0 to 9 do
  begin
    px := @Data[c1];
    Confirm.IsTrue(  List.CountOf(px) = 0  );
  end;


  // Check remaining items are still in list.
  for c1 := 10 to 20 do
  begin
    px := @Data[c1];
    Confirm.IsTrue(  List.CountOf(px) = 1  );
  end;


  // Check that duplicate entries will be counted correctly.
  for c1 := 10 to 20 do
  begin
    px := @Data[c1];
    List.Add(px);
  end;

  for c1 := 10 to 20 do
  begin
    px := @Data[c1];
    Confirm.IsTrue(  List.CountOf(px) = 2  );
  end;

end;

procedure TRecListTest.PushAndPop;
var
  List : TRecList;
  Data : array[0..20] of integer;
  px : PInteger;
  c1 : integer;
begin
  List := TRecList.Create(10,10,100);

  for c1 := 0 to 20 do
  begin
    Data[c1] := c1;
  end;

  px := @Data[2];
  List.Push(px);

  Confirm.IsTrue(  List.Count = 1 );

  px := List.Pop;

  Confirm.IsTrue(  List.Count = 0 );
  Confirm.IsTrue(  px^ = Data[2] );

end;





end.
