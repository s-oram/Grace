unit Test.VamLib.Collection.DoubleLinkedList;

interface

uses
  WatchTower,
  VamLib.Collection.DoubleLinkedList;

type
  TDoubleLinkedListTest = class(TWatchTowerTest)
  private
    DataA : array[0..20] of integer;
    DataB : array[0..20] of string;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure Basics;

    [Test]
    procedure AddAndRemoveItemsA;

    [Test]
    procedure AddAndRemoveItemsB;

    [Test]
    procedure PopFirst;

    [Test]
    procedure PopLast;
  end;

implementation

uses
  SysUtils,
  WatchTower.Confirm;

{ TDoubleLinkedListTest }

procedure TDoubleLinkedListTest.Setup;
var
  c1: Integer;
begin
  inherited;

  for c1 := 0 to 20 do
  begin
    DataA[c1] := c1;
    DataB[c1] := IntToStr(c1);
  end;
end;

procedure TDoubleLinkedListTest.TearDown;
begin
  inherited;

end;

procedure TDoubleLinkedListTest.Basics;
var
  List : TRecDoubleLinkedList;
begin
  List := TRecDoubleLinkedList.Create(10,10,100);

  Confirm.IsTrue( not assigned(List.First) );
  Confirm.IsTrue( not assigned(List.Last)  );
  Confirm.IsTrue( List.Capacity    = 10   );
  Confirm.IsTrue( List.Count       = 0    );
  Confirm.IsTrue( List.GrowBy      = 10   );
  Confirm.IsTrue( List.MaxCapacity = 100  );


end;

procedure TDoubleLinkedListTest.AddAndRemoveItemsA;
var
  List : TRecDoubleLinkedList;
  Data : PInteger;
begin
  List := TRecDoubleLinkedList.Create(10,10,100);

  Data := @DataA[1];
  List.AppendItem(Data);

  Confirm.IsTrue( assigned(List.First) );
  Confirm.IsTrue( assigned(List.Last)  );
  Confirm.IsTrue( List.Count = 1    );

  Data := List.First;
  Confirm.IsTrue(  Data^ = DataA[1]  );

  Data := List.PopFirst;
  Confirm.IsTrue(  Data^ = DataA[1]  );
  Confirm.IsTrue( List.Count = 0    );

end;



procedure TDoubleLinkedListTest.AddAndRemoveItemsB;
var
  List : TRecDoubleLinkedList;
  Data : PInteger;
  c1: Integer;
begin
  List := TRecDoubleLinkedList.Create(10,10,100);

  for c1 := 0 to 20 do
  begin
    List.AppendItem(@DataA[c1]);
  end;
  Confirm.IsTrue(  List.Count = 21  );
  Confirm.IsTrue(  assigned(list.First) );
  Confirm.IsTrue(  assigned(List.Last)  );

  Data := List.Last;
  Confirm.IsTrue(  Data^ = dataA[20]  );

  Data := List.First;
  Confirm.IsTrue(  Data^ = dataA[0]  );

end;

procedure TDoubleLinkedListTest.PopFirst;
var
  List : TRecDoubleLinkedList;
  Data : PInteger;
  c1: Integer;
begin
  List := TRecDoubleLinkedList.Create(10,10,100);

  for c1 := 0 to 20 do
  begin
    List.AppendItem(@DataA[c1]);
  end;

  for c1 := 0 to 20 do
  begin
    Data := List.PopFirst;
    Confirm.IsTrue(  Data^ = DataA[c1]  );
  end;

end;

procedure TDoubleLinkedListTest.PopLast;
var
  List : TRecDoubleLinkedList;
  Data : PInteger;
  c1: Integer;
begin
  List := TRecDoubleLinkedList.Create(10,10,100);

  for c1 := 0 to 20 do
  begin
    List.AppendItem(@DataA[c1]);
  end;

  for c1 := 20 downto 0 do
  begin
    Data := List.PopLast;
    Confirm.IsTrue(  Data^ = DataA[c1]  );
  end;

end;



end.
