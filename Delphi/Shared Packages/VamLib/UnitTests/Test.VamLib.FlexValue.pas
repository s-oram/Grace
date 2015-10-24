unit Test.VamLib.FlexValue;

interface


uses
  WatchTower,
  VamLib.FlexValue;


type
  TFlexValueTest = class(TWatchTowerTest)
  private
    fvA : IFlexValue;
    fvB : IFlexValue;
    fvC : IFlexValue;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure ReadingAndWritingValues;

    [Test]
    procedure PointerValues;

    [Test]
    procedure StringValues;

    [Test]
    procedure CopyFrom;

    [Test]
    procedure FlexValueArray;
  end;




  TFlexContainerTest = class(TWatchTowerTest)
  private
    Container : TFlexContainer;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure SetCapacity;

    [Test]
    procedure SetSomeValues;

    [Test]
    procedure AreValuesAreMaintainedWhenCapacityChanges_TestA;

    [Test]
    procedure AreValuesAreMaintainedWhenCapacityChanges_TestB;
  end;


  TFlexListTest = class(TWatchTowerTest)
  private
    List : TFlexList;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure BasicTest;
  end;


implementation

uses
  Decal,
  SysUtils,
  WatchTower.Confirm;

procedure TFlexValueTest.Setup;
begin
  inherited;
  fvA := TFlexValue.Create;
  fvB := TFlexValue.Create;
  fvC := TFlexValue.Create;
end;

procedure TFlexValueTest.TearDown;
begin
  fvA := nil;
  fvB := nil;
  fvC := nil;
  inherited;
end;


procedure TFlexValueTest.ReadingAndWritingValues;
var
  x : single;
begin
  confirm.IsTrue(fvA.IsEmpty);

  fvA.AsInteger := 10;
  confirm.IsTrue(fvA.IsInteger);
  confirm.IsTrue(fvA.AsInteger = 10);

  x := 0.42;
  fvA.AsSingle := x;
  confirm.IsTrue(fvA.IsSingle);
  confirm.IsTrue(fvA.AsSingle = x);

  fvA.AsBoolean := true;
  confirm.IsTrue(fvA.IsBoolean);
  confirm.IsTrue(fvA.AsBoolean = true);

  fvA.AsBoolean := false;
  confirm.IsTrue(fvA.IsBoolean);
  confirm.IsTrue(fvA.AsBoolean = false);
end;

procedure TFlexValueTest.CopyFrom;
var
  x : single;
  text : string;
  ix : integer;
  pix : ^integer;
begin
  x := 3.14;
  text := 'James Brown Is Dead';

  fvA.AsInteger := 1;
  fvB.CopyFrom(fvA);
  Confirm.IsTrue(fvB.AsInteger = 1);

  fvA.AsSingle := x;
  fvB.CopyFrom(fvA);
  Confirm.IsTrue(fvB.AsSingle = x);

  fvA.AsBoolean := true;
  fvB.CopyFrom(fvA);
  Confirm.IsTrue(fvB.AsBoolean);

  ix := 42;
  fvA.AsPointer := @ix;
  fvB.CopyFrom(fvA);
  pix := fvB.AsPointer;
  Confirm.IsTrue(pix^ = 42);

  fvA.AsString := text;
  fvB.CopyFrom(fvA);
  Confirm.IsTrue(fvB.AsString = text);
end;

procedure TFlexValueTest.FlexValueArray;
var
  arr : array of IFlexValue;
begin
  SetLength(arr, 10);
  arr[0] := TFlexValue.Create;
  arr[0].AsString := 'Tom';
end;

procedure TFlexValueTest.PointerValues;
var
  x : integer;
  px : ^integer;
begin
  x := 10;
  fvA.AsPointer := @x;
  px := fvA.AsPointer;
  Confirm.IsTrue(px^ = 10);

  px^ := 12;
  Confirm.IsTrue(x = 12);
end;

procedure TFlexValueTest.StringValues;
var
  x : string;
begin
  x := 'JamesBrownIsDead';
  fvA.AsString := x;
  Confirm.IsTrue(fvA.AsString = x);
end;


{ TFlexContainerTest }

procedure TFlexContainerTest.Setup;
begin
  inherited;
  Container := TFlexContainer.Create;
end;

procedure TFlexContainerTest.TearDown;
begin
  inherited;
  Container.Free;
end;

procedure TFlexContainerTest.SetCapacity;
var
  dataLength : integer;
begin
  Container.Capacity := 12;
  Container.Capacity := 15;
  Container.Capacity := 5;
  Container.Capacity := 12;

  DataLength := Length(Container.Data);
  Confirm.IsTrue(DataLength = 12);
end;

procedure TFlexContainerTest.SetSomeValues;
begin
  Container.Capacity := 12;

  Container.Data[0].AsInteger := 10;
  Confirm.IsTrue(Container.Data[0].AsInteger = 10);

  Container.Data[1].AsString := 'PizzaFace';
  Confirm.IsTrue(Container.Data[1].AsString = 'PizzaFace');

  Container.Data[2].AsString := 'PizzaFace';
  Confirm.IsTrue(Container.Data[2].AsString = 'PizzaFace');

  Container.Data[3].AsString := 'PizzaFace';
  Confirm.IsTrue(Container.Data[3].AsString = 'PizzaFace');
end;


procedure TFlexContainerTest.AreValuesAreMaintainedWhenCapacityChanges_TestA;
var
  c1: Integer;
  OldCapacity : integer;
begin
  Container.Capacity := 12;

  for c1 := 0 to Container.Capacity-1 do
  begin
    Container.Data[c1].AsInteger := c1;
  end;

  OldCapacity := Container.Capacity;
  Container.Capacity := 22;

  for c1 := 0 to OldCapacity-1 do
  begin
    Confirm.IsTrue( Container.Data[c1].AsInteger = c1 );
  end;

end;

procedure TFlexContainerTest.AreValuesAreMaintainedWhenCapacityChanges_TestB;
var
  c1: Integer;
  OldCapacity : integer;
  Text : string;
begin
  Container.Capacity := 12;

  for c1 := 0 to Container.Capacity-1 do
  begin
    Text := 'Bang' + IntToStr(c1);
    Container.Data[c1].AsString := Text;
  end;

  OldCapacity := Container.Capacity;
  Container.Capacity := 22;

  for c1 := 0 to OldCapacity-1 do
  begin
    Text := 'Bang' + IntToStr(c1);
    Confirm.IsTrue( Container.Data[c1].AsString = Text );
  end;

end;

{ TFlexListTest }

procedure TFlexListTest.Setup;
begin
  inherited;
  List := TFlexList.Create;
end;

procedure TFlexListTest.TearDown;
begin
  inherited;
  List.Free;
end;

procedure TFlexListTest.BasicTest;
begin
  List.Add(Flex('TomJones'));
  Confirm.IsTrue( List.Count = 1 );
  Confirm.IsTrue( List[0].AsString = 'TomJones' );

  List.Add(Flex('PizzaFace'));
  List.Add(Flex('StringBean'));
  List.Add(Flex('JamesBrown'));
  List.Add(Flex('Faker'));

  Confirm.IsTrue( List.Count = 5 );
  Confirm.IsTrue( List[4].AsString = 'Faker' );


end;



end.
