unit VamLib.Collection.List;

interface

type
  // TRecList is an unordered list object.
  TRecList = record
  private
    FCount: integer;
    FCapacity: integer;
    FMaxCapacity: integer;
    FGrowBy: integer;
    ListData : array of pointer;
    procedure SetCapacity(const Value: integer);
    function GetData(const Index: integer): Pointer;
  public
    constructor Create(const Capacity, GrowBy, MaxCapacity : integer);

    function Add(const Data : Pointer):integer;     // adds the data item to the list.
    procedure Remove(const Data : Pointer);         // Any list items matching the data will be nil'ed.
    function IndexOf(const Data : Pointer):integer; // returns the first item matching the data.
    function CountOf(const Data : Pointer):integer; // returns count of items matching the data.

    procedure Push(const Data : Pointer); // adds the data item to the list.
    function Pop:Pointer; // returns the last non-nil value and removes it from the list, decreasing the list count.

    procedure Compress; // removes nil values from the list. May change the order of the list.

    property Capacity    : integer read FCapacity;
    property GrowBy      : integer read FGrowBy;
    property MaxCapacity : integer read FMaxCapacity;
    property Count       : integer read FCount;

    property Data[const Index : integer]:Pointer read GetData; default;
  end;

implementation

uses
  VamLib.Types;

{ TRecList }

constructor TRecList.Create(const Capacity, GrowBy, MaxCapacity: integer);
begin
  FCount := 0;
  FGrowBy := GrowBy;
  FMaxCapacity := MaxCapacity;
  SetCapacity(Capacity);
end;

procedure TRecList.SetCapacity(const Value: integer);
begin
  FCapacity := Value;
  SetLength(ListData, Value);
end;

function TRecList.GetData(const Index: integer): Pointer;
begin
  assert(Index >= 0);
  assert(Index < Count);
  result := ListData[Index];
end;

function TRecList.IndexOf(const Data: Pointer): integer;
var
  c1: Integer;
begin
  for c1 := 0 to Count-1 do
  begin
    if Data = ListData[c1] then exit(c1);
  end;
  // if we make it this far, the item hasn't been found.
  result := -1;
end;

procedure TRecList.Push(const Data: Pointer);
begin
  Add(Data);
end;

function TRecList.Pop: Pointer;
var
  data : Pointer;
begin
  while Count > 0 do
  begin
    dec(FCount);

    if assigned(ListData[Count]) then
    begin
      result := ListData[Count];
      ListData[Count] := nil;
      exit; //=======================>> exit >>=====>>
    end;
  end;

  // if we make it this far, nothing has been found.
  result := nil;
end;



function TRecList.Add(const Data: Pointer): integer;
begin
  //=== Grow the list if needed ===
  if (Count >= Capacity) then
  begin
    assert(GrowBy > 0);
    if Capacity + GrowBy > MaxCapacity
      then EVamLibException.Create('Cannot grow list. Max capacity reached.');
    SetCapacity(Capacity+GrowBy);
  end;


  ListData[Count] := Data;
  result := Count;

  // finally,
  inc(FCount);
end;

procedure TRecList.Remove(const Data: Pointer);
var
  c1: Integer;
begin
  for c1 := 0 to Count-1 do
  begin
    if ListData[c1] = Data
      then ListData[c1] := nil;
  end;
end;

procedure TRecList.Compress;
var
  WriteIndex : integer;
  ReadIndex : integer;
begin
  // IMPORTANT:
  // The compress method removes nil values from the list.
  // In order to speed up list compression, nil values
  // are replaced by items from the end of the list.
  // **This changes the ordering of the list items.**

  WriteIndex := 0;
  while WriteIndex < Count do
  begin
    if assigned(ListData[WriteIndex]) then
    begin
      inc(WriteIndex);
    end else
    begin
      ReadIndex := Count-1;
      if assigned(ListData[ReadIndex]) then
      begin
        ListData[WriteIndex] := ListData[ReadIndex];
        ListData[ReadIndex] := nil;
        inc(WriteIndex);
        dec(FCount);
      end else
      begin
        // last item in list is not assigned so decrease the list count.
        dec(FCount);
      end;
    end;
  end;
end;

function TRecList.CountOf(const Data: Pointer): integer;
var
  c1: Integer;
  x : integer;
begin
  x := 0;
  for c1 := 0 to Count-1 do
  begin
    if ListData[c1] = Data
      then inc(x);
  end;
  result := x;
end;

end.
