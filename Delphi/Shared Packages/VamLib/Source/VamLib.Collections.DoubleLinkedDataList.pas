unit VamLib.Collections.DoubleLinkedDataList;

interface

type
  PListItem = ^TListItem;
  TListItem = record
    InUse : boolean;
    Prev : PListItem;
    Next : PListItem;
    Data : Pointer;
  end;

type
  ///  TDoubleLinkedDataList list items all have a pre-defined data paypload size.
  ///  The memory for the item data is allocated when the item is created by
  ///  the list.
  ///  TODO:MED Instead of using GetMem() and FreeMem() each time a list item
  ///  is added or removed, it would be better to use a "common memory pool"
  ///  to avoid memory requests. (This is important for use in the audio
  ///  thread.)

  TDoubleLinkedDataList = class
  private
    fLast: PListItem;
    fFirst: PListItem;
    function GetListItem(Index: integer): PListItem;
  protected
    ItemDataSize  : integer;
    ListIncrementSize : integer;
    fItemCount : integer;
    fCapacity : integer;
    ListItems : array of TListItem;
    LastItemIndex : integer;

    procedure ChangeListSize(const NewListSize: integer);

    function FindAvailableItem:PListItem;
  public
    constructor Create(const aItemDataSize, aInitialSize, aIncrementSize : integer);
    destructor Destroy; override;

    property First : PListItem read fFirst;
    property Last  : PListItem read fLast;

    function New : PListItem; //appends a new item to the end of the list and returns it.
    procedure Delete(Item : PListItem); //deletes the item data an removes it from the list.

    procedure ClearList;

    property ItemCount : integer read fItemCount;
    property Items[Index : integer]:PListItem read GetListItem; //NOTE: Direct access is slow. The function needs to iterate through the list to find the required item.

    // The number of items the list can currently hold.
    property Capacity : integer read fCapacity;
  end;

implementation

uses
  SysUtils;

{ TThreadSafeDoubleLinkedList }


constructor TDoubleLinkedDataList.Create(const aItemDataSize, aInitialSize, aIncrementSize: integer);
begin
  fItemCount := 0;
  LastItemIndex := 0;

  fFirst := nil;
  fLast := nil;

  ItemDataSize  := aItemDataSize;
  ListIncrementSize := aIncrementSize;
  fCapacity := 0;

  ChangeListSize(aInitialSize);
end;

destructor TDoubleLinkedDataList.Destroy;
begin
  ClearList;
  ChangeListSize(0);
  inherited;
end;

procedure TDoubleLinkedDataList.ClearList;
var
  c1 : integer;
  Item : PListItem;
begin
  for c1 := 0 to fCapacity-1 do
  begin
    Item := @ListItems[c1];
    if assigned(Item^.Data) then FreeMem(Item^.Data, ItemDataSize);
    Item^.Data := nil;
    Item^.InUse := false;
  end;

  fItemCount := 0;
  LastItemIndex := 0;

  fFirst := nil;
  fLast := nil;
end;

procedure TDoubleLinkedDataList.ChangeListSize(const NewListSize: integer);
var
  c1: Integer;
begin
  assert(NewListSize >= 0);

  if NewListSize > fCapacity then
  begin
    SetLength(ListItems, NewListSize);
    fCapacity := NewListSize;
  end else
  if NewListSize < fCapacity then
  begin
    for c1 := fCapacity-1 downto NewListSize do
    begin
      if assigned(ListItems[c1].Data) then raise Exception.Create('Can''t resize list. Data still in use.');
    end;
    SetLength(ListItems, NewListSize);
    fCapacity := NewListSize;
  end;
end;

function TDoubleLinkedDataList.New: PListItem;
var
  Item : PListItem;
begin
  if (Last <> nil) then
  begin
    Item := FindAvailableItem;
    Item^.Prev := Last;
    Item^.Next := nil;

    fLast^.Next := Item;

    fLast := Item;
  end else
  begin
    Item := FindAvailableItem;
    Item^.Prev := nil;
    Item^.Next := nil;

    fFirst := Item;
    fLast := Item;
  end;

  result := Item;
end;

procedure TDoubleLinkedDataList.Delete(Item: PListItem);
begin
  assert(Item^.InUse);

  if (Item = Last) then
  begin
    fLast := Item^.Prev;
  end;

  if (Item = First) then
  begin
    fFirst := Item^.Next;
  end;

  if (Item^.Prev <> nil) then
  begin
    Item^.Prev^.Next := Item^.Next;
  end;

  if (Item^.Next <> nil) then
  begin
    Item^.Next^.Prev := Item^.Prev;
  end;

  if assigned(Item^.Data) then FreeMem(Item^.Data, ItemDataSize);
  Item^.Data := nil;
  Item^.InUse := false;

  dec(fItemCount);
end;

function TDoubleLinkedDataList.FindAvailableItem: PListItem;
var
  c1 : integer;
  SearchIndex : integer;
  Item : PListItem;
begin
  Inc(fItemCount);

  SearchIndex := LastItemIndex;

  for c1 := 0 to fCapacity-1 do
  begin
    inc(SearchIndex);
    if SearchIndex >= fCapacity then SearchIndex := 0;

    if ListItems[SearchIndex].InUse = false then
    begin
      Item := @ListItems[SearchIndex];

      Item^.InUse := true;
      GetMem(Item^.Data, ItemDataSize);
      result := Item;

      LastItemIndex := SearchIndex;
      exit; //============================>> exit >>==============>>
    end;
  end;

  // If we make it this far, we need to grow the list..
  LastItemIndex := fCapacity;
  ChangeListSize(fCapacity + ListIncrementSize);

  Item := @ListItems[LastItemIndex];

  Item^.InUse := true;
  GetMem(Item^.Data, ItemDataSize);
  result := Item;
end;

function TDoubleLinkedDataList.GetListItem(Index: integer): PListItem;
var
  x : PListItem;
  c1: Integer;
begin
  assert(Index <= ItemCount);
  x := First;
  for c1 := 0 to Index-1 do
  begin
    x := x^.Next;
  end;
  result := x;
end;

end.
