unit VamLib.Collection.DoubleLinkedList;

interface

type
  PDoubleLinkedListItem = ^TDoubleLinkedListItem;
  TDoubleLinkedListItem = record
    InUse    : boolean;
    Data     : Pointer;
    CurItemIndex  : integer;
    PrevItemIndex : integer;
    NextItemIndex : integer;
  end;

  // TRecDoubleLinkedList is a list type with static memory requirements.
  // It doesn't request or release memory when data is added or removed from
  // the list.
  TRecDoubleLinkedList = record
  private
    ListData : array of TDoubleLinkedListItem;
    FCapacity: integer;
    FCount: integer;
    FGrowBy: integer;
    FMaxCapacity: integer;
    FirstItemIndex : integer;
    LastItemIndex  : integer;

    procedure SetCapacity(const Value: integer);
    function GetFirst: Pointer;
    function GetLast: Pointer;
  public
    constructor Create(const Capacity, GrowBy, MaxCapacity : integer);

    // Read the first and last data values without removing them from the list.
    property First : Pointer read GetFirst;
    property Last  : Pointer read GetLast;

    procedure AppendItem(const Data : Pointer); // Add item to end of list.
    function PopFirst:Pointer; // Pop the oldest item from the list.
    function PopLast:Pointer;  // pop the newest item from the list.

    property Capacity    : integer read FCapacity;
    property GrowBy      : integer read FGrowBy;
    property MaxCapacity : integer read FMaxCapacity;
    property Count       : integer read FCount;
  end;

implementation

uses
  VamLib.Types,
  VamLib.ArrayUtils;

{ TDoubleLinkedList }

constructor TRecDoubleLinkedList.Create(const Capacity, GrowBy, MaxCapacity : integer);
begin
  assert(GrowBy > 0);
  assert(MaxCapacity >= Capacity);

  FCount := 0;
  FGrowBy := GrowBy;
  FMaxCapacity := MaxCapacity;
  SetCapacity(Capacity);

  FirstItemIndex := -1;
  LastItemIndex  := -1;
end;

function TRecDoubleLinkedList.GetFirst: Pointer;
begin
  if FirstItemIndex <> -1
    then result := ListData[FirstItemIndex].Data
    else result := nil;
end;

function TRecDoubleLinkedList.GetLast: Pointer;
begin
  if LastItemIndex <> -1
    then result := ListData[LastItemIndex].Data
    else result := nil;
end;

procedure TRecDoubleLinkedList.SetCapacity(const Value: integer);
var
  c1: Integer;
begin
  assert(Value >= 0);
  FCapacity := Value;
  SetLength(ListData, Value);

  // TODO:MED only need to set the index of new items.
  for c1 := 0 to Value-1 do
  begin
    ListData[c1].CurItemIndex := c1;
  end;
end;

procedure TRecDoubleLinkedList.AppendItem(const Data: Pointer);
var
  c1 : integer;
  WriteIndex : integer;
  CurLast : PDoubleLinkedListItem;
  FFirstItem : PDoubleLinkedListItem;
  FLastItem  : PDoubleLinkedListItem;
begin
  //=== Grow the list if needed ===
  if (Count >= Capacity) then
  begin
    assert(GrowBy > 0);
    if Capacity + GrowBy > MaxCapacity
      then EVamLibException.Create('Cannot grow list. Max capacity reached.');
    SetCapacity(Capacity+GrowBy);
  end;

  //=== Find an unused data location ====
  WriteIndex := -1;
  for c1 := 0 to Capacity-1 do
  begin
    if not ListData[c1].InUse then
    begin
      WriteIndex := c1;
      break;
    end;
  end;
  if WriteIndex = -1 then raise EVamLibException.Create('Couldn''t find a data location to write to. This is very unexpected and indicates a bug!');
  //=====================================

  if FirstItemIndex <> -1
    then FFirstItem := @ListData[FirstItemIndex]
    else FFirstItem := nil;

  if LastItemIndex <> -1
    then FLastItem := @ListData[LastItemIndex]
    else FLastItem := nil;

  if (assigned(FFirstItem) <> assigned(FLastItem))
    then raise EVamLibException.Create('Something has gone wrong. First & Last items assignment state must match.');

  if not assigned(FFirstItem) then
  begin
    assert(not assigned(FLastItem));

    ListData[WriteIndex].InUse := true;
    ListData[WriteIndex].Data := Data;
    ListData[WriteIndex].PrevItemIndex := -1;
    ListData[WriteIndex].NextItemIndex := -1;
    FirstItemIndex := WriteIndex;
    LastItemIndex  := WriteIndex;
  end else
  begin
    assert(assigned(FLastItem));

    // Update the current last item
    FLastItem^.NextItemIndex := WriteIndex;

    // Add values for the new last item.
    ListData[WriteIndex].InUse := true;
    ListData[WriteIndex].Data := Data;
    ListData[WriteIndex].PrevItemIndex := FLastItem^.CurItemIndex;
    ListData[WriteIndex].NextItemIndex := -1;

    // Update the last item pointer to point to the new item.
    LastItemIndex := WriteIndex;
  end;

  inc(FCount);
end;

function TRecDoubleLinkedList.PopFirst:Pointer;
var
  Data : Pointer;
  NextItemIndex : integer;
  FFirstItem : PDoubleLinkedListItem;
  FLastItem  : PDoubleLinkedListItem;
begin
  if FirstItemIndex <> -1
    then FFirstItem := @ListData[FirstItemIndex]
    else FFirstItem := nil;

  if LastItemIndex <> -1
    then FLastItem := @ListData[LastItemIndex]
    else FLastItem := nil;

  if not assigned(FFirstItem) then exit(nil);

  dec(FCount);

  if FFirstItem = FLastItem then
  begin
    Data := FFirstItem^.Data;

    FFirstItem^.InUse    := false;
    FFirstItem^.Data     := nil;
    FFirstItem^.PrevItemIndex := -1;
    FFirstItem^.NextItemIndex := -1;

    FLastItem^.InUse    := false;
    FLastItem^.Data     := nil;
    FLastItem^.PrevItemIndex := -1;
    FLastItem^.NextItemIndex := -1;

    FirstItemIndex := -1;
    LastItemIndex  := -1;
  end else
  begin
    if FFirstItem^.NextItemIndex = -1 then raise EVamLibException.Create('Next item must be assigned.');


    Data          := FFirstItem^.Data;
    NextItemIndex := FFirstItem^.NextItemIndex;

    // reset the current first item.
    FFirstItem^.InUse    := false;
    FFirstItem^.Data     := nil;
    FFirstItem^.PrevItemIndex := -1;
    FFirstItem^.NextItemIndex := -1;

    ListData[NextItemIndex].PrevItemIndex := -1;
    FirstItemIndex := NextItemIndex;
  end;

  // If we make it this far, we've returned some data.
  result := Data;
end;

function TRecDoubleLinkedList.PopLast:Pointer;
var
  Data : Pointer;
  PrevItemIndex : integer;
  FFirstItem : PDoubleLinkedListItem;
  FLastItem  : PDoubleLinkedListItem;
begin
  if FirstItemIndex <> -1
    then FFirstItem := @ListData[FirstItemIndex]
    else FFirstItem := nil;

  if LastItemIndex <> -1
    then FLastItem := @ListData[LastItemIndex]
    else FLastItem := nil;

  if not assigned(FFirstItem) then exit(nil);

  dec(FCount);

  if FFirstItem = FLastItem then
  begin
    Data := FFirstItem^.Data;

    FFirstItem^.InUse    := false;
    FFirstItem^.Data     := nil;
    FFirstItem^.PrevItemIndex := -1;
    FFirstItem^.NextItemIndex := -1;

    FLastItem^.InUse    := false;
    FLastItem^.Data     := nil;
    FLastItem^.PrevItemIndex := -1;
    FLastItem^.NextItemIndex := -1;

    FirstItemIndex := -1;
    LastItemIndex  := -1;
  end else
  begin
    if FLastItem^.PrevItemIndex = -1 then raise EVamLibException.Create('Next item must be assigned.');

    Data     := FLastItem^.Data;
    PrevItemIndex := FLastItem^.PrevItemIndex;

    // reset the current last item.
    FLastItem^.InUse    := false;
    FLastItem^.Data     := nil;
    FLastItem^.PrevItemIndex := -1;
    FLastItem^.NextItemIndex := -1;

    ListData[PrevItemIndex].NextItemIndex := -1;
    LastItemIndex := PrevItemIndex;
  end;

  // If we make it this far, we've returned some data.
  result := Data;
end;



end.
