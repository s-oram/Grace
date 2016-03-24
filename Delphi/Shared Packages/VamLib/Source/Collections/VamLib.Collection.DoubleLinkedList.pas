unit VamLib.Collection.DoubleLinkedList;

interface

type
  PDoubleLinkedListItem = ^TDoubleLinkedListItem;
  TDoubleLinkedListItem = record
    InUse    : boolean;
    Data     : Pointer;
    PrevItem : Pointer;
    NextItem : Pointer;
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
    FFirstItem: PDoubleLinkedListItem;
    FLastItem: PDoubleLinkedListItem;
    FMaxCapacity: integer;
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

  FFirstItem := nil;
  FLastItem  := nil;
end;

function TRecDoubleLinkedList.GetFirst: Pointer;
begin
  if assigned(FFirstItem)
    then result := FFirstItem^.Data
    else result := nil;
end;

function TRecDoubleLinkedList.GetLast: Pointer;
begin
  if assigned(FLastItem)
    then result := FLastItem^.Data
    else result := nil;
end;

procedure TRecDoubleLinkedList.SetCapacity(const Value: integer);
begin
  assert(Value >= 0);
  FCapacity := Value;
  SetLength(ListData, Value);
end;

procedure TRecDoubleLinkedList.AppendItem(const Data: Pointer);
var
  c1 : integer;
  WriteIndex : integer;
  CurLast : PDoubleLinkedListItem;
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

  if (assigned(FFirstItem) <> assigned(FLastItem))
    then raise EVamLibException.Create('Something has gone wrong. First & Last items assignment state must match.');

  if not assigned(FFirstItem) then
  begin
    assert(not assigned(FLastItem));

    ListData[WriteIndex].InUse := true;
    ListData[WriteIndex].Data := Data;
    ListData[WriteIndex].PrevItem := nil;
    ListData[WriteIndex].NextItem := nil;
    FFirstItem := @ListData[WriteIndex];
    FLastItem  := @ListData[WriteIndex];
  end else
  begin
    assert(assigned(FLastItem));

    // Update the current last item
    FLastItem^.NextItem := @ListData[WriteIndex];

    // Add values for the new last item.
    ListData[WriteIndex].InUse := true;
    ListData[WriteIndex].Data := Data;
    ListData[WriteIndex].PrevItem := FLastItem;
    ListData[WriteIndex].NextItem := nil;

    // Update the last item pointer to point to the new item.
    FLastItem  := @ListData[WriteIndex];
  end;

  inc(FCount);
end;

function TRecDoubleLinkedList.PopFirst:Pointer;
var
  Data : Pointer;
  NextItem : PDoubleLinkedListItem;
begin
  if not assigned(FFirstItem) then exit(nil);

  dec(FCount);

  if FFirstItem = FLastItem then
  begin
    Data := FFirstItem^.Data;

    FFirstItem^.InUse    := false;
    FFirstItem^.Data     := nil;
    FFirstItem^.PrevItem := nil;
    FFirstItem^.NextItem := nil;

    FLastItem^.InUse    := false;
    FLastItem^.Data     := nil;
    FLastItem^.PrevItem := nil;
    FLastItem^.NextItem := nil;

    FFirstItem := nil;
    FLastItem := nil;
  end else
  begin
    if not assigned(FFirstItem^.NextItem) then raise EVamLibException.Create('Next item must be assigned.');

    Data     := FFirstItem^.Data;
    NextItem := FFirstItem^.NextItem;

    // reset the current first item.
    FFirstItem^.InUse    := false;
    FFirstItem^.Data     := nil;
    FFirstItem^.PrevItem := nil;
    FFirstItem^.NextItem := nil;

    NextItem^.PrevItem := nil;
    FFirstItem := NextItem;
  end;

  // If we make it this far, we've returned some data.
  result := Data;
end;

function TRecDoubleLinkedList.PopLast:Pointer;
var
  Data : Pointer;
  PrevItem : PDoubleLinkedListItem;
begin
  if not assigned(FFirstItem) then exit(nil);

  dec(FCount);

  if FFirstItem = FLastItem then
  begin
    Data := FFirstItem^.Data;

    FFirstItem^.InUse    := false;
    FFirstItem^.Data     := nil;
    FFirstItem^.PrevItem := nil;
    FFirstItem^.NextItem := nil;

    FLastItem^.InUse    := false;
    FLastItem^.Data     := nil;
    FLastItem^.PrevItem := nil;
    FLastItem^.NextItem := nil;

    FFirstItem := nil;
    FLastItem := nil;
  end else
  begin
    if not assigned(FLastItem^.PrevItem) then raise EVamLibException.Create('Previous item must be assigned.');

    Data     := FLastItem^.Data;
    PrevItem := FLastItem^.PrevItem;

    // reset the current last item.
    FLastItem^.InUse    := false;
    FLastItem^.Data     := nil;
    FLastItem^.PrevItem := nil;
    FLastItem^.NextItem := nil;

    PrevItem^.NextItem := nil;
    FLastItem := PrevItem;
  end;

  // If we make it this far, we've returned some data.
  result := Data;
end;



end.
