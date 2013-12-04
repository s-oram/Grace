unit VamLib.Collections.RecordArray;

interface

type
  // TRecordArray is a generic type so arrays can be used with some basic list-like
  // methods.
  // At the moment it is quite light weight compared to TList.
  // TList ~8000 bytes.
  // TRecordArray ~900 bytes.
  // I think the access to "Raw" underlying array will provide faster access in
  // some time critical applications as well.

  TRecordArray<T : record> = record
  private
    fCount : integer;
    fCapacity : integer;
    procedure SetCount(const Value: integer);
    procedure SetCapacity(const Value: integer);
    function GetItem(Index: integer): T;  inline;

    procedure Grow; inline;
    procedure SetItem(Index: integer; const Value: T);
  public
    Raw : TArray<T>; //The raw, naked, array.

    function New:Pointer;

    function Append(const Value : T):integer;
    procedure Delete(const Index : integer);

    procedure Clear;

    property Count    : integer read fCount    write SetCount;
    property Capacity : integer read fCapacity write SetCapacity;

    property Items[Index:integer]:T read GetItem write SetItem; default;
  end;

implementation


{ TRecordArray<T> }

procedure TRecordArray<T>.Clear;
begin
  fCapacity := 0;
  fCount    := 0;
  SetLength(Raw, 0);
end;

function TRecordArray<T>.GetItem(Index: integer): T;
begin
  result := Raw[Index];
end;

procedure TRecordArray<T>.Grow;
begin
  SetLength(Raw, fCapacity + 10);
  inc(fCapacity, 10);
end;

function TRecordArray<T>.New: Pointer;
begin
  if (fCount) >= fCapacity then Grow;
  result := @Raw[fCount];
  inc(fCount);
end;

procedure TRecordArray<T>.SetCapacity(const Value: integer);
begin
  fCapacity := Value;
  SetLength(Raw, Value);
  if fCapacity < fCount then fCount := fCapacity;
end;

procedure TRecordArray<T>.SetCount(const Value: integer);
begin
  fCount := Value;
  if fCount > fCapacity then
  begin
    SetCapacity(Value);
  end;
end;

procedure TRecordArray<T>.SetItem(Index: integer; const Value: T);
begin
  Finalize(Raw[Index]);
  Move(Value, Raw[Index], SizeOf(T));
end;

function TRecordArray<T>.Append(const Value: T):integer;
begin
  if (fCount) >= fCapacity then Grow;

  // important: call finalize before moving the new value in to avoid memory
  // leaks.
  Finalize(Raw[fCount]);

  Move(Value, Raw[fCount], SizeOf(T));
  inc(fCount);
  result := fCount;
end;

procedure TRecordArray<T>.Delete(const Index: integer);
var
  DataSize   : integer;
  c1: Integer;
begin
  assert(Index >= 0);
  assert(Index < fCount);

  if Index = fCount-1 then
  begin
    dec(fCount);
  end else
  begin
    DataSize := SizeOf(T);

    for c1 := Index to fCount-2 do
    begin
      Finalize(Raw[c1]);
      Move(Raw[c1+1], Raw[c1], DataSize);
    end;

    dec(fCount);
  end;
end;





end.
