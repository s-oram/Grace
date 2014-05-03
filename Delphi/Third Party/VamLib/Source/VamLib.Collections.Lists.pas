unit VamLib.Collections.Lists;

interface


type
  // The 'Simple' list classes are lightweight list classes for use
  // where a full list class isn't required.
  // - Iteration via the Raw:TArray field is quick.
  // - Delete operations are slow.
  // - Insert operations are slow.
  // - Append operations are good.

  TSimpleList<T> = class;
  TSimpleObjectList<T : class> = class;

  //TObjectList = class;

  TSimplePointerList = class;



  TSimpleList<T> = class
  private
    fCount : integer;
    fCapacity : integer;
    fGrowBy: integer;
    procedure SetCount(const Value: integer);
    procedure SetCapacity(const Value: integer);
    function GetItem(Index: integer): T;  inline;

    procedure Grow;
    procedure SetItem(Index: integer; const Value: T);
    procedure SetGrowBy(const Value: integer);
  public
    Raw : TArray<T>; //The raw, naked, array.

    constructor Create; virtual;
    destructor Destroy; override;

    function New:Pointer;

    function Add(const Value : T):integer; virtual;
    function Append(const Value : T):integer; deprecated; //TODO: delete these append methods.
    procedure Delete(const Index : integer);

    procedure Clear;

    property Count    : integer read fCount    write SetCount;
    property Capacity : integer read fCapacity write SetCapacity;
    property GrowBy   : integer read fGrowBy   write SetGrowBy;

    property Items[Index:integer]:T read GetItem write SetItem; default;
  end;


  //TODO: It would be better to move some of this code to a TCustomObjectList so
  // it isn't re-compiled into generic versions of the list.
  //TODO: this object list isn't working as expected.
  //  I can't add an object and then test if it exists via IndexOf().
  //  This behaviour was noticed within the eeGuiStandard.RedFoxKnob handler class
  //  implementation. I should come back and rewrite this and retest everything...
  TSimpleObjectList<T : class> = class
  private
    fCount : integer;
    fCapacity : integer;
    fOwnsObjects: boolean;
    fAllowDuplicates: boolean;
    procedure SetCount(const Value: integer);
    procedure SetCapacity(const Value: integer);
    function GetItem(Index: integer): T;  inline;
    procedure Grow;
    procedure SetItem(Index: integer; const Value: T);
  protected
    procedure FreeObjectAt(Index : integer);
  public
    Raw : TArray<T>; //The raw, naked, array.

    destructor Destroy; override;

    function IndexOf(Item : T):integer;

    function Add(Item : T):integer;
    procedure Delete(const Index : integer); overload;
    procedure Delete(const Item : T); overload;

    procedure Clear;

    property Count    : integer read fCount    write SetCount;
    property Capacity : integer read fCapacity write SetCapacity;

    property Items[Index:integer]:T read GetItem write SetItem; default;

    property OwnsObjects : boolean read fOwnsObjects write fOwnsObjects;

    property AllowDuplicates : boolean read fAllowDuplicates write fAllowDuplicates default true;
  end;

  TSimpleRecordList<T : record> = class
  private
    fCount : integer;
    fCapacity : integer;
    procedure SetCount(const Value: integer);
    procedure SetCapacity(const Value: integer);
    function GetItem(Index: integer): T;  inline;

    procedure Grow;
    procedure SetItem(Index: integer; const Value: T);
  public
    Raw : TArray<T>; //The raw, naked, array.

    destructor Destroy; override;

    function New:Pointer;

    function Add(const Value : T):integer;
    procedure Delete(const Index : integer);

    procedure Clear;

    property Count    : integer read fCount    write SetCount;
    property Capacity : integer read fCapacity write SetCapacity;

    property Items[Index:integer]:T read GetItem write SetItem; default;
  end;


  {
  //DO not use for now...
  TObjectList = class(TSimpleObjectList<TObject>)
  private
  public
  end;
  }

  TSimplePointerList = class(TSimpleList<Pointer>)
  private
  public
    procedure Delete(Value : Pointer); overload;
  end;




  TIntegerList = class(TSimpleList<Integer>)
  private
    fAllowDuplicates: boolean;
  public
    constructor Create; override;

    function Contains(const Value : integer): boolean;
    function Add(const Value : integer):integer; override;
    procedure DeleteValue(const Value : integer);

    property AllowDuplicates : boolean read fAllowDuplicates write fAllowDuplicates;
  end;

implementation

uses
  SysUtils;

{ TSimpleList<T> }

destructor TSimpleList<T>.Destroy;
begin
  SetLength(Raw, 0);
  inherited;
end;

procedure TSimpleList<T>.Clear;
begin
  fCapacity := 0;
  fCount    := 0;
  SetLength(Raw, 0);
end;

constructor TSimpleList<T>.Create;
begin
  fGrowBy := 1;
end;

function TSimpleList<T>.GetItem(Index: integer): T;
begin
  result := Raw[Index];
end;

procedure TSimpleList<T>.Grow;
begin
  SetLength(Raw, fCapacity + fGrowBy);
  inc(fCapacity, fGrowBy);
end;

function TSimpleList<T>.New: Pointer;
begin
  if (fCount) >= fCapacity then Grow;
  result := @Raw[fCount];
  inc(fCount);
end;

procedure TSimpleList<T>.SetCapacity(const Value: integer);
begin
  fCapacity := Value;
  SetLength(Raw, Value);
  if fCapacity < fCount then fCount := fCapacity;
end;

procedure TSimpleList<T>.SetCount(const Value: integer);
begin
  fCount := Value;
  if fCount > fCapacity then
  begin
    SetCapacity(Value);
  end;
end;

procedure TSimpleList<T>.SetGrowBy(const Value: integer);
begin
  assert(Value >= 1);
  fGrowBy := Value;
end;

procedure TSimpleList<T>.SetItem(Index: integer; const Value: T);
begin
  Finalize(Raw[Index]);
  Move(Value, Raw[Index], SizeOf(T));
end;

function TSimpleList<T>.Add(const Value: T): integer;
begin
  if (fCount) >= fCapacity then Grow;

  // important: call finalize before moving the new value in to avoid memory
  // leaks.
  Finalize(Raw[fCount]);

  Move(Value, Raw[fCount], SizeOf(T));
  inc(fCount);
  result := fCount-1;
end;

function TSimpleList<T>.Append(const Value: T):integer;
begin
  result := Add(Value);
end;

procedure TSimpleList<T>.Delete(const Index: integer);
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







{ TSimpleObjectList<T> }

destructor TSimpleObjectList<T>.Destroy;
begin
  Clear;
  inherited;
end;


procedure TSimpleObjectList<T>.FreeObjectAt(Index: integer);
begin
  if assigned(Raw[Index]) then
  begin
    (Raw[Index] as TObject).Free;
    Raw[Index] := nil;
  end;
end;

procedure TSimpleObjectList<T>.Clear;
var
  c1: Integer;
begin
  if OwnsObjects then
  begin
    for c1 := 0 to fCount-1 do FreeObjectAt(c1);
  end;

  fCapacity := 0;
  fCount    := 0;
  SetLength(Raw, 0);
end;

function TSimpleObjectList<T>.GetItem(Index: integer): T;
begin
  result := Raw[Index];
end;

procedure TSimpleObjectList<T>.Grow;
begin
  SetLength(Raw, fCapacity + 10);
  inc(fCapacity, 10);
end;

function TSimpleObjectList<T>.IndexOf(Item: T): integer;
var
  c1: Integer;
begin
  for c1 := 0 to fCount-1 do
  begin
    if Raw[c1] = T
      then exit(c1);
  end;

  result := -1;
end;

procedure TSimpleObjectList<T>.SetCapacity(const Value: integer);
begin
  fCapacity := Value;
  SetLength(Raw, Value);
  if fCapacity < fCount then fCount := fCapacity;
end;

procedure TSimpleObjectList<T>.SetCount(const Value: integer);
begin
  fCount := Value;
  if fCount > fCapacity then
  begin
    SetCapacity(Value);
  end;
end;

procedure TSimpleObjectList<T>.SetItem(Index: integer; const Value: T);
begin
  Finalize(Raw[Index]);
  Move(Value, Raw[Index], SizeOf(T));
end;


function TSimpleObjectList<T>.Add(Item: T): integer;
begin
  if (AllowDuplicates = false) and (IndexOf(Item) <> -1) then
  begin
    exit; //Item already in list.
  end;

  if fCount = fCapacity then Grow;
  Raw[fCount] := Item;
  inc(fCount);
end;

procedure TSimpleObjectList<T>.Delete(const Index: integer);
var
  DataSize   : integer;
  c1: Integer;
begin
  assert(Index >= 0);
  assert(Index < fCount);

  if Index = fCount-1 then
  begin
    if OwnsObjects then FreeObjectAt(Index);
    dec(fCount);
  end else
  begin
    if OwnsObjects then FreeObjectAt(Index);

    DataSize := SizeOf(T);

    for c1 := Index to fCount-2 do
    begin
      Move(Raw[c1+1], Raw[c1], DataSize);
    end;

    dec(fCount);
  end;
end;

procedure TSimpleObjectList<T>.Delete(const Item: T);
var
  Index : integer;
begin
  Index := IndexOf(Item);
  if Index <> -1
    then Delete(Index);
end;



{ TRecordArray<T> }

destructor TSimpleRecordList<T>.Destroy;
begin
  Clear;
  SetLength(Raw, 0);
  inherited;
end;

procedure TSimpleRecordList<T>.Clear;
var
  c1 : integer;
begin
  for c1 := 0 to fCount-1 do
  begin
    Finalize(Raw[c1]);
  end;

  fCapacity := 0;
  fCount    := 0;
end;

function TSimpleRecordList<T>.GetItem(Index: integer): T;
begin
  result := Raw[Index];
end;

procedure TSimpleRecordList<T>.Grow;
begin
  SetLength(Raw, fCapacity + 10);
  inc(fCapacity, 10);
end;

function TSimpleRecordList<T>.New: Pointer;
begin
  if (fCount) >= fCapacity then Grow;
  result := @Raw[fCount];
  inc(fCount);
end;

procedure TSimpleRecordList<T>.SetCapacity(const Value: integer);
begin
  fCapacity := Value;
  SetLength(Raw, Value);
  if fCapacity < fCount then fCount := fCapacity;
end;

procedure TSimpleRecordList<T>.SetCount(const Value: integer);
begin
  fCount := Value;
  if fCount > fCapacity then
  begin
    SetCapacity(Value);
  end;
end;

procedure TSimpleRecordList<T>.SetItem(Index: integer; const Value: T);
begin
  Finalize(Raw[Index]);
  Move(Value, Raw[Index], SizeOf(T));
end;

function TSimpleRecordList<T>.Add(const Value: T):integer;
begin
  if (fCount) >= fCapacity then Grow;

  // important: call finalize before moving the new value in to avoid memory
  // leaks.
  Finalize(Raw[fCount]);

  Move(Value, Raw[fCount], SizeOf(T));
  inc(fCount);
  result := fCount-1;
end;

procedure TSimpleRecordList<T>.Delete(const Index: integer);
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





{ TSimplePointerList }

procedure TSimplePointerList.Delete(Value: Pointer);
var
  c1: Integer;
begin
  for c1 := self.Count-1 downto 0 do
  begin
    if Items[c1] = Value
      then Delete(c1);
  end;
end;

{ TIntegerList }

constructor TIntegerList.Create;
begin
  inherited;
  AllowDuplicates := true;
end;

function TIntegerList.Add(const Value: integer): integer;
begin
  if AllowDuplicates then
  begin
    result := inherited Add(Value);
  end else
  if Contains(Value) = false then
  begin
    result := inherited Add(Value);
  end else
  begin
    result := -1;
  end;
end;

procedure TIntegerList.DeleteValue(const Value: integer);
var
  c1: Integer;
begin
  for c1 := Count-1 downto 0 do
  begin
    if Items[c1] = Value
      then Delete(c1);
  end;
end;



function TIntegerList.Contains(const Value: integer): boolean;
var
  c1: Integer;
begin
  for c1 := 0 to Count-1 do
  begin
    if Raw[c1] = Value
      then exit(true); //==== exit ====>>
  end;

  result := false;
end;


end.

