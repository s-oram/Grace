unit uIntegerList;

interface

uses
  Classes;

type
  TIntegerList = class
  private
    List:TList;
    function GetCount: integer;
    function GetItems(Index: integer): integer;
    procedure SetItems(Index: integer; const Value: integer);

  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value:integer):integer;
    procedure Clear;
    procedure Delete(Index:integer);
    function IndexOf(Value:integer):integer;
    function Remove(Value:integer):integer;
    procedure Sort;

    property Count:integer read GetCount;
    property Items[Index:integer]:integer read GetItems write SetItems;
  end;

function SortCompare(Item1, Item2: Pointer): Integer;


implementation

function SortCompare(Item1, Item2: Pointer): Integer;
var a,b:^integer;
begin
  a := Item1;
  b := Item2;
  if a^ = b^ then result := 0
    else
  if a^ > b^ then result := 1
  else result := -1;

  {
  if integer(Item1) = integer(Item2) then result := 0
    else
  if integer(Item1) > integer(Item2) then result := 1
  else result := -1
  }
end;

{ TIntegerList }
constructor TIntegerList.Create;
begin
  List := TList.Create;
end;

destructor TIntegerList.Destroy;
begin
  Clear;
  List.Free;
  inherited;
end;

function TIntegerList.GetCount: integer;
begin
  result := List.Count;
end;

function TIntegerList.Add(Value: integer): integer;
var p:^integer;
begin
  new(p);
  p^ := Value;
  result := List.Add(p);
end;

procedure TIntegerList.Delete(Index: integer);
var p:^integer;
begin
  p := List.Items[Index];
  Dispose(p);
  List.Delete(Index);
end;

procedure TIntegerList.Clear;
var
  c1:integer;
  p:^integer;
begin
  for c1 := 0 to List.Count-1 do
  begin
    p := List.Items[c1];
    Dispose(p);
  end;
  List.Clear;
end;


//If the 'Value' is in the list, IndexOf returns the index, if 'Value'
//is not in the list, IndexOf returns -1.
function TIntegerList.IndexOf(Value: integer): integer;
var
  c1:integer;
  p:^integer;
begin
  result := -1;

  for c1 := 0 to List.Count - 1 do
  begin
    p := List.Items[c1];
    if p^ = Value then
    begin
      result := c1;
      exit;
    end;
  end;

end;

function TIntegerList.Remove(Value: integer): integer;
var
  c1:integer;
  p:^integer;
begin
  result := -1;

  for c1 := 0 to List.Count - 1 do
  begin
    p := List.Items[c1];
    if p^ = Value then
    begin
      List.Delete(c1);
      result := c1;
      exit;
    end;
  end;
end;

procedure TIntegerList.SetItems(Index: integer; const Value: integer);
var pi:^integer;
begin
  pi := (List.Items[Index]);
  pi^ := Value;
end;

function TIntegerList.GetItems(Index: integer): integer;
var pi:^integer;
begin
  pi := (List.Items[Index]);
  result := pi^;
end;


procedure TIntegerList.Sort;
begin
  List.Sort(SortCompare);
end;

end.
