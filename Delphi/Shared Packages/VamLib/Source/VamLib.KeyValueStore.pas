unit VamLib.KeyValueStore;

interface

uses
  Classes;


type
  TKeyValueStore = class
  private
    Keys, Data : TStringList;
    function GetCount: integer;
  protected
    procedure SetKeyValueByName(KeyName: string; Value: string);
    function GetKeyValueName(KeyName : string) : string;
    function IndexOf(KeyStr : string) : integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function Exists(KeyName : string) : boolean; overload;
    function Exists(const KeyName : string; out Value : string) : boolean; overload;

    procedure AddKey(const KeyName, KeyValue : string);
    procedure DeleteKey(const KeyName : string);

    procedure KeyByIndex(const Index : integer; out KeyName, KeyValue : string);
    property KeyCount : integer read GetCount;

    property Key[KeyName : string] : string read GetKeyValueName write SetKeyValueByName;
  end;

implementation

uses
  SysUtils;

{ TKeyValueStore }

procedure TKeyValueStore.Clear;
begin
  Keys.Clear;
  Data.Clear;
end;

constructor TKeyValueStore.Create;
begin
  Keys := TStringList.Create;
  Data := TStringList.Create;
end;

destructor TKeyValueStore.Destroy;
begin
  Keys.Free;
  Data.Free;
  inherited;
end;

function TKeyValueStore.Exists(const KeyName: string; out Value: string): boolean;
var
  Index : integer;
begin
  assert(KeyName <> '');

  Index := IndexOf(KeyName);
  if Index <> -1 then
  begin
    Value := Data[Index];
    result := true;
  end else
  begin
    Value := '';
    result := false;
  end;

end;

function TKeyValueStore.GetKeyValueName(KeyName: string): string;
var
  Index : integer;
begin
  assert(KeyName <> '');

  Index := IndexOf(KeyName);
  if Index <> -1 then
  begin
    result := Data[Index];
  end else
  begin
    result := '';
    raise Exception.Create('Key doesn''t exist.');
  end;

end;



function TKeyValueStore.Exists(KeyName: string): boolean;
begin
  assert(KeyName <> '');

  if IndexOf(KeyName) <> -1
    then result := true
    else result := false;
end;

function TKeyValueStore.GetCount: integer;
begin
  result := Keys.Count;
end;

function TKeyValueStore.IndexOf(KeyStr: string): integer;
var
  c1: Integer;
begin
  // NOTE: IMPORTANT:
  if KeyStr = '' then raise Exception.Create('KeyStr cannot be empty.');

  for c1 := 0 to Keys.Count-1 do
  begin
    if SameText(Keys[c1], KeyStr) then exit(c1);
  end;
  // matching key not found.
  result := -1;
end;


procedure TKeyValueStore.KeyByIndex(const Index: integer; out KeyName, KeyValue: string);
begin
  KeyName  := Keys[Index];
  KeyValue := Data[Index];
end;

procedure TKeyValueStore.AddKey(const KeyName, KeyValue: string);
begin
  SetKeyValueByName(KeyName, KeyValue);
end;

procedure TKeyValueStore.SetKeyValueByName(KeyName: string; Value: string);
var
  Index : integer;
begin
  assert(KeyName <> '');

  Index := IndexOf(KeyName);
  if Index <> -1 then
  begin
    Data[Index] := Value;
  end else
  begin
    Keys.Add(KeyName);
    Data.Add(Value);
    assert(Keys.Count = Data.Count);
  end;
end;

procedure TKeyValueStore.DeleteKey(const KeyName: string);
var
  Index : integer;
begin
  assert(KeyName <> '');

  Index := IndexOf(KeyName);
  if Index <> -1 then
  begin
    Keys.Delete(Index);
    Data.Delete(Index);
    assert(Keys.Count = Data.Count);
  end;
end;

end.
