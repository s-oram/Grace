unit AudioPlugin.Globals;

interface

uses
  Classes;

type
  TObjectStore = class
  private
    FObjectList: TStringList;
    function GetObj(Key: string): TObject;
  public
    constructor Create;
    destructor Destroy; override;

    function Exists(const Key : string):boolean;
    procedure Add(const Key : string; const Obj : TObject);
    property Obj[Key : string]:TObject read GetObj; default;
  end;

  TGlobals = class
  private
    FObjectStore: TObjectStore;
  public
    constructor Create;
    destructor Destroy; override;

    property ObjectStore : TObjectStore read FObjectStore;
  end;

implementation

{ TGlobals }

constructor TGlobals.Create;
begin
  FObjectStore := TObjectStore.Create;
end;

destructor TGlobals.Destroy;
begin
  FObjectStore.Free;
  inherited;
end;

{ TGlobalObjectStore }

constructor TObjectStore.Create;
begin
  FObjectList := TStringList.Create;
  FObjectList.OwnsObjects := true;
  FObjectList.Duplicates := dupError;
end;

destructor TObjectStore.Destroy;
begin
  FObjectList.Free;
  inherited;
end;

function TObjectStore.Exists(const Key: string): boolean;
begin
  if FObjectList.IndexOf(Key) <> -1
    then result := true
    else result := false;
end;

procedure TObjectStore.Add(const Key: string; const Obj: TObject);
begin
  FObjectList.AddObject(Key, Obj);
end;

function TObjectStore.GetObj(Key: string): TObject;
var
  Index : integer;
begin
  Index := FObjectList.IndexOf(Key);
  if Index <> -1
    then result := FObjectList.Objects[Index]
    else result := nil;
end;

end.
