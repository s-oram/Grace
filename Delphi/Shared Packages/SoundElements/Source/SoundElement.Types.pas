unit SoundElement.Types;

interface

uses
  SysUtils,
  Contnrs;

type
  SoundElementModuleException = Exception;

  TNotifyAnonEvent = reference to procedure(Sender: TObject);


  // TSoundElementObjectArray is intended to be a light
  // weight object list component. Right now it
  // wraps an object list class. Later on I might try
  // to add an optimised lightweight alternative implementation.
  TSoundElementObjectArray = class
  private
    ObjList : TObjectList;
    function GetCount: integer;
    function GetObject(Index: integer): TObject;
    function GetFirst: TObject;
    function GetLast: TObject;
  public
    constructor Create;
    destructor Destroy; override;

    function Contains(const Obj : TObject):boolean;

    procedure AddObject(const Obj : TObject);
    procedure Remove(const Obj : TObject);
    property Count : integer read GetCount;

    property Objects[Index:integer] : TObject read GetObject; default;
    property First : TObject read GetFirst;
    property Last : TObject read GetLast;
  end;

implementation

{ TSoundElementObjectArray }

function TSoundElementObjectArray.Contains(const Obj: TObject): boolean;
begin
  if ObjList.IndexOf(Obj) <> -1
    then result := true
    else result := false;
end;

constructor TSoundElementObjectArray.Create;
begin
  ObjList := TObjectList.Create;
  ObjList.OwnsObjects := false;
end;

destructor TSoundElementObjectArray.Destroy;
begin
  ObjList.Free;
  inherited;
end;

procedure TSoundElementObjectArray.AddObject(const Obj: TObject);
begin
  ObjList.Add(Obj);
end;



function TSoundElementObjectArray.GetCount: integer;
begin
  result := ObjList.Count;
end;

function TSoundElementObjectArray.GetFirst: TObject;
begin
  result := ObjList[0];
end;

function TSoundElementObjectArray.GetLast: TObject;
var
  Index : integer;
begin
  Index := ObjList.Count-1;
  result := ObjList[Index];
end;

function TSoundElementObjectArray.GetObject(Index: integer): TObject;
begin
  result := ObjList[Index];
end;

procedure TSoundElementObjectArray.Remove(const Obj: TObject);
begin
  ObjList.Remove(Obj);
end;


end.
