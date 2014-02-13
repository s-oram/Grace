unit VamLib.ZeroObject;

interface

uses
  Classes,
  VamLib.Types;

type
  {
    TZeroObject and TMotherShip are implementations of
    an idea detailed by Urs Heckmann.

    TZeroObject is a generic root class that can be
    used as an ancestor for any class. TZeroObjects
    can be optionally be reference counted when
    used via interfaces.

    TMotherShip is a repository that ZeroObjects can
    be registered with. The TMotherShip will provide
    methods for interacting with the collected
    ZeroObjects, notably message sending.
  }

  //Forward declarations
  IZeroObject = interface;
  TZeroObject = class;

  IMotherShip = interface;
  TMotherShip = class;

  IZeroObject = interface
    ['{F7C2493B-01CF-4980-A1E0-F6FB862DC576}']
    function GetClassName : string;
    function GetZeroObject:TZeroObject;
    procedure RegisterWithMotherShip(const Mothership:IMotherShip);
  end;

  IMotherShip = interface
    ['{3668F765-A3E2-4CDC-8B3A-BDCE6C430172}']
    procedure RegisterZeroObject(obj:IZeroObject);
    procedure DeregisterZeroObject(obj:IZeroObject);
  end;

  TZeroObject = class(TObject, IInterface, IZeroObject)
  private
    FMotherShip : IMotherShip;
    function GetZeroObject:TZeroObject;
    function GetClassName : string;
  protected
    // FIsReferenceCounted should normally be set in the contstructor of the object.
    FIsReferenceCounted: boolean;
    FRefCount: Integer;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property RefCount           : Integer read FRefCount;
  public
    destructor Destroy; override;

    // ZeroObjects can register with a MotherShip. They will automatically
    // deregister when the object is freed or released.
    procedure RegisterWithMotherShip(const Mothership:IMotherShip);

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
  end;


  // TMotherShip is not reference counted.
  TMotherShip = class(TPureInterfacedObject, IMotherShip)
  private
    Objects : TInterfaceList;
    function GetZeroObjectCount: integer;

    procedure RegisterZeroObject(obj:IZeroObject);
    procedure DeregisterZeroObject(obj:IZeroObject);
  public
    constructor Create;
    destructor Destroy; override;

    property ZeroObjectCount : integer read GetZeroObjectCount;
  end;



implementation

uses
  SysUtils,
  VamLib.WinUtils;

{$I InterlockedAPIs.inc}

{ TZeroObject }

procedure TZeroObject.AfterConstruction;
begin
  // Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TZeroObject.BeforeDestruction;
begin

end;

destructor TZeroObject.Destroy;
begin
  if (not FIsReferenceCounted) and (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
  end;

  inherited;
end;

procedure TZeroObject.RegisterWithMotherShip(const Mothership: IMotherShip);
begin
  FMotherShip := MotherShip;
  FMotherShip.RegisterZeroObject(self);
end;


function TZeroObject.GetClassName: string;
begin
  result := self.ClassName;
end;

function TZeroObject.GetZeroObject: TZeroObject;
begin
  result := self;
end;

class function TZeroObject.NewInstance: TObject;
begin
  // Set an implicit refcount so that refcounting
  // during construction won't destroy the object.
  Result := inherited NewInstance;
  TZeroObject(Result).FRefCount := 1;
end;

function TZeroObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TZeroObject._AddRef: Integer;
begin
  FRefCount := InterlockedIncrement(FRefCount);

  if FIsReferenceCounted
    then result := FRefCount
    else result := -1;
end;

function TZeroObject._Release: Integer;
begin
  {
    The lifetime management of ZeroObjects when they
    are reference counted is a little complicated.

    ZeroObjects are stored via an interface reference
    in the motherShip. They need to detect when the last
    reference is held by the mothership, when so the
    ZeroObject will automatically deregister (from the
    mothership) allowing itself to be freed.

    This problem could be avoid by replacing the MotherShip
    interface list with an object list. The ZeroObjects
    would reference as object instances, not interface
    references.
  }

  FRefCount := InterlockedDecrement(FRefCount);

  if FIsReferenceCounted
    then result := FRefCount
    else result := -1;

  if (result = 1) and (assigned(FMotherShip)) and (FIsReferenceCounted) then
  begin
    // The mothership is holding the last interface reference and
    // preventing this object from being freed. Deregister
    // this object so we can be free!
    FMotherShip.DeregisterZeroObject(self);
  end;

  if (result = 0) and (FIsReferenceCounted) then
  begin
    Destroy;
  end;

end;

{ TMotherShip }

constructor TMotherShip.Create;
begin
  Objects := TInterfaceList.Create;
end;

destructor TMotherShip.Destroy;
var
  c1: Integer;
  Text : string;
  zo : IZeroObject;
begin
  if Objects.Count > 0 then
  begin
    for c1 := Objects.Count-1 downto 0 do
    begin
      zo :=  (Objects[c1] as IZeroObject);
      Text := zo.GetClassName;
      Text := Text + ' has not been freed!';
      SendDebugMesssage(Text);

      //TODO: the softly-softly approach to not freeing all the zero objects.
      //self.DeregisterZeroObject(zo);
      //zo := nil;
    end;

    // TODO: the hard arse way to respond to unfreed zero objects. This
    // should probably be removed in release builds!
    raise Exception.Create('Not all ZeroObjects have been freed.');
  end;




  Objects.Free;
  inherited;
end;

function TMotherShip.GetZeroObjectCount: integer;
begin
  result := Objects.Count;
end;

procedure TMotherShip.RegisterZeroObject(obj: IZeroObject);
begin
  //Add obj to interface list.
  if Objects.IndexOf(obj) = -1 then
  begin
    Objects.Add(obj);
  end;
end;

procedure TMotherShip.DeregisterZeroObject(obj: IZeroObject);
begin
  // Important: clear the MotherShip reference here
  // before remove the ZeroObject from the interface list.
  // Not doing so will lead to a stackoverflow error.
  obj.GetZeroObject.FMotherShip := nil;

  //remove obj from interface list.
  if Objects.IndexOf(obj) <> -1 then
  begin
    Objects.Remove(obj);
  end;
end;



end.
