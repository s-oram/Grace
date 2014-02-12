unit EasyEffect.ZeroObject;

interface

uses
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

    TMotherShip is not reference counted but still
    implements a IMotherShip interface.

    The lifetime management of ZeroObjects when they
    are reference counted is a little complicated.
    Essentially ZeroObjects detect when the last
    interface reference is held by the mothership,
    then they deregister themselves, allowing them
    to be freed.

  }




  //Forward declarations
  IZeroObject = interface;
  TZeroObject = class;

  IMotherShip = interface;
  TMotherShip = class;

  IZeroObject = interface
    ['{F7C2493B-01CF-4980-A1E0-F6FB862DC576}']
    function GetZeroObject:TZeroObject;
  end;

  IMotherShip = interface
    ['{3668F765-A3E2-4CDC-8B3A-BDCE6C430172}']
    procedure RegisterZeroObject(obj:IZeroObject);
    procedure DeregisterZeroObject(obj:IZeroObject);
  end;

  TZeroObject = class(TObject, IInterface, IZeroObject)
  private
    FIsReferenceCounted: boolean;
    FMotherShip : IMotherShip;
    function GetZeroObject:TZeroObject;
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property IsReferenceCounted : boolean read FIsReferenceCounted write FIsReferenceCounted;
    property RefCount           : Integer read FRefCount;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
  end;

  TMotherShip = class(TPureInterfacedObject, IMotherShip)
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterZeroObject(obj:IZeroObject);
    procedure DeregisterZeroObject(obj:IZeroObject);
  end;



implementation

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
    assert(not assigned(FMotherShip));
    Destroy;
  end;

end;

{ TMotherShip }

constructor TMotherShip.Create;
begin

end;

destructor TMotherShip.Destroy;
begin

  inherited;
end;

procedure TMotherShip.RegisterZeroObject(obj: IZeroObject);
var
  zo : TZeroObject;
begin
  zo := obj.GetZeroObject;
  zo.FMotherShip := self;

  //Add obj to interface list.
end;

procedure TMotherShip.DeregisterZeroObject(obj: IZeroObject);
var
  zo : TZeroObject;
begin
  // important: remove reference to mothership first.
  zo := obj.GetZeroObject;
  zo.FMotherShip := nil;

  //remove obj from interface list.



end;



end.
