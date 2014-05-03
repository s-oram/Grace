unit VamLib.ManagedObject;

interface

uses
  Classes,
  VamLib.UniqueID,
  VamLib.ZeroObject;

type
  IManagedObject = interface
    ['{46C89351-FAF7-422D-87DD-D50CE8BC8F62}']

    procedure FreeObject;

    function IncManagedObjectReference : integer;
    function DecManagedObjectReference : integer;
    function GetManagedID : TUniqueID;
  end;


  TManagedObjectLifeTimeManager = class
  private
  protected
    ObjectList : TList;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    // Add a new object to have it's life time automatically managed.
    // Adding a object will not change it's reference count.
    procedure Add(var Obj : IManagedObject);

    // request an existing object. Will incremented the objects reference count.
    function Request(ObjID : TUniqueID):IManagedObject;

    // dipose of an object when no longer needed. Object's reference count
    // will be reduced by one. If the object's reference count is zero it will
    // be destroyed.
    procedure Dispose(var Obj : IManagedObject); overload;
    procedure Dispose(const ObjID : TUniqueID); overload;
  end;


  TManagedObject = class(TObject, IInterface, IManagedObject)
  private
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    fManagedID: TUniqueID;
    FMangedObjectReferenceCount : integer;
    function IncManagedObjectReference : integer; virtual;
    function DecManagedObjectReference : integer; virtual;
    procedure FreeObject; virtual;
    function GetManagedID : TUniqueID;
  public
    procedure AfterConstruction; override;

    property ManagedID : TUniqueID read fManagedID;
  end;


  TManagedZeroObject = class(TManagedObject, IZeroObject)
  private
    FMotherShip : IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
  protected
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); virtual;
  public
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  VamLib.LoggingProxy;

{$I InterlockedAPIs.inc}

{ TManagedObjectLifeTimeManager }

constructor TManagedObjectLifeTimeManager.Create;
begin
  ObjectList := TList.Create;
end;

destructor TManagedObjectLifeTimeManager.Destroy;
begin
  if ObjectList.Count > 0 then
  begin
    Log.LogMessage('Managed Objects still registered (' + IntToStr(ObjectList.Count) + ').');

    // All managed objects *should* have been returned and free'ed by now.
    // Calling Clear will forcibly destroy any remaining objects regardless of
    // their reference count. Obviously this will cause problems if those
    // objects are still being used somewhere. However that indicates a bug
    // or a failure application design. (Fail fast!)
    Clear;
  end;

  ObjectList.Free;
  inherited;
end;

procedure TManagedObjectLifeTimeManager.Clear;
var
  c1: Integer;
begin
  // WARNING: Clear will destroy all objects regardless of their reference count.
  for c1 := ObjectList.Count-1 downto 0 do
  begin
    IManagedObject(ObjectList[c1]).FreeObject;
  end;
end;



procedure TManagedObjectLifeTimeManager.Add(var Obj: IManagedObject);
var
  ptr : Pointer;
  c1: Integer;
  ObjID : TUniqueID;
begin
  assert(assigned(Obj));

  ptr := Pointer(obj); //Weak reference to IManagedObject

  //=====================================================================
  // Check if the object is already in the list...
  ObjID := Obj.GetManagedID;
  for c1 := 0 to ObjectList.Count-1 do
  begin
    if ObjID = IManagedObject(ObjectList[c1]).GetManagedID
      then raise Exception.Create('Object already in list.');
  end;
  //=====================================================================

  ObjectList.Add(ptr);
end;

function TManagedObjectLifeTimeManager.Request(ObjID: TUniqueID): IManagedObject;
var
  c1: Integer;
  Obj : IManagedObject;
begin
  for c1 := 0 to ObjectList.Count-1 do
  begin
    Obj := IManagedObject(ObjectList[c1]);
    if ObjID = Obj.GetManagedID then
    begin
      Obj.IncManagedObjectReference;
      exit(Obj);
    end;
  end;

  //=== no match has been found ==
  result := nil;
end;


procedure TManagedObjectLifeTimeManager.Dispose(var Obj: IManagedObject);
var
  c1: Integer;
  TestObj : IManagedObject;
  TargetID : TUniqueID;
  TargetRefCount : integer;
begin
  TargetID := Obj.GetManagedID;

  // nil the source object as it's no longer needed.
  Obj := nil;

  for c1 := ObjectList.Count-1 downto 0 do
  begin
    TestObj := IManagedObject(ObjectList[c1]);
    if TargetID = TestObj.GetManagedID then
    begin
      TargetRefCount := TestObj.DecManagedObjectReference;

      if TargetRefCount = 0 then
      begin
        TestObj.FreeObject;
        ObjectList.Delete(c1);
      end;

      exit; //===============>> early exit >>=============>>
    end;
  end;
end;

procedure TManagedObjectLifeTimeManager.Dispose(const ObjID: TUniqueID);
var
  c1: Integer;
  TestObj : IManagedObject;
  TargetRefCount : integer;
begin
  for c1 := ObjectList.Count-1 downto 0 do
  begin
    TestObj := IManagedObject(ObjectList[c1]);
    if ObjID = TestObj.GetManagedID then
    begin
      TargetRefCount := TestObj.DecManagedObjectReference;

      if TargetRefCount = 0 then
      begin
        TestObj.FreeObject;
        ObjectList.Delete(c1);
      end;

      exit; //===============>> early exit >>=============>>
    end;
  end;
end;




{ TManagedObject }

procedure TManagedObject.AfterConstruction;
begin
  inherited;
  fManagedID.Init;
  FMangedObjectReferenceCount := 1;
end;

procedure TManagedObject.FreeObject;
begin
  if Self <> nil then
    Destroy;
end;

function TManagedObject.GetManagedID: TUniqueID;
begin
  result := fManagedID;
end;

function TManagedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := S_OK
    else Result := E_NOINTERFACE;
end;

function TManagedObject._AddRef: Integer;
begin
  result := -1;
end;

function TManagedObject._Release: Integer;
begin
  result := -1;
end;

function TManagedObject.IncManagedObjectReference: integer;
begin
  result := InterlockedIncrement(FMangedObjectReferenceCount);
end;

function TManagedObject.DecManagedObjectReference: integer;
begin
  result := InterlockedDecrement(FMangedObjectReferenceCount);
end;



{ TManagedZeroObject }

destructor TManagedZeroObject.Destroy;
var
  ptr : IZeroObjectPtr;
begin
  // Important: Deregister from the mother ship..
  if (assigned(FMotherShip)) then
  begin
    ptr := Pointer(IZeroObject(Self));
    FMotherShip.DeregisterZeroObject(ptr);
    FMotherShip := nil;
  end;

  inherited;
end;

procedure TManagedZeroObject.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TManagedZeroObject.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  // override this method to respond to zero object messages.
end;


end.
