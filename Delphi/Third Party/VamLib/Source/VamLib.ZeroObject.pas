unit VamLib.ZeroObject;

interface

uses
  SysUtils,
  Classes,
  ExtCtrls,
  VamLib.Types,
  OtlCommon,
  OtlContainers;

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


    Putting the RegisterWithMotherShip() method as belonging to
    the IZeroObject kind of feels wrong. I wonder if the
    the mother ship should have the RegisterZeroObject method....

    TODO:
    It might be possible to only store ZeroObjects as TObjects
    and cast back to IZeroObject when needing to send messages....

  }

  //Forward declarations
  IZeroObject = interface;
  TZeroObject = class;

  IMotherShip = interface;
  TMotherShip = class;

  IZeroObject = interface
    ['{F7C2493B-01CF-4980-A1E0-F6FB862DC576}']
    function GetClassName : string;
    procedure ClearMotherShipReference;
    procedure RegisterWithMotherShip(const Mothership:IMotherShip);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  end;

  IMotherShip = interface
    ['{3668F765-A3E2-4CDC-8B3A-BDCE6C430172}']
    procedure RegisterZeroObject(obj:IZeroObject);
    procedure DeregisterZeroObject(obj:IZeroObject);

    procedure SendMessage(MsgID : cardinal; Data : Pointer);
    procedure SendMessageUsingGuiThread(MsgID : cardinal; Data : Pointer; CleanUp : TProc);
  end;


  // NOTE: TZeroObjects aren't reference counded;
  TZeroObject = class(TObject, IInterface, IZeroObject)
  private
    FMotherShip : IMotherShip;
    function GetClassName : string;
    procedure ClearMotherShipReference;
  protected
    FRefCount: Integer;
    function GetIsReferenceCounted: boolean; virtual;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    property IsReferenceCounted : boolean read GetIsReferenceCounted;
    property RefCount           : Integer read FRefCount;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); virtual;
  public
    destructor Destroy; override;
    procedure RegisterWithMotherShip(const Mothership:IMotherShip);

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
  end;

  // NOTE TRefCountedZeroObjects are reference counted.
  TRefCountedZeroObject = class(TZeroObject)
  protected
    function GetIsReferenceCounted: boolean; virtual;
  end;

  // TMotherShip is not reference counted.
  TMotherShip = class(TPureInterfacedObject, IMotherShip)
  private type
    TGuiMessage = record
      MsgID   : cardinal;
      Data    : pointer;
      CleanUp : TProc;
    end;
  private
    GuiMessageQueue : TOmniQueue;
    GuiMessageTimer : TTimer;
    Objects : TInterfaceList;
    function GetZeroObjectCount: integer;
    procedure RegisterZeroObject(obj:IZeroObject);
    procedure DeregisterZeroObject(obj:IZeroObject);

    procedure Handle_GuiMessageTimer(Sender : TObject);

    property ZeroObjectCount : integer read GetZeroObjectCount;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SendMessage(MsgID : cardinal; Data : Pointer);
    procedure SendMessageUsingGuiThread(MsgID : cardinal; Data : Pointer; CleanUp : TProc);


  end;



implementation

uses
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

procedure TZeroObject.ClearMotherShipReference;
begin
  FMotherShip := nil;
end;

destructor TZeroObject.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
  end;

  inherited;
end;

procedure TZeroObject.RegisterWithMotherShip(const Mothership: IMotherShip);
begin
  // NOTE:
  // ZeroObjects can register with a MotherShip. They will automatically
  // deregister when the object is freed or released.
  FMotherShip := MotherShip;
  FMotherShip.RegisterZeroObject(self);
end;


function TZeroObject.GetClassName: string;
begin
  result := self.ClassName;
end;

function TZeroObject.GetIsReferenceCounted: boolean;
begin
  result := false;
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

  if IsReferenceCounted
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

  if IsReferenceCounted
    then result := FRefCount
    else result := -1;

  if (result = 1) and (assigned(FMotherShip)) then
  begin
    // The mothership is holding the last interface reference and
    // preventing this object from being freed. Deregister
    // this object so we can be free!
    FMotherShip.DeregisterZeroObject(self);
  end;

  if (result = 0) then
  begin
    Destroy;
  end;

end;

procedure TZeroObject.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
end;


{ TMotherShip }

constructor TMotherShip.Create;
begin
  GuiMessageQueue := TOmniQueue.Create;
  GuiMessageTimer := TTimer.Create(nil);
  GuiMessageTimer.Interval := 1;
  GuiMessageTimer.Enabled := false;
  GuiMessagetimer.OnTimer := Handle_GuiMessageTimer;

  Objects := TInterfaceList.Create;
end;

destructor TMotherShip.Destroy;
var
  c1: Integer;
  Text : string;
  zo : IZeroObject;
begin
  GuiMessageTimer.Free;
  GuiMessageQueue.Free;

  if Objects.Count > 0 then
  begin
    for c1 := Objects.Count-1 downto 0 do
    begin
      zo :=  (Objects[c1] as IZeroObject);
      Text := Zo.GetClassName + ' has not been freed!';
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
  obj.ClearMotherShipReference;

  //remove obj from interface list.
  if Objects.IndexOf(obj) <> -1 then
  begin
    Objects.Remove(obj);
  end;
end;

procedure TMotherShip.SendMessage(MsgID: cardinal; Data: Pointer);
var
  c1: Integer;
begin
  for c1 := 0 to Objects.Count-1 do
  begin
    (Objects[c1] as IZeroObject).ProcessZeroObjectMessage(MsgID, Data);
  end;
end;



procedure TMotherShip.SendMessageUsingGuiThread(MsgID: cardinal; Data: Pointer; CleanUp: TProc);
var
  GuiMessage : TGuiMessage;
  OmniValue : TOmniValue;
begin
  GuiMessage.MsgID   := MsgID;
  GuiMessage.Data    := Data;
  GuiMessage.CleanUp := CleanUp;

  OmniValue := TOmniValue.FromRecord<TGuiMessage>(GuiMessage);
  GuiMessageQueue.Enqueue(OmniValue);

  GuiMessageTimer.Enabled := true;
end;

procedure TMotherShip.Handle_GuiMessageTimer(Sender: TObject);
var
  GuiMessage : TGuiMessage;
  OmniValue : TOmniValue;
  c1: Integer;
begin
  GuiMessageTimer.Enabled := false;

  while GuiMessageQueue.TryDequeue(OmniValue) do
  begin
    GuiMessage := OmniValue.ToRecord<TGuiMessage>;

    for c1 := 0 to Objects.Count-1 do
    begin
      (Objects[c1] as IZeroObject).ProcessZeroObjectMessage(GuiMessage.MsgID, GuiMessage.Data);
    end;

    if assigned(GuiMessage.CleanUp)
      then GuiMessage.CleanUp();
  end;
end;



{ TRefCountedZeroObject }

function TRefCountedZeroObject.GetIsReferenceCounted: boolean;
begin
  result := true;
end;

end.
