unit VamLib.ZeroObject;

interface

uses
  SysUtils,
  Classes,
  ExtCtrls,
  VamLib.Types,
  Contnrs,
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
    function GetMotherShipReference:IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  end;

  IMotherShip = interface
    ['{3668F765-A3E2-4CDC-8B3A-BDCE6C430172}']
    procedure RegisterZeroObject(obj:TObject);
    procedure DeregisterZeroObject(obj:TObject);

    procedure SendMessage(MsgID : cardinal); overload;
    procedure SendMessage(MsgID : cardinal; Data : Pointer); overload;
    procedure SendMessageUsingGuiThread(MsgID : cardinal); overload;
    procedure SendMessageUsingGuiThread(MsgID : cardinal; Data : Pointer; CleanUp : TProc); overload;
  end;


  // NOTE: TZeroObjects aren't reference counded;
  TZeroObject = class(TObject, IInterface, IZeroObject)
  private
    FMotherShip : IMotherShip;
    function GetMotherShipReference:IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
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
    procedure RegisterWithMotherShip(const aMothership:IMotherShip);

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
  end;

  // NOTE TRefCountedZeroObjects are reference counted.
  TRefCountedZeroObject = class(TZeroObject)
  protected
    function GetIsReferenceCounted: boolean; override;
  end;

  // TMotherShip is not reference counted.
  TMotherShip = class(TPureInterfacedObject, IMotherShip)
  private type
    TMessageData = record
      MsgID   : cardinal;
      Data    : pointer;
      CleanUp : TProc;
    end;
  private
    Objects      : TObjectList;

    // TODO: Instead of using a timer, it might be better to try and implement a
    // background window handle or something similer so the window handle has
    // a Process Messages loop.... I'm not sure of the exact terminolgy.
    GuiMessageQueue : TOmniQueue;
    GuiMessageTimer : TTimer;
    procedure Handle_GuiMessageTimerEvent(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterZeroObject(obj:TObject);
    procedure DeregisterZeroObject(obj:TObject);

    procedure SendMessage(MsgID : cardinal); overload;
    procedure SendMessage(MsgID : cardinal; Data : Pointer); overload;

    procedure SendMessageUsingGuiThread(MsgID : cardinal); overload;
    procedure SendMessageUsingGuiThread(MsgID : cardinal; Data : Pointer; CleanUp : TProc); overload;


  end;



implementation

uses
  OtlParallel,
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
  // Important: Deregister from the mother ship..
  if (assigned(FMotherShip))
    then FMotherShip.DeregisterZeroObject(self);

  inherited;
end;

procedure TZeroObject.RegisterWithMotherShip(const aMothership: IMotherShip);
begin
  // NOTE:
  // ZeroObjects can register with a MotherShip. They must derigister themselves
  // in the destructor.
  aMothership.RegisterZeroObject(self);
end;

procedure TZeroObject.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

function TZeroObject.GetMotherShipReference: IMotherShip;
begin
  result := FMotherShip;
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
  FRefCount := InterlockedDecrement(FRefCount);

  if IsReferenceCounted
    then result := FRefCount
    else result := -1;

  if (result = 0)
    then Destroy;
end;

procedure TZeroObject.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
end;


{ TMotherShip }

constructor TMotherShip.Create;
begin
  Objects := TObjectList.Create;
  Objects.OwnsObjects := false;

  GuiMessageQueue := TOmniQueue.Create;
  GuiMessageTimer := TTimer.Create(nil);
  GuiMessageTimer.Interval := 25;
  GuiMessageTimer.OnTimer := Handle_GuiMessageTimerEvent;
  GuiMessageTimer.Enabled := true;
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
      Text := Objects[c1].ClassName + ' has not been freed!';
      //VamLib.WinUtils.SendDebugMesssage(Text);
    end;
    // TODO: the hard arse way to respond to unfreed zero objects. This
    // should probably be removed in release builds!
    raise Exception.Create('Not all ZeroObjects have been freed.');
  end;

  GuiMessageTimer.Free;
  Objects.Free;
  GuiMessageQueue.Free;

  inherited;
end;

procedure TMotherShip.RegisterZeroObject(obj: TObject);
var
  zo : IZeroObject;
begin
  if Supports(obj, IZeroObject, zo) then
  begin
    zo.SetMotherShipReference(self);
    if Objects.IndexOf(Obj) = -1
      then Objects.Add(Obj);
  end else
  begin
    raise Exception.Create('Object isn''t a ZeroObject.');
  end;
end;

procedure TMotherShip.DeregisterZeroObject(obj: TObject);
var
  zo : IZeroObject;
begin
  if Supports(obj, IZeroObject, zo) then
  begin
    zo.SetMotherShipReference(nil);
    Objects.Remove(Obj);
  end;
end;

procedure TMotherShip.SendMessage(MsgID: cardinal);
begin
  SendMessage(MsgID, nil);
end;

procedure TMotherShip.SendMessage(MsgID: cardinal; Data: Pointer);
var
  c1: Integer;
  zo : IZeroObject;
begin
  for c1 := 0 to Objects.Count - 1 do
  begin
    if Supports(Objects[c1], IZeroObject, zo) then
    begin
      zo.ProcessZeroObjectMessage(MsgID, Data);
    end;
  end;
end;

procedure TMotherShip.SendMessageUsingGuiThread(MsgID: cardinal);
begin
  SendMessageUsingGuiThread(MsgID, nil, nil);
end;

procedure TMotherShip.SendMessageUsingGuiThread(MsgID: cardinal; Data: Pointer; CleanUp: TProc);
var
  msgData : TMessageData;
  QueueValue : TOmniValue;
  DoIt, DoNothing : TProc;
begin
  msgData.MsgID := MsgID;
  msgData.Data := Data;
  msgData.CleanUp := CleanUp;

  QueueValue := TOmniValue.FromRecord<TMessageData>(msgData);
  GuiMessageQueue.Enqueue(QueueValue);
end;

procedure TMotherShip.Handle_GuiMessageTimerEvent(Sender: TObject);
var
  msgData : TMessageData;
  QueueValue : TOmniValue;
begin
  while GuiMessageQueue.TryDequeue(QueueValue) do
  begin
    MsgData := QueueValue.ToRecord<TMessageData>;

    SendMessage(msgData.MsgID, msgData.Data);

    if assigned(msgData.CleanUp)
      then msgData.CleanUp();
  end;
end;




{ TRefCountedZeroObject }

function TRefCountedZeroObject.GetIsReferenceCounted: boolean;
begin
  result := true;
end;

end.
