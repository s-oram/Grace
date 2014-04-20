unit VamLib.ZeroObject;

interface

uses
  SysUtils,
  Classes,
  ExtCtrls,
  Contnrs,
  OtlCommon,
  OtlContainers,
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


    Putting the RegisterWithMotherShip() method as belonging to
    the IZeroObject kind of feels wrong. I wonder if the
    the mother ship should have the RegisterZeroObject method....

    TODO:
    It might be possible to only store ZeroObjects as TObjects
    and cast back to IZeroObject when needing to send messages....


    TODO:

    I think I want to modify the mother ship so that
    objects are registered as
    Audio Objects or Main Objects.
    - Audio Objects generally process audio on the audio thread
      and have high priority. Audio Objects should be minimised if
      possible and only sent messages when necessary.
    - Main Objects operate on the main or GUI thread. They run at
      less then real-time priority and can be used more freely.

    Create a ZeroObject implementation that can be added to frames etc
    that can't descend from the TZeroObject class.

    Think about getting rid of the SendMessageUsingGuiThread() function.
    - It would really simplify the mother ship implementation.
    - Cross thread communication would be good but the current implement
      only goes one way and it's a tad complicated. Perhaps a more
      general solution could be implemented.

    - Zero objects should have a 'Name' parameter so objects can
      be received. Maybe the name parameter should be dynamic so
      objects can match multiple names at different times...

  }


  // Main is for the GUI and VCL objects. It needs to be thead-safe.
  // Audio is for audio/realtime objects.
  TZeroObjectRank = (zoMain, zoAudio);

  //Forward declarations
  IZeroObject = interface;
  TZeroObject = class;
  IZeroObjectPtr = Pointer;

  IMotherShip = interface;

  IZeroObject = interface
    ['{F7C2493B-01CF-4980-A1E0-F6FB862DC576}']
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
    function ClassType: TClass;
  end;

  IMotherShip = interface
    ['{3668F765-A3E2-4CDC-8B3A-BDCE6C430172}']
    procedure RegisterZeroObject(obj: IZeroObject; const Rank : TZeroObjectRank);
    procedure DeregisterZeroObject(obj:IZeroObjectPtr);

    procedure SendMessage(MsgID : cardinal); overload;
    procedure SendMessage(MsgID : cardinal; Data : Pointer); overload;
    procedure SendMessageUsingGuiThread(MsgID : cardinal); overload;
    procedure SendMessageUsingGuiThread(MsgID : cardinal; Data : Pointer; CleanUp : TProc); overload;


    procedure MsgMain(MsgID : cardinal); overload;
    procedure MsgMain(MsgID : cardinal; Data : Pointer); overload;
    procedure MsgMainTS(MsgID : cardinal); overload;
    procedure MsgMainTS(MsgID : cardinal; Data : Pointer; CleanUp : TProc); overload; //Thread Safe version.

    procedure MsgAudio(MsgID : cardinal); overload;
    procedure MsgAudio(MsgID : cardinal; Data : Pointer); overload;

    procedure SetIsGuiOpen(IsOpen : boolean);


    procedure LogAudioObjects;
    procedure LogMainObjects;
  end;


  // NOTE: TZeroObjects aren't reference counded;
  TZeroObject = class(TObject, IInterface, IZeroObject)
  private
    FMotherShip : IMotherShip;
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
    AudioObjects : TList;
    MainObjects  : TList;

    MainMessageLock : TFixedCriticalSection;
    DisableMessageSending : boolean;

    // TODO: Instead of using a timer, it might be better to try and implement a
    // background window handle or something similer so the window handle has
    // a Process Messages loop.... I'm not sure of the exact terminolgy.
    MainMessageQueue : TOmniQueue;
    MainMessageTimer : TTimer;
    fIsGuiOpen: boolean;
    procedure Handle_GuiMessageTimerEvent(Sender : TObject);

    procedure DeregisterZeroObject(obj:IZeroObjectPtr);

    procedure SetIsGuiOpen(IsOpen : boolean);

    property IsGuiOpen : boolean read fIsGuiOpen;

    procedure SendMessageToList(const ObjectList : TList; const MsgID : cardinal; const Data : Pointer);
    procedure ClearMotherShipReferences;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterZeroObject(obj: IZeroObject; const Rank : TZeroObjectRank);


    procedure MsgMain(MsgID : cardinal); overload;
    procedure MsgMain(MsgID : cardinal; Data : Pointer); overload;
    procedure MsgMainTS(MsgID : cardinal); overload;
    procedure MsgMainTS(MsgID : cardinal; Data : Pointer; CleanUp : TProc); overload; //Thread Safe version.

    procedure MsgAudio(MsgID : cardinal); overload;
    procedure MsgAudio(MsgID : cardinal; Data : Pointer); overload;

    procedure SendMessage(MsgID : cardinal); overload;
    procedure SendMessage(MsgID : cardinal; Data : Pointer); overload;

    procedure SendMessageUsingGuiThread(MsgID : cardinal); overload;
    procedure SendMessageUsingGuiThread(MsgID : cardinal; Data : Pointer; CleanUp : TProc); overload;

    procedure LogAudioObjects;
    procedure LogMainObjects;
  end;



implementation

uses
  OtlParallel,
  VamLib.WinUtils,
  VamLib.LoggingProxy;

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
var
  ptr : IZeroObjectPtr;
begin
  // TODO: instead of maintaining a reference to the mother ship, the zero object
  // could use a multi-event to notify objects of it's destruction.
  // IE. a NotifyOnFree() event.

  // Important: Deregister from the mother ship..
  if (assigned(FMotherShip)) then
  begin
    ptr := Pointer(IZeroObject(Self));
    FMotherShip.DeregisterZeroObject(ptr);
    FMotherShip := nil;
  end;

  inherited;
end;


procedure TZeroObject.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
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

{ TRefCountedZeroObject }

function TRefCountedZeroObject.GetIsReferenceCounted: boolean;
begin
  result := true;
end;



{ TMotherShip }

constructor TMotherShip.Create;
begin
  DisableMessageSending := false;

  MainMessageLock := TFixedCriticalSection.Create;

  AudioObjects := TList.Create;

  MainObjects := TList.Create;


  MainMessageQueue := TOmniQueue.Create;
  MainMessageTimer := TTimer.Create(nil);
  MainMessageTimer.Interval := 25;
  MainMessageTimer.OnTimer := Handle_GuiMessageTimerEvent;
  MainMessageTimer.Enabled := true;
end;

destructor TMotherShip.Destroy;
var
  c1: Integer;
  Text : string;
begin
  if AudioObjects.Count > 0
    then Log.LogMessage('Audio Objects still registered (' + IntToStr(AudioObjects.Count) + ').');

  if MainObjects.Count > 0
    then Log.LogMessage('Main Objects still registered (' + IntToStr(MainObjects.Count) + ').');

  ClearMotherShipReferences;


  MainMessageTimer.Free;
  MainMessageQueue.Free;
  MainMessageLock.Free;

  FreeAndNil(AudioObjects);
  FreeAndNil(MainObjects);

  inherited;
end;

procedure TMotherShip.ClearMotherShipReferences;
var
  c1: Integer;
  zo : IZeroObject;
begin
  for c1 := 0 to MainObjects.Count-1 do
  begin
    zo := IZeroObject(MainObjects[c1]);
    zo.SetMotherShipReference(nil);
  end;

  for c1 := 0 to AudioObjects.Count-1 do
  begin
    zo := IZeroObject(AudioObjects[c1]);
    zo.SetMotherShipReference(nil);
  end;
end;



procedure TMotherShip.RegisterZeroObject(obj: IZeroObject; const Rank : TZeroObjectRank);
var
  zo : IZeroObject;
  ptr : Pointer;
begin
  ptr := Pointer(obj); //Weak reference to zero object
  obj.SetMotherShipReference(self);

  case Rank of
    zoMain:
    begin
      if MainObjects.IndexOf(ptr) = -1
        then MainObjects.Add(ptr);
    end;

    zoAudio:
    begin
      if AudioObjects.IndexOf(ptr) = -1
        then AudioObjects.Add(ptr);
    end;
  else
    raise Exception.Create('Rank not supported.');
  end;

end;

procedure TMotherShip.DeregisterZeroObject(obj: IZeroObjectPtr);
var
  ptr : Pointer;
  zo : IZeroObject;
begin
  if MainObjects.IndexOf(obj) <> -1 then
  begin
    MainObjects.Remove(obj);
  end else
  if AudioObjects.IndexOf(obj) <> -1 then
  begin
    AudioObjects.Remove(obj);
  end else
  begin
    raise Exception.Create('ZeroObject faided to deregister itself.');
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
  MsgMain(MsgID, Data);
end;

procedure TMotherShip.SendMessageUsingGuiThread(MsgID: cardinal);
begin
  SendMessageUsingGuiThread(MsgID, nil, nil);
end;

procedure TMotherShip.SendMessageUsingGuiThread(MsgID: cardinal; Data: Pointer; CleanUp: TProc);
begin
  MsgMainTS(MsgID, Data, CleanUp);
end;

procedure TMotherShip.MsgAudio(MsgID: cardinal; Data: Pointer);
begin
  SendMessageToList(AudioObjects, MsgID, Data);
end;

procedure TMotherShip.MsgAudio(MsgID: cardinal);
begin
  MsgAudio(MsgID, nil);
end;

procedure TMotherShip.MsgMain(MsgID: cardinal);
begin
  MsgMain(MsgID, nil);
end;

procedure TMotherShip.MsgMain(MsgID: cardinal; Data: Pointer);
begin
  SendMessageToList(MainObjects, MsgID, Data);
end;

procedure TMotherShip.MsgMainTS(MsgID: cardinal);
begin
  MsgMainTS(MsgID, nil, nil);
end;

procedure TMotherShip.MsgMainTS(MsgID: cardinal; Data: Pointer; CleanUp: TProc);
var
  msgData : TMessageData;
  QueueValue : TOmniValue;
begin
  msgData.MsgID := MsgID;
  msgData.Data := Data;
  msgData.CleanUp := CleanUp;

  QueueValue := TOmniValue.FromRecord<TMessageData>(msgData);
  MainMessageQueue.Enqueue(QueueValue);
end;

procedure TMotherShip.Handle_GuiMessageTimerEvent(Sender: TObject);
var
  msgData : TMessageData;
  QueueValue : TOmniValue;
begin
  while MainMessageQueue.TryDequeue(QueueValue) do
  begin
    MsgData := QueueValue.ToRecord<TMessageData>;

    MsgMain(msgData.MsgID, msgData.Data);

    if assigned(msgData.CleanUp)
      then msgData.CleanUp();
  end;
end;

procedure TMotherShip.SetIsGuiOpen(IsOpen: boolean);
begin
  MainMessageLock.Enter;
  try
    fIsGuiOpen := IsOpen;
  finally
    MainMessageLock.Leave;
  end;
end;



procedure TMotherShip.SendMessageToList(const ObjectList: TList; const MsgID: cardinal; const Data: Pointer);
var
  c1: Integer;
  zo : IZeroObject;
  LogMsg : string;
  aClass : TClass;
begin
  MainMessageLock.Enter;
  try
    if ObjectList = MainObjects then LogMsg := 'ZeroObject.MsgMain(MsgID = ' + IntToStr(MsgID) + ')'
    else
    if ObjectList = AudioObjects then LogMsg := 'ZeroObject.MsgAudio(MsgID = ' + IntToStr(MsgID) + ')'
    else
      LogMsg := 'Error : Unknown Object List';

    if (ObjectList = MainObjects) and (IsGuiOpen = false)
      then exit;

    if DisableMessageSending then exit;

    try
      for c1 := 0 to ObjectList.Count - 1 do
      begin
        zo := IZeroObject(ObjectList[c1]);

        aClass := zo.ClassType;
        LogMsg := LogMsg + ' ClassName = ' + aClass.ClassName;

        zo.ProcessZeroObjectMessage(MsgID, Data);
      end;
    except
      DisableMessageSending := true;
      Log.LogMessage('ERROR' + LogMsg);
      raise;
    end;
  finally
    MainMessageLock.Leave;
  end;
end;


procedure TMotherShip.LogAudioObjects;
var
  c1: Integer;
  zo : IZeroObject;
  LogMsg : string;
  ObjectList : TList;
  aClass : TClass;
begin
  Log.LogMessage('==========================');
  LogMsg := 'Current Audio Objects';
  Log.LogMessage(LogMsg);

  ObjectList := AudioObjects;

  for c1 := 0 to ObjectList.Count - 1 do
  begin
    zo := IZeroObject(ObjectList[c1]);
    aClass := zo.ClassType;
    LogMsg := 'ClassName = ' + aClass.ClassName;
    Log.LogMessage(LogMsg);
  end;
  Log.LogMessage('==========================');
end;

procedure TMotherShip.LogMainObjects;
var
  c1: Integer;
  zo : IZeroObject;
  LogMsg : string;
  ObjectList : TList;
  aClass : TClass;
begin
  Log.LogMessage('==========================');
  LogMsg := 'Current Main Objects';
  Log.LogMessage(LogMsg);

  ObjectList := MainObjects;

  for c1 := 0 to ObjectList.Count - 1 do
  begin
    zo := IZeroObject(ObjectList[c1]);
    aClass := zo.ClassType;
    LogMsg := 'ClassName = ' + aClass.ClassName;
    Log.LogMessage(LogMsg);
  end;
  Log.LogMessage('==========================');
end;







end.
