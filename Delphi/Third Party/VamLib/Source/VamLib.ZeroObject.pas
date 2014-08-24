unit VamLib.ZeroObject;

interface

uses
  //VamLib.CpuOverloadWatcher,
  SysUtils,
  Classes,
  ExtCtrls,
  Contnrs,
  OtlCommon,
  OtlContainers,
  VamLib.Types;

{$SCOPEDENUMS ON}

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

    - Zero objects should have a 'Name' parameter so objects can
      be received. Maybe the name parameter should be dynamic so
      objects can match multiple names at different times...

  }


  // There are three Zero Object 'Ranks'. Objects are assigned a 'rank'
  // when they are added to the mothership. (Objects can be added multiple
  // times with multiple ranks.) Objects will only receive messages of the
  // same rank that they were assigned with.
  // - Audio rank messages are generally sent/received in the audio thread.
  //   Timing is critical for these messages so their usage should be minimised
  //   and messages should be processed quickly.
  // - Main rank messages are for general purpose usage. While they can be
  //   sent in the audio thread, perhaps consider not doing so.
  // - VCL rank messages are for GUI objects. VCL rank objects should
  //   be removed from the mothership when the GUI is closed.
  //   VCL rank messages are always sent in the 'Main' GUI thread because
  //   the Delph VCL isn't threadsafe.
  //
  // - Audio messages are only sent to Audio objects.
  // - Main messages are sent to Main objects. Main messages will
  //   also be sent to VCL objects if the GUI is open.
  // - VCL messages will only be sent to VCL objects, and only if the GUI
  //   is open.
  //
  //  I'm not sure if "Main" is actually required.
  TZeroObjectRank = (Audio, Main, VCL);

  //Forward declarations
  IZeroObject = interface;
  TZeroObject = class;
  IZeroObjectPtr = Pointer;

  IMotherShip = interface;

  IZeroMessageData = interface
    ['{6D90ECB8-9EC8-40E6-8908-AB4C7CCF9C15}']
  end;

  IZeroObject = interface
    ['{F7C2493B-01CF-4980-A1E0-F6FB862DC576}']
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IZeroMessageData);
    function ClassType: TClass;
  end;

  IMotherShip = interface
    ['{3668F765-A3E2-4CDC-8B3A-BDCE6C430172}']
    procedure SetIsGuiOpen(const Value: boolean);

    procedure RegisterZeroObject(const obj: IZeroObject; const Rank : TZeroObjectRank);
    procedure DeregisterZeroObject(const obj:IZeroObject);

    procedure MsgAudio(MsgID : cardinal); overload;
    procedure MsgAudio(MsgID : cardinal; Data : Pointer); overload;

    procedure MsgMain(MsgID : cardinal); overload;
    procedure MsgMain(MsgID : cardinal; Data : Pointer); overload;

    procedure MsgVcl(MsgID : cardinal); overload;
    procedure MsgVcl(MsgID : cardinal; Data : Pointer); overload;
    procedure MsgVclTS(MsgID : cardinal; DataB:IZeroMessageData);

    procedure LogAudioObjects;
    procedure LogMainObjects;
  end;


  // NOTE: TZeroObjects aren't reference counded;
  TZeroObject = class(TObject, IInterface, IZeroObject)
  private
    FMotherShip : IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IZeroMessageData); virtual;
  public
    destructor Destroy; override;
  end;

  // NOTE TRefCountedZeroObjects are reference counted.
  TRefCountedZeroObject = class(TInterfacedObject, IZeroObject)
  private
    FMotherShip : IMotherShip;
  protected
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IZeroMessageData); virtual;
  public
    destructor Destroy; override;
  end;

  // TMotherShip is not reference counted.
  TMotherShip = class(TPureInterfacedObject, IMotherShip)
  private type
    TMessageData = record
      MsgID   : cardinal;
      DataB   : IZeroMessageData;
    end;
  private
    AudioObjects : TList;
    MainObjects  : TList;
    VclObjects   : TList;

    MessageLock : TFixedCriticalSection;
    //MessageLock : TFakeCriticalSection;
    DisableMessageSending : boolean;

    MainThreadID : cardinal;

    // TODO: Instead of using a timer, it might be better to try and implement a
    // background window handle or something similer so the window handle has
    // a Process Messages loop.... I'm not sure of the exact terminolgy.
    VclMessageQueue : TOmniQueue;
    VclMessageTimer : TTimer;
    fIsGuiOpen: boolean;
    procedure Handle_VclMessageTimerEvent(Sender : TObject);




    procedure SendMessageToList(const ObjectList : TList; const MsgID : cardinal; const Data : Pointer; DataB:IZeroMessageData);
    procedure ClearMotherShipReferences;
    procedure SetIsGuiOpen(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterZeroObject(const obj: IZeroObject; const Rank : TZeroObjectRank);
    procedure DeregisterZeroObject(const obj: IZeroObject);

    procedure MsgAudio(MsgID : cardinal); overload;
    procedure MsgAudio(MsgID : cardinal; Data : Pointer); overload;

    procedure MsgMain(MsgID : cardinal); overload;
    procedure MsgMain(MsgID : cardinal; Data : Pointer); overload;

    procedure MsgVcl(MsgID : cardinal); overload;
    procedure MsgVcl(MsgID : cardinal; Data : Pointer); overload;
    procedure MsgVclTS(MsgID : cardinal; DataB:IZeroMessageData);

    property IsGuiOpen : boolean read fIsGuiOpen write SetIsGuiOpen;

    procedure LogAudioObjects;
    procedure LogMainObjects;
  end;



implementation

uses
  Windows,
  OtlParallel,
  VamLib.WinUtils,
  VamLib.LoggingProxy;

{$I InterlockedAPIs.inc}

{ TZeroObject }

destructor TZeroObject.Destroy;
begin
  // Important: Deregister from the mother ship..
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  inherited;
end;


procedure TZeroObject.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

function TZeroObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := S_OK
    else Result := E_NOINTERFACE;
end;

function TZeroObject._AddRef: Integer;
begin
  result := -1;
end;

function TZeroObject._Release: Integer;
begin
  result := -1;
end;

procedure TZeroObject.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IZeroMessageData);
begin
end;

{ TRefCountedZeroObject }

destructor TRefCountedZeroObject.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  inherited;
end;


procedure TRefCountedZeroObject.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IZeroMessageData);
begin

end;

procedure TRefCountedZeroObject.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

{ TMotherShip }

constructor TMotherShip.Create;
begin
  MainThreadID := 0;

  DisableMessageSending := false;

  MessageLock := TFixedCriticalSection.Create;
  //MessageLock := TFakeCriticalSection.Create;

  AudioObjects := TList.Create;
  MainObjects  := TList.Create;
  VclObjects   := TList.Create;

  VclMessageQueue := TOmniQueue.Create;
  VclMessageTimer := TTimer.Create(nil);
  VclMessageTimer.Interval := 25;
  VclMessageTimer.OnTimer := Handle_VclMessageTimerEvent;
  VclMessageTimer.Enabled := true;
end;

destructor TMotherShip.Destroy;
begin
  if AudioObjects.Count > 0
    then Log.LogMessage('Audio Objects still registered (' + IntToStr(AudioObjects.Count) + ').');

  if MainObjects.Count > 0
    then Log.LogMessage('Main Objects still registered (' + IntToStr(MainObjects.Count) + ').');

  ClearMotherShipReferences;


  VclMessageTimer.Free;
  VclMessageQueue.Free;
  MessageLock.Free;

  FreeAndNil(AudioObjects);
  FreeAndNil(MainObjects);
  FreeAndNil(VclObjects);

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



procedure TMotherShip.RegisterZeroObject(const obj: IZeroObject; const Rank : TZeroObjectRank);
var
  ptr : Pointer;
begin
  ptr := Pointer(obj); //Weak reference to zero object
  obj.SetMotherShipReference(self);

  case Rank of
    TZeroObjectRank.Audio:
    begin
      if AudioObjects.IndexOf(ptr) = -1
        then AudioObjects.Add(ptr);
    end;

    TZeroObjectRank.Main:
    begin
      if MainObjects.IndexOf(ptr) = -1
        then MainObjects.Add(ptr);
    end;

    TZeroObjectRank.VCL:
    begin
      if VclObjects.IndexOf(ptr) = -1
        then VclObjects.Add(ptr);
    end;
  else
    raise Exception.Create('Rank not supported.');
  end;

end;

procedure TMotherShip.DeregisterZeroObject(const obj: IZeroObject);
var
  ptr : Pointer;
  IsD : boolean;
begin
  ptr := Pointer(obj); //Weak reference to zero object
  obj.SetMotherShipReference(nil);
  IsD := false;

  if MainObjects.IndexOf(ptr) <> -1 then
  begin
    MainObjects.Remove(ptr);
    IsD := true;
  end;

  if AudioObjects.IndexOf(ptr) <> -1 then
  begin
    AudioObjects.Remove(ptr);
    IsD := true;
  end;

  if VclObjects.IndexOf(ptr) <> -1 then
  begin
    VclObjects.Remove(ptr);
    IsD := true;
  end;

  if IsD = false
    then raise Exception.Create('ZeroObject faided to deregister itself.');
end;

procedure TMotherShip.SetIsGuiOpen(const Value: boolean);
var
  QueueValue : TOmniValue;
begin
  fIsGuiOpen := Value;

  if fIsGuiOpen = true then
  begin
    MainThreadID := GetCurrentThreadId;
    VclMessageTimer.Enabled := true;
  end else
  begin
    VclMessageTimer.Enabled := false;
    MainThreadID := 0;

    //=== clear the VCL message queue ===
    while VclMessageQueue.TryDequeue(QueueValue) do
    begin
      //do nothing, we're just clearing the queue.
    end;
  end;

end;

procedure TMotherShip.MsgAudio(MsgID: cardinal; Data: Pointer);
begin


  //VamLib.LoggingProxy.Log.LogMessage('Audio MsgID = ' + IntToStr(MsgID));
  SendMessageToList(AudioObjects, MsgID, Data, nil);

end;

procedure TMotherShip.MsgAudio(MsgID: cardinal);
begin
  MsgAudio(MsgID, nil);
end;

procedure TMotherShip.MsgMain(MsgID: cardinal);
begin
  MsgMain(MsgID, nil);
  MsgVclTS(MsgID, nil);
end;

procedure TMotherShip.MsgMain(MsgID: cardinal; Data: Pointer);
begin
  //VamLib.LoggingProxy.Log.LogMessage('Main MsgID = ' + IntToStr(MsgID));
  SendMessageToList(MainObjects, MsgID, Data, nil);

  if (IsGuiOpen) and (MainThreadID = GetCurrentThreadId) then
  begin
    SendMessageToList(VclObjects, MsgID, Data, nil);
  end else
  begin
    // TODO: probably should log a warning here.
  end;
end;

procedure TMotherShip.MsgVcl(MsgID: cardinal);
begin
  if (IsGuiOpen)  then
  begin
    if (MainThreadID = GetCurrentThreadId) then
    begin
      SendMessageToList(VclObjects, MsgID, nil, nil);
    end else
    begin
      // TODO:MED probably should log a warning or raise an error here.
      //SendMessageToList(VclObjects, MsgID, nil);
      Log.LogError('MsgVCL Wrong Thread.');
    end;
  end;
end;

procedure TMotherShip.MsgVcl(MsgID: cardinal; Data: Pointer);
begin
  // TODO: need to check calling thread ID.
  if (IsGuiOpen) then
  begin
    if (MainThreadID = GetCurrentThreadId) then
    begin
      SendMessageToList(VclObjects, MsgID, Data, nil);
    end else
    begin
      // TODO:MED probably should log a warning or raise an error here.
      Log.LogError('MsgVCL Wrong Thread.');
    end;
  end;
end;

procedure TMotherShip.MsgVclTS(MsgID: cardinal; DataB:IZeroMessageData);
var
  msgData : TMessageData;
  QueueValue : TOmniValue;
begin
  if IsGuiOpen then
  begin
    if (MainThreadID = GetCurrentThreadId) then
    begin
      SendMessageToList(VclObjects, MsgID, nil, nil);
    end else
    begin
      // TODO: a possible improvement would be to check the calling thread id. If
      // it's the VCL thread, dispatch the message immediatly. If not, queue the
      // message for later processing.
      msgData.MsgID   := MsgID;
      msgData.DataB   := DataB;
      QueueValue := TOmniValue.FromRecord<TMessageData>(msgData);
      VclMessageQueue.Enqueue(QueueValue);
    end;
  end;
end;

procedure TMotherShip.Handle_VclMessageTimerEvent(Sender: TObject);
var
  msgData : TMessageData;
  QueueValue : TOmniValue;
begin
  MainThreadID := GetCurrentThreadId;

  while VclMessageQueue.TryDequeue(QueueValue) do
  begin
    MsgData := QueueValue.ToRecord<TMessageData>;
    SendMessageToList(VclObjects, msgData.MsgID, nil, msgData.DataB);
  end;
end;

procedure TMotherShip.SendMessageToList(const ObjectList: TList; const MsgID: cardinal; const Data: Pointer; DataB:IZeroMessageData);
var
  LastIndex : integer;
  c1: Integer;
  zo : IZeroObject;
  LogMsg : string;
  aClass : TClass;
begin
  LastIndex := -1;

  MessageLock.Enter;
  try
    if ObjectList = MainObjects then LogMsg := 'ZeroObject.MsgMain(MsgID = ' + IntToStr(MsgID) + ')'
    else
    if ObjectList = AudioObjects then LogMsg := 'ZeroObject.MsgAudio(MsgID = ' + IntToStr(MsgID) + ')'
    else
      LogMsg := 'Error : Unknown Object List';

    if DisableMessageSending then exit;

    try
      for c1 := 0 to ObjectList.Count - 1 do
      begin
        LastIndex := c1;
        zo := IZeroObject(ObjectList[c1]);
        zo.ProcessZeroObjectMessage(MsgID, Data, nil);
      end;
    except
      zo := IZeroObject(ObjectList[LastIndex]);
      aClass := zo.ClassType;
      LogMsg := LogMsg + ' ClassName = ' + aClass.ClassName;

      DisableMessageSending := true;
      Log.LogError('ERROR' + LogMsg);
      raise;
    end;

  finally
    MessageLock.Leave;
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
