unit VamLib.ZeroObject;

interface

uses
  //VamLib.CpuOverloadWatcher,
  SysUtils,
  Classes,
  ExtCtrls,
  Contnrs,
  VamLib.Types;

{$SCOPEDENUMS ON}

{.$DEFINE ExtraLogging}
//{$IFDEF ExtraLogging}{$ENDIF}

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

    - Zero objects should have a 'Name' parameter so objects can
      be received. Maybe the name parameter should be dynamic so
      objects can match multiple names at different times...

  }


  // There are two Zero Object 'Ranks'. Objects are assigned a 'rank'
  // when they are added to the mothership. (Objects can be added multiple
  // times with multiple ranks.) Objects will only receive messages of the
  // same rank that they were assigned with.
  // - NonVisual rank messages are generally sent/received in the audio thread.
  //   Timing is critical for these messages so their usage should be minimised
  //   and messages should be processed quickly.
  // - VCL rank messages are for GUI objects. VCL rank objects should
  //   be removed from the mothership when the GUI is closed.
  //   VCL rank messages are always sent in the 'Main' GUI thread because
  //   the Delph VCL isn't threadsafe.
  //
  // - NonVisual messages are only sent to NonVisual objects.
  // - VCL messages will only be sent to VCL objects, and only if the GUI
  //   is open.
  TZeroObjectRank = (NonVisual, VCL);

  //Forward declarations
  IZeroObject = interface;
  TZeroObject = class;
  IZeroObjectPtr = Pointer;

  IMotherShip = interface;

  TMsgIdToStrFunction = reference to function(const ID : cardinal):string;

  IZeroMessageData = interface(IInterface)
    ['{6D90ECB8-9EC8-40E6-8908-AB4C7CCF9C15}']
    function GetObject : TObject;
  end;

  TCustomZeroMessageData = class(TInterfacedObject, IInterface, IZeroMessageData)
  private
  public
    function GetObject : TObject;
  end;

  IZeroObject = interface
    ['{F7C2493B-01CF-4980-A1E0-F6FB862DC576}']
    procedure AddMotherShipReference(aMotherShip : IMothership);
    procedure RemoveMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface);
    function ClassType: TClass;
  end;

  IMotherShip = interface
    ['{3668F765-A3E2-4CDC-8B3A-BDCE6C430172}']

    procedure Inject_MsgIdToStr(const f : TMsgIdToStrFunction);

    procedure SetIsGuiOpen(const Value: boolean);

    procedure RegisterZeroObject(const obj: IZeroObject; const Rank : TZeroObjectRank);
    procedure DeregisterZeroObject(const obj:IZeroObject);

    procedure MsgNonVisual(MsgID : cardinal); overload;
    procedure MsgNonVisual(MsgID : cardinal; Data : Pointer); overload;

    procedure MsgVcl(MsgID : cardinal); overload;
    procedure MsgVcl(MsgID : cardinal; Data : Pointer); overload;
    procedure MsgVcl(MsgID : cardinal; Data : Pointer; DataB:IZeroMessageData); overload;

    procedure LogObjects;
  end;


  // NOTE: TZeroObjects aren't reference counded;
  TZeroObject = class(TObject, IInterface, IZeroObject)
  private
    FMotherShip : IMotherShip;
    procedure AddMotherShipReference(aMotherShip : IMothership);
    procedure RemoveMotherShipReference(aMotherShip : IMothership);
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface); virtual;
  public
    destructor Destroy; override;
  end;

  // NOTE TRefCountedZeroObjects are reference counted.
  TRefCountedZeroObject = class(TInterfacedObject, IZeroObject)
  private
    FMotherShip : IMotherShip;
    procedure AddMotherShipReference(aMotherShip : IMothership);
    procedure RemoveMotherShipReference(aMotherShip : IMothership);
  protected
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface); virtual;
  public
    destructor Destroy; override;
  end;

  // TMotherShip is not reference counted.
  TMotherShip = class(TPureInterfacedObject, IMotherShip)
  private
    // TODO:MED it would be much better to have list objects
    // that didn't require locks. Any message that triggers
    // an object to be created or destroyed has a real potential
    // to cause a deadlock. List objects are locked when sending messages.
    // List objects are locked when adding/removing items.
    //
    // I can't find any Lock-free list objects online. It might be
    // possible to write one specifically for this context.
    // I think my requirements are limited so it might be possible.
    // Maybe a lock-free linked list might be suitable. Or some
    // setup that allows objects to be added to a list.
    //
    // The MultiReadSingleWrite lock falls back to a critical section
    // on Windows XP. The critical section isn't great if I remember correctly.
    // I think it can cause crackles as the multiple threads can be trying
    // send messages and that can block the audio thread.
    //

    NonVisualObjects : TList;
    VclObjects   : TList;

    NonVisualListLock : TMultiReadSingleWrite;
    VclListLock   : TMultiReadSingleWrite;

    Injected_MsgIdToStr : TMsgIdToStrFunction;

    DisableMessageSending : boolean;

    FMainThreadID : cardinal;

    IsGuiOpen: boolean;
    IsGuiOpenLock : TMultiReadSingleWrite;

    procedure SendMessageToList(const ObjectList : TList; const ListLock : TMultiReadSingleWrite; const MsgID : cardinal; const Data : Pointer; DataB:IZeroMessageData);
    procedure ClearMotherShipReferences;
    procedure SetIsGuiOpen(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inject_MsgIdToStr(const f : TMsgIdToStrFunction);

    procedure RegisterZeroObject(const obj: IZeroObject; const Rank : TZeroObjectRank);
    procedure DeregisterZeroObject(const obj: IZeroObject);

    procedure MsgNonVisual(MsgID : cardinal); overload;
    procedure MsgNonVisual(MsgID : cardinal; Data : Pointer); overload;

    procedure MsgVcl(MsgID : cardinal); overload;
    procedure MsgVcl(MsgID : cardinal; Data : Pointer); overload;
    procedure MsgVcl(MsgID : cardinal; Data : Pointer; DataB:IZeroMessageData); overload;

    procedure LogObjects;

    property MainThreadID : cardinal read FMainThreadID;
  end;



implementation

uses
  VamLib.Logging,
  Windows,
  VamLib.WinUtils;

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

procedure TZeroObject.AddMotherShipReference(aMotherShip: IMothership);
begin
  if assigned(FMotherShip) then raise Exception.Create('MotherShip reference already set. Cannot assign again.');
  FMotherShip := aMotherShip;
end;

procedure TZeroObject.RemoveMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := nil;
end;

procedure TZeroObject.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB: IInterface);
begin

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

procedure TRefCountedZeroObject.AddMotherShipReference(aMotherShip: IMothership);
begin
  if assigned(FMotherShip) then raise Exception.Create('MotherShip reference already set. Cannot assign again.');
  FMotherShip := aMotherShip;
end;

procedure TRefCountedZeroObject.RemoveMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := nil;
end;

procedure TRefCountedZeroObject.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB: IInterface);
begin

end;

{ TMotherShip }

constructor TMotherShip.Create;
begin
  Injected_MsgIdToStr := nil;

  IsGuiOpenLock := TMultiReadSingleWrite.Create;

  FMainThreadID := 0;

  DisableMessageSending := false;

  NonVisualListLock := TMultiReadSingleWrite.Create(false);
  VclListLock   := TMultiReadSingleWrite.Create(false);

  NonVisualObjects := TList.Create;
  VclObjects   := TList.Create;
end;

destructor TMotherShip.Destroy;
begin
  Injected_MsgIdToStr := nil;

  if NonVisualObjects.Count > 0
    then Log.Lib.LogMessage('Non-Visual Objects still registered (' + IntToStr(NonVisualObjects.Count) + ').');

  ClearMotherShipReferences;

  NonVisualObjects.Free;
  VclObjects.Free;

  NonVisualListLock.Free;
  VclListLock.Free;

  IsGuiOpenLock.Free;

  inherited;
end;

procedure TMotherShip.ClearMotherShipReferences;
var
  c1: Integer;
  zo : IZeroObject;
begin
  for c1 := 0 to NonVisualObjects.Count-1 do
  begin
    zo := IZeroObject(NonVisualObjects[c1]);
    zo.RemoveMotherShipReference(nil);
  end;
end;



procedure TMotherShip.RegisterZeroObject(const obj: IZeroObject; const Rank : TZeroObjectRank);
var
  ptr : Pointer;
  ListLock : TMultiReadSingleWrite;
  {$IFDEF ExtraLogging}LogMsg : string;{$ENDIF}
begin
  //============================================================================
  {$IFDEF ExtraLogging}
  LogMsg := ' ClassName = ' + obj.ClassType.ClassName;
  case Rank of
    TZeroObjectRank.NonVisual: LogMsg := LogMsg + ' (Non Visual Rank)';
    TZeroObjectRank.VCL:   LogMsg := LogMsg + ' (VCL Rank)';
  else
    LogMsg := LogMsg + ' (Unknown Rank)';
  end;
  Log.TrackMethod('MotherShip.RegisterZeroObject(' + LogMsg + ')');
  {$ENDIF}
  //============================================================================



  ptr := Pointer(obj); //Weak reference to zero object
  obj.AddMotherShipReference(self);

  case Rank of
    TZeroObjectRank.NonVisual: ListLock := NonVisualListLock;
    TZeroObjectRank.VCL:       ListLock := VclListLock;
  else
    raise Exception.Create('unexpected value and not handled.');
  end;

  if ListLock.TryBeginWrite then
  begin
    try
      case Rank of
        TZeroObjectRank.NonVisual:
        begin
          if NonVisualObjects.IndexOf(ptr) = -1
            then NonVisualObjects.Add(ptr);
        end;

        TZeroObjectRank.VCL:
        begin
          if VclObjects.IndexOf(ptr) = -1
            then VclObjects.Add(ptr);
        end;
      else
        raise Exception.Create('Rank not supported.');
      end;
    finally
      ListLock.EndWrite;
    end;
  end else
  begin
    Log.Lib.LogError('LIST LOCKED: Couldn''t add object.');
  end;
end;

procedure TMotherShip.DeregisterZeroObject(const obj: IZeroObject);
var
  ptr : Pointer;
  IsVclObject   : boolean;
  IsNonVisualObject : boolean;
  {$IFDEF ExtraLogging}LogMsg : string;{$ENDIF}
begin
  //============================================================================
  {$IFDEF ExtraLogging}
  LogMsg := obj.ClassType.ClassName;
  Log.TrackMethod('MotherShip.DeregisterZeroObject(' + LogMsg + ')');
  {$ENDIF}
  //============================================================================

  ptr := Pointer(obj); //Weak reference to zero object
  obj.RemoveMotherShipReference(self);

  //=== first find which list the object's belong to.
  NonVisualListLock.BeginRead;
  VclListLock.BeginRead;
  try
    if NonVisualObjects.IndexOf(ptr) = -1
      then IsNonVisualObject := false
      else IsNonVisualObject := true;

    if VclObjects.IndexOf(ptr) = -1
      then IsVclObject := false
      else IsVclObject := true;
  finally
    NonVisualListLock.EndRead;
    VclListLock.EndRead;
  end;

  if (IsVclObject) then
  begin
    if VclListLock.TryBeginWrite then
    try
      if VclObjects.IndexOf(ptr) <> -1 then VclObjects.Remove(ptr);
    finally
      VclListLock.EndWrite;
    end
      else Log.Lib.LogError('LIST LOCKED: Couldn''t remove object.');
  end;

  if (IsNonVisualObject) then
  begin
    if NonVisualListLock.TryBeginWrite then
    try
      if NonVisualObjects.IndexOf(ptr) <> -1 then NonVisualObjects.Remove(ptr);
    finally
      NonVisualListLock.EndWrite;
    end
      else Log.Lib.LogError('LIST LOCKED: Couldn''t remove object.');
  end;
end;

procedure TMotherShip.SetIsGuiOpen(const Value: boolean);
begin
  IsGuiOpenLock.BeginWrite;
  try
    IsGuiOpen := Value;
    if IsGuiOpen = true
      then FMainThreadID := GetCurrentThreadId
      else FMainThreadID := 0;
  finally
    IsGuiOpenLock.EndWrite;
  end;
end;

procedure TMotherShip.MsgNonVisual(MsgID: cardinal; Data: Pointer);
begin
  SendMessageToList(NonVisualObjects, NonVisualListLock, MsgID, Data, nil);
end;

procedure TMotherShip.MsgNonVisual(MsgID: cardinal);
begin
  MsgNonVisual(MsgID, nil);
end;

procedure TMotherShip.MsgVcl(MsgID: cardinal);
var
  LogMsg : string;
begin
  IsGuiOpenLock.BeginRead;
  try
    if (IsGuiOpen)  then
    begin
      if (MainThreadID = GetCurrentThreadId) then
      begin
        SendMessageToList(VclObjects, VclListLock, MsgID, nil, nil);
      end else
      begin
        if assigned(Injected_MsgIdToStr)
          then LogMsg := 'MsgVCL Wrong Thread.' + Injected_MsgIdToStr(MsgID) + ' (' + IntToStr(MsgID) + ')'
          else LogMsg := 'MsgVCL Wrong Thread. ID = ' + IntToStr(MsgID);
        Log.Lib.LogError(LogMsg);
      end;
    end;
  finally
    IsGuiOpenLock.EndRead;
  end;
end;

procedure TMotherShip.MsgVcl(MsgID: cardinal; Data: Pointer; DataB:IZeroMessageData);
var
  LogMsg : string;
begin
  if (MainThreadID <> GetCurrentThreadId)
    then raise Exception.Create('MsgVCL has been called from non-vcl thread.');

  IsGuiOpenLock.BeginRead;
  try
    if (IsGuiOpen)  then
    begin
      if (MainThreadID = GetCurrentThreadId) then
      begin
        //SendMessageToList(VclObjects, VclListLock, MsgID, nil, nil);
        SendMessageToList(VclObjects, VclListLock, MsgID, Data, DataB);
      end else
      begin
        if assigned(Injected_MsgIdToStr)
          then LogMsg := 'MsgVCL Wrong Thread.' + Injected_MsgIdToStr(MsgID) + ' (' + IntToStr(MsgID) + ')'
          else LogMsg := 'MsgVCL Wrong Thread. ID = ' + IntToStr(MsgID);
        Log.Lib.LogError(LogMsg);
      end;
    end;
  finally
    IsGuiOpenLock.EndRead;
  end;
end;

procedure TMotherShip.MsgVcl(MsgID: cardinal; Data: Pointer);
begin
  MsgVcl(MsgID, Data, nil);
end;

procedure TMotherShip.Inject_MsgIdToStr(const f: TMsgIdToStrFunction);
begin
  Injected_MsgIdToStr := f;
end;

procedure TMotherShip.SendMessageToList(const ObjectList: TList; const ListLock : TMultiReadSingleWrite; const MsgID: cardinal; const Data: Pointer; DataB:IZeroMessageData);
var
  LastIndex : integer;
  c1: Integer;
  zo : IZeroObject;
  LogMsg : string;
  aClass : TClass;
begin
  if DisableMessageSending then exit;

  //=================================================================================
  {$IFDEF ExtraLogging}
    if ObjectList = NonVisualObjects    then LogMsg := 'Non-Visual SendMessage ID = '
    else if ObjectList = VclObjects then LogMsg := 'Vcl SendMessage ID = '
    else LogMsg := 'ERROR - No matching list lock. ';
    if assigned(Injected_MsgIdToStr) then
    begin
      LogMsg := LogMsg + Injected_MsgIdToStr(MsgID) + ' (' + IntToStr(MsgID) + ')';
    end;
    Log.TrackMethod('MotherShip.SendMessageToList() ' + LogMsg);
  {$ENDIF}
  //=================================================================================

  if ListLock.TryBeginRead then
  begin
    try
      LastIndex := -1;
      try
        for c1 := 0 to ObjectList.Count - 1 do
        begin
          LastIndex := c1;
          zo := IZeroObject(ObjectList[c1]);
          zo.ProcessZeroObjectMessage(MsgID, Data, DataB);
        end;
      except
        zo := IZeroObject(ObjectList[LastIndex]);
        aClass := zo.ClassType;
        LogMsg := ' ClassName = ' + aClass.ClassName;
        if assigned(Injected_MsgIdToStr) then
        begin
          LogMsg := LogMsg + ' Msg = ' + Injected_MsgIdToStr(MsgID) + ' (' + IntToStr(MsgID) + ')';
        end;
        DisableMessageSending := true;
        Log.Lib.LogError('ERROR TMotherShip.SendMessageToList() ' + LogMsg);
        raise;
      end;
    finally
      ListLock.EndRead;
    end;
  end else
  begin
    Log.Lib.LogError('LIST LOCKED: Unable to send message.');
  end;
end;


procedure TMotherShip.LogObjects;
var
  c1: Integer;
  zo : IZeroObject;
  LogMsg : string;
  ObjectList : TList;
  aClass : TClass;
begin
  Log.Lib.LogMessage('==========================');
  LogMsg := 'Current Audio Objects';
  Log.Lib.LogMessage(LogMsg);

  ObjectList := NonVisualObjects;

  for c1 := 0 to ObjectList.Count - 1 do
  begin
    zo := IZeroObject(ObjectList[c1]);
    aClass := zo.ClassType;
    LogMsg := 'ClassName = ' + aClass.ClassName;
    Log.Lib.LogMessage(LogMsg);
  end;
  Log.Lib.LogMessage('==========================');

  // TODO:MED should log all objects here.
end;






{ TCustomZeroMessageData }

function TCustomZeroMessageData.GetObject: TObject;
begin
  result := self;
end;

end.
