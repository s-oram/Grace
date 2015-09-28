unit VamLib.Types;

interface

uses
  SysUtils,
  WinApi.Windows,
  SyncObjs;

type
  EVamLibException = class(Exception);

  TVamInterfacedObject = class(TInterfacedObject)
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  // A non-reference-counted IInterface implementation.
  TPureInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;



  //============================================================================
  // TODO:LOW It might make sense to move TFixedCriticalSection
  // to a VamLib.Sync unit.
  //============================================================================
   // see http://delphitools.info/2011/11/30/fixing-tcriticalsection/
   {$HINTS OFF}
   // NOTE: TFixedCriticalSection has been copied from Delphi Web Script.
   TFixedCriticalSection = class
      private
         FDummy : array [0..95-SizeOf(TRTLCRiticalSection)-2*SizeOf(Pointer)] of Byte;
         FCS : TRTLCriticalSection;
      public
         constructor Create;
         destructor Destroy; override;
         procedure Enter;
         procedure Leave;
         procedure Acquire; deprecated;
         procedure Release; deprecated;
         function TryEnter : Boolean;
   end;

   // NOTE: TMultiReadSingleWrite has been copied from Delphi Web Script.
   TMultiReadSingleWrite = class
      private
         FCS : TFixedCriticalSection; // used as fallback
         FSRWLock : Pointer;
         FDummy : array [0..95-4*SizeOf(Pointer)] of Byte; // padding
      public
         constructor Create(forceFallBack : Boolean = False);
         destructor Destroy; override;
         function TryBeginRead:boolean;
         function TryBeginWrite:boolean;
         procedure BeginRead;
         procedure EndRead;
         procedure BeginWrite;
         procedure EndWrite;
   end;
   {$HINTS ON}

  // TFakeCriticalSection doesn't do anything. It's a fakie and can be useful for
  // debugging.
  TFakeCriticalSection = class
  private
  public
    procedure Acquire;
    procedure Release;
    function TryEnter: Boolean;
    procedure Enter; inline;
    procedure Leave; inline;
  end;

  ///  TAutomaticObjectArray<T> is a generic wrapper around an
  ///  array of objects. It automatically creates new objects
  ///  when the array length is increased and frees objects
  ///  when the array length is decreased.
  ///  This generic would be useful to use when needing
  ///  a dynamic array of objects for a particular purpose.
  ///  This object avoids the application developer from
  ///  having to manage creating/freeing objects as the
  ///  array is resized.
  TAutomaticObjectArray<T : class> = record
  private
    function CreateInstance: T;
  public
    Raw : array of T;
    procedure SetLength(aCount : integer);
  end;


implementation

uses
  Rtti;

{ TFakeCriticalSection }

procedure TFakeCriticalSection.Acquire;
begin
  // It's not called fake for nothing.
end;

procedure TFakeCriticalSection.Enter;
begin
  // It's not called fake for nothing.
end;

procedure TFakeCriticalSection.Leave;
begin
  // It's not called fake for nothing.
end;

procedure TFakeCriticalSection.Release;
begin
  // It's not called fake for nothing.
end;

function TFakeCriticalSection.TryEnter: Boolean;
begin
  // It's not called fake for nothing.
 result := true;
end;


{ TVamInterfacedObject }

procedure TVamInterfacedObject.AfterConstruction;
begin
  inherited;

end;

procedure TVamInterfacedObject.BeforeDestruction;
begin
  inherited;

end;


{ TPureInterfacedObject }

function TPureInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := S_OK
    else Result := E_NOINTERFACE;
end;

function TPureInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TPureInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

// ------------------
// ------------------ TFixedCriticalSection ------------------
// ------------------

// Create
//
constructor TFixedCriticalSection.Create;
begin
   InitializeCriticalSection(FCS);
end;

// Destroy
//
destructor TFixedCriticalSection.Destroy;
begin
   DeleteCriticalSection(FCS);
end;

// Enter
//
procedure TFixedCriticalSection.Enter;
begin
   EnterCriticalSection(FCS);
end;

// Leave
//
procedure TFixedCriticalSection.Leave;
begin
   LeaveCriticalSection(FCS);
end;

procedure TFixedCriticalSection.Acquire;
begin
  Enter;
end;

procedure TFixedCriticalSection.Release;
begin
  Leave;
end;

// TryEnter
//
function TFixedCriticalSection.TryEnter : Boolean;
begin
   Result:=TryEnterCriticalSection(FCS);
end;

// ------------------
// ------------------ TMultiReadSingleWrite ------------------
// ------------------

// light-weight SRW is supported on Vista and above
// we detect by feature rather than OS Version
type
   SRWLOCK = Pointer;
var vSupportsSRWChecked : Boolean;
var vSupportsSRWResult  : Boolean;
var _InitializeSRWLock          : function (out P: Pointer): NativeUInt; stdcall;
var _AcquireSRWLockExclusive    : procedure (var SRWLock : SRWLOCK); stdcall;
var _ReleaseSRWLockExclusive    : procedure (var SRWLock : SRWLOCK); stdcall;
var _AcquireSRWLockShared       : procedure(var SRWLock : SRWLOCK); stdcall;
var _ReleaseSRWLockShared       : procedure (var SRWLock : SRWLOCK); stdcall;
var _TryAcquireSRWLockExclusive : function (var SRWLock : SRWLOCK):boolean; stdcall;
var _TryAcquireSRWLockShared    : function (var SRWLock : SRWLOCK):boolean; stdcall;

// TInitializeSRWLock = function (out P: Pointer): NativeUInt; stdcall;
// TAcquireSRWLockShared = function (var P: Pointer): NativeUInt; stdcall;
// TReleaseSRWLockShared = function (var P: Pointer): NativeUInt; stdcall;
// TAcquireSRWLockExclusive = function (var P: Pointer): NativeUInt; stdcall;
// TReleaseSRWLockExclusive = function (var P: Pointer): NativeUInt; stdcall;
// TTryAcquireSRWLockExclusive = function (var P: Pointer): BOOL; stdcall;
// TTryAcquireSRWLockShared = function (var P: Pointer): BOOL; stdcall;

function SupportsSRW : Boolean;
var
   h : HMODULE;
begin
  try
    if not vSupportsSRWChecked then
    begin
      h := GetModuleHandle('kernel32');
      _InitializeSRWLock          := GetProcAddress(h, 'InitializeSRWLock');
      _AcquireSRWLockExclusive    := GetProcAddress(h, 'AcquireSRWLockExclusive');
      _ReleaseSRWLockExclusive    := GetProcAddress(h, 'ReleaseSRWLockExclusive');
      _AcquireSRWLockShared       := GetProcAddress(h, 'AcquireSRWLockShared');
      _ReleaseSRWLockShared       := GetProcAddress(h, 'ReleaseSRWLockShared');
      _TryAcquireSRWLockExclusive := GetProcAddress(h, 'TryAcquireSRWLockExclusive');
      _TryAcquireSRWLockShared    := GetProcAddress(h, 'TryAcquireSRWLockShared');

      if  (assigned(_InitializeSRWLock))
      and (assigned(_AcquireSRWLockExclusive))
      and (assigned(_ReleaseSRWLockExclusive))
      and (assigned(_AcquireSRWLockShared))
      and (assigned(_ReleaseSRWLockShared))
      and (assigned(_TryAcquireSRWLockExclusive))
      and (assigned(_TryAcquireSRWLockShared)) then
      begin
        vSupportsSRWResult := true;
      end else
      begin
        vSupportsSRWResult := false;
      end;
      vSupportsSRWChecked := true;
    end;
  finally
    Result := vSupportsSRWResult;
  end;
end;

// Create
//
constructor TMultiReadSingleWrite.Create(forceFallBack : Boolean = False);
begin
   if (ForceFallBack) or (not SupportsSRW) then
   begin
      FCS := TFixedCriticalSection.Create;
   end else
   begin
     // TODO:HIGH
     // For some reason FSRWLock is not assigned after calling initalize lock.
     // But it still works. Even more strangely the lock still works if initialize
     // **isn't** called. WTF??? I need to investigate this more.
     _InitializeSRWLock(FSRWLock);

     //FSRWLock := Pointer(InitializeSRWLock(FSRWLock));
   end;
end;

// Destroy
//
destructor TMultiReadSingleWrite.Destroy;
begin
  if assigned(FCS) then FCS.Free;
end;

// BeginRead
//
procedure TMultiReadSingleWrite.BeginRead;
begin
   if Assigned(FCS)
     then FCS.Enter
     else _AcquireSRWLockShared(FSRWLock);
end;

// EndRead
//
procedure TMultiReadSingleWrite.EndRead;
begin
   if Assigned(FCS)
     then FCS.Leave
     else _ReleaseSRWLockShared(FSRWLock)
end;

// BeginWrite
//
procedure TMultiReadSingleWrite.BeginWrite;
begin
   if Assigned(FCS)
     then FCS.Enter
     else _AcquireSRWLockExclusive(FSRWLock);
end;

// EndWrite
//
procedure TMultiReadSingleWrite.EndWrite;
begin
   if Assigned(FCS)
     then FCS.Leave
     else _ReleaseSRWLockExclusive(FSRWLock)
end;

function TMultiReadSingleWrite.TryBeginRead: boolean;
begin
  if Assigned(FCS)
    then result := FCS.TryEnter
    else result := _TryAcquireSRWLockShared(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginWrite: boolean;
begin
  if Assigned(FCS)
    then result := FCS.TryEnter
    else result := _TryAcquireSRWLockExclusive(FSRWLock);
end;

// ------------------------------------------------------------------


{ TAutomaticObjectArray<T> }

function TAutomaticObjectArray<T>.CreateInstance: T;
var
  AValue: TValue;
  ctx: TRttiContext;
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  ctx := TRttiContext.Create;
  rType := ctx.GetType(TypeInfo(T));
  for AMethCreate in rType.GetMethods do
  begin
    if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 0) then
    begin
      instanceType := rType.AsInstance;
      AValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
      Result := AValue.AsType<T>;
      Exit;
    end;
  end;
end;

procedure TAutomaticObjectArray<T>.SetLength(aCount: integer);
var
  curLen : integer;
  NewLen : integer;
  c1: Integer;
begin
  //assert(aCount >= 0);

  curLen := Length(Raw);
  NewLen := aCount;

  if NewLen = CurLen then
  begin
    exit;
  end else
  if NewLen > CurLen then
  begin
    system.SetLength(Raw, NewLen);
    for c1 := CurLen to NewLen-1 do
    begin
      Raw[c1] := CreateInstance;
    end;
  end else
  begin
    for c1 := CurLen-1 downto NewLen do
    begin
      TObject(Raw[c1]).Free;
    end;
    system.SetLength(Raw, NewLen);
  end;
end;



end.
