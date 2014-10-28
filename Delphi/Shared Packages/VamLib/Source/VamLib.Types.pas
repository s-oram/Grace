unit VamLib.Types;

interface

uses
  WinApi.Windows,
  SyncObjs;

type
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


implementation

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
var AcquireSRWLockExclusive : procedure (var SRWLock : SRWLOCK); stdcall;
var ReleaseSRWLockExclusive : procedure (var SRWLock : SRWLOCK); stdcall;
var AcquireSRWLockShared : procedure(var SRWLock : SRWLOCK); stdcall;
var ReleaseSRWLockShared : procedure (var SRWLock : SRWLOCK); stdcall;

function SupportsSRW : Boolean;
var
   h : HMODULE;
begin
   if not vSupportsSRWChecked then begin
      vSupportsSRWChecked:=True;
      h:=GetModuleHandle('kernel32');
      AcquireSRWLockExclusive:=GetProcAddress(h, 'AcquireSRWLockExclusive');
      ReleaseSRWLockExclusive:=GetProcAddress(h, 'ReleaseSRWLockExclusive');
      AcquireSRWLockShared:=GetProcAddress(h, 'AcquireSRWLockShared');
      ReleaseSRWLockShared:=GetProcAddress(h, 'ReleaseSRWLockShared');
   end;
   Result:=Assigned(AcquireSRWLockExclusive);
end;

// Create
//
constructor TMultiReadSingleWrite.Create(forceFallBack : Boolean = False);
begin
   if forceFallBack or not SupportsSRW then
      FCS:=TFixedCriticalSection.Create;
end;

// Destroy
//
destructor TMultiReadSingleWrite.Destroy;
begin
   FCS.Free;
end;

// BeginRead
//
procedure TMultiReadSingleWrite.BeginRead;
begin
   if Assigned(FCS) then
      FCS.Enter
   else AcquireSRWLockShared(FSRWLock);
end;

// EndRead
//
procedure TMultiReadSingleWrite.EndRead;
begin
   if Assigned(FCS) then
      FCS.Leave
   else ReleaseSRWLockShared(FSRWLock)
end;

// BeginWrite
//
procedure TMultiReadSingleWrite.BeginWrite;
begin
   if Assigned(FCS) then
      FCS.Enter
   else AcquireSRWLockExclusive(FSRWLock);
end;

// EndWrite
//
procedure TMultiReadSingleWrite.EndWrite;
begin
   if Assigned(FCS) then
      FCS.Leave
   else ReleaseSRWLockExclusive(FSRWLock)
end;

// ------------------------------------------------------------------



end.
