unit PlugLib.Types;

interface

uses
  SysUtils,
  WinApi.Windows;

type
  EPlugLibException = class(Exception);

  // A non-reference-counted IInterface implementation.
  TPureInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

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
   {$HINTS ON}

implementation

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

end.
