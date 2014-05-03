unit VamLib.Types;

interface

uses
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

  // NOTE: On TFixedCriticalSection
  // http://delphitools.info/2011/11/30/fixing-tcriticalsection/
  TFixedCriticalSection = class(TCriticalSection)
  private
    {$Hints Off}
    FDummy : array [0..95] of Byte;
    {$Hints On}
  end;


implementation

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



end.
