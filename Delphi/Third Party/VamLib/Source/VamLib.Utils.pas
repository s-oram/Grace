unit VamLib.Utils;

interface

type
  PObject = ^TObject;
  function AutoFree(const aObject: PObject): IUnknown;

implementation

uses
  SysUtils;


//==============================================================================
//   Auto Free
//==============================================================================

type
  TAutoFree = class(TInterfacedObject, IUnknown)
  private
    fObject: PObject;
  public
    constructor Create(const aObject: PObject);
    destructor Destroy; override;
  end;

constructor TAutoFree.Create(const aObject: PObject);
begin
  inherited Create;
  fObject := aObject;
end;

destructor TAutoFree.Destroy;
begin
  FreeAndNIL(fObject^);
  inherited;
end;

function AutoFree(const aObject: PObject): IUnknown;
begin
  result := TAutoFree.Create(aObject);
end;


//==============================================================================
//==============================================================================


end.
