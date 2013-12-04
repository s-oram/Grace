unit VamLib.Utils;

interface

type
  PObject = ^TObject;

function AutoFree(const aObject: PObject): IUnknown;


// a couple of methods to help with removing the 'combining signed and unsigned' types...
function CastToInteger(Value : cardinal):integer;
function CastToCardinal(Value : integer):cardinal;

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

function CastToInteger(Value : cardinal):integer;
const
  kMaxInt = 2147483647;
begin
  if Value > kMaxInt then raise Exception.Create('Cannot convert type to integer. The result will overflow.');
  result := Integer(Value);
end;

function CastToCardinal(Value : integer):cardinal;
begin
  if Value < 0 then raise Exception.Create('Cannot convert type to cardinal. The result will overflow.');
  result := Cardinal(Value);
end;

end.
