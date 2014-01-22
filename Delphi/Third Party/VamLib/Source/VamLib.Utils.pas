unit VamLib.Utils;

interface

uses
  SyncObjs;

type
  PObject = ^TObject;

function AutoFree(const aObject: PObject): IUnknown;


// a couple of methods to help with removing the 'combining signed and unsigned' types...
function CastToInteger(Value : cardinal):integer;
function CastToCardinal(Value : integer):cardinal;



function Clamp(const Value, MinValue, MaxValue : integer):integer; overload;
function Clamp(const Value, MinValue, MaxValue : single):single; overload;

function InRange(const Value, MinValue, MaxValue : single):boolean; inline;


type
  TVamInterfacedObject = class(TInterfacedObject)
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;


  // NOTE: On TFixedCriticalSection
  // http://delphitools.info/2011/11/30/fixing-tcriticalsection/
  TFixedCriticalSection = class(TCriticalSection)
  private
    FDummy : array [0..95] of Byte;
  end;

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

{ TVamInterfacedObject }

procedure TVamInterfacedObject.AfterConstruction;
begin
  inherited;

end;

procedure TVamInterfacedObject.BeforeDestruction;
begin
  inherited;

end;




function Clamp(const Value, MinValue, MaxValue : integer):integer; overload;
begin
  assert(MinValue <= MaxValue);

  if Value < MinValue then result := MinValue
  else
  if Value > MaxValue then result := MaxValue
  else
    result := Value;
end;

function Clamp(const Value, MinValue, MaxValue : single):single; overload;
begin
  assert(MinValue <= MaxValue);

  if Value < MinValue then result := MinValue
  else
  if Value > MaxValue then result := MaxValue
  else
    result := Value;
end;

function InRange(const Value, MinValue, MaxValue : single):boolean; inline;
begin
  if (Value >= MinValue) and (Value <= MaxValue)
    then result := true
    else result := false;
end;

end.
