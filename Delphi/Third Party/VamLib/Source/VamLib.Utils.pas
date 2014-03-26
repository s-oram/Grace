unit VamLib.Utils;

interface

type
  PObject = ^TObject;

function AutoFree(const aObject: PObject): IUnknown;


// a couple of methods to help with removing the 'combining signed and unsigned' types...
function CastToInteger(Value : cardinal):integer;
function CastToCardinal(Value : integer):cardinal;

function Clamp(const Value, MinValue, MaxValue : integer):integer; overload;
function Clamp(const Value, MinValue, MaxValue : single):single; overload;

// The Wrap() function forces a value to overflow around an arbitary minimum
// and maximum.
function Wrap(Input : single; const MinValue, MaxValue : single):single;

function InRange(const Value, MinValue, MaxValue : single):boolean; inline;

procedure SwapValues(var x1, x2:single); inline; overload;
procedure SwapValues(var x1, x2:integer); inline; overload;

// Expands a 0-1 ranged float to an arbitary integer range.
function ExpandFloat(const Value : single; MinValue, MaxValue : integer):integer;

implementation

uses
  Math,
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

function Wrap(Input : single; const MinValue, MaxValue : single):single; overload;
begin
  while Input < MinValue do
  begin
    Input := Input + (MaxValue - MinValue);
  end;

  while Input > MaxValue do
  begin
    Input := Input - (MaxValue - MinValue);
  end;

  result := Input;
end;



function InRange(const Value, MinValue, MaxValue : single):boolean; inline;
begin
  if (Value >= MinValue) and (Value <= MaxValue)
    then result := true
    else result := false;
end;

procedure SwapValues(var x1, x2:single); inline; overload;
var
  tx : single;
begin
  tx := x1;
  x1 := x2;
  x2 := tx;
end;


procedure SwapValues(var x1, x2:integer); inline; overload;
var
  tx : integer;
begin
  tx := x1;
  x1 := x2;
  x2 := tx;
end;


function ExpandFloat(const Value : single; MinValue, MaxValue : integer):integer;
var
  Dist : integer;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  Dist := MaxValue - MinValue;

  result := MinValue + round(Dist * Value);


  assert(result >= MinValue);
  assert(result <= MaxValue);

end;



end.
