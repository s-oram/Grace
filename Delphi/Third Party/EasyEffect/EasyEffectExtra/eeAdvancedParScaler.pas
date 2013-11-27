unit eeAdvancedParScaler;

interface

type
  PParNotch = ^TParNotch;
  TParNotch = record
    KnobPos  : single; //normally 0..1 range.
    ParValue : single; //normally set to required parameter value.
  end;

  function ParNotch(const KnobPos, ParValue:single):TParNotch;

type
  TParScaling = (psLinear, psExp, psLog);

  TAdvancedParScaler = class
  private
    fParScaling: TParScaling;
    fNotchCount: integer;
    fNotches   : array of TParNotch;
    procedure SetNotchCount(const Value: integer);
    function GetParNotch(Index: integer): TParNotch;
    procedure SetParNotch(Index: integer; const Value: TParNotch);
  public
    constructor Create;
	  destructor Destroy; override;

    function KnobPosToParValue(KnobPos:single):single;
    function ParValueToKnobPos(const ParValue:single):single;

    property ParScaling:TParScaling read fParScaling write fParScaling;
    property NotchCount : integer read fNotchCount write SetNotchCount;
    property Notches[Index:integer]:TParNotch read GetParNotch write SetParNotch;
  end;

implementation

uses
  SysUtils, uGeneralFunctions;

function ParNotch(const KnobPos, ParValue:single):TParNotch;
begin
  result.KnobPos  := KnobPos;
  result.ParValue := ParValue;
end;

{ TAdvancedParScaler }

constructor TAdvancedParScaler.Create;
begin
  NotchCount := 2;
  Notches[0] := ParNotch(0,0);
  Notches[1] := ParNotch(1,1);

end;

destructor TAdvancedParScaler.Destroy;
begin
  NotchCount := 0;
  inherited;
end;

procedure TAdvancedParScaler.SetNotchCount(const Value: integer);
begin
  fNotchCount := Value;
  SetLength(fNotches, Value);
end;

procedure TAdvancedParScaler.SetParNotch(Index: integer; const Value: TParNotch);
begin
  if (Index < 0) or (Index >= NotchCount) then raise Exception.Create('Index out of bounds.');
  fNotches[Index] := Value;
end;

function TAdvancedParScaler.GetParNotch(Index: integer): TParNotch;
begin
  if (Index < 0) or (Index >= NotchCount) then raise Exception.Create('Index out of bounds.');
  result := fNotches[Index];
end;

function TAdvancedParScaler.KnobPosToParValue(KnobPos: single): single;
var
  IndexA, IndexB:integer;
  ValueA, ValueB:single;
  PosA, PosB:single;
  Frac:single;
  c1: Integer;
begin
  if KnobPos > 1 then KnobPos := 1;
  if KnobPos < 0 then KnobPos := 0;

  IndexA := -1;
  IndexB := -1;

  for c1 := 0 to NotchCount - 2 do
  begin
    if (KnobPos >= Notches[c1].KnobPos) and (KnobPos <= Notches[c1 + 1].KnobPos) then
    begin
      IndexA := c1;
      IndexB := c1+1;
      Break;
    end;
  end;

  //Check if the index values aren't in bounds. if not something is wrong with the setup of
  // the par scaler.
  if (IndexA = -1) or (IndexB = -1) then
  begin
    result := Notches[0].ParValue;
    exit; //==================================>>>
  end;

  ValueA := Notches[IndexA].ParValue;
  ValueB := Notches[IndexB].ParValue;
  PosA   := Notches[IndexA].KnobPos;
  PosB   := Notches[IndexB].KnobPos;

  //assume linear scaling for the time being..
  Frac := (KnobPos - PosA) / (PosB - PosA);

  result := ValueA + Frac * (ValueB-ValueA);
end;

function TAdvancedParScaler.ParValueToKnobPos(const ParValue: single): single;
var
  IndexA, IndexB:integer;
  ValueA, ValueB:single;
  PosA, PosB:single;
  Frac:single;
  c1: Integer;
  x1, x2:single;
begin
  IndexA := -1;
  IndexB := -1;

  for c1 := 0 to NotchCount - 2 do
  begin
    x1 := Notches[c1].ParValue;
    x2 := Notches[c1 + 1].ParValue;

    // NOTE: If the parameter values are falling (example [12 to 0] or [0 to -96]), the second value will be
    // smaller. In that case the values will need to be swapped, otherwise the following comparaison will get
    // not work as intended.
    if x1 > x2 then SwapValues(x1, x2);

    if (ParValue >= x1) and (ParValue <= x2) then
    begin
      IndexA := c1;
      IndexB := c1+1;
      Break;
    end;
  end;

  //Check if the index values aren't in bounds. if not something is wrong with the setup of
  // the par scaler.
  if (IndexA = -1) or (IndexB = -1) then
  begin
    result := Notches[0].ParValue;
    exit; //==================================>>>
  end;

  ValueA := Notches[IndexA].ParValue;
  ValueB := Notches[IndexB].ParValue;
  PosA   := Notches[IndexA].KnobPos;
  PosB   := Notches[IndexB].KnobPos;

  //assume linear scaling for the time being..
  Frac := (ParValue - ValueA) / (ValueB - ValueA);

  result := PosA + Frac * (PosB - PosA);


end;



end.
