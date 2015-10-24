unit Helm.AnyValue;

interface

type
  TAnyValueType = (
    avtEmpty, //Value is not in use.
    avtInteger,
    avtSingle,
    avtBoolean,
    avtPointer,
    avtString,
    avtPString
  );

  TAnyValueData = record
  case Byte of
      0: (VInteger : integer);
      1: (VBoolean : boolean);
      2: (VPointer : pointer);
      3: (VSingle  : single);
  end;

  TAnyValue = record
  private
    FValueType : TAnyValueType;
    FValueData : TAnyValueData;
    FValueString : string;
    FValuePString : PString;
    function GetAsBoolean: boolean;
    function GetAsInteger: integer;
    function GetAsPointer: pointer;
    function GetAsSingle: single;
    function GetAsString: string;
    procedure SetAsBoolean(const Value: boolean);
    procedure SetAsInteger(const Value: integer);
    procedure SetAsPointer(const Value: pointer);
    procedure SetAsSingle(const Value: single);
    procedure SetAsString(const Value: string);
    function GetAsPString: PString;
    procedure SetAsPString(const Value: PString);
  public
    function GetValueType : TAnyValueType;

    function IsInteger : boolean;
    function IsSingle  : boolean;
    function IsBoolean : boolean;
    function IsPointer : boolean;
    function IsString  : boolean;
    function IsEmpty   : boolean; // Value is not in use.

    procedure Clear;

    property AsInteger : integer   read GetAsInteger    write SetAsInteger;
    property AsSingle  : single    read GetAsSingle     write SetAsSingle;
    property AsBoolean : boolean   read GetAsBoolean    write SetAsBoolean;
    property AsPointer : pointer   read GetAsPointer    write SetAsPointer;
    property AsString  : string    read GetAsString     write SetAsString;
    property AsPString : PString   read GetAsPString    write SetAsPString;
  end;



implementation

{ TFlexValue }

procedure TAnyValue.Clear;
var
  ps : PString;
begin
  case FValueType of
    avtString: FValueString := '';
  end;
  FValueType := avtEmpty;
  FValueData.VPointer := nil;
end;

procedure TAnyValue.SetAsInteger(const Value: integer);
begin
  if (FValueType <> avtEmpty) and (FValueType <> avtInteger) then Clear;
  FValueType := TAnyValueType.avtInteger;
  FValueData.VInteger := Value;
end;

procedure TAnyValue.SetAsSingle(const Value: single);
begin
  if (FValueType <> avtEmpty) and (FValueType <> avtSingle) then Clear;
  FValueType := TAnyValueType.avtSingle;
  FValueData.VSingle := Value;
end;

procedure TAnyValue.SetAsBoolean(const Value: boolean);
begin
  if (FValueType <> avtEmpty) and (FValueType <> avtBoolean) then Clear;
  FValueType := TAnyValueType.avtBoolean;
  FValueData.VBoolean := Value;
end;

procedure TAnyValue.SetAsPointer(const Value: pointer);
begin
  if (FValueType <> avtEmpty) and (FValueType <> avtPointer) then Clear;
  FValueType := TAnyValueType.avtPointer;
  FValueData.VPointer := Value;
end;

procedure TAnyValue.SetAsPString(const Value: PString);
begin
  if (FValueType <> avtEmpty) and (FValueType <> avtPString) then Clear;
  FValueType := TAnyValueType.avtPString;
  FValuePString := Value;
end;

procedure TAnyValue.SetAsString(const Value: string);
begin
  if (FValueType <> avtEmpty) and (FValueType <> avtString) then Clear;
  FValueType := TAnyValueType.avtString;
  FValueString := Value;
end;

function TAnyValue.GetAsInteger: integer;
begin
  assert(FValueType = TAnyValueType.avtInteger);
  result := FValueData.VInteger;
end;

function TAnyValue.GetAsSingle: single;
begin
  assert(FValueType = TAnyValueType.avtSingle);
  result := FValueData.VSingle;
end;

function TAnyValue.GetAsBoolean: boolean;
begin
  assert(FValueType = TAnyValueType.avtBoolean);
  result := FValueData.VBoolean;
end;

function TAnyValue.GetAsPointer: pointer;
begin
  assert(FValueType = TAnyValueType.avtPointer);
  result := FValueData.VPointer;
end;

function TAnyValue.GetAsPString: PString;
begin
  assert(FValueType = TAnyValueType.avtPString);
  result := FValuePString;
end;

function TAnyValue.GetAsString: string;
begin
  assert(FValueType = TAnyValueType.avtString);
  result := FValueString;
end;

function TAnyValue.GetValueType: TAnyValueType;
begin
  result := FValueType;
end;

function TAnyValue.IsBoolean: boolean;
begin
  result := (FValueType = TAnyValueType.avtBoolean);
end;


function TAnyValue.IsInteger: boolean;
begin
  result := (FValueType = TAnyValueType.avtInteger);
end;

function TAnyValue.IsEmpty: boolean;
begin
  result := (FValueType = TAnyValueType.avtEmpty);
end;

function TAnyValue.IsPointer: boolean;
begin
  result := (FValueType = TAnyValueType.avtPointer);
end;

function TAnyValue.IsSingle: boolean;
begin
  result := (FValueType = TAnyValueType.avtSingle);
end;

function TAnyValue.IsString: boolean;
begin
  result := (FValueType = TAnyValueType.avtString);
end;


end.
