unit VamLib.FlexValue;

interface

uses
  SysUtils;

type
  EFlexException = class(Exception);

  TFlexValue = class;
  IFlexValue = interface;

  // NOTE: Start by using a limited number of FlexValue types. It might be useful
  // to extend the supported types to Cromis.AnyValue, TObject, TClass, IInterface and more in future.
  // When doing so, consult TOmniValue, the DeCAL library and System.TVarRec
  // for implementation ideas.
  TFlexValueType = (
    fvtEmpty, //Value is not in use.
    fvtInteger,
    fvtSingle,
    fvtBoolean,
    fvtPointer,
    fvtString
  );

  TFlexValueData = record
  case Byte of
      0: (VInteger : integer);
      1: (VBoolean : boolean);
      2: (VPointer : pointer);
      3: (VSingle  : single);
  end;

  IFlexValue = interface
    ['{ADD18B40-21DF-4804-9804-6B4A47FE7A7C}']

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

    function GetValueType : TFlexValueType; //TODO:MED rename to ValueType

    procedure CopyFrom(const Source : IFlexValue);

    function IsInteger : boolean;
    function IsSingle  : boolean;
    function IsBoolean : boolean;
    function IsPointer : boolean;
    function IsString  : boolean;

    function IsEmpty   : boolean; // Value is not in use.

    property AsInteger : integer   read GetAsInteger    write SetAsInteger;
    property AsSingle  : single    read GetAsSingle     write SetAsSingle;
    property AsBoolean : boolean   read GetAsBoolean    write SetAsBoolean;
    property AsPointer : pointer   read GetAsPointer    write SetAsPointer;
    property AsString  : string    read GetAsString     write SetAsString;

    procedure Clear;
  end;

  TFlexValue = class(TInterfacedObject, IFlexValue)
  private
    FValueType : TFlexValueType;
    FValueData : TFlexValueData;
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

    function GetValueType : TFlexValueType;

    procedure Clear;

    procedure CopyFrom(const Source : IFlexValue);

    function IsInteger : boolean;
    function IsSingle  : boolean;
    function IsBoolean : boolean;
    function IsPointer : boolean;
    function IsString  : boolean;

    function IsEmpty   : boolean; // Value is not in use.

    property AsInteger : integer   read GetAsInteger    write SetAsInteger;
    property AsSingle  : single    read GetAsSingle     write SetAsSingle;
    property AsBoolean : boolean   read GetAsBoolean    write SetAsBoolean;
    property AsPointer : pointer   read GetAsPointer    write SetAsPointer;
    property AsString  : string    read GetAsString     write SetAsString;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TFlexArray = array of IFlexValue;

  TFlexContainer = class
  private
    FCapacity: integer;
  protected
    FData : TFlexArray;
    procedure SetCapacity(const Value: integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Data : TFlexArray read FData;
    property Capacity : integer read FCapacity write SetCapacity;
  end;

  TFlexList = class(TFlexContainer)
  private
    FCount: integer;
    FAutoGrowCapacity: boolean;
    function GetItem(Index: integer): IFlexValue;
  public
    constructor Create; override;

    procedure Add(const Value : IFlexValue);
    procedure Clear;

    property AutoGrowCapaicity : boolean read FAutoGrowCapacity write FAutoGrowCapacity;
    property Items[Index : integer] : IFlexValue read GetItem; default;
    property Count : integer read FCount;
  end;


function Flex(const Value : integer):IFlexValue; overload;
function Flex(const Value : string):IFlexValue; overload;
function Flex(const Value : boolean):IFlexValue; overload;
function Flex(const Value : pointer):IFlexValue; overload;
function Flex(const Value : single):IFlexValue; overload;

implementation

uses
  Math;

function Flex(const Value : integer):IFlexValue;
begin
  result := TFlexValue.Create;
  result.AsInteger := Value;
end;

function Flex(const Value : string):IFlexValue;
begin
  result := TFlexValue.Create;
  result.AsString := Value;
end;

function Flex(const Value : boolean):IFlexValue;
begin
  result := TFlexValue.Create;
  result.AsBoolean := Value;
end;

function Flex(const Value : pointer):IFlexValue;
begin
  result := TFlexValue.Create;
  result.AsPointer := Value;
end;

function Flex(const Value : single):IFlexValue;
begin
  result := TFlexValue.Create;
  result.AsSingle := Value;
end;


{ TFlexValue }

constructor TFlexValue.Create;
begin
  FValueType := fvtEmpty;
  FValueData.VPointer := nil;
end;

destructor TFlexValue.Destroy;
begin
  Clear;
  inherited;
end;

procedure TFlexValue.Clear;
var
  ps : PString;
begin
  case FValueType of
    fvtString:
    begin
      if assigned(FValueData.VPointer) then
      begin
        ps := FValueData.VPointer;
        ps^ := '';
        Dispose(ps);
        FValueData.VPointer := nil;
      end;
    end;
  end;
  FValueType := fvtEmpty;
  FValueData.VPointer := nil;
end;

procedure TFlexValue.SetAsInteger(const Value: integer);
begin
  if (FValueType <> fvtEmpty) and (FValueType <> fvtInteger) then Clear;
  FValueType := TFlexValueType.fvtInteger;
  FValueData.VInteger := Value;
end;

procedure TFlexValue.SetAsSingle(const Value: single);
begin
  if (FValueType <> fvtEmpty) and (FValueType <> fvtSingle) then Clear;
  FValueType := TFlexValueType.fvtSingle;
  FValueData.VSingle := Value;
end;

procedure TFlexValue.SetAsBoolean(const Value: boolean);
begin
  if (FValueType <> fvtEmpty) and (FValueType <> fvtBoolean) then Clear;
  FValueType := TFlexValueType.fvtBoolean;
  FValueData.VBoolean := Value;
end;

procedure TFlexValue.SetAsPointer(const Value: pointer);
begin
  if (FValueType <> fvtEmpty) and (FValueType <> fvtPointer) then Clear;
  FValueType := TFlexValueType.fvtPointer;
  FValueData.VPointer := Value;
end;

procedure TFlexValue.SetAsString(const Value: string);
var
  ps : PString;
begin
  if (FValueType <> fvtString) then
  begin
    Clear;
    New(ps);
    FValueData.VPointer := ps;
    FValueType := TFlexValueType.fvtString;
  end else
  begin
    ps := FValueData.VPointer;
  end;
  ps^ := Value;
end;

function TFlexValue.GetAsInteger: integer;
begin
  assert(FValueType = TFlexValueType.fvtInteger);
  result := FValueData.VInteger;
end;

function TFlexValue.GetAsSingle: single;
begin
  assert(FValueType = TFlexValueType.fvtSingle);
  result := FValueData.VSingle;
end;

procedure TFlexValue.CopyFrom(const Source: IFlexValue);
begin
  case source.GetValueType of
    fvtEmpty:   Clear;
    fvtInteger: AsInteger := Source.GetAsInteger;
    fvtSingle:  AsSingle  := Source.GetAsSingle;
    fvtBoolean: AsBoolean := Source.GetAsBoolean;
    fvtPointer: AsPointer := Source.GetAsPointer;
    fvtString:  AsString  := Source.GetAsString;
  end;
end;



function TFlexValue.GetAsBoolean: boolean;
begin
  assert(FValueType = TFlexValueType.fvtBoolean);
  result := FValueData.VBoolean;
end;

function TFlexValue.GetAsPointer: pointer;
begin
  assert(FValueType = TFlexValueType.fvtPointer);
  result := FValueData.VPointer;
end;

function TFlexValue.GetAsString: string;
var
  ps :PString;
begin
  assert(FValueType = TFlexValueType.fvtString);
  ps := FValueData.VPointer;
  result := ps^;
end;

function TFlexValue.GetValueType: TFlexValueType;
begin
  result := FValueType;
end;

function TFlexValue.IsBoolean: boolean;
begin
  result := (FValueType = TFlexValueType.fvtBoolean);
end;


function TFlexValue.IsInteger: boolean;
begin
  result := (FValueType = TFlexValueType.fvtInteger);
end;

function TFlexValue.IsEmpty: boolean;
begin
  result := (FValueType = TFlexValueType.fvtEmpty);
end;

function TFlexValue.IsPointer: boolean;
begin
  result := (FValueType = TFlexValueType.fvtPointer);
end;

function TFlexValue.IsSingle: boolean;
begin
  result := (FValueType = TFlexValueType.fvtSingle);
end;

function TFlexValue.IsString: boolean;
begin
  result := (FValueType = TFlexValueType.fvtString);
end;



{ TFlexContainer }

constructor TFlexContainer.Create;
begin
  FCapacity := 0;
end;

destructor TFlexContainer.Destroy;
begin
  SetLength(FData, 0);
  inherited;
end;

procedure TFlexContainer.SetCapacity(const Value: integer);
var
  c1: Integer;
begin
  assert(Value >= 0);

  if Value = FCapacity then exit;

  SetLength(FData, Value);

  if Value > FCapacity then
  begin
    for c1 := FCapacity to Value-1 do
    begin
      FData[c1] := TFlexValue.Create;
    end;
  end;

  FCapacity := Value;
end;

{ TFlexList }

procedure TFlexList.Clear;
var
  c1: Integer;
begin
  for c1 := 0 to FCount-1 do
  begin
    FData[c1].Clear;
  end;
  FCount := 0;
end;

constructor TFlexList.Create;
begin
  inherited;
  FCount := 0;
  AutoGrowCapaicity := true;
end;

function TFlexList.GetItem(Index: integer): IFlexValue;
begin
  assert(Index < FCount);
  result := FData[Index];
end;

procedure TFlexList.Add(const Value: IFlexValue);
begin
  if (FCount = Capacity) then
  begin
    if (AutoGrowCapaicity)
      then Capacity := round((self.Capacity + 1) * 1.65)
      else raise EFlexException.Create('List is full.');
  end;

  FData[FCount] := Value;

  inc(FCount);
end;



end.
