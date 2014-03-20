unit eeEnumHelper;

interface

uses
  SysUtils;

type
  // TEnumHelper is a utility class to ease converting VST parameter values to and from
  // enumerated typed values. The VST protocol limits VST parameters to the 0..1 range.
  //
  // A 'helper' class needs to be created for each Enumerated type. Generics makes that simple.
  // Example:
  //     TMyEnumType = (ValueOne, ValueTwo, ValueThree);
  //     TMyEnumTypeHelper = class(TEnumHelper<TMyEnumType>);
  //
  // WARNING: The TEnumHelper class can't be used with enumerated types that
  // assign custom integer values to the individual types such as:
  //     TBadEnum = (Frog = 1, Cat = 4, Tree = 7);
  // TEnumHelper class relies on each type having it's default integer value.
  //

  //TBadEnum = (Frog = 1, Cat = 4, Tree = 7);


  //NOTE: http://delphi.fosdal.com/2011/04/generics-enumerated-types-and-ordinal.html

  EnumHelperException = Exception;

  TCustomEnumHelper = class;
  TCustomEnumHelperClass = class of TCustomEnumHelper;

  TCustomEnumHelper = class
  public
    class function GetEnumTypeCount : integer; virtual; abstract;

    class function ToInteger(s : string):integer; overload; virtual; abstract;
    class function ToInteger(x : single):integer; overload; virtual; abstract;

    class function ToString(Ordinal : integer):string; overload; virtual; abstract;
    class function ToString(x : single):string; overload; virtual; abstract;

    class function ToSingle(s : string):single; overload; virtual; abstract;
    class function ToSingle(Ordinal : integer):single; overload; virtual; abstract;

    class function ToFullGuiString(Ordinal : integer):string; overload; virtual; abstract;
    class function ToFullGuiString(x : single):string; overload; virtual; abstract;

    class function ToShortGuiString(Ordinal : integer):string; overload; virtual; abstract;
    class function ToShortGuiString(x : single):string; overload; virtual; abstract;
  end;

  TEnumHelper<TEnum> = class(TCustomEnumHelper)
  protected
    class function ConvertToStringFromInt(AsInt:integer):string; inline;
  public
    class function GetEnumTypeCount : integer; override;

    class function ToInteger(s : string):integer; override;
    class function ToInteger(x : single):integer; override;
    class function ToInteger(aEnum : TEnum):integer; overload;

    class function ToSingle(s : string):single; override;
    class function ToSingle(Ordinal : integer):single; override;
    class function ToSingle(aEnum : TEnum):single; overload;

    class function ToString(x : single):string; override;
    class function ToString(Ordinal : integer):string; override;
    class function ToString(aEnum : TEnum):string; overload;

    class function ToEnum(s : string): TEnum; overload;
    class function ToEnum(x : single): TEnum; overload;
    class function ToEnum(Ordinal : integer): TEnum; overload;

    class function ToFullGuiString(aEnum : TEnum):string; overload; virtual;
    class function ToFullGuiString(Ordinal : integer):string; overload; override;
    class function ToFullGuiString(x : single):string; overload; override;

    class function ToShortGuiString(aEnum : TEnum):string; overload; virtual;
    class function ToShortGuiString(Ordinal : integer):string; overload; override;
    class function ToShortGuiString(x : single):string; overload; override;
  end;


  // TODO:
  // write a cast function using generics
  // http://stackoverflow.com/questions/16433104/delphi-how-to-get-type-of-enum






implementation

uses
  Rtti, TypInfo, Math;

{ TEnumHelper<TEnum> }
class function TEnumHelper<TEnum>.GetEnumTypeCount: integer;
begin
  result := GetTypeData(TypeInfo(TEnum)).MaxValue + 1;
end;

class function TEnumHelper<TEnum>.ToEnum(s: string): TEnum;
var
  c1: Integer;
  TestString : string;
  v: TValue;
begin
  //result := TFilterType(GetEnumValue(TypeInfo(TFilterType), s));

  for c1 := 0 to GetEnumTypeCount-1 do
  begin
    //TestString := ToString(ToEnum(c1));
    TestString := ToString(c1);
    if SameText(TestString, s) then
    begin
      result := ToEnum(c1);
      exit; //==============>> exit >>=========>>
    end;
  end;

  //if we've made it this far...
  raise EnumHelperException.Create('TEnumHelper couldn''t compute value from string.');
end;

class function TEnumHelper<TEnum>.ToEnum(x: single): TEnum;
var
  AsInt : integer;
begin
  if (x < 0) or (x > 1) then raise Exception.Create('Enum value is out of range. x = ' + FloatToStr(x));

  AsInt := floor(x * GetEnumTypeCount);
  if AsInt >= GetEnumTypeCount then AsInt := GetEnumTypeCount-1;
  result := ToEnum(AsInt);
end;

class function TEnumHelper<TEnum>.ToEnum(Ordinal: integer): TEnum;
var
  v: TValue;
begin
  TValue.Make(Ordinal, TypeInfo(TEnum), v);
  Result := v.AsType<TEnum>;
end;

class function TEnumHelper<TEnum>.ToInteger(s: string): integer;
begin
  assert(false, 'TODO');
end;

class function TEnumHelper<TEnum>.ToInteger(x: single): integer;
var
  AsInt : integer;
begin
  if (x < 0) or (x > 1) then raise Exception.Create('Enum value is out of range. x = ' + FloatToStr(x));

  AsInt := floor(x * GetEnumTypeCount);
  if AsInt >= GetEnumTypeCount then AsInt := GetEnumTypeCount-1;

  result := AsInt;
end;

class function TEnumHelper<TEnum>.ToInteger(aEnum: TEnum): integer;
var
  i: Int64;
begin
  TValue.From<TEnum>(aEnum).TryAsOrdinal(i);
  result := i;
end;

class function TEnumHelper<TEnum>.ToSingle(aEnum: TEnum): single;
var
  x : integer;
begin
  x := ToInteger(aEnum);
  result := x / (GetEnumTypeCount - 1);
end;

class function TEnumHelper<TEnum>.ToSingle(s: string): single;
var
  c1: Integer;
  TestString : string;
begin
  for c1 := 0 to GetEnumTypeCount - 1 do
  begin
    TestString := ToString(c1);
    if SameText(s, TestString) then
    begin
      result := ToSingle(c1);
      exit; //===================>> exit >>==============>>
    end;
  end;

  //if we've made it this far...
  raise EnumHelperException.Create('TEnumHelper couldn''t compute value from string.');
end;

class function TEnumHelper<TEnum>.ToSingle(Ordinal: integer): single;
begin
  if (Ordinal < 0) or (Ordinal > (GetEnumTypeCount - 1)) then raise Exception.Create('Enum value is out of range. Ordinal = ' + IntToStr(Ordinal));

  result := Ordinal / (GetEnumTypeCount - 1);
end;

class function TEnumHelper<TEnum>.ToString(Ordinal: integer): string;
begin
  result := ConvertToStringFromInt(Ordinal);
end;

class function TEnumHelper<TEnum>.ToString(aEnum: TEnum): string;
var
  i: Int64;
begin
  TValue.From<TEnum>(aEnum).TryAsOrdinal(i);
  result := ConvertToStringFromInt(i);
end;

class function TEnumHelper<TEnum>.ToString(x: single): string;
var
  AsInt : integer;
begin
  if (x < 0) or (x > 1) then raise Exception.Create('Enum value is out of range. x = ' + FloatToStr(x));

  AsInt := floor(x * GetEnumTypeCount);
  if AsInt >= GetEnumTypeCount then AsInt := GetEnumTypeCount-1;

  result := ConvertToStringFromInt(AsInt);
end;

class function TEnumHelper<TEnum>.ConvertToStringFromInt(AsInt:integer): string;
begin
  // NOTE: All ToString() methods use this method as the final step in converting
  // to a string. This ensures all produced strings will have the same 'formatting'
  // regardless of whether they are converted from Enum, single or integer type values.
  result := Format('%s', [GetEnumName(TypeInfo(TEnum), AsInt)]);
end;

class function TEnumHelper<TEnum>.ToFullGuiString(aEnum: TEnum): string;
begin
  result := Self.ToString(aEnum);
end;

class function TEnumHelper<TEnum>.ToFullGuiString(Ordinal: integer): string;
var
  e : TEnum;
begin
  e := ToEnum(Ordinal);
  result := ToFullGuiString(e);
end;

class function TEnumHelper<TEnum>.ToFullGuiString(x: single): string;
var
  e : TEnum;
begin
  e := ToEnum(x);
  result := ToFullGuiString(e);
end;


class function TEnumHelper<TEnum>.ToShortGuiString(aEnum: TEnum): string;
begin
  result := Self.ToString(aEnum);
end;

class function TEnumHelper<TEnum>.ToShortGuiString(Ordinal: integer): string;
var
  e : TEnum;
begin
  e := ToEnum(Ordinal);
  result := ToShortGuiString(e);
end;

class function TEnumHelper<TEnum>.ToShortGuiString(x: single): string;
var
  e : TEnum;
begin
  e := ToEnum(x);
  result := ToShortGuiString(e);
end;














end.

