unit VamLib.EnumHelper;

interface

type
  TEnumHelper = class
  public
    class function EnumToStr<TEnum>(const Value : TEnum):string;
    class function GetEnumTypeCount<TEnum>: integer; static;
  end;

  EnumHelper = TEnumHelper;

implementation

uses
  RTTI,
  TypInfo;

{ EnumHelper }

class function TEnumHelper.GetEnumTypeCount<TEnum>: integer;
begin
  result := GetTypeData(TypeInfo(TEnum)).MaxValue + 1;
end;

class function TEnumHelper.EnumToStr<TEnum>(const Value: TEnum): string;
var
  AsInt: Int64;
begin
  TValue.From<TEnum>(Value).TryAsOrdinal(AsInt);
  result := GetEnumName(TypeInfo(TEnum), AsInt)
end;


{
var
  Tipo: PTypeInfo;
  Temp: Integer;
  PTemp: Pointer;
begin
  // == NEW METHOD ==
  // Source: http://stackoverflow.com/q/2472487/395461
  Tipo := TypeInfo(TEnum);
  Temp := GetEnumValue(Tipo, Value);
  PTemp := @Temp;
  Result := TEnum(PTemp^);
end;
}

end.
