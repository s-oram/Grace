unit Temp_PluginPar;

interface

type
  TFloatPluginPar = record
  private
    fValue : single;
    function GetValueAsFloat: single;
    procedure SetValueAsFloat(const Value: single);
  public
    procedure SetAsEnum<TEnum>(x : TEnum);
    function AsEnum<TEnum>:TEnum;
    property ValueAsFloat : single read GetValueAsFloat write SetValueAsFloat;
  end;

  TPluginParHelper = record

    class function GetEnumTypeCount<TEnum>: integer; static;
  end;

implementation

uses
  TypInfo;

{
function GetEnumTypeCount<TEnum>: integer;
begin
  result := GetTypeData(TypeInfo(TEnum)).MaxValue + 1;
end;
}

{ TPluginParHelper }

class function TPluginParHelper.GetEnumTypeCount<TEnum>: integer;
begin

  result := GetTypeData(TypeInfo(TEnum)).MaxValue + 1;
end;


{ TFloatPluginPar }

function TFloatPluginPar.AsEnum<TEnum>: TEnum;
begin

end;

function TFloatPluginPar.GetValueAsFloat: single;
begin
  result := fValue;
end;

procedure TFloatPluginPar.SetValueAsFloat(const Value: single);
begin
  fValue := Value;
end;


end.
