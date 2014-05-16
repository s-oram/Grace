unit GuidEx;

{

TGuidEx - Delphi class for manipulating Guid values

http://delphi.about.com/library/weekly/aa022205a.htm

A Guid type represent a 128-bit integer value.
The TGuidEx class exposes class (static) methods
that help operate GUID values and TGuidField
database field types.

~ Zarko Gajic
}


interface

uses SysUtils;

type
  TGuidEx = class
    class function NewGuid : TGuid;
    class function EmptyGuid : TGuid;
    class function IsEmptyGuid(Guid : TGuid) : boolean;
    class function ToUnicodeString(Guid : TGuid) : string;
    class function ToQuotedString(Guid : TGuid) : string;
    class function FromString(Value : string) : TGuid;
    class function EqualGuids(Guid1, Guid2 : TGuid) : boolean;
  end;

implementation

{ TGuidEx }

class function TGuidEx.EmptyGuid: TGuid;
begin
  result := FromString('{00000000-0000-0000-0000-000000000000}');
end;

class function TGuidEx.EqualGuids(Guid1, Guid2: TGuid): boolean;
begin
  result := IsEqualGUID(Guid1, Guid2);
end;

class function TGuidEx.FromString(Value: string): TGuid;
begin
  result := StringToGuid(Value);
end;

class function TGuidEx.IsEmptyGuid(Guid : TGuid): boolean;
begin
  result := EqualGuids(Guid,EmptyGuid);
end;

class function TGuidEx.NewGuid: TGuid;
var
  Guid : TGuid;
begin
  CreateGUID(Guid);
  Result := Guid;
end;

class function TGuidEx.ToQuotedString(Guid: TGuid): string;
begin
  result := QuotedStr(ToUnicodeString(Guid));
end;

class function TGuidEx.ToUnicodeString(Guid: TGuid): string;
begin
  result := GuidToString(Guid);
end;

end.






