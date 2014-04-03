unit uEnumDemo;

interface

procedure DoEnumDemo;

implementation

uses
      Spring.Utils
    , Types
    ;

type
  TNumberEnum = (One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten);

procedure DoEnumDemo;
var
  Names: TStringDynArray;
  ValueStrings: TStringDynArray;
  S: string;
  Values: TIntegerDynArray;
  i: integer;
  NE: TNumberEnum;
begin

  if TEnum.IsValid<TNumberEnum>(Four) then
  begin
    WriteLn(TEnum.GetName<TNumberEnum>(Four), ' is a valid enum value');
  end;

  WriteLn('Here are all the Names for TNumberEnum: ');
  Names := TEnum.GetNames<TNumberEnum>;
  for S in Names do
  begin
    WriteLn(S);
  end;

  WriteLn('Here are all the values for TNumberEnum: ');
  Values := TEnum.GetValues<TNumberEnum>;
  for I in Values do
  begin
    WriteLn(I);
  end;

  WriteLn('Here are all the value strings for TNumberEnum: ');
  ValueStrings := TEnum.GetValueStrings<TNumberEnum>;
  for S in ValueStrings do
  begin
    WriteLn(S);
  end;

  S := 'Seven';
  NE := TEnum.Parse<TNumberEnum>(S);
  if S = TEnum.GetName<TNumberEnum>(NE) then
  begin
    WriteLn(S, ' was properly parsed as ', TEnum.GetName<TNumberEnum>(NE));
  end else
  begin
    WriteLn('The TEnum.Parse call failed');
  end;


end;

end.
