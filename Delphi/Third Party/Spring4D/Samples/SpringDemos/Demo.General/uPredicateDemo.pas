unit uPredicateDemo;

interface

procedure CheckIsLessThan10;
procedure FilterList;
procedure FlipCoins;


implementation

uses
    Spring
  , Spring.Collections
  , Spring.Collections.Lists
  ;


procedure CheckIsLessThan10;
var
  IsLessThan10: TPredicate<integer>;
  i: integer;
begin
  IsLessThan10 := function(const aValue: integer): Boolean
                    begin
                      Result := aValue < 10;
                    end;
  Write('Enter an integer: ');
  Readln(i);

  if IsLessThan10(i) then
  begin
    Writeln(i, ' is LESS THAN 10');
  end else
  begin
    Writeln(i, ' is GREATER THAN OR EQUAL to 10');
  end;
end;

procedure FilterList;
var
  Temp: IEnumerable<integer>;
  i: integer;
  List: IList<integer>;
begin
  List := TList<integer>.Create;
  List.Add(3);
  List.Add(6);
  List.Add(8);
  List.Add(34);
  List.Add(65);
  List.Add(86);

  Temp := List.TakeWhile(function(const aInt: integer): Boolean begin Result := aInt < 50; end);
  for i in Temp do
  begin
    WriteLn(i, ' is less than 50');
  end;

end;

procedure FlipCoins;
var
  FlipResults: IList<integer>;
  i: Integer;
  TestTrue: TPredicate<integer>;
  TheTrueOnes: IEnumerable<integer>;
const
  TotalFlips = 10000;
begin
  Randomize;

  FlipResults := TList<integer>.Create;
  for i := 1 to TotalFlips do
    begin
      FlipResults.Add(Random(2));
    end;
    TestTrue := function(const aValue: integer): Boolean
                  begin
                    Result := aValue = 1;
                  end;
    TheTrueOnes := FlipResults.Where(TestTrue);
    Writeln('Total Heads: ', TheTrueOnes.Count, ' out of ', TotalFlips);
end;


end.
