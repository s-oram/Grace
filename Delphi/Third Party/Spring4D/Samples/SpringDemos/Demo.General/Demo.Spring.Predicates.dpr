program Demo.Spring.Predicates;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  uPredicateDemo in 'uPredicateDemo.pas';



begin
  try
    CheckIsLessThan10;
      WriteLn;
    FilterList;
      WriteLn;
    FlipCoins;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
