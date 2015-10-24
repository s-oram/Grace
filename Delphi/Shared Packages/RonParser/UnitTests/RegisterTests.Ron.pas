unit RegisterTests.Ron;

interface

implementation

uses
  WatchTower.Global,
  Test.Ron.Lexer,
  Test.Ron.Parser;

procedure RegisterTests;
begin
  WatchTower.Global.RegisterTest(TRonLowLevelFunctions);
  WatchTower.Global.RegisterTest(TRonStringTest);
  WatchTower.Global.RegisterTest(TRonHelperTest);
  WatchTower.Global.RegisterTest(TTokenMatcherTest);
  WatchTower.Global.RegisterTest(TTokenizer_NextToken);
  WatchTower.Global.RegisterTest(TSyntaxCheckerTest);
  WatchTower.Global.RegisterTest(TReadRonValueTest);
  WatchTower.Global.RegisterTest(TRonParserTest);
end;

initialization
  RegisterTests();

end.
