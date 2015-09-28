unit Main;

interface

procedure Run;

implementation

uses
  WatchTower,
  WatchTower.Global,
  VamLib.Console;

procedure Run;
var
  WriteToLog : TWriteToLogMethod;

begin
  WriteToLog := procedure(Msg : string)
  begin
    WriteLn(Msg);
  end;

  WatchTower.Global.RunTests(WriteToLog);

  WriteLn(' ');

  WaitAnyKeyPressed('Press any key to continue...');
end;

end.
