unit Main;

interface

procedure Run;

implementation

uses
  ActiveX,
  WatchTower,
  WatchTower.Global,
  RegisterTests.WatchTower,
  RegisterTests.VamLib,
  RegisterTests.Lucidity,
  VamLib.Console;

{$TYPEDADDRESS OFF}

procedure Run;
var
  WriteToLog : TWriteToLogMethod;

begin
  CoInitialize(nil);
  try
    WriteToLog := procedure(Msg : string)
    begin
      WriteLn(Msg);
    end;

    //WatchTower.Global.RunTests(WriteToLog, 'S:\Delphi\Shared Packages\WatchTowerTestData');
    WatchTower.Global.RunTests(WriteToLog, ['S:\Delphi\Shared Packages\WatchTowerTestData']);

    WriteLn(' ');

    WaitAnyKeyPressed('Press any key to continue...');
  finally
    CoUninitialize;
  end;
end;

end.
