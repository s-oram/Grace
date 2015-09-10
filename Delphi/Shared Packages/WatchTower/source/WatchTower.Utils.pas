unit WatchTower.Utils;

interface

function GetTestID(const PackageName, GroupName, TestName : string):string;
function GetTestingFinishedMessage(const TestCount, ErrorCount : integer):string;

procedure CreateSingletonObject(var GlobalReference : pointer; NewInstance : TObject);


implementation

uses
  SysUtils,
  Test.WatchTower;

{$I InterlockedAPIs.inc}

function GetTestID(const PackageName, GroupName, TestName : string):string;
begin
  result := PackageName + '::' + GroupName + '::' + TestName;
end;

procedure CreateSingletonObject(var GlobalReference : pointer; NewInstance : TObject);
begin
  // NOTE: Lock-free initialization.
  // http://stackoverflow.com/a/24657705/395461

  //It's possible another thread also created one.
  //Only one of us will be able to set the AObject singleton variable
  if InterlockedCompareExchangePointer(GlobalReference, NewInstance, nil) <> nil then
  begin
     //The other beat us. Destroy our newly created object and use theirs.
     NewInstance.Free;
  end;

end;


function GetTestingFinishedMessage(const TestCount, ErrorCount : integer):string;
var
  s : string;
begin
  s := 'Testing Finished. ';

  if TestCount = 1
    then s := s + '1 test ran. '
    else s := s + IntToStr(TestCount) + ' tests ran. ';

  if ErrorCount = 1
    then s := s + '1 error found. '
    else s := s + IntToStr(ErrorCount) + ' errors found. ';

  result := s;
end;

end.
