unit uEasyHostFunctions;

interface

uses
  uMachineList, uEasyHostTypes;

function GetUniqueMachineID(aMachineList:TMachineList):TMachineID;

implementation

uses
  SysUtils;

function GetUniqueMachineID(aMachineList:TMachineList):TMachineID;
var
  mc:integer;
  LastMachineID:TMachineID;
begin
  assert(aMachineList.Count < 65000);  //Max 65000 unique machineID's.

  if aMachineList.Count = 0 then
  begin
    result := 1;
    exit; //=================================================>
  end;

  mc := aMachineList.Count;

  if aMachineList.Machines[mc-1].MachineID < 65000 then
  begin
    LastMachineID := aMachineList.Machines[mc-1].MachineID;
    result := LastMachineID + 1;
    exit; //=================================================>
  end;


  //TODO: the procedure should never make it this far.
  raise Exception.Create('Unique MachineID not found.');
  result := 0;



end;

end.
