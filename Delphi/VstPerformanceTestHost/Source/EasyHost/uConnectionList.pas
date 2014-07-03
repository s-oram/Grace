unit uConnectionList;

interface

uses
  uEasyHostTypes, uIntegerList,
  Contnrs;

type
  TConnectionList = class
  private
    List:TObjectList;
    function GetCount:integer;
    function GetConnection(Index: integer): TConnection;
    procedure SetConnection(Index: integer; const Value: TConnection);
  protected


  public
    constructor Create;
	  destructor Destroy; override;

    function CreateConnection(FromMachine,ToMachine:TMachineID; FromPin, ToPin:integer):integer;
    procedure DeleteConnection(Index:integer);

    procedure FindSourceMachines(aMachine:TMachineID; var results:TIntegerList; IncludeSourceParents:boolean = false);

    function WillCreateFeedbackLoop(FromMachine,ToMachine:TMachineID; FromPin, ToPin:integer):boolean;

    property Connections[Index:integer]:TConnection read GetConnection write SetConnection; default;
    property Count:integer read GetCount;

  end;


implementation

uses
  SysUtils;

{ TConnectionList }

constructor TConnectionList.Create;
begin
  List := TObjectList.Create;
  List.OwnsObjects := true;
end;

destructor TConnectionList.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TConnectionList.FindSourceMachines(aMachine:TMachineID;
  var results:TIntegerList; IncludeSourceParents:boolean = false);
var
  c1:integer;
begin
  for c1 := 0 to Count - 1 do
  begin
    if (Connections[c1].ToMachine = aMachine) and (results.IndexOf(Connections[c1].FromMachine) = -1) then
    begin
      results.Add(Connections[c1].FromMachine);
      if IncludeSourceParents
        then FindSourceMachines(Connections[c1].FromMachine, results, true);
    end;
  end;
end;

function TConnectionList.GetConnection(Index: integer): TConnection;
begin
  result := List[Index] as TConnection;
end;

procedure TConnectionList.SetConnection(Index: integer;
  const Value: TConnection);
begin
  List[Index] := Value;
end;


function TConnectionList.GetCount: integer;
begin
  result := List.Count;
end;


function TConnectionList.WillCreateFeedbackLoop(FromMachine,
  ToMachine: TMachineID; FromPin, ToPin: integer): boolean;
var
  SourceMachines:TIntegerList;
begin
  SourceMachines := TIntegerList.Create;

  FindSourceMachines(FromMachine,SourceMachines,true);

  if SourceMachines.IndexOf(ToMachine) = -1
    then result := true    //Proposed connection will create feedback loop. This is bad and to be avoided! 
    else result := false;
    
end;

function TConnectionList.CreateConnection(FromMachine, ToMachine: TMachineID;
  FromPin, ToPin: integer): integer;
var
  aConnection:TConnection;
begin
  assert(WillCreateFeedbackLoop(FromMachine,ToMachine,FromPin,ToPin) = false);

  if WillCreateFeedbackLoop(FromMachine,ToMachine,FromPin,ToPin) = false then
  begin
    raise Exception.Create('Feedback loop created.');
    exit;
  end;


  aConnection := TConnection.Create;

  aConnection.FromMachine := FromMachine;
  aConnection.ToMachine   := ToMachine;
  aConnection.FromPin := FromPin;
  aConnection.ToPin   := ToPin;

  result := List.Add(aConnection);
end;

procedure TConnectionList.DeleteConnection(Index: integer);
begin
  List.Delete(Index);
end;



end.
