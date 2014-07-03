unit uMachineList;

interface

uses
  uMachine,
  uEasyHostTypes,
  Contnrs;

type
  TMachineList = class
  private
    List:TObjectList;
    function GetCount: integer;
    function GetMachine(Index: integer): TMachine;
    procedure SetMachine(Index: integer; const Value: TMachine);
  protected
  public
    constructor Create;
	  destructor Destroy; override;

    function AddMachine(aMachine:TMachine):integer;
    procedure Delete(Index:integer);

    property Count:integer read GetCount;

    property Machines[Index:integer]:TMachine read GetMachine write SetMachine;
  end;
implementation

{ TMachineList }

constructor TMachineList.Create;
begin
  List := TObjectList.Create;
  List.OwnsObjects := true;
end;

destructor TMachineList.Destroy;
begin
  List.Free;
  inherited;
end;

function TMachineList.AddMachine(aMachine: TMachine): integer;
begin
  result := List.Add(aMachine);
end;

procedure TMachineList.Delete(Index: integer);
begin
  List.Delete(Index);
end;

function TMachineList.GetCount: integer;
begin
  result := List.Count;
end;

function TMachineList.GetMachine(Index: integer): TMachine;
begin
  result := List[Index] as TMachine;
end;

procedure TMachineList.SetMachine(Index: integer; const Value: TMachine);
begin
  List[Index] := Value;
end;

end.
