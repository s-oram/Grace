unit uMachine;

interface

uses
  uEasyHostTypes;

type
  TMachine = class
  private
    fMachineID: TMachineID;
    fHasFinishedProcessing: boolean;
    fMachineType: TMachineType;
  public
    constructor Create; virtual;
	  destructor Destroy; override;

    property MachineID:TMachineID read fMachineID write fMachineID;
    property MachineType:TMachineType read fMachineType write fMachineType;
    property HasFinishedProcessing:boolean read fHasFinishedProcessing write fHasFinishedProcessing;
  end;

implementation

{ TMachine }

constructor TMachine.Create;
begin
  MachineID := 0;
  HasFinishedProcessing := false;
end;

destructor TMachine.Destroy;
begin

  inherited;
end;

end.
