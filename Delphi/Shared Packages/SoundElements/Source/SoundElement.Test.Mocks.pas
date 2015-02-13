unit SoundElement.Test.Mocks;

interface

uses
  SoundElement.Types,
  SoundElement.ModulePins,
  SoundElement.Modules,
  SoundElement.ModuleController;

type
  TSteroModule = class(TCustomModule)
  public
    constructor Create; override;
  end;

  TEventHandler = class
  private
    FIsDead: boolean;
  public
    property IsDead : boolean read FIsDead write FIsDead;

    procedure EventHandle(Sender : TObject);
  end;

implementation

{ TSteroModule }

constructor TSteroModule.Create;
begin
  inherited;

  Inputs.NewPin('In1', TPinType.Float64Stream);
  Inputs.NewPin('In2', TPinType.Float64Stream);

  Outputs.NewPin('Out1', TPinType.Float64Stream);
  Outputs.NewPin('Out2', TPinType.Float64Stream);
end;

{ TEventHandler }

procedure TEventHandler.EventHandle(Sender: TObject);
begin
  FIsDead := true;
end;

end.
