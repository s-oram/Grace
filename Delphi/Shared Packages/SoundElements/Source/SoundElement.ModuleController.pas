unit SoundElement.ModuleController;

interface

uses
  SysUtils,
  Contnrs,
  SoundElement.Types,
  SoundElement.ModulePins,
  SoundElement.Modules;

type
  TModuleController = class
  private
    FModules: TChildModuleList;
    FProcessingList : TObjectList;

  protected
    procedure EventHandle_ModuleListChanged(Sender : TObject);

  public
    constructor Create;
    destructor Destroy; override;

    // ProcessingList is automatically cleared when modules are added or removed.
    procedure UpdateProcessingList(const OutputModule : TCustomModule);
    property ProcessingList : TObjectList read FProcessingList; // Read-Only property

    //Modules aren't owned. Client code is responsible for creating/freeing them.
    property Modules : TChildModuleList read FModules;
  end;

implementation

{ TModuleController }

constructor TModuleController.Create;
begin
  fModules := TChildModuleList.Create;
  fModules.OnChanged := EventHandle_ModuleListChanged;

  FProcessingList := TObjectList.Create;
end;

destructor TModuleController.Destroy;
begin
  fModules.RemoveAll;
  fModules.Free;

  FProcessingList.Free;
  inherited;
end;

procedure TModuleController.EventHandle_ModuleListChanged(Sender: TObject);
begin
  FProcessingList.Clear;
end;

procedure TModuleController.UpdateProcessingList(const OutputModule: TCustomModule);
begin
  if not Modules.Exists(OutputModule.Name) then raise SoundElementModuleException.Create('Output module not found.');


end;

end.
