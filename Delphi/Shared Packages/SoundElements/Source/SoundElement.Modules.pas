unit SoundElement.Modules;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  SoundElement.Types,
  SoundElement.ModulePins;

{


}

type
  //======== Forward Declarations ========================
  TModulePinList = class;
  TCustomModule = class;
  //======================================================
  //======================================================

  TModulePinList = class
  private
    PinList : TObjectList;
    function GetPin(Index: string): TModulePin; overload;
    function GetPin(Index: integer): TModulePin; overload;
    function GetCount: integer;
  protected
    FPinPolarity : TPinPolarity;
    FParentModule : TCustomModule;
  public
    constructor Create(const aPolarity : TPinPolarity; const aParentModule : TCustomModule);
    destructor Destroy; override;

    function NewPin(const PinName : string; const PinType : TPinType):TModulePin;
    function Exists(const PinName : string):boolean; overload;
    function Exists(const PinName : string; out Pin : TModulePin):boolean; overload;
    procedure Delete(const PinName : string);

    property Pins[Index : integer]:TModulePin read GetPin; default;
    property Pins[Index : string]:TModulePin read GetPin; default;

    property Count : integer read GetCount;
  end;


  TChildModuleList = class
  private
    fOnChanged: TNotifyEvent;
    function GetModule(Index: integer): TCustomModule; overload;
    function GetModule(Index: string): TCustomModule; overload;
    function GetCount: integer;
  protected
    FModuleList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const aModule : TCustomModule);
    procedure Remove(const aModule : TCustomModule);
    procedure RemoveAll;

    function Exists(const ModuleName : string):boolean; overload;
    function Exists(const ModuleName : string; out aModule : TCustomModule):boolean; overload;

    property Modules[Index : integer] : TCustomModule read GetModule; default;
    property Modules[Index : string]  : TCustomModule read GetModule; default;

    property Count : integer read GetCount;

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;
  end;

  TCustomModule = class
  private
    FName: string;
    fOutputs: TModulePinList;
    fInputs: TModulePinList;
    FChildren : TChildModuleList;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetMaxBufferSize(const Size : integer); virtual;

    // Modules with streaming inputs/outputs will require block processing.
    // Modules only using event inputs/outputs don't require block processing.
    function RequiresBlockProcessing:boolean; virtual;

    property Name : string read FName write FName;

    property Inputs  : TModulePinList read fInputs;
    property Outputs : TModulePinList read fOutputs;

    property Children : TChildModuleList read FChildren;

    procedure ProcessBuffer(const SampleFrames : integer); virtual; abstract;
  end;


  ModuleCommand = record
    class function CheckForFeedbackConnections(const Target : TCustomModule):boolean; static;

    // Find modules immediately connected to the ReferenceModule.
    class procedure FindUpstreamModules(const ReferenceModule : TCustomModule; const List : TObjectList); static;

    // Find connected modules, find modules connected to those and so on until no more modules found.
    class procedure FindAllUpstreamModules(const ReferenceModule : TCustomModule; const List : TObjectList); static;
    class procedure FindAllDownstreamModules(const ReferenceModule : TCustomModule; const List : TObjectList); static;

    class procedure BreakAllConnections(const Target : TCustomModule); static;

    // Finds the correct processing order for an arbitary chain of modules.
    class procedure BuildProcessGraph(const FinalOutputModule : TCustomModule; const OutputList : TObjectList); static;
  end;

implementation

uses
  VamLib.Utils;



{ TModulePinList }

constructor TModulePinList.Create(const aPolarity : TPinPolarity; const aParentModule : TCustomModule);
begin
  FParentModule := aParentModule;
  FPinPolarity  := aPolarity;

  PinList := TObjectList.Create;
  PinList.OwnsObjects := true;
end;

destructor TModulePinList.Destroy;
begin
  PinList.Free;
  inherited;
end;

function TModulePinList.NewPin(const PinName: string; const PinType: TPinType): TModulePin;
var
  aPin : TModulePin;
begin
  if PinName = '' then raise SoundElementModuleException.Create('Pin name must not be empty.');
  if Exists(PinName) then raise SoundElementModuleException.Create('Pin with this name already exists! (' + PinName + ')');

  aPin := PinCommand.CreatePin(PinType, FPinPolarity);
  aPin.Name  := PinName;
  aPin.Owner := FParentModule;

  PinList.Add(aPin);

  result := aPin;
end;

function TModulePinList.Exists(const PinName: string): boolean;
var
  Pin : TModulePin;
begin
  Pin := GetPin(PinName);
  result := assigned(Pin);
end;

function TModulePinList.Exists(const PinName: string; out Pin: TModulePin): boolean;
begin
  Pin := GetPin(PinName);
  result := assigned(Pin);
end;

function TModulePinList.GetCount: integer;
begin
  result := PinList.Count;
end;

function TModulePinList.GetPin(Index: integer): TModulePin;
begin
  result := PinList[Index] as TModulePin;
end;

function TModulePinList.GetPin(Index: string): TModulePin;
var
  c1: Integer;
begin
  for c1 := 0 to PinList.Count-1 do
  begin
    if SameText((PinList[c1] as TModulePin).Name, Index) then
    begin
      exit(PinList[c1] as TModulePin);
    end;
  end;

  // if we made it this far, the pin doesn't exist.
  result := nil;
end;

procedure TModulePinList.Delete(const PinName: string);
var
  c1: Integer;
begin
  for c1 := PinList.Count-1 downto 0 do
  begin
    if SameText((PinList[c1] as TModulePin).Name, PinName) then
    begin
      PinList.Delete(c1);
    end;
  end;
end;

{ TChildModuleList }

constructor TChildModuleList.Create;
begin
  FModuleList := TObjectList.Create;
  FModuleList.OwnsObjects := true;
end;

destructor TChildModuleList.Destroy;
begin
  FModuleList.Free;
  inherited;
end;

function TChildModuleList.Exists(const ModuleName: string): boolean;
var
  aModule : TCustomModule;
begin
  aModule := GetModule(ModuleName);
  result := assigned(aModule);
end;

function TChildModuleList.Exists(const ModuleName: string; out aModule: TCustomModule): boolean;
begin
  aModule := GetModule(ModuleName);
  result := assigned(aModule);
end;

procedure TChildModuleList.Add(const aModule: TCustomModule);
begin
  assert(assigned(aModule));
  if aModule.Name = '' then raise SoundElementModuleException.Create('Module name cannot be empty.');
  if Exists(aModule.Name) then raise SoundElementModuleException.Create('Module with same name already registered. (' + aModule.Name + ')');

  FModuleList.Add(aModule);

  if assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TChildModuleList.Remove(const aModule: TCustomModule);
begin
  FModuleList.Remove(aModule);
  if assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TChildModuleList.RemoveAll;
var
  Obj : TObject;
  c1: Integer;
begin
  for c1 := FModuleList.Count-1 downto 0 do
  begin
    Obj := FModuleList[c1];
    FModuleList.Remove(Obj);
  end;
  if assigned(FOnChanged) then FOnChanged(Self);
end;

function TChildModuleList.GetCount: integer;
begin
  result := FModuleList.Count;
end;

function TChildModuleList.GetModule(Index: string): TCustomModule;
var
  c1 : integer;
begin
  for c1 := FModuleList.Count-1 downto 0 do
  begin
    if SameText((FModuleList[c1] as TCustomModule).Name, Index) then
    begin
      result := (FModuleList[c1] as TCustomModule);
      exit;
    end;
  end;

  // module not found.
  result := nil;
end;

function TChildModuleList.GetModule(Index: integer): TCustomModule;
begin
  result := FModuleList[Index] as TCustomModule;
end;


{ TCustomModule }

constructor TCustomModule.Create;
begin
  fInputs  := TModulePinList.Create(TPinPolarity.Input, self);
  fOutputs := TModulePinList.Create(TPinPolarity.Output, self);

  FChildren := TChildModuleList.Create;
end;

destructor TCustomModule.Destroy;
begin
  FInputs.Free;
  FOutputs.Free;
  FChildren.Free;
  inherited;
end;



function TCustomModule.RequiresBlockProcessing: boolean;
var
  c1: Integer;
begin
  for c1 := 0 to FInputs.Count-1 do
  begin
    if (FInputs[c1] is TStreamInputPin) then exit(true);
  end;

  for c1 := 0 to FOutputs.Count-1 do
  begin
    if (FOutputs[c1] is TStreamOutputPin) then exit(true);
  end;

  // if we make it this far, result is false.
  result := false;
end;

procedure TCustomModule.SetMaxBufferSize(const Size: integer);
var
  c1: Integer;
begin
  for c1 := 0 to Inputs.Count-1 do
  begin
    Inputs[c1].SetMaxBufferSize(Size);
  end;

  for c1 := 0 to Outputs.Count-1 do
  begin
    Outputs[c1].SetMaxBufferSize(Size);
  end;
end;

{ ModuleCommand }

class procedure ModuleCommand.FindUpstreamModules(const ReferenceModule: TCustomModule; const List: TObjectList);
var
  c1, c2: Integer;
  Obj : TObject;
begin
  assert(List.OwnsObjects = false); //You probably do not want the list to take own the modules.

  for c1 := 0 to ReferenceModule.Inputs.Count-1 do
  begin
    for c2 := 0 to ReferenceModule.Inputs[c1].ConnectionCount-1 do
    begin
      Obj := ReferenceModule.Inputs[c1].Connections[c2].Owner;
      if (assigned(Obj)) and (Obj is TCustomModule) then
      begin
        List.Add(Obj);
      end;
    end;

  end;
end;


class procedure ModuleCommand.FindAllUpstreamModules(const ReferenceModule: TCustomModule; const List: TObjectList);
var
  c1, c2: Integer;
  Obj : TObject;
begin
  assert(List.OwnsObjects = false); //You probably do not want the list to take own the modules.

  for c1 := 0 to ReferenceModule.Inputs.Count-1 do
  begin
    for c2 := 0 to ReferenceModule.Inputs[c1].ConnectionCount-1 do
    begin
      Obj := ReferenceModule.Inputs[c1].Connections[c2].Owner;
      if (assigned(Obj)) and (Obj is TCustomModule) and (List.IndexOf(Obj) = -1) then
      begin
        List.Add(Obj);
        FindAllUpstreamModules(Obj as TCustomModule, List);
      end;
    end;
  end;
end;

class procedure ModuleCommand.FindAllDownstreamModules(const ReferenceModule: TCustomModule; const List: TObjectList);
var
  c1, c2: Integer;
  Obj : TObject;
begin
  assert(List.OwnsObjects = false); //You probably do not want the list to take own the modules.

  for c1 := 0 to ReferenceModule.Outputs.Count-1 do
  begin
    for c2 := 0 to ReferenceModule.Outputs[c1].ConnectionCount-1 do
    begin
      Obj := ReferenceModule.Outputs[c1].Connections[c2].Owner;
      if (assigned(Obj)) and (Obj is TCustomModule) and (List.IndexOf(Obj) = -1) then
      begin
        List.Add(Obj);
        FindAllDownstreamModules(Obj as TCustomModule, List);
      end;
    end;
  end;
end;

class procedure ModuleCommand.BreakAllConnections(const Target: TCustomModule);
var
  c1: Integer;
begin
  for c1 := 0 to Target.Inputs.Count-1 do
  begin
    PinCommand.BreakAllConnections(Target.Inputs[c1]);
  end;

  for c1 := 0 to Target.Outputs.Count-1 do
  begin
    PinCommand.BreakAllConnections(Target.Outputs[c1]);
  end;
end;

class procedure ModuleCommand.BuildProcessGraph(const FinalOutputModule: TCustomModule; const OutputList: TObjectList);
  procedure AppendUpstreamModules(const ReferenceModule: TCustomModule; const List: TObjectList);
  var
    c1, c2: Integer;
    Obj : TObject;
    ObjIndex : integer;
  begin
    for c1 := 0 to ReferenceModule.Inputs.Count-1 do
    begin
      for c2 := 0 to ReferenceModule.Inputs[c1].ConnectionCount-1 do
      begin
        Obj := ReferenceModule.Inputs[c1].Connections[c2].Owner;
        if (assigned(Obj)) and (Obj is TCustomModule) then
        begin
          ObjIndex := List.IndexOf(Obj);
          if ObjIndex <> -1 then List[ObjIndex] := nil;
          List.Add(Obj);
        end;
      end;
    end;
  end;
var
  CurrentIndex : integer;
  TempList : TObjectList;
  RefMod : TCustomModule;
  c1: Integer;
begin
  assert(OutputList.OwnsObjects = false); //You probably do not want the list to take own the modules.
  if OutputList.Count <> 0 then raise SoundElementModuleException.Create('Output list is not empty.');

  // WARNING: The BuildProcessGraph() method currently only looks for modules connected upstream.
  // Therefore modules without a downstream connection to the FinalOutputModule will not be processed.
  // There are some instances that might require downstream connected modules to be processed, but
  // right now this whole module thing is only at proof-of-concept stage. I'll add downstream
  // checking at a later date.

  TempList := TObjectList.Create;
  TempList.OwnsObjects := false;
  AutoFree(@TempList);

  TempList.Add(FinalOutputModule);
  CurrentIndex := 0;

  while CurrentIndex < TempList.Count do
  begin
    RefMod := TempList[CurrentIndex] as TCustomModule;
    inc(CurrentIndex);

    if assigned(RefMod) then
    begin
      //NOTE: The BuildProcessGraph() method can't handle feedback between modules.
      if CheckForFeedbackConnections(RefMod) then
      begin
        OutputList.Clear;
        raise SoundElementModuleException.Create('Feedback detected!');
      end;
      AppendUpstreamModules(RefMod, TempList);
    end;
  end;

  // reverse the list and filter out any nil values.
  for c1 := TempList.Count-1 downto 0 do
  begin
    if assigned(TempList[c1]) then
    begin
      if (TempList[c1] as TCustomModule).RequiresBlockProcessing then
      begin
        // If a module has already been added to the list, something has gone wrong in the AppendUpstreamModules() method.
        assert(OutputList.IndexOf(TempList[c1]) = -1);
        OutputList.Add(TempList[c1]);
      end;
    end;
  end;
end;

class function ModuleCommand.CheckForFeedbackConnections(const Target: TCustomModule): boolean;
  function Internal_CheckForFeedbackConnections(const Target, Current : TCustomModule):boolean;
  var
    c1, c2: Integer;
    Obj : TObject;
  begin
    for c1 := 0 to Current.Inputs.Count-1 do
    begin
      for c2 := 0 to Current.Inputs[c1].ConnectionCount-1 do
      begin
        Obj := Current.Inputs[c1].Connections[c2].Owner;
        if (assigned(Obj)) and (Obj is TCustomModule) then
        begin
          if Obj = Target then exit(true); // Feedback detected ========================================================>> exit >>===>>
          if Internal_CheckForFeedbackConnections(Target, Obj as TCustomModule) then exit(true); // Feedback detected ==>> exit >>===>>
        end;
      end;
    end;
    // If we make it this far, no feedback has been detected.
    result := false;
  end;
begin
  result := Internal_CheckForFeedbackConnections(Target, Target);
end;

end.
