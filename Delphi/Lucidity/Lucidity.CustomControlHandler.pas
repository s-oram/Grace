unit Lucidity.CustomControlHandler;

interface

{$INCLUDE Defines.inc}

uses
  eePlugin,
  Classes,
  Contnrs,
  VamLib.GuiUtils,
  VamLib.UniqueID,
  VamLib.ZeroObject;

type
  TCustomControlHandler = class(TZeroObject)
  private
    procedure LowLevel_ParmeterBeginEdit(const ParName : string);
    procedure LowLevel_ParmeterEndEdit(const ParName : string);

  protected
    Plugin : TeePlugin;
    ControlList : TObjectList;
    ParChangedTK : TThrottleToken;
    procedure UpdateControl(const c : TObject); virtual; abstract;

    procedure PluginParameterBeginEdit(const ParName : string); overload;
    procedure PluginParameterBeginEdit(const ParName1, ParName2: string); overload;
    procedure PluginParameterEndEdit(const ParName : string); overload;
    procedure PluginParameterEndEdit(const ParName1, ParName2: string); overload;
    procedure PluginParameterReset(const ParName : string);
    procedure PluginParameterChanged(const ParName : string; ParValue : single);

  public
    constructor Create(AGuiOwner: TComponent; const aPlugin : TeePlugin); virtual;
    destructor Destroy; override;

    procedure RegisterControl(const c : TObject); virtual;
    procedure DeregisterControl(const c : TObject); virtual;
    procedure UpdateAllControls;
  end;

implementation

uses
  uConstants,
  eeTypes,
  Lucidity.GuiUtils,
  Lucidity.PluginParameters;

{ TCustomControlHandler }

constructor TCustomControlHandler.Create(AGuiOwner: TComponent; const aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;
end;

destructor TCustomControlHandler.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TCustomControlHandler.PluginParameterBeginEdit(const ParName: string);
begin
  assert(IsValidPluginParName(ParName));
  Plugin.Globals.GuiState.ActivePluginParameterID1 := PluginParNameToID(ParName);
  Plugin.Globals.GuiState.ActivePluginParameterID2 := -1;
  LowLevel_ParmeterBeginEdit(ParName);
end;

procedure TCustomControlHandler.PluginParameterEndEdit(const ParName: string);
begin
  assert(IsValidPluginParName(ParName));
  Plugin.Globals.GuiState.ActivePluginParameterID1 := -1;
  Plugin.Globals.GuiState.ActivePluginParameterID2 := -1;
  LowLevel_ParmeterEndEdit(ParName);
end;

procedure TCustomControlHandler.PluginParameterBeginEdit(const ParName1, ParName2: string);
begin
  assert(IsValidPluginParName(ParName1));
  assert(IsValidPluginParName(ParName2));
  Plugin.Globals.GuiState.ActivePluginParameterID1 := PluginParNameToID(ParName1);
  Plugin.Globals.GuiState.ActivePluginParameterID2 := PluginParNameToID(ParName2);
  LowLevel_ParmeterBeginEdit(ParName1);
  LowLevel_ParmeterBeginEdit(ParName2);
end;

procedure TCustomControlHandler.PluginParameterEndEdit(const ParName1, ParName2: string);
begin
  assert(IsValidPluginParName(ParName1));
  assert(IsValidPluginParName(ParName2));
  Plugin.Globals.GuiState.ActivePluginParameterID1 := -1;
  Plugin.Globals.GuiState.ActivePluginParameterID2 := -1;
  LowLevel_ParmeterEndEdit(ParName1);
  LowLevel_ParmeterEndEdit(ParName2);
end;

procedure TCustomControlHandler.LowLevel_ParmeterBeginEdit(const ParName: string);
var
  Par : TPluginParameterClass;
  ParID    : TPluginParameterID;
begin
  ParID    := PluginParNameToID(ParName);
  Par := Plugin.PluginParameters.FindByName(ParName);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.OnParControlEnter, @ParName, nil);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_ShowParChangeInfo, @ParID, nil);
  if (Par.IsPublishedVstParameter) then Command.VstPar_BeginEdit(Plugin, Par.VstParameterIndex);
end;

procedure TCustomControlHandler.LowLevel_ParmeterEndEdit(const ParName: string);
var
  Par : TPluginParameterClass;
begin
  Par := Plugin.PluginParameters.FindByName(ParName);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_HideParChangeInfo);
  if (Par.IsPublishedVstParameter) then Command.VstPar_EndEdit(Plugin, Par.VstParameterIndex);
end;



procedure TCustomControlHandler.PluginParameterChanged(const ParName: string; ParValue: single);
var
  ParID    : TPluginParameterID;
  Par : TPluginParameterClass;
begin
  assert(ParValue >= 0);
  assert(ParValue <= 1);

  ParID    := PluginParNameToID(ParName);

  Par := Plugin.PluginParameters.FindByName(ParName);
  assert(assigned(Par));

  if Par.IsQuantised then
  begin
    ParValue := QuantiseParameterValue(ParValue, Par.QuantisedMin, Par.QuantisedMax);
  end;

  if Par.IsPublishedVstParameter then
  begin
    Command.VstPar_SetParameterAutomated(Plugin, Par.VstParameterIndex, ParValue);
    Plugin.SetPluginParameter(ParID, ParValue, TParChangeScope.psFocused);
  end else
  begin
    Plugin.SetPluginParameter(ParID, ParValue, TParChangeScope.psFocused);
  end;

  Throttle(ParChangedTK, 25,
  procedure
  begin
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_UpdateParChangeInfo, @ParID, nil);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_UpdateScope);
  end);
end;

procedure TCustomControlHandler.PluginParameterReset(const ParName: string);
begin
  // TODO:HIGH this reset value isn't ultimately sent via SetParameterAutomated()
  // so the host application will not see it for parameter automation.
  Plugin.ResetPluginParameter(TParChangeScope.psFocused, ParName);
end;

procedure TCustomControlHandler.RegisterControl(const c: TObject);
begin
  if ControlList.IndexOf(c) = -1 then ControlList.Add(c);
end;

procedure TCustomControlHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TCustomControlHandler.UpdateAllControls;
var
  c1: Integer;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    UpdateControl(ControlList[c1]);
  end;
end;

end.
