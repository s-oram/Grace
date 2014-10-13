unit Lucidity.CustomControlHandler;

interface

{$INCLUDE Defines.inc}

uses
  eePlugin,
  Classes,
  Contnrs,
  VamLib.UniqueID,
  VamLib.ZeroObject;

type
  TCustomControlHandler = class(TZeroObject)
  private
  protected
    Plugin : TeePlugin;
    ControlList : TObjectList;
    ThrottleHandle : TUniqueID;
    procedure UpdateControl(const c : TObject); virtual; abstract;


    procedure PluginParameterReset(const ParName : string);
    procedure PluginParameterBeginEdit(const ParName : string);
    procedure PluginParameterEndEdit(const ParName : string);
    procedure PluginParameterChanged(const ParName : string; ParValue : single);

  public
    constructor Create(const aPlugin : TeePlugin); virtual;
    destructor Destroy; override;

    procedure RegisterControl(const c : TObject); virtual;
    procedure DeregisterControl(const c : TObject); virtual;
    procedure UpdateAllControls;
  end;

implementation

uses
  uConstants,
  VamLib.Throttler,
  eeTypes,
  Lucidity.GuiUtils,
  Lucidity.PluginParameters;

{ TCustomControlHandler }

constructor TCustomControlHandler.Create(const aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;
  ThrottleHandle.Init;
end;

destructor TCustomControlHandler.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TCustomControlHandler.PluginParameterBeginEdit(const ParName: string);
var
  Par : TPluginParameterClass;
  ParID    : TPluginParameterID;
begin
  ParID    := PluginParNameToID(ParName);
  Par := Plugin.PluginParameters.FindByName(ParName);

  assert(assigned(Par));

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.OnParControlEnter, @ParName, nil);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_ShowParChangeInfo, @ParID, nil);

  if (Par.IsPublishedVstParameter) then
  begin
    Plugin.Globals.GuiState.ActiveVstPluginParameterID := PluginParNameToID(ParName);
    Command.VstPar_BeginEdit(Plugin, Par.VstParameterIndex);
  end;
end;

procedure TCustomControlHandler.PluginParameterEndEdit(const ParName: string);
var
  Par : TPluginParameterClass;
begin
  Par := Plugin.PluginParameters.FindByName(ParName);
  assert(assigned(Par));

  if (Par.IsPublishedVstParameter) then
  begin
    Command.VstPar_EndEdit(Plugin, Par.VstParameterIndex);
    Plugin.Globals.GuiState.ActiveVstPluginParameterID := -1;
  end;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_HideParChangeInfo);
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

  Throttle(ThrottleHandle, 25,
  procedure
  begin
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_UpdateParChangeInfo, @ParID, nil);
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_UpdateScope);
  end);
end;

procedure TCustomControlHandler.PluginParameterReset(const ParName: string);
begin
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
