unit eePluginGuiMeta;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.ZeroObject,
  WinApi.Windows,
  eePlugin, eePluginGui,
  GuiMeta.ActiveModDisplay,
  GuiMeta.ScopeHandler;

type
  ///  The TPluginGuiMeta class adds some additional functionality
  ///  to the GUI.

  TPluginGuiMeta = class(TZeroObject)
  private
  protected
    Plugin       : TeePlugin;
    Gui          : TPluginGUI;
    SystemWindow : hwnd;

    ScopeHandler : TScopeHandler;
    ActiveModDetector : TActiveParameterDetector;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface); override;

    procedure EventHandled_MidiNoteTriggered(const MidiData1, MidiData2 : byte);
  public
    constructor Create(const aPlugin : TeePlugin; const aSystemWindow : hwnd);
    destructor Destroy; override;

    procedure PostCreate(const aGui : TPluginGui);
  end;

implementation

uses
  SysUtils,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  VamQuery,
  Classes,
  Controls,
  LucidityGui.Scope,
  VamKnob,
  VamTextBox,
  VamButton,
  uConstants,
  Lucidity.Interfaces,
  Lucidity.SampleMap;

{ TPluginGuiMeta }


constructor TPluginGuiMeta.Create(const aPlugin: TeePlugin; const aSystemWindow: hwnd);
begin
  Plugin       := aPlugin;
  SystemWindow := aSystemWindow;

  // Register self to mother ship.
  Plugin.Globals.MotherShip.RegisterZeroObject(self, TZeroObjectRank.VCL);


  ScopeHandler := TScopeHandler.Create(Plugin);
  Plugin.Globals.MotherShip.RegisterZeroObject(ScopeHandler, TZeroObjectRank.VCL);

  ActiveModDetector := TActiveParameterDetector.Create(Plugin);
  aPlugin.Globals.MotherShip.RegisterZeroObject(ActiveModDetector, TZeroObjectRank.VCL);
end;

destructor TPluginGuiMeta.Destroy;
begin
  Plugin.Globals.MotherShip.DeregisterZeroObject(self);

  ScopeHandler.Free;
  ActiveModDetector.Free;
  inherited;
end;

procedure TPluginGuiMeta.PostCreate(const aGui: TPluginGui);
var
  c : TControl;
begin
  Gui := aGui;

  c := FindControlbyName(Gui, 'Scope');
  if assigned(c) then
  begin
    ScopeHandler.ScopeControl := c as TLucidityScope;
  end;
end;

procedure TPluginGuiMeta.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB: IInterface);
var
  MidiNoteTriggerData : TMsgData_MidiNoteTriggered;
begin
  if MsgID = TLucidMsgID.MidiNoteTriggered then
  begin
    MidiNoteTriggerData := (DataB as IZeroMessageData).GetObject as TMsgData_MidiNoteTriggered;
    EventHandled_MidiNoteTriggered(MidiNoteTriggerData.Data1, MidiNoteTriggerData.Data2);
  end;


  if MsgID = TLucidMsgID.CheckForSampleFocusChange then
  begin
    // TODO:HIGH here we need to check what the sample focus should be, and check if it's
    // changed. If so send SampleFocusChanged message. Right now we just send the message
    // by default. (It's a chance for future optimisation.)
    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
  end;

end;

procedure TPluginGuiMeta.EventHandled_MidiNoteTriggered(const MidiData1, MidiData2: byte);
var
  c1 : integer;
  rg : IRegion;
  kg : IKeygroup;
begin
  // TODO:HIGH instead of checking for a new region to focus, I should check if
  // the currently focused region is among the triggered items. If not, then look
  // for an item to trigger.
  for c1 := Plugin.SampleMap.RegionCount-1 downto 0 do
  begin
    rg := Plugin.SampleMap.Regions[c1];
    kg := rg.GetKeyGroup;
    if (IsNoteInsideRegion(rg, MidiData1, MidiData2)) then
    begin
      Plugin.FocusRegion(rg.GetProperties^.UniqueID);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
      break;
    end;
  end;
end;



end.
