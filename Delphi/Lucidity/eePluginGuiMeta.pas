unit eePluginGuiMeta;

interface

uses
  WinApi.Windows,
  eePlugin, eePluginGui,
  GuiMeta.ActiveModDisplay,
  GuiMeta.ScopeHandler;

type
  ///  The TPluginGuiMeta class adds some additional functionality
  ///  to the GUI.

  TPluginGuiMeta = class
  private
  protected
    Plugin       : TeePlugin;
    Gui          : TPluginGUI;
    SystemWindow : hwnd;

    ScopeHandler : TScopeHandler;
    ActiveModDetector : TModDisplayDetector;
  public
    constructor Create(aPlugin : TeePlugin; aGui : TPluginGui; aSystemWindow : hwnd);
    destructor Destroy; override;
  end;

implementation

uses
  VamLib.ZeroObject,
  VamQuery,
  Classes,
  Controls,
  LucidityGui.Scope,
  VamKnob,
  VamTextBox,
  VamButton;

{ TPluginGuiMeta }


constructor TPluginGuiMeta.Create(aPlugin: TeePlugin; aGui: TPluginGui; aSystemWindow: hwnd);
var
  c : TControl;
begin
  Plugin       := aPlugin;
  Gui          := aGui;
  SystemWindow := aSystemWindow;

  ScopeHandler := TScopeHandler.Create(Plugin);
  Plugin.Globals.MotherShip.RegisterZeroObject(ScopeHandler, TZeroObjectRank.VCL);

  c := FindControlbyName(Gui, 'Scope');
  if assigned(c) then
  begin
    ScopeHandler.ScopeControl := c as TLucidityScope;
  end;

  ActiveModDetector := TModDisplayDetector.Create(Plugin);
  aPlugin.Globals.MotherShip.RegisterZeroObject(ActiveModDetector, TZeroObjectRank.VCL);
end;

destructor TPluginGuiMeta.Destroy;
begin
  ScopeHandler.Free;
  ActiveModDetector.Free;
end;

end.
