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
  VQ : IVamQuery;
  Parent : TComponent;
  c : TControl;
begin
  Plugin       := aPlugin;
  Gui          := aGui;
  SystemWindow := aSystemWindow;

  ScopeHandler := TScopeHandler.Create(Plugin.Globals);
  Plugin.Globals.MotherShip.RegisterZeroObject(ScopeHandler);

  c := FindControlbyName(Gui, 'Scope');
  if assigned(c) then
  begin
    ScopeHandler.ScopeControl := c as TLucidityScope;
  end;

  Parent := Gui.FindComponent('RedFoxContainer');
  if assigned(Parent) then
  begin
    VQ := VamQueryRequest(Parent as TControl, TVamKnob);
    for c in VQ.List do
    begin
      ScopeHandler.RegisterControl(c);
    end;


    VQ := VamQueryRequest(Parent as TControl, TVamTextBox);
    for c in VQ.List do
    begin
      ScopeHandler.RegisterControl(c);
    end;

    VQ := VamQueryRequest(Parent as TControl, TVamButton);
    for c in VQ.List do
    begin
      ScopeHandler.RegisterControl(c);
    end;

    c := FindControlByName(Parent as TControl, 'LfoSelector');
    ScopeHandler.RegisterControl(c);
  end;


  ActiveModDetector := TModDisplayDetector.Create(Plugin);
  aPlugin.Globals.MotherShip.RegisterZeroObject(ActiveModDetector);



end;

destructor TPluginGuiMeta.Destroy;
begin
  ScopeHandler.Free;
  ActiveModDetector.Free;
end;

end.
