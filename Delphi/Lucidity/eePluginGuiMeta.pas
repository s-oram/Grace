unit eePluginGuiMeta;

interface

uses
  WinApi.Windows,
  eePlugin, eePluginGui,
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
  public
    constructor Create(aPlugin : TeePlugin; aGui : TPluginGui; aSystemWindow : hwnd);
    destructor Destroy;

  end;

implementation

uses
  VamQuery,
  Classes,
  Controls,
  LucidityGui.Scope,
  VamKnob;

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

  ScopeHandler := TScopeHandler.Create;

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
  end;






end;

destructor TPluginGuiMeta.Destroy;
begin
  ScopeHandler.Free;
end;

end.
