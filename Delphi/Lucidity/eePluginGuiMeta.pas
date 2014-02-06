unit eePluginGuiMeta;

interface

uses
  WinApi.Windows,
  eePlugin, eePluginGui;

type
  ///  The TPluginGuiMeta class adds some additional functionality
  ///  to the GUI.

  TPluginGuiMeta = class
  private
  protected
    Plugin       : TeePlugin;
    Gui          : TPluginGUI;
    SystemWindow : hwnd;
  public
    constructor Create(aPlugin : TeePlugin; aGui : TPluginGui; aSystemWindow : hwnd);
    destructor Destroy;

  end;

implementation

{ TPluginGuiMeta }


constructor TPluginGuiMeta.Create(aPlugin: TeePlugin; aGui: TPluginGui; aSystemWindow: hwnd);
begin
  Plugin       := aPlugin;
  Gui          := aGui;
  SystemWindow := aSystemWindow;
end;

destructor TPluginGuiMeta.Destroy;
begin

end;

end.
