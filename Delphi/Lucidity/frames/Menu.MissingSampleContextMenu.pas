unit Menu.MissingSampleContextMenu;

interface

uses
  Vcl.Menus,
  Menu.CustomPopupMenu;

type
  TMissingSampleContextMenu = class(TCustomPopupMenu)
  private

  protected
    procedure HandleEvent_LocateMissingSamples(Sender : TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);

  end;

implementation

uses
  SysUtils,
  VamLib.Utils,
  uConstants,
  Lucidity.Types,
  Lucidity.Interfaces,
  Lucidity.GuiUtils;

{ TMissingSampleContextMenu }

constructor TMissingSampleContextMenu.Create;
begin
  inherited;
end;

destructor TMissingSampleContextMenu.Destroy;
begin

  inherited;
end;

procedure TMissingSampleContextMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Locate Missing Samples...';
  mi.OnClick := HandleEvent_LocateMissingSamples;
  Menu.Items.Add(mi);

  Menu.Popup(x, y);
end;

procedure TMissingSampleContextMenu.HandleEvent_LocateMissingSamples(Sender: TObject);
begin
  Command.FindMissingSamples(Plugin);
end;

end.
