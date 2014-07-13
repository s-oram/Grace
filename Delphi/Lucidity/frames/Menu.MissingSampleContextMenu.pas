unit Menu.MissingSampleContextMenu;

interface

uses
  Vcl.Menus,
  Menu.CustomPopupMenu;

type
  TMissingSampleContextMenu = class(TCustomPopupMenu)
  private
    Menu : TPopUpMenu;
  protected
    procedure HandleEvent_LocateMissingSample(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);

  end;

implementation

uses
  Dialogs;

{ TMissingSampleContextMenu }

constructor TMissingSampleContextMenu.Create;
begin
  Menu := TPopUpMenu.Create(nil);
end;

destructor TMissingSampleContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TMissingSampleContextMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Locate Missing Sample...';
  mi.OnClick := HandleEvent_LocateMissingSample;
  Menu.Items.Add(mi);

  Menu.Popup(x, y);
end;

procedure TMissingSampleContextMenu.HandleEvent_LocateMissingSample(Sender: TObject);
begin


  ShowMessage(Plugin.FocusedRegion.GetProperties^.SampleFileName);
  //Plugin.FocusedRegion.GetSample^.
end;



end.
