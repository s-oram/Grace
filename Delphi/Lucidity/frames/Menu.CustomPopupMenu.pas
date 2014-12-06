unit Menu.CustomPopupMenu;

interface

uses
  Classes,
  Vcl.Menus,
  VclEx.PopupMenuEx,
  eePlugin;

type
  TCustomPopupMenu = class
  private
    fPlugin: TeePlugin;
    fAutoFreeMenu: boolean;
  protected
    FOwner : TComponent;
    Menu : TPopupMenuEx;

    property Plugin : TeePlugin read fPlugin;

    procedure EventHandle_MenuClosed(Sender: TObject; Cancelled: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin);

    // TODO:MED = An AutoFreeMenu property would be really good to have.
    // A menu can free itself if it is dismissed by the OnDismissed event handle.
    // I need to figure out how to respond when a menu item is clicked.
    // -- create a new MenuItem class.
    // -- use a detour to add a new Click() method.
    // -- redirect the onClick event handler when the menu item is added to the
    //    TPopupMenuEx class.
    // ----- After a bit of experimenting i think it might be easier to
    //       use custom TMenuItem and TPopup menu classes that extend or wrap
    //       the original classes.
    property AutoFreeMenu : boolean read fAutoFreeMenu write fAutoFreeMenu;
  end;

implementation

uses
  Dialogs;

{ TCustomPopupMenu }

constructor TCustomPopupMenu.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  AutoFreeMenu := false;
  Menu := TPopupMenuEx.Create(AOwner);
  Menu.OnDismissed := EventHandle_MenuClosed;
end;

destructor TCustomPopupMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TCustomPopupMenu.EventHandle_MenuClosed(Sender: TObject; Cancelled: Boolean);
begin
  if (AutoFreeMenu) and (Cancelled)
    then self.Free;
end;

procedure TCustomPopupMenu.Initialize(aPlugin: TeePlugin);
begin
  fPlugin := aPlugin;
end;

end.
