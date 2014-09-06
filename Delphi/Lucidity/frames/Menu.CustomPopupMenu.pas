unit Menu.CustomPopupMenu;

interface

uses
  Vcl.Menus,
  VclEx.PopupMenuEx,
  eePlugin, uDialogDisplayArea;

type
  TCustomPopupMenu = class
  private
    fPlugin: TeePlugin;
    fDialogDisplayArea: TDialogDisplayArea;
    fAutoFreeMenu: boolean;
  protected
    Menu : TPopupMenuEx;

    property Plugin : TeePlugin read fPlugin;
    property DialogDisplay : TDialogDisplayArea read fDialogDisplayArea;

    procedure EventHandle_MenuClosed(Sender: TObject; Cancelled: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin; aDialogDisplayArea: TDialogDisplayArea);

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

constructor TCustomPopupMenu.Create;
begin
  AutoFreeMenu := false;
  Menu := TPopupMenuEx.Create(nil);
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

procedure TCustomPopupMenu.Initialize(aPlugin: TeePlugin; aDialogDisplayArea: TDialogDisplayArea);
begin
  fPlugin := aPlugin;
  fDialogDisplayArea := aDialogDisplayArea;
end;

end.
