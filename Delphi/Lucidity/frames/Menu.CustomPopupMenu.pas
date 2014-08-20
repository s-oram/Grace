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
  protected
    Menu : TPopUpMenu;
    property Plugin : TeePlugin read fPlugin;
    property DialogDisplay : TDialogDisplayArea read fDialogDisplayArea;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin; aDialogDisplayArea: TDialogDisplayArea);
  end;

implementation

{ TCustomPopupMenu }

constructor TCustomPopupMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TCustomPopupMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TCustomPopupMenu.Initialize(aPlugin: TeePlugin; aDialogDisplayArea: TDialogDisplayArea);
begin
  fPlugin := aPlugin;
  fDialogDisplayArea := aDialogDisplayArea;
end;

end.
