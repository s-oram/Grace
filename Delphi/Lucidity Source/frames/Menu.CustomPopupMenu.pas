unit Menu.CustomPopupMenu;

interface

uses
  eePlugin, uDialogDisplayArea;

type
  TCustomPopupMenu = class
  private
    fPlugin: TeePlugin;
    fDialogDisplayArea: TDialogDisplayArea;
  protected
    property Plugin : TeePlugin read fPlugin;
    property DialogDisplay : TDialogDisplayArea read fDialogDisplayArea;
  public

    procedure Initialize(aPlugin : TeePlugin; aDialogDisplayArea: TDialogDisplayArea);

  end;

implementation

{ TCustomPopupMenu }

procedure TCustomPopupMenu.Initialize(aPlugin: TeePlugin; aDialogDisplayArea: TDialogDisplayArea);
begin
  fPlugin := aPlugin;
  fDialogDisplayArea := aDialogDisplayArea;
end;

end.
