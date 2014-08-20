unit VclEx.PopupMenuEx;

{

  The TPopupMenu.Popup method (which is used to display such a menu even when
  presented "automatically" by the VCL) has it's own message pump whilst being
  displayed. i.e. the Popup procedure only returns to the caller when the menu
  has been dismissed.

  I used this feature to implement a minor extension to TPopupMenu that not only
   raises an event when the menu has been dismissed, but also peeks in the
   relevant message queue for the presence of a WM_COMMAND message - i.e. was
   the menu dismissed because an item was selected or because the menu was
   cancelled with no item selected. This can then be reflected in the event.

   http://users.atw.hu/delphicikk/listaz.php?id=364&oldal=13
}

interface

uses
  Menus;

type
  TxPopupMenuEvent = procedure(Sender: TObject; Cancelled: Boolean) of object;

  TxPopupMenu = class(TPopupMenu)
  private
    eOnDismissed: TxPopupMenuEvent;
  public
    procedure Popup(X, Y: Integer); override;
  published
    property OnDismissed: TxPopupMenuEvent read eOnDismissed write eOnDismissed;
  end;

implementation

uses
  Controls, Windows,
  Messages, Forms, Menus;

{TIXPopupMenu}

procedure TxPopupMenu.Popup(X, Y: Integer);
var
  msg: tagMSG;
begin
  inherited;
  if Assigned(OnDismissed) then
    OnDismissed(Self, PeekMessage(msg, PopupList.Window, WM_COMMAND,
      WM_COMMAND, PM_NOREMOVE) = FALSE);
end;

end.
