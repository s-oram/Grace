unit VclEx.PopupListEx;

interface

// http://users.atw.hu/delphicikk/listaz.php?id=364&oldal=13
// http://delphi.about.com/od/adptips2006/qt/popuplistex.htm

uses
  Controls;

const
  CM_MENU_CLOSED = CM_BASE + 1001;
  CM_ENTER_MENU_LOOP = CM_BASE + 1002;
  CM_EXIT_MENU_LOOP = CM_BASE + 1003;


implementation

uses Messages, Forms, Menus;

type
  TPopupListEx = class(TPopupList)
  protected
    procedure WndProc(var Message: TMessage) ; override;
  private
    procedure PerformMessage(cm_msg : integer; msg : TMessage) ;
  end;

{ TPopupListEx }
procedure TPopupListEx.PerformMessage(cm_msg: integer; msg : TMessage) ;
begin
  if Screen.Activeform <> nil then
    Screen.ActiveForm.Perform(cm_msg, msg.WParam, msg.LParam) ;
end;

procedure TPopupListEx.WndProc(var Message: TMessage) ;
begin
  case message.Msg of
    WM_ENTERMENULOOP: PerformMessage(CM_ENTER_MENU_LOOP, Message) ;
    WM_EXITMENULOOP : PerformMessage(CM_EXIT_MENU_LOOP, Message) ;
    WM_MENUSELECT :
    with TWMMenuSelect(Message) do
    begin
      if (Menu = 0) and (Menuflag = $FFFF) then
      begin
        PerformMessage(CM_MENU_CLOSED, Message) ;
      end;
    end;
  end;
  inherited;
end;

initialization
  //Popuplist.Free; //free the "default", "old" list
  //PopupList:= TPopupListEx.Create; //create the new one
  // The new PopupList will be freed by
  // finalization section of Menus unit.

end.
