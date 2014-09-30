unit InWindowDialog;

interface

uses
  Vcl.Forms,
  InWindowDialog.MessageDialog, InWindowDialog.InputDialog;

procedure InWindow_ShowMessage(const TopLevelForm : TForm; const Msg : string);

implementation

procedure InWindow_ShowMessage(const TopLevelForm : TForm; const Msg : string);
var
  MsgDialog : TMessageDialog;
begin
  MsgDialog := TMessageDialog.Create;
  MsgDialog.Text := Msg;
  MsgDialog.ShowInWindow_WithAutoFree(TopLevelForm, true, true);
end;

end.
