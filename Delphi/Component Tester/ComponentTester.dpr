program ComponentTester;



uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  ACS_MemFloat in '..\Third Party\newAC\src\ACS_MemFloat.pas',
  InWindowDialog.MessageDialog in '..\Lucidity\Dialogs\InWindowDialog.MessageDialog.pas',
  InWindowDialog.MessageDialog.Form in '..\Lucidity\Dialogs\InWindowDialog.MessageDialog.Form.pas' {MessageDialogForm},
  InWindowDialog.ModalShadow.Form in '..\Lucidity\Dialogs\InWindowDialog.ModalShadow.Form.pas',
  InWindowDialog.Prototypes in '..\Lucidity\Dialogs\InWindowDialog.Prototypes.pas',
  InWindowDialog.InputDialog.Form in '..\Lucidity\Dialogs\InWindowDialog.InputDialog.Form.pas',
  InWindowDialog.InputDialog in '..\Lucidity\Dialogs\InWindowDialog.InputDialog.pas',
  InWindowDialog.CustomDialog.Form in '..\Lucidity\Dialogs\InWindowDialog.CustomDialog.Form.pas',
  InWindowDialog.CustomDialog in '..\Lucidity\Dialogs\InWindowDialog.CustomDialog.pas',
  InWindowDialog in '..\Lucidity\Dialogs\InWindowDialog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
