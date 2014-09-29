program ComponentTester;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  ACS_MemFloat in '..\Third Party\newAC\src\ACS_MemFloat.pas',
  InWindowsDialog.MessageDialog in '..\Lucidity\Dialogs\InWindowsDialog.MessageDialog.pas',
  InWindowsDialog.MessageDialog.Form in '..\Lucidity\Dialogs\InWindowsDialog.MessageDialog.Form.pas' {MessageDialogForm},
  InWindowDialog.ModalShadow.Form in '..\Lucidity\Dialogs\InWindowDialog.ModalShadow.Form.pas',
  InWindowDialog.Prototypes in '..\Lucidity\Dialogs\InWindowDialog.Prototypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
