program ComponentTester;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  ACS_MemFloat in '..\Third Party\newAC\src\ACS_MemFloat.pas',
  InWindowDialog.MessageDialog in '..\Lucidity\Dialogs\InWindowDialog.MessageDialog.pas',
  InWindowDialog.MessageDialog.Form in '..\Lucidity\Dialogs\InWindowDialog.MessageDialog.Form.pas' {MessageDialogForm},
  InWindowDialog.ModalShadow.Form in '..\Lucidity\Dialogs\InWindowDialog.ModalShadow.Form.pas',
  InWindowDialog.Prototypes in '..\Lucidity\Dialogs\InWindowDialog.Prototypes.pas',
  InWindowDialog.InputDialog in '..\Lucidity\Dialogs\InWindowDialog.InputDialog.pas',
  InWindowDialog.InputBoxDialog.Form in '..\Lucidity\Dialogs\InWindowDialog.InputBoxDialog.Form.pas' {InputBoxDialogForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
