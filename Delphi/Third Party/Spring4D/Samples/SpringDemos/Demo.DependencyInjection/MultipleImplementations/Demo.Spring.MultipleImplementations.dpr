program Demo.Spring.MultipleImplementations;

uses
  Forms,
  frmMultipleImplementations in 'frmMultipleImplementations.pas' {MultipleImplementationsForm},
  uCreditCardInterface in 'uCreditCardInterface.pas',
  uCreditCards in 'uCreditCards.pas',
  Spring.Container;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  GlobalContainer.Build;
  Application.CreateForm(TMultipleImplementationsForm, MultipleImplementationsForm);
  Application.Run;
end.
