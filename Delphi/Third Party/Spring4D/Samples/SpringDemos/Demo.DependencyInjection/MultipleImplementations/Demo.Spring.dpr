program Demo.Spring;

uses
  Vcl.Forms,
  frmMultipleImplementations in 'frmMultipleImplementations.pas' {Form30},
  uCreditCardInterface in 'uCreditCardInterface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm30, Form30);
  Application.Run;
end.
