program Demo.Spring.FactoryPattern;

uses
  Forms,
  frmFactoryDemo in 'frmFactoryDemo.pas' {Form29},
  uFactoryDemo in 'uFactoryDemo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm29, Form29);
  Application.Run;
end.
