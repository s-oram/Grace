program Demo.Spring.ObserverPattern;

uses
  Forms,
  frmObserverPattern in 'frmObserverPattern.pas' {Form28},
  uObserverDemo in 'uObserverDemo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm28, Form28);
  Application.Run;
end.
