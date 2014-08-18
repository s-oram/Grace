program ComponentTester;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  ACS_MemFloat in '..\Third Party\newAC\src\ACS_MemFloat.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
