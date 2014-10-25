program GraceProgramTool;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form4},
  Lucidity.ProgramFileUtils in '..\Lucidity\Lucidity.ProgramFileUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
