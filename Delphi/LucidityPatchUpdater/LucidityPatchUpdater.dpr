program LucidityPatchUpdater;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form3},
  Lucidity.StateManager.PatchVersionUpdater in '..\Lucidity\Lucidity.StateManager.PatchVersionUpdater.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
