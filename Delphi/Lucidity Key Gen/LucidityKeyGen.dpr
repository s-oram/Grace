program LucidityKeyGen;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  Lucidity.KeyGen in 'Lucidity.KeyGen.pas',
  Lucidity.CopyProtection in '..\Lucidity\Lucidity.CopyProtection.pas',
  Lucidity.KeyGenMaster in 'Lucidity.KeyGenMaster.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
