program LucidityKeyGen;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  Lucidity.CopyProtection in '..\..\Lucidity.CopyProtection.pas',
  Lucidity.KeyGen in 'Lucidity.KeyGen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
