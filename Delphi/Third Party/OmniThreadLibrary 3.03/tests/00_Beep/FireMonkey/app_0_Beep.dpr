program app_0_Beep;

uses
  FMX.Forms,
  test_0_Beep in 'test_0_Beep.pas' {frmTestSimple};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTestSimple, frmTestSimple);
  Application.Run;
end.
