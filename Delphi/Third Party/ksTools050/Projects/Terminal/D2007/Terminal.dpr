program Terminal;

uses
  Forms,
  main in '..\Source\main.pas' {frmMain},
  DCBFrm in '..\Source\DCBFrm.pas' {frmDCB},
  ksComm in '..\..\..\Source\ksComm.pas',
  ksClasses in '..\..\..\Source\ksClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDCB, frmDCB);
  Application.Run;
end.
