program GraceCpuTester;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uMain in 'source\uMain.pas',
  phCore in 'source\phCore.pas',
  uAudioMaster in 'source\uAudioMaster.pas';

begin
  try
    uMain.Run;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
