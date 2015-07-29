program GraceCpuTester;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uMain in 'source\uMain.pas',
  phCore in 'source\phCore.pas',
  uAudioMaster in 'source\uAudioMaster.pas',
  TestHost.Vst2 in 'source\Vst2Host\TestHost.Vst2.pas',
  TestHost.Vst2.DAEffect in 'source\Vst2Host\TestHost.Vst2.DAEffect.pas',
  TestHost.Vst2.DAEffectX in 'source\Vst2Host\TestHost.Vst2.DAEffectX.pas';

begin
  try
    uMain.Run;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
