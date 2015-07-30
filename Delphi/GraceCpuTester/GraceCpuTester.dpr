program GraceCpuTester;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uMain in 'source\uMain.pas',
  phCore in 'source\phCore.pas',
  TestHost.Vst2 in 'source\Vst2Host\TestHost.Vst2.pas',
  TestHost.Vst2.DAEffect in 'source\Vst2Host\TestHost.Vst2.DAEffect.pas',
  TestHost.Vst2.DAEffectX in 'source\Vst2Host\TestHost.Vst2.DAEffectX.pas',
  Vst2PerformanceTest in 'source\Vst2PerformanceTest.pas',
  TestHost.Vst2.DAEffect.CustomEx.Vam in 'source\Vst2Host\TestHost.Vst2.DAEffect.CustomEx.Vam.pas';

begin
  try
    uMain.Run;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
