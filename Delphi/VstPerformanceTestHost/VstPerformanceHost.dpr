program VstPerformanceHost;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uBuffers in 'Source\EasyHost\uBuffers.pas',
  uConnectionList in 'Source\EasyHost\uConnectionList.pas',
  uEasyHostFunctions in 'Source\EasyHost\uEasyHostFunctions.pas',
  uEasyHostTypes in 'Source\EasyHost\uEasyHostTypes.pas',
  uMachine in 'Source\EasyHost\uMachine.pas',
  uMachineList in 'Source\EasyHost\uMachineList.pas',
  uVstWrapper in 'Source\EasyHost\uVstWrapper.pas',
  uAudioMaster in 'Source\EasyHost\uAudioMaster.pas',
  DAEffect in 'Source\DVST\DAEffect.pas',
  DAEffectX in 'Source\DVST\DAEffectX.pas',
  DAudioEffect in 'Source\DVST\DAudioEffect.pas',
  DAudioEffectX in 'Source\DVST\DAudioEffectX.pas',
  DVstFxStore in 'Source\DVST\DVstFxStore.pas',
  DVSTUtils in 'Source\DVST\DVSTUtils.pas',
  ProcessPerformanceTest in 'Source\ProcessPerformanceTest.pas',
  uVstMidiInput in 'Source\EasyHost\uVstMidiInput.pas',
  phCore in 'phCore.pas',
  MoreTypes in 'Source\General\MoreTypes.pas',
  uIntegerList in 'Source\General\uIntegerList.pas',
  CpuCycleTimer in 'Source\General\CpuCycleTimer.pas',
  Console in 'Source\General\Console.pas',
  eeVSTExtra in 'Source\EasyEffect\eeVSTExtra.pas';

var
  Test1 : TProcessPerformanceTest;
begin
  try
    Test1 := TProcessPerformanceTest.Create;
    try
      Test1.RunTest;
    finally
      Test1.Free;
    end;

    ReadLn;
    //ReadKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
