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
  MoreTypes in '..\..\Library\Units - other\MoreTypes.pas',
  uIntegerList in '..\..\Library\Units - other\uIntegerList.pas',
  DAEffect in 'Source\DVST\DAEffect.pas',
  DAEffectX in 'Source\DVST\DAEffectX.pas',
  DAudioEffect in 'Source\DVST\DAudioEffect.pas',
  DAudioEffectX in 'Source\DVST\DAudioEffectX.pas',
  DVstFxStore in 'Source\DVST\DVstFxStore.pas',
  DVSTUtils in 'Source\DVST\DVSTUtils.pas',
  ProcessPerformanceTest in 'Source\ProcessPerformanceTest.pas',
  uVstMidiInput in 'Source\EasyHost\uVstMidiInput.pas',
  Console in '..\..\Library\Console\Console.pas',
  CpuCycleTimer in '..\..\Library\Units - other\CpuCycleTimer.pas',
  phCore in 'phCore.pas';

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

    ReadKey;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
