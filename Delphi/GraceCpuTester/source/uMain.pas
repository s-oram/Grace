unit uMain;

interface

procedure Run;

implementation

uses
  VamLib.Utils,
  Vst2PerformanceTest,
  SysUtils,
  VamLib.PerformanceTuning,
  Graphics,
  TestHost.Vst2,
  WinApi.Windows,
  DAEffect,
  uAudioMaster,
  phCore;

procedure Run;
var
  Test : TVst2PerformanceTest;
  TestInfo   : TTestInfo;
  DllFileName: string;
  VstPlugin : TVst2Plugin;
  VstHostInfo : TVst2HostInfo;
  LoadResult : boolean;
  TotalTime : single;
  StartTime : int64;
begin
  WriteLn('== Grace Performance Tester ==');
  TestInfo := LoadTestInfo;
  WriteLn('TEST: ' + TestInfo.VstPluginFile);
  WriteLn('');

  VstHostInfo.SampleRate    := TestInfo.SampleRate;
  VstHostInfo.MaxBufferSize := TestInfo.BufferSize;

  VstPlugin := TVst2Plugin.Create(@VstHostInfo);
  try
    if VstPlugin.LoadPlugin(TestInfo.VstPluginFile) then
    begin
      Test := TVst2PerformanceTest.Create;
      AutoFree(@Test);

      Test.SetupTest(VstPlugin, TestInfo.SampleRate, TestInfo.BufferSize);

      TotalTime := 0;
      StartTime := StartTimerMS;

      Test.RunTest;

      TotalTime := StopTimerMS(STartTime);
      WriteLn(FloatToStr(TotalTime));
    end;
  finally
    VstPlugin.Free;
  end;
end;


end.
