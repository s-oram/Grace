unit uMain;

interface

procedure Run;
procedure RunPerformanceTest(const PluginFileName : string; const SampleRate, BufferSize : integer);

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
  TestInfo   : TTestInfo;
  c1: Integer;
begin
  WriteLn('== Grace Performance Tester ==');
  TestInfo := LoadTestInfo;
  WriteLn('TEST: ' + TestInfo.VstPluginFile);
  WriteLn('');

  for c1 := 0 to 4 do
  begin
    RunPerformanceTest(TestInfo.VstPluginFile, TestInfo.SampleRate, TestInfo.BufferSize);
    WriteLn('');
  end;

  WriteLn('');
  WriteLn('Testing Finished.');

end;

procedure RunPerformanceTest(const PluginFileName : string; const SampleRate, BufferSize : integer);
var
  Test : TVst2PerformanceTest;
  DllFileName: string;
  VstPlugin : TVst2Plugin;
  VstHostInfo : TVst2HostInfo;
  LoadResult : boolean;
  TotalTime : single;
  StartTime : int64;
begin
  VstHostInfo.SampleRate    := SampleRate;
  VstHostInfo.MaxBufferSize := BufferSize;

  VstPlugin := TVst2Plugin.Create(@VstHostInfo);
  try
    if VstPlugin.LoadPlugin(PluginFileName) then
    begin
      Test := TVst2PerformanceTest.Create;
      AutoFree(@Test);

      Test.SetupTest(VstPlugin, SampleRate, BufferSize);

      TotalTime := 0;
      StartTime := StartTimerMS;
      Test.RunTest;
      TotalTime := StopTimerMS(StartTime);
      WriteLn(FloatToStr(TotalTime));
    end;
  finally
    VstPlugin.Free;
  end;
end;


end.
