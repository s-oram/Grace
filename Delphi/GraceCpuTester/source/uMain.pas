unit uMain;

interface

procedure Run;


implementation

uses
  TestHost.Vst2,
  WinApi.Windows,
  DAEffect,
  uAudioMaster,
  phCore;

procedure Run;
var
  TestInfo   : TTestInfo;
  DllFileName: string;
  VstPlugin : TVst2Plugin;
  VstHostInfo : TVst2HostInfo;
  LoadResult : boolean;
begin
  WriteLn('== Grace Performance Tester ==');
  TestInfo := LoadTestInfo;
  WriteLn('TEST: ' + TestInfo.VstPluginFile);
  WriteLn('');

  VstHostInfo.SampleRate := TestInfo.SampleRate;
  VstHostInfo.MaxBufferSize := TestInfo.BufferSize;

  VstPlugin := TVst2Plugin.Create(@VstHostInfo);
  try
    if VstPlugin.LoadPlugin(TestInfo.VstPluginFile) then
    begin

    end;
  finally
    VstPlugin.Free;
  end;
end;


end.
