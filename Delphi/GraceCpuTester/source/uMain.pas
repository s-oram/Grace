unit uMain;

interface

procedure Run;

implementation

uses
  WinApi.Windows,
  DAEffect,
  uAudioMaster,
  phCore;



procedure Run;
var
  Main:TMainProc;
  TestInfo   : TTestInfo;
  DllFileName: string;
  VstDllHandle:THandle;
  AudioMaster:TAudioMasterCallbackFunc;
  VstPlugin:PAEffect;




begin
  WriteLn('== Grace Performance Tester ==');

  TestInfo := LoadTestInfo;
  WriteLn('TEST: ' + TestInfo.VstPluginFile);

  DllFileName := TestInfo.VstPluginFile;
  VstDllHandle := LoadLibrary(@DllFileName[1]);

  try
    if VstDllHandle <> 0 then
    begin
      @Main := GetProcAddress(VstDllHandle,'main');
      //TODO: Check main has been found.

      //==== Create the plugin ====
      AudioMaster := uAudioMaster.AudioMasterCallBack;
      VstPlugin := Pointer(Main(AudioMaster));
      if assigned(VstPlugin) then
      begin
        WriteLn('VST Plugin has been created.');


        //Init Vst plugin.
        VstDispatch(VstPlugin, effOpen);
        VstDispatch(VstPlugin, effSetSampleRate,0,0,nil, TestInfo.SampleRate);
        VstDispatch(VstPlugin, effSetBlockSize,0, TestInfo.BufferSize,nil,0);

        // InputCount := Vst^.numInputs;
        // OutputCount := Vst^.numOutputs;

        //QueryPlugCanDo;

        //After all initialisation, calling TurnOn allows the plugin to ready itself for audio processing.
        VstDispatch(VstPlugin, effMainsChanged,0,0); //Call 'suspend'
        VstDispatch(VstPlugin, effMainsChanged,0,1); //Call 'resume'

        //==== perform audio processing here ====


      end;
    end;
  finally
    if assigned(VstPlugin) then
    begin
      // turn plugin off
      VstDispatch(VstPlugin, effMainsChanged);  //Call 'suspend'
      //=== close the plugin =====
      VstDispatch(VstPlugin, effClose);
    end;

    if VstDllHandle <> 0
      then FreeLibrary(VstDllHandle);

    VstDllHandle := 0;
  end;

  WriteLn('..');
  WriteLn('Finished.');

end;

end.
