unit eeProcessController;

interface

{$INCLUDE Defines.inc}

{
  SyncKeeper.pas provides a method for a plugin to send sample accurate midi
  to output buffer. This method needs to be reimplemented by this process
  controller.
}

uses
  VamLib.CpuOverloadWatcher,
  eeCpuMonitor, eeVstMidiTypes, eeMidiEvents,
  eePlugin, //TODO: I think this should eventually be removed.
  {$IFDEF HasAudioOuts}
  EasyEffect.AudioOutputController.Custom,
  //EasyEffect.AudioOutputController.Standard,
  //EasyEffect.AudioOutputController.DspMaster,
  //EasyEffect.AudioOutputController.BufferedR8Brain,
  EasyEffect.AudioOutputController.r8Brain,
  {$ENDIF}

  {$IFDEF HasAudioIns}eeVstAudioInputController,{$ENDIF}
  DVstUtils, DAEffect, DAEffectX, DAudioEffect, DAudioEffectX;

type
  TTimeInfoMethod = function(Filter: VstInt32): PVstTimeInfo of object;

  TProcessController = class
  private
    fPlugin: TeePlugin;
    fTimeInfoMethod: TTimeInfoMethod;
  protected
    CpuMonitor : TCpuMonitor;
    OverloadWatch : TCpuOverloadWatcher;

    fSampleRate : integer;
    fFastControlRateDivision : integer;
    fSlowControlRateDivision : integer;

    SlowControlRateStepMax   : integer;
    SlowControlRateStepCount : integer;

    ControlRateOffset : integer;

    ProcessReplacingOffset : integer;

    {$IFDEF HasAudioIns}
    VstInputsController  : TVstAudioInputController;
    {$ENDIF}

    {$IFDEF HasAudioOuts}
    VstOutputsController : TVstAudioOutputController;
    {$ENDIF}

    MidiInput: TeeMidiEventList;
    MidiOutput: TeeMidiEventList;

    function GetTimeInfo(Filter: VstInt32): PVstTimeInfo;

    procedure UpdatePluginTimeInfo(TimeInfo: PVstTimeInfo);

    procedure ProcessAudioBlock(SampleFrames: integer);
  public
    constructor Create(aPlugin : TeePlugin);
    destructor Destroy; override;

    procedure Suspend;
    procedure Resume(const aBlockSize, aSampleRate, aOverSampleFactor, aInputCount, aOutputCount : integer);

    function ProcessEvents(ev: PVstEvents): longint;

    procedure ProcessReplacing(Inputs, Outputs: PPSingle; Sampleframes: Integer);
    procedure ProcessReplacing2(Inputs, Outputs: PPSingle; Sampleframes: Integer);

    property Plugin : TeePlugin read fPlugin;

    property TimeInfoMethod : TTimeInfoMethod read fTimeInfoMethod write fTimeInfoMethod;
  end;



implementation

uses
  Windows,
  SysUtils,
  eeTypes;

// Procedure FilterAndAddMidiEvents
// This procedure takes the vst event data, filters out all the midi events
// and adds them to the plugins MidiInput
procedure FilterAndAddMidiEvents(ev: PVstEvents; aMidiEventList:TeeMidiEventList);
var
  c1: Integer;
  Status: Byte;
  MidiEv: PVstMidiEvent;
  Data1: Byte;
  Data2: Byte;
  Delta: longint;
  Channel: Byte;
begin
  // Check if range checking is active. If it is we need to toggle it off and on for
  // following section of code.
  {$ifopt R+}
     {$define TOGGLE_RANGE_CHECK}
  {$endif}

  {$ifdef TOGGLE_RANGE_CHECK}
     {$R-}
  {$endif}
  for c1 := 0 to ev^.numEvents - 1 do
  begin
    if (ev^.events[c1]^.vtype = kVstMidiType) then
    begin
      //Cast VstEvent as VstMidiEvent
      MidiEv := PVstMidiEvent(ev^.events[c1]);

      Channel := MidiEv^.midiData[0] and $F;
      Status := MidiEv^.midiData[0] and $F0;
      Data1 := MidiEv^.midiData[1] and $7F;
      Data2 := MidiEv^.midiData[2] and $7F;
      Delta := MidiEv^.deltaframes;

      aMidiEventList.AddEvent(Status, Channel, Data1, Data2, Delta);
    end;
  end;
  {$ifdef TOGGLE_RANGE_CHECK}
     {$R+}
  {$endif}
end;


{ TProcessControllerV2 }

constructor TProcessController.Create(aPlugin : TeePlugin);
begin
  fPlugin := aPlugin;

  Plugin.Globals.CpuSampleRate   := 44100;
  Plugin.Globals.CpuSampleFrames := 64;

  OverloadWatch := TCpuOverloadWatcher.Create;

  CpuMonitor := TCpuMonitor.Create;

  fPlugin.Globals.CpuMonitor := CpuMonitor;

  {$IFDEF HasAudioIns}
  VstInputsController := TVstAudioInputController.Create;
  VstInputsController.Setup_PluginInputBuffers(@Plugin.Inputs);
  {$ENDIF}

  {$IFDEF HasAudioOuts}
  VstOutputsController := TVstAudioOutputController.Create;
  VstOutputsController.Setup_PluginOutputBuffers(@Plugin.Outputs);
  if Plugin.Settings.IsSynth
    then VstOutputsController.UseSilentBuffers := true
    else VstOutputsController.UseSilentBuffers := false;
  {$ENDIF}

  fFastControlRateDivision := Plugin.Settings.FastControlRateDivision;
  fSlowControlRateDivision := Plugin.Settings.SlowControlRateDivision;

  MidiInput  := TeeMidiEventList.Create;
  MidiOutput := TeeMidiEventList.Create;
end;

destructor TProcessController.Destroy;
begin
  {$IFDEF HasAudioIns}
  VstInputsController.Free;
  {$ENDIF}

  {$IFDEF HasAudioOuts}
  VstOutputsController.Free;
  {$ENDIF}

  CpuMonitor.Free;

  MidiInput.Free;
  MidiOutput.Free;

  OverloadWatch.Free;

  inherited;
end;

function TProcessController.GetTimeInfo(Filter: VstInt32): PVstTimeInfo;
begin
  result := fTimeInfoMethod(Filter);
end;

function TProcessController.ProcessEvents(ev: PVstEvents): longint;
var
  {$IFDEF OverSampleEnabled}
  c1          : integer;
  pxMidiEvent : PeeMidiEvent;
  {$ENDIF}
begin
  {$IFDEF CpuMonitor}
    CpuMonitor.StartVstEventTimer;
  {$ENDIF}

  MidiInput.ClearEvents;
  FilterAndAddMidiEvents(ev, MidiInput);

  {$IFDEF OverSampleEnabled}
  for c1 := 0 to MidiInput.EventCount - 1 do
  begin
    pxMidiEvent := MidiInput.EventAsPointer[c1];
    pxMidiEvent^.Deltaframes := pxMidiEvent^.Deltaframes * Plugin.Settings.OverSampleFactor;
  end;
  {$ENDIF}

  {$IFDEF CpuMonitor}
    CpuMonitor.StopVstEventTimer;
  {$ENDIF}

  // NOTE: According to VST 2.4 docs, the return value of ProcessEvents is ignored.
  result := 0;
end;

procedure TProcessController.ProcessReplacing(Inputs, Outputs: PPSingle; Sampleframes: Integer);
var
  TimeInfo : PVstTimeInfo;
  ModSampleFrames : integer;
begin
  {$EXCESSPRECISION OFF}

  //OutputDebugString('SAMPLING ON');

  {$IFDEF CpuMonitor}
    CpuMonitor.StartProcessReplacingTimer(SampleFrames, fSampleRate);
  {$ENDIF}

  //Globals.CpuMonitor.StartAudioProcessTimer2;
  //Globals.CpuMonitor.StopAudioProcessTimer2;

  ModSampleFrames := SampleFrames * Plugin.Settings.OverSampleFactor;



  //-----------------------------------------------------
  //Setup the input/output pointers.
  //-----------------------------------------------------
  {$IFDEF HasAudioIns}
  // IMPORTANT: NOTE: TODO: The INput output controller doesn't need to be resized when using the r8brain based controllers.
  // They do need to be resized when using the DSP master controllers.
  //if VstOutputsController.BlockSize <> SampleFrames then VstOutputsController.BlockSize := SampleFrames;
  //if VstInputsController.BlockSize  <> SampleFrames then VstInputsController.BlockSize  := SampleFrames;
  VstInputsController.ProcessVstInputs(Inputs, SampleFrames);
  {$ENDIF}




  {$IFDEF HasAudioOuts}
  // IMPORTANT: NOTE: TODO: The INput output controller doesn't need to be resized when using the r8brain based controllers.
  // They do need to be resized when using the DSP master controllers.
  //if VstOutputsController.BlockSize <> SampleFrames then VstOutputsController.BlockSize := SampleFrames;
  VstOutputsController.PreProcessVstOutputs(Outputs, SampleFrames);
  {$ENDIF}


  //-----------------------------------------------------
  //reset the midi output data.
  //-----------------------------------------------------
  MidiOutput.ClearEvents;

  //-----------------------------------------------------
  //  Time info
  //-----------------------------------------------------
  Plugin.Globals.TimeInfo := GetTimeInfo(32767);
  TimeInfo := GetTimeInfo(32767);
  UpdatePluginTimeInfo(TimeInfo);
  //-----------------------------------------------------
  if MidiInput.EventCount > 0 then MidiInput.FilterEvents(ModSampleFrames);



  //-----------------------------------------------------
  //   Process the audio input.
  //-----------------------------------------------------
  //CpuMonitor.StartAudioProcessTimer2;
  ProcessAudioBlock(ModSampleFrames);
  //CpuMonitor.StopAudioProcessTimer2;





  //Ensure midi input data is deleted. (ProcessEvents isn't called each sample block in all VST hosts)
  if MidiInput.EventCount > 0 then MidiInput.ClearEvents;
  //-----------------------------------------------------
  //-----------------------------------------------------


  {$IFDEF OverSampleEnabled}
  VstOutputsController.PostProcessVstOutputs(Outputs, SampleFrames);
  {$ENDIF}

  {$IFDEF ForceRawMidiOutput}
    //== force RAW midi output ================
    if MidiOutput.EventCount > 0 then DoMidiOutput;
  {$ELSE}
    //== Filter midi output to ensure correctness =========
    //TODO: DoBufferMidiOutput(SampleFrames);
    if MidiOutput.EventCount > 0 then
    begin
      MidiOutput.SortEventsByDeltaFrames;
      //TODO: DoMidiOutput;
    end;
  {$ENDIF}


  {$IFDEF CpuMonitor}

    CpuMonitor.StopProcessingReplacingTimer;
    Plugin.Globals.CpuUsage^.ProcessReplacingTime := CpuMonitor.ProcessReplacingTime;
    Plugin.Globals.CpuUsage^.ProcessReplacingLoad := CpuMonitor.ProcessReplacingLoad;
  {$ENDIF}


  //OutputDebugString('SAMPLING OFF');
end;




procedure TProcessController.ProcessReplacing2(Inputs, Outputs: PPSingle; Sampleframes: Integer);
begin
end;


procedure TProcessController.Resume(const aBlockSize, aSampleRate, aOverSampleFactor, aInputCount, aOutputCount : integer);
begin
  fSampleRate := aSampleRate;

  fFastControlRateDivision := Plugin.Settings.FastControlRateDivision;
  fSlowControlRateDivision := Plugin.Settings.SlowControlRateDivision;

  SlowControlRateStepMax   := fSlowControlRateDivision div fFastControlRateDivision;
  SlowControlRateStepCount := 0;

  if (fSlowControlRateDivision mod fFastControlRateDivision <> 0)
    then raise Exception.Create('Slow and Fast control rates are evenly divisable.');


  {$IFDEF HasAudioIns}
  VstInputsController.BlockSize := aBlockSize;
  VstInputsController.InputChannelCount := aInputCount;
  VstInputsController.OverSampleFactor  := aOverSampleFactor;
  {$ENDIF}

  {$IFDEF HasAudioOuts}
  VstOutputsController.BlockSize := aBlockSize;
  VstOutputsController.OutputChannelCount := aOutputCount;
  VstOutputsController.OverSampleFactor := aOverSampleFactor;
  {$ENDIF}

  ControlRateOffset := 0;
end;

procedure TProcessController.Suspend;
begin

end;

procedure TProcessController.UpdatePluginTimeInfo(TimeInfo: PVstTimeInfo);
var
  Playing, CycleActive, Recording:boolean;
begin
  if (TimeInfo^.flags and kVstTempoValid) >=1 then
  begin
    if TimeInfo^.Tempo <> Plugin.Globals.Tempo then Plugin.Globals.Tempo := TimeInfo^.Tempo;
  end;

  if (TimeInfo^.Flags and kVstPpqPosValid) >= 1 then Plugin.Globals.ppqPos             := TimeInfo^.ppqPos;
  if (TimeInfo^.Flags and kVstBarsValid)   >= 1 then Plugin.Globals.BarStartPos        := TimeInfo^.barStartPos;

  {$IFDEF OverSampleEnabled}
    if (TimeInfo^.Flags and kVstClockValid)  >= 1 then Plugin.Globals.SamplesToNextClock := TimeInfo^.samplesToNextClock * Plugin.Settings.OverSampleFactor;
  {$ELSE}
    if (TimeInfo^.Flags and kVstClockValid)  >= 1 then Plugin.Globals.SamplesToNextClock := TimeInfo^.samplesToNextClock;
  {$ENDIF}

  if (TimeInfo^.Flags and kVstTransportChanged) >= 1 then
  begin
    // TODO: This is depreciated and should be removed.
    if (TimeInfo^.Flags and kVstTransportPlaying) >= 1
      then Plugin.HostPlayState := psHostIsPlaying
      else Plugin.HostPlayState := psHostIsStopped;
    //================================================

    if (TimeInfo^.Flags and kVstTransportPlaying) >= 1
      then Playing := true
      else Playing := false;

    if (TimeInfo^.Flags and kVstTransportCycleActive) >= 1
      then CycleActive := true
      else CycleActive := false;

    if (TimeInfo^.Flags and kVstTransportRecording) >= 1
      then Recording := true
      else Recording := false;

    Plugin.Globals.UpdateTransportState(Playing, CycleActive, Recording);
  end;

  Plugin.Globals.TimeSigNumerator   := TimeInfo^.timeSigNumerator;
  Plugin.Globals.TimeSigDenominator := TimeInfo^.timeSigDenominator;

  Plugin.Globals.UpdateSyncInfo;
end;

procedure TProcessController.ProcessAudioBlock(SampleFrames: integer);
const
  ksf = 32;
  ksr = 44100;
var
  ev       : TeeMidiEvent;
  NumEv, CurEv:integer;

  NextMidiEventDelta    : integer;
  NextControlRateDelta  : integer;
  NextBufferEndDelta    : integer;
  SamplesProcessed      : integer;
  SamplesToProcess      :integer;
begin
  NumEv := MidiInput.EventCount;
  CurEv := 0;
  SamplesProcessed := 0;

  while SampleFrames > 0 do
  begin
    ProcessReplacingOffset := SamplesProcessed;

    //Get the next delta values and calucate how many samples to process.
    NextBufferEndDelta   := SampleFrames;
    NextControlRateDelta := fFastControlRateDivision - ControlRateOffset;

    if NextBufferEndDelta < NextControlRateDelta
      then SamplesToProcess := NextBufferEndDelta
      else SamplesToProcess := NextControlRateDelta;

    if CurEv < NumEv then
    begin
      NextMidiEventDelta := MidiInput.Events[CurEv].Deltaframes - SamplesProcessed;
      if NextMidiEventDelta < SamplesToProcess
        then SamplesToProcess := NextMidiEventDelta;
    end else
    begin
      NextMidiEventDelta := SampleFrames+1;
    end;

    if SamplesToProcess > 0 then
    begin
      //Process those samples..
      //OverloadWatch.Start(ksf, ksr, 'Plugin-AudioProcess');
      Plugin.AudioProcess(SamplesToProcess);
      //OverloadWatch.Stop;
      inc(SamplesProcessed, SamplesToProcess);
      dec(SampleFrames, SamplesToProcess);
      inc(ControlRateOffset, SamplesToProcess);
    end else
    begin
      //OverloadWatch.Start(ksf, ksr, 'Plugin-ProcessMidiEvent');
      // or process whatever events...
      if NextMidiEventDelta = 0 then
      begin
        ev := MidiInput[CurEv];
        Plugin.ProcessMidiEvent(ev);
        Inc(CurEv);
      end;
      //OverloadWatch.Stop;

      //OverloadWatch.Start(ksf, ksr, 'Plugin-ControlRateProcessing');
      if NextControlRateDelta = 0 then
      begin
        //Important: Always call FastControlProcess before SlowControlProcess
        Plugin.FastControlProcess;
        ControlRateOffset := 0;

        inc(self.SlowControlRateStepCount);
        if SlowControlRateStepCount >= SlowControlRateStepMax then
        begin
          SlowControlRateStepCount := 0;
          Plugin.SlowControlProcess;
        end;
      end;
      //OverloadWatch.Stop;
    end;
  end;

  //OverloadWatch.Start(ksf, ksr, 'Plugin-ProcessEnd');
  Plugin.ProcessEnd;
  //OverloadWatch.Stop;



end;



end.
