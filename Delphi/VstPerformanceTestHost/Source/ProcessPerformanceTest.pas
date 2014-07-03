unit ProcessPerformanceTest;

interface

uses
  phCore,
  uVstWrapper, uBuffers, DAEffectX,
  uVstMidiInput;

type
  TProcessPerformanceTest = class
  private
  protected
    TestInfo   : TTestInfo;

    MidiInput  : TVstMidiInputBuffer;
    VstWrapper : TVstWrapper;
    TimeInfo : VstTimeInfo;
    InputBuffers, OutputBuffers : TBuffers;

  protected
    procedure ProcessPlugin(TimeInSeconds : integer);
    procedure SendMidiNoteOnEvents;
    procedure SendMidiNoteOffEvents;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunTest;
  end;

implementation

uses
  CpuCycleTimer,
  SysUtils;

{ TProcessPerformanceTest }

constructor TProcessPerformanceTest.Create;
begin
  VstWrapper := TVstWrapper.Create;
  InputBuffers  := TBuffers.Create;
  OutputBuffers := TBuffers.Create;

  MidiInput  := TVstMidiInputBuffer.Create;


  TestInfo := LoadTestInfo;

end;

destructor TProcessPerformanceTest.Destroy;
begin
  if VstWrapper.IsPluginLoaded then VstWrapper.Unload;
  VstWrapper.Free;
  InputBuffers.Free;
  OutputBuffers.Free;
  MidiInput.Free;
  inherited;
end;

procedure TProcessPerformanceTest.ProcessPlugin(TimeInSeconds: integer);
var
  c1 : integer;
  BuffersPerSec : integer;
begin
  BuffersPerSec := TestInfo.SampleRate div TestInfo.BufferSize;

  for c1 := 0 to (BuffersPerSec * TimeInSeconds)-1 do
  begin
    InputBuffers.ZeroBuffers;
    OutputBuffers.ZeroBuffers;
    VstWrapper.ProcessReplacing(InputBuffers.VstBufferPointer, OutputBuffers.VstBufferPointer, TestInfo.BufferSize);
  end;

end;

procedure TProcessPerformanceTest.RunTest;
var
  TotalTime : single;
  StartTime : int64;
  c1: Integer;
  BuffersPerSec : integer;
begin
  BuffersPerSec := TestInfo.SampleRate div TestInfo.BufferSize;


  WriteLn('== Process Performance Test ==');
  WriteLn('Plugin: ' + ExtractFileName(TestInfo.VstPluginFile));
  WriteLn('Path: ' + TestInfo.VstPluginFile);

  VstWrapper.LoadVst(TestInfo.VstPluginFile);

  if VstWrapper.IsPluginLoaded
    then WriteLn('Plugin Load: pass')
    else WriteLn('Plugin Load: FAIL');

  if VstWrapper.IsPluginLoaded then
  begin
    WriteLn('Vendor: ' + VstWrapper.GetPluginVendor);
    WriteLn('Effect: ' + VstWrapper.GetPluginName);
    WriteLn('Version: ' + IntToStr(VstWrapper.GetPluginVersion));

    WriteLn('Output Count: ' + IntToStr(VstWrapper.OutputCount));

    WriteLn('');

    InputBuffers.Count := VstWrapper.InputCount;
    Inputbuffers.BufferLength := TestInfo.BufferSize;

    OutputBuffers.Count := VstWrapper.OutputCount;
    OutputBuffers.BufferLength := TestInfo.BufferSize;

    VstWrapper.SampleRate := TestInfo.SampleRate;
    VstWrapper.BlockSize  := TestInfo.BufferSize;
    VstWrapper.TimeInfo := @TimeInfo;
    VstWrapper.IsTimeInfoValid := true;



    VstWrapper.TurnOn;

    TimeInfo.sampleRate := TestInfo.SampleRate;
    TimeInfo.tempo      := 120;
    TimeInfo.samplePos  := 0;


    WriteLn('Processing...');


    ProcessPlugin(10);
    SendMidiNoteOnEvents;
    ProcessPlugin(10);
    SendMidiNoteOffEvents;
    ProcessPlugin(30);


    TotalTime := 0;
    StartTime := CpuCycleTimer.StartTimerMS;

    ProcessPlugin(10);
    SendMidiNoteOnEvents;
    ProcessPlugin(10);
    SendMidiNoteOffEvents;
    ProcessPlugin(30);

    TotalTime := StopTimerMS(STartTime);
    WriteLn(FloatToStr(TotalTime));








    {
    //== Process for 10 seconds ==
    for c1 := 0 to (BuffersPerSec * 10)-1 do
    begin
      InputBuffers.ZeroBuffers;
      OutputBuffers.ZeroBuffers;
      VstWrapper.ProcessReplacing(InputBuffers.VstBufferPointer, OutputBuffers.VstBufferPointer, TestInfo.BufferSize);
    end;


    //== send midi note on ==
    MidiInput.Clear;
    MidiInput.AddMidiEvent(0,kNoteOn, 60,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 59,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 58,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 57,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 56,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 55,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 54,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 53,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 52,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 51,60,0);
    VstWrapper.SendVstEventsToPlugin(MidiInput.VstEventsPointer);

    TotalTime := 0;
    StartTime := CpuCycleTimer.StartTimerMS;

    //== Process for 5 seconds ==
    for c1 := 0 to (BuffersPerSec * 2)-1 do
    begin
      InputBuffers.ZeroBuffers;
      OutputBuffers.ZeroBuffers;
      VstWrapper.ProcessReplacing(InputBuffers.VstBufferPointer, OutputBuffers.VstBufferPointer, BufferSize);
    end;

    TotalTime := StopTimerMS(STartTime);

    WriteLn(FloatToStr(TotalTime));

    //== send midi note off ==
    MidiInput.Clear;
    MidiInput.AddMidiEvent(0,kNoteOff, 60,0,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 59,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 58,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 57,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 56,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 55,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 54,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 53,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 52,60,0);
    MidiInput.AddMidiEvent(0,kNoteOff, 51,60,0);
    VstWrapper.SendVstEventsToPlugin(MidiInput.VstEventsPointer);


    //== Process for 5 seconds ==
    for c1 := 0 to (BuffersPerSec * 30)-1 do
    begin
      InputBuffers.ZeroBuffers;
      OutputBuffers.ZeroBuffers;
      VstWrapper.ProcessReplacing(InputBuffers.VstBufferPointer, OutputBuffers.VstBufferPointer, BufferSize);
    end;





    //== send midi note on ==
    MidiInput.Clear;
    MidiInput.AddMidiEvent(0,kNoteOn, 60,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 59,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 58,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 57,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 56,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 55,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 54,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 53,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 52,60,0);
    MidiInput.AddMidiEvent(0,kNoteOn, 51,60,0);
    VstWrapper.SendVstEventsToPlugin(MidiInput.VstEventsPointer);

    TotalTime := 0;
    StartTime := CpuCycleTimer.StartTimerMS;

    //== Process for 5 seconds ==
    for c1 := 0 to (BuffersPerSec * 2)-1 do
    begin
      InputBuffers.ZeroBuffers;
      OutputBuffers.ZeroBuffers;
      VstWrapper.ProcessReplacing(InputBuffers.VstBufferPointer, OutputBuffers.VstBufferPointer, BufferSize);
    end;

    TotalTime := StopTimerMS(STartTime);



    }
    //WriteLn(FloatToStr(TotalTime));


    WriteLn('Finished');


    VstWrapper.TurnOff;
  end;







   if VstWrapper.IsPluginLoaded then
   begin
     VstWrapper.Unload;
   end;


end;

procedure TProcessPerformanceTest.SendMidiNoteOnEvents;
begin
  //== send midi note on ==
  MidiInput.Clear;
  MidiInput.AddMidiEvent(0,kNoteOn, 60,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 59,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 58,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 57,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 56,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 55,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 54,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 53,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 52,60,0);
  MidiInput.AddMidiEvent(0,kNoteOn, 51,60,0);
  VstWrapper.SendVstEventsToPlugin(MidiInput.VstEventsPointer);
end;

procedure TProcessPerformanceTest.SendMidiNoteOffEvents;
begin
  //== send midi note off ==
  MidiInput.Clear;
  MidiInput.AddMidiEvent(0,kNoteOff, 60,0,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 59,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 58,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 57,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 56,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 55,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 54,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 53,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 52,60,0);
  MidiInput.AddMidiEvent(0,kNoteOff, 51,60,0);
  VstWrapper.SendVstEventsToPlugin(MidiInput.VstEventsPointer);
end;



end.
