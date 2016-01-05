unit Test.AudioPlugin.ProcessController;

interface

uses
  WatchTower,
  AudioPlugin.ProcessController,
  PlugHost.MultiChannelAudioBuffer,
  AudioPlugin.RunTimeInfo,
  Mocks.AudioPlugin,
  VamVst2.VstEventBuffer;

type
  TProcessControllerTest = class(TWatchTowerTest)
  private
    Plug : TMockAudioPlugin;
    ProcessController : TProcessController;
    OutputBuffer : TMultiChannelAudioBuffer32;
    EventBuffer : TVstEventBuffer;
  public
    procedure Setup; override;
    procedure TearDown; override;

    // TODO:LOW There tests aren't working.

    [_Test]
    procedure BasicProcessingTest;

    [_Test]
    procedure ProcessWithEvents_Test1;

    [_Test]
    procedure ProcessWithEvents_Test2;

    [_Test]
    procedure ProcessWithEvents_Test3;

    [_Test]
    procedure ProcessWithControlRateSteps;

    [_Test]
    procedure ProcessWithControlRateStepsAndEvents_Test1;

    [_Test]
    procedure ProcessWithControlRateStepsAndEvents_Test2;

  end;

implementation

uses
  WatchTower.Confirm,
  VamVst2.MidiEvent;

{ TProcessControllerTest }

procedure TProcessControllerTest.Setup;
begin
  inherited;
  Plug := TMockAudioPlugin.Create(nil);
  Plug.Open;

  ProcessController := TProcessController.Create(Plug);
  ProcessController.Suspend;

  OutputBuffer := TMultiChannelAudioBuffer32.Create;

  EventBuffer := TVstEventBuffer.Create(32);
end;

procedure TProcessControllerTest.TearDown;
begin
  Plug.Close;
  Plug.Free;

  ProcessController.Free;
  OutputBuffer.Free;
  EventBuffer.Free;
  inherited;
end;

procedure TProcessControllerTest.BasicProcessingTest;
var
  rtInfo : TRunTimeInfo;
begin
  OutputBuffer.SetSize(2, 512);

  rtInfo.InputCount := 0;
  rtInfo.OutputCount := 2;
  rtInfo.SampleRate := 44100;
  rtInfo.MaxSampleFrames := 512;
  rtInfo.FastControlBufferSize := 1000;
  rtInfo.SlowControlBufferSize := 4000;

  ProcessController.Resume(rtInfo);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 100);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 20);

  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 2 );
  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 100 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 20 );


  ProcessController.Suspend;
  ProcessController.Resume(rtInfo);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 50);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 70);

  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 2 );
  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 50 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 70 );
end;

procedure TProcessControllerTest.ProcessWithEvents_Test1;
var
  rtInfo : TRunTimeInfo;
begin
  EventBuffer.AddMidiEvent(kMidiEventStatus.NoteOn, 0, 64, 100, 0);

  OutputBuffer.SetSize(2, 512);

  rtInfo.InputCount := 0;
  rtInfo.OutputCount := 2;
  rtInfo.SampleRate := 44100;
  rtInfo.MaxSampleFrames := 512;
  rtInfo.FastControlBufferSize := 1000;
  rtInfo.SlowControlBufferSize := 4000;

  ProcessController.Resume(rtInfo);

  ProcessController.ProcessVstEvents(EventBuffer.Buffer);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 100);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 20);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 30);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 60);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 90);

  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 5 );
  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 100 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 20 );
  Confirm.IsTrue( Plug.AudioProcessHistory[2].AsInteger = 30 );
  Confirm.IsTrue( Plug.AudioProcessHistory[3].AsInteger = 60 );
  Confirm.IsTrue( Plug.AudioProcessHistory[4].AsInteger = 90 );
end;

procedure TProcessControllerTest.ProcessWithEvents_Test2;
var
  rtInfo : TRunTimeInfo;
  EventDelta : integer;
begin
  EventDelta := 10;
  EventBuffer.AddMidiEvent(kMidiEventStatus.NoteOn, 0, 64, 100, 10);

  OutputBuffer.SetSize(2, 512);

  rtInfo.InputCount := 0;
  rtInfo.OutputCount := 2;
  rtInfo.SampleRate := 44100;
  rtInfo.MaxSampleFrames := 512;
  rtInfo.FastControlBufferSize := 1000;
  rtInfo.SlowControlBufferSize := 4000;

  ProcessController.Resume(rtInfo);

  ProcessController.ProcessVstEvents(EventBuffer.Buffer);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 100);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 20);

  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 3 );
  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 10 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 90 );
  Confirm.IsTrue( Plug.AudioProcessHistory[2].AsInteger = 20 );
end;

procedure TProcessControllerTest.ProcessWithEvents_Test3;
var
  rtInfo : TRunTimeInfo;
  EventDelta : integer;
begin
  EventDelta := 10;
  EventBuffer.AddMidiEvent(kMidiEventStatus.NoteOn, 0, 64, 100, 10);
  EventBuffer.AddMidiEvent(kMidiEventStatus.NoteOn, 0, 64, 100, 20);
  EventBuffer.AddMidiEvent(kMidiEventStatus.NoteOn, 0, 64, 100, 30);

  OutputBuffer.SetSize(2, 512);

  rtInfo.InputCount := 0;
  rtInfo.OutputCount := 2;
  rtInfo.SampleRate := 44100;
  rtInfo.MaxSampleFrames := 512;
  rtInfo.FastControlBufferSize := 1000;
  rtInfo.SlowControlBufferSize := 4000;

  ProcessController.Resume(rtInfo);

  ProcessController.ProcessVstEvents(EventBuffer.Buffer);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 100);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 20);

  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 5 );
  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 10 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 10 );
  Confirm.IsTrue( Plug.AudioProcessHistory[2].AsInteger = 10 );
  Confirm.IsTrue( Plug.AudioProcessHistory[3].AsInteger = 70 );
  Confirm.IsTrue( Plug.AudioProcessHistory[4].AsInteger = 20 );
end;

procedure TProcessControllerTest.ProcessWithControlRateSteps;
var
  rtInfo : TRunTimeInfo;
begin
  OutputBuffer.SetSize(2, 1000);

  rtInfo.InputCount := 0;
  rtInfo.OutputCount := 2;
  rtInfo.SampleRate := 44100;
  rtInfo.MaxSampleFrames := 1000;
  rtInfo.FastControlBufferSize := 25;
  rtInfo.SlowControlBufferSize := 100;

  ProcessController.Resume(rtInfo);

  ProcessController.ProcessVstEvents(EventBuffer.Buffer);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 200);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 20);

  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 9 );

  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[2].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[3].AsInteger = 25 );

  Confirm.IsTrue( Plug.AudioProcessHistory[4].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[5].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[6].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[7].AsInteger = 25 );

  Confirm.IsTrue( Plug.AudioProcessHistory[8].AsInteger = 20 );
end;



procedure TProcessControllerTest.ProcessWithControlRateStepsAndEvents_Test1;
var
  rtInfo : TRunTimeInfo;
begin
  OutputBuffer.SetSize(2, 1000);

  EventBuffer.AddMidiEvent(kMidiEventStatus.NoteOn, 0, 64, 100, 10);

  rtInfo.InputCount := 0;
  rtInfo.OutputCount := 2;
  rtInfo.SampleRate := 44100;
  rtInfo.MaxSampleFrames := 1000;
  rtInfo.FastControlBufferSize := 25;
  rtInfo.SlowControlBufferSize := 100;

  ProcessController.Resume(rtInfo);

  ProcessController.ProcessVstEvents(EventBuffer.Buffer);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 200);


  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 9 );

  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 10 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 15 );
  Confirm.IsTrue( Plug.AudioProcessHistory[2].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[3].AsInteger = 25 );

  Confirm.IsTrue( Plug.AudioProcessHistory[4].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[5].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[6].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[7].AsInteger = 25 );

  Confirm.IsTrue( Plug.AudioProcessHistory[8].AsInteger = 25 );
end;

procedure TProcessControllerTest.ProcessWithControlRateStepsAndEvents_Test2;
var
  rtInfo : TRunTimeInfo;
begin
  OutputBuffer.SetSize(2, 1000);

  EventBuffer.AddMidiEvent(kMidiEventStatus.NoteOn, 0, 64, 100, 25);

  rtInfo.InputCount := 0;
  rtInfo.OutputCount := 2;
  rtInfo.SampleRate := 44100;
  rtInfo.MaxSampleFrames := 1000;
  rtInfo.FastControlBufferSize := 25;
  rtInfo.SlowControlBufferSize := 100;

  ProcessController.Resume(rtInfo);

  ProcessController.ProcessVstEvents(EventBuffer.Buffer);
  ProcessController.ProcessAudio(nil, OutputBuffer.PPBuffer, 200);


  Confirm.IsTrue( Plug.AudioProcessHistory.Count = 8 );

  Confirm.IsTrue( Plug.AudioProcessHistory[0].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[1].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[2].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[3].AsInteger = 25 );

  Confirm.IsTrue( Plug.AudioProcessHistory[4].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[5].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[6].AsInteger = 25 );
  Confirm.IsTrue( Plug.AudioProcessHistory[7].AsInteger = 25 );
end;

end.
