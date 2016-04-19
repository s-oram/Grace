unit Test.AudioPlugin.ProcessController;

interface

uses
  WatchTower,
  PlugHost.MultiChannelAudioBuffer,
  AudioPlugin.RunTimeInfo,
  Mocks.AudioPlugin,
  VamVst2.VstEventBuffer;

type
  TProcessControllerTest = class(TWatchTowerTest)
  private
    ProcessController : TProcessController;
    OutputBuffer : TMultiChannelAudioBuffer32;
    EventBuffer : TVstEventBuffer;
  public





  end;

implementation

uses
  WatchTower.Confirm,
  VamVst2.MidiEvent;

end.
