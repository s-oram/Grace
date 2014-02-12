unit LucidityGui.Scope.SignalRecorder;

interface

uses
  VamLib.MoreTypes;

type
  IScopeSignalRecorder = interface
    ['{A01F1BD5-F4DD-48A2-A466-5AD7C9195647}']
    procedure GetReadPointer(const MaxReadSampleFrames : integer; out ActualReadSampleFrames, ReadIndex, BufferSize : integer; out Buffer : PArrayOfSingle);
  end;

implementation

end.
