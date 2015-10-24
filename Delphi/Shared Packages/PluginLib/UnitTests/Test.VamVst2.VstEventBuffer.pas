unit Test.VamVst2.VstEventBuffer;

interface

uses
  WatchTower, VamVst2.VstEventBuffer;

type
  TVstEventBufferTest = class(TWatchTowerTest)
  private
    ev1, ev2 : TVstEventBuffer;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure WriteEventsToBuffer;

    [Test]
    procedure AssignFromSourceBuffer;
  end;

implementation

uses
  WatchTower.Confirm,
  VamVst2.DAEffectX,
  VamVst2.VstEvent,
  VamVst2.MidiEvent;

{ TVstEventBufferTest }

procedure TVstEventBufferTest.Setup;
begin
  inherited;

  ev1 := TVstEventBuffer.Create(32);
  ev2 := TVstEventBuffer.Create(32);

end;

procedure TVstEventBufferTest.TearDown;
begin
  inherited;

  ev1.Free;
  ev2.Free;
end;

procedure TVstEventBufferTest.WriteEventsToBuffer;
var
  ev : VstEvent;
  OutStatus, OutChannel, OutData1, OutData2, OutDelta : integer;
begin
  WriteMidiEventToVstEvent(@ev, kMidiEventStatus.NoteOn, 0, 64, 100, 0);
  ev1.AddEvent(@ev);

  WriteMidiEventToVstEvent(@ev, kMidiEventStatus.NoteOn, 1, 63, 75, 10);
  ev1.AddEvent(@ev);

  Confirm.IsTrue( ev1.Count = 2 );

  ReadMidiEventFromVstEvent(ev1[0], OutStatus, OutChannel, OutData1, OutData2, OutDelta);

  Confirm.IsTrue( OutStatus = kMidiEventStatus.NoteOn );
  Confirm.IsTrue( OutChannel = 0);
  Confirm.IsTrue( OutData1   = 64);
  Confirm.IsTrue( OutData2   = 100 );
  Confirm.IsTrue( OutDelta   = 0 );


  ReadMidiEventFromVstEvent(ev1[1], OutStatus, OutChannel, OutData1, OutData2, OutDelta);

  Confirm.IsTrue( OutStatus = kMidiEventStatus.NoteOn );
  Confirm.IsTrue( OutChannel = 1);
  Confirm.IsTrue( OutData1   = 63);
  Confirm.IsTrue( OutData2   = 75 );
  Confirm.IsTrue( OutDelta   = 10 );
end;

procedure TVstEventBufferTest.AssignFromSourceBuffer;
var
  ev : VstEvent;
  OutStatus, OutChannel, OutData1, OutData2, OutDelta : integer;
begin
  WriteMidiEventToVstEvent(@ev, kMidiEventStatus.NoteOn, 0, 64, 100, 0);
  ev1.AddEvent(@ev);

  WriteMidiEventToVstEvent(@ev, kMidiEventStatus.NoteOn, 1, 63, 75, 10);
  ev1.AddEvent(@ev);

  // copy events to second buffer.
  ev2.AssignFrom(ev1.Buffer);

  // overwrite events in the first buffer to ensure copy is actual copy.
  ev1.Clear;

  WriteMidiEventToVstEvent(@ev, kMidiEventStatus.NoteOn, 6, 66, 66, 666);
  ev1.AddEvent(@ev);

  WriteMidiEventToVstEvent(@ev, kMidiEventStatus.NoteOn, 6, 66, 66, 666);
  ev1.AddEvent(@ev);



  Confirm.IsTrue( ev2.Count = 2 );

  ReadMidiEventFromVstEvent(ev2[0], OutStatus, OutChannel, OutData1, OutData2, OutDelta);

  Confirm.IsTrue( OutStatus = kMidiEventStatus.NoteOn );
  Confirm.IsTrue( OutChannel = 0);
  Confirm.IsTrue( OutData1   = 64);
  Confirm.IsTrue( OutData2   = 100 );
  Confirm.IsTrue( OutDelta   = 0 );


  ReadMidiEventFromVstEvent(ev2[1], OutStatus, OutChannel, OutData1, OutData2, OutDelta);

  Confirm.IsTrue( OutStatus = kMidiEventStatus.NoteOn );
  Confirm.IsTrue( OutChannel = 1);
  Confirm.IsTrue( OutData1   = 63);
  Confirm.IsTrue( OutData2   = 75 );
  Confirm.IsTrue( OutDelta   = 10 );


end;



end.
