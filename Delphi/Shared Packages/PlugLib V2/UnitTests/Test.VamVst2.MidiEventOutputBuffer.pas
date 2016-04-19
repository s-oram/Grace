unit Test.VamVst2.MidiEventOutputBuffer;

interface

uses
  WatchTower, VamVst2.MidiEventOutputBuffer;

type
  TMidiEventOutputBufferTest = class(TWatchTowerTest)
  private
    Buffer1, Buffer2 : TMidiEventOutputBuffer;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure PrepareEmptyBuffer;

    [Test]
    procedure AddAnEventB;

    [Test]
    procedure AddAnEventD;

    [Test]
    procedure EventClipping;

    [Test]
    procedure ResetA;

    [Test]
    procedure ResetB;

    [Test]
    procedure OutOfOrderEvents;

    //[Test]
    //procedure AssignFromSourceBuffer;
  end;

implementation

uses
  WatchTower.Confirm,
  VamVst2.DAEffectX,
  VamVst2.VstEvent,
  VamVst2.MidiEvent;


{ TMidiEventOutputBufferTest }

procedure TMidiEventOutputBufferTest.Setup;
begin
  inherited;
  Buffer1 := TMidiEventOutputBuffer.Create(12);
  Buffer2 := TMidiEventOutputBuffer.Create(12);
end;

procedure TMidiEventOutputBufferTest.TearDown;
begin
  inherited;
  if assigned(Buffer1) then Buffer1.Free;
  if assigned(Buffer2) then Buffer2.Free;
end;

procedure TMidiEventOutputBufferTest.PrepareEmptyBuffer;
var
  evs : PVstEvents;
begin
  evs := Buffer1.PrepareOutputBuffer(512);
  confirm.IsTrue(  evs^.numEvents = 0  );
end;

procedure TMidiEventOutputBufferTest.AddAnEventB;
var
  evs : PVstEvents;
  mev : PVstMidiEvent;
begin
  Buffer1.IncrementGlobalDelta(42);

  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32);

  evs := Buffer1.PrepareOutputBuffer(512);
  confirm.IsTrue(  evs^.numEvents = 1  );

  mev := PVstMidiEvent(evs^.events[0]);
  confirm.IsTrue(  mev.vType = kVstMidiType );

  confirm.IsTrue(  mev.deltaFrames = 42  );
  confirm.IsTrue(  mev.midiData[1] = 64   );
  confirm.IsTrue(  mev.midiData[2] = 32   );
end;

procedure TMidiEventOutputBufferTest.AddAnEventD;
var
  evs : PVstEvents;
  mev : PVstMidiEvent;
begin
  // Confirm events can be added using a global delta.
  Buffer1.ResetGlobalDelta;

  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32); // Note: Deltaframe value not set.

  Buffer1.IncrementGlobalDelta(252);

  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32); // Note: Deltaframe value not set.

  evs := Buffer1.PrepareOutputBuffer(512);
  confirm.IsTrue(  evs^.numEvents = 2  );

  mev := PVstMidiEvent(evs^.events[0]);
  confirm.IsTrue(  mev.deltaFrames = 0  );

  mev := PVstMidiEvent(evs^.events[1]);
  confirm.IsTrue(  mev.deltaFrames = 252  );
end;

procedure TMidiEventOutputBufferTest.EventClipping;
var
  evs : PVstEvents;
begin
  Buffer1.ResetGlobalDelta;
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 100);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 200);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 300);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 400);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 500);

  evs := Buffer1.PrepareOutputBuffer(250);
  confirm.IsTrue(  evs^.numEvents = 2  );

  Buffer1.RemoveStaleEvents(250);

  evs := Buffer1.PrepareOutputBuffer(260);
  confirm.IsTrue(  evs^.numEvents = 3  );
end;

procedure TMidiEventOutputBufferTest.ResetA;
var
  evs : PVstEvents;
  mev : PVstMidiEvent;
begin
  // Does calling reset break adding new events?
  Buffer1.Reset;

  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 105);

  evs := Buffer1.PrepareOutputBuffer(512);
  confirm.IsTrue(  evs^.numEvents = 1  );

  mev := PVstMidiEvent(evs^.events[0]);
  confirm.IsTrue(  mev.vType = kVstMidiType );

  confirm.IsTrue(  mev.deltaFrames = 105  );
  confirm.IsTrue(  mev.midiData[1] = 64   );
  confirm.IsTrue(  mev.midiData[2] = 32   );
end;



procedure TMidiEventOutputBufferTest.ResetB;
var
  evs : PVstEvents;
begin
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 100);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 200);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 300);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 400);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 500);

  evs := Buffer1.PrepareOutputBuffer(250);
  confirm.IsTrue(  evs^.numEvents = 2  );

  Buffer1.RemoveStaleEvents(250);

  Buffer1.Reset; // <-- reset called here!

  evs := Buffer1.PrepareOutputBuffer(260);
  confirm.IsTrue(  evs^.numEvents = 0  );
end;

procedure TMidiEventOutputBufferTest.OutOfOrderEvents;
var
  evs : PVstEvents;
  mev : ^PVstMidiEvent;
begin
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 200);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 500);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 300);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 100);
  Buffer1.AddMidiEvent(kMidiEventStatus.NoteOn, 2, 64, 32, 400);

  evs := Buffer1.PrepareOutputBuffer(650);
  confirm.IsTrue(  evs^.numEvents = 5  );

  confirm.IsTrue(  VerifyEventSort(evs)  );

  mev := @evs^.events[0];
  confirm.IsTrue(  mev^^.vType = kVstMidiType );
  confirm.IsTrue(  mev^^.deltaFrames = 100  );

  inc(mev);
  confirm.IsTrue(  mev^^.vType = kVstMidiType );
  confirm.IsTrue(  mev^^.deltaFrames = 200  );

  inc(mev);
  confirm.IsTrue(  mev^^.vType = kVstMidiType );
  confirm.IsTrue(  mev^^.deltaFrames = 300  );

  inc(mev);
  confirm.IsTrue(  mev^^.vType = kVstMidiType );
  confirm.IsTrue(  mev^^.deltaFrames = 400  );

  inc(mev);
  confirm.IsTrue(  mev^^.vType = kVstMidiType );
  confirm.IsTrue(  mev^^.deltaFrames = 500  );


end;




end.
