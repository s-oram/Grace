unit Test.VamVst2.MidiEventOutputBuffer;

interface

uses
  VamVst2.MidiEventOutputBuffer;

function TestA:boolean;

implementation

uses
  VamVst2.DAEffectX,
  VamVst2.MidiEvent,
  VamVst2.MidiEventInputBuffer;

function TestA:boolean;
const
  MidiEventCount = 16;
  kDeltaOffset = 42;
var
  TestResult : boolean;
  InputBufferA, InputBufferB : TMidiEventInputBuffer;
  OutputBufferA : TMidiEventOutputBuffer;
  MidiData : TArrayOfMidiEvent;
  c1: Integer;
  ev : TMidiEvent;
  OutputEvents : PVstEvents;
begin
  TestResult := true;

  InputBufferA := TMidiEventInputBuffer.Create(4);
  InputBufferB := TMidiEventInputBuffer.Create(4);
  OutputBufferA := TMidiEventOutputBuffer.Create(4);

  SetLength(MidiData, MidiEventCount);

  for c1 := 0 to MidiEventCount-1 do
  begin
    MidiData[c1].Status := kMidiEventStatus.NoteOn;
    MidiData[c1].Channel := 0;
    MidiData[c1].Data1 := 32 + c1;
    MidiData[c1].Data2 := 64;
    MidiData[c1].Deltaframes := 50 * c1;
  end;

  InputBufferA.AssignFrom(MidiData);


  OutputBufferA.AddEvents(InputBufferA.Buffer, kDeltaOffset);

  OutputEvents := OutputBufferA.PrepareOutputBuffer(50 * MidiEventCount);

  InputBufferB.AssignFrom(OutputEvents);

  for c1 := 0 to MidiEventCount-1 do
  begin
    ev := InputBufferB.ReadMidiEvent(c1);
    if ev.Status      <> MidiData[c1].Status      then TestResult := false;
    if ev.Channel     <> MidiData[c1].Channel     then TestResult := false;
    if ev.Data1       <> MidiData[c1].Data1       then TestResult := false;
    if ev.Data2       <> MidiData[c1].Data2       then TestResult := false;
    if ev.Deltaframes <> MidiData[c1].Deltaframes + kDeltaOffset then TestResult := false;
  end;

  OutputEvents := OutputBufferA.PrepareOutputBuffer(100);
  if OutputEvents^.numEvents <> 2 then TestResult := false;
  OutputBufferA.RemoveStaleEvents(100);

  OutputEvents := OutputBufferA.PrepareOutputBuffer(50 * MidiEventCount);
  if OutputEvents^.numEvents <> 14 then TestResult := false;
  OutputBufferA.RemoveStaleEvents(50 * MidiEventCount);

  OutputEvents := OutputBufferA.PrepareOutputBuffer(50 * MidiEventCount);
  if OutputEvents^.numEvents <> 0 then TestResult := false;


  // Clean up.
  InputBufferA.Free;
  InputBufferB.Free;
  OutputBufferA.Free;
  SetLength(MidiData, 0);
  result := TestResult;
end;


end.
