unit Test.VamVst2.MidiEventInputBuffer;

interface

uses
  WatchTower;

function TestA:boolean;

implementation

uses
  VamVst2.MidiEvent,
  VamVst2.MidiEventInputBuffer;

function TestA:boolean;
const
  MidiEventCount = 16;
var
  TestResult : boolean;
  InputBufferA, InputBufferB : TMidiEventInputBuffer;
  MidiData : TArrayOfMidiEvent;
  c1: Integer;
  ev : TMidiEvent;
begin
  TestResult := true;

  InputBufferA := TMidiEventInputBuffer.Create(4);
  InputBufferB := TMidiEventInputBuffer.Create(4);
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

  if InputBufferA.Capacity < Length(MidiData) then
  begin
    TestResult := false;
  end;

  for c1 := 0 to MidiEventCount-1 do
  begin
    ev := InputBufferA.ReadMidiEvent(c1);
    if ev.Status      <> MidiData[c1].Status      then TestResult := false;
    if ev.Channel     <> MidiData[c1].Channel     then TestResult := false;
    if ev.Data1       <> MidiData[c1].Data1       then TestResult := false;
    if ev.Data2       <> MidiData[c1].Data2       then TestResult := false;
    if ev.Deltaframes <> MidiData[c1].Deltaframes then TestResult := false;
  end;

  InputBufferB.AssignFrom(InputBufferA.Buffer);

  if InputBufferB.Buffer^.numEvents <> InputBufferA.Buffer^.numEvents then TestResult := false;

  for c1 := 0 to MidiEventCount-1 do
  begin
    ev := InputBufferB.ReadMidiEvent(c1);
    if ev.Status      <> MidiData[c1].Status      then TestResult := false;
    if ev.Channel     <> MidiData[c1].Channel     then TestResult := false;
    if ev.Data1       <> MidiData[c1].Data1       then TestResult := false;
    if ev.Data2       <> MidiData[c1].Data2       then TestResult := false;
    if ev.Deltaframes <> MidiData[c1].Deltaframes then TestResult := false;
  end;

  // Clean up.
  InputBufferA.Free;
  InputBufferB.Free;
  SetLength(MidiData, 0);
  result := TestResult;
end;

end.
