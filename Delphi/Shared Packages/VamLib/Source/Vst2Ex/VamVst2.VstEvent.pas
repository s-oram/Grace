unit VamVst2.VstEvent;

interface

uses
  VamVst2.DAEffectX;

procedure WriteMidiEventToVstEvent(const Dest : PVstEvent; const Status, Channel, Data1, Data2, Deltaframes: integer);

implementation

procedure WriteMidiEventToVstEvent(const Dest : PVstEvent; const Status, Channel, Data1, Data2, Deltaframes: integer);
var
  Event:PVstMidiEvent;
begin
  assert(SizeOf(Event) <= SizeOf(Dest));

  Event := PVstMidiEvent(Dest);

  Event^.vType := kVstMidiType;
  Event^.byteSize := sizeof(Event);
  Event^.flags := 0;
  Event^.noteLength := 0;
  Event^.noteOffset := 0;

  Event^.midiData[0] := Channel + Status;
  Event^.midiData[1] := Data1;
  Event^.midiData[2] := Data2;
  Event^.deltaFrames := Deltaframes;
end;


end.
