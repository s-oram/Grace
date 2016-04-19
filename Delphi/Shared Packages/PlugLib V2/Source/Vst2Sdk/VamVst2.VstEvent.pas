unit VamVst2.VstEvent;

interface

uses
  VamVst2.DAEffectX;

procedure WriteMidiEventToVstEvent(const Dest : PVstEvent; const Status, Channel, Data1, Data2, Deltaframes: integer);
procedure ReadMidiEventFromVstEvent(const Source : PVstEvent; out Status, Channel, Data1, Data2, Deltaframes: integer);

implementation

procedure WriteMidiEventToVstEvent(const Dest : PVstEvent; const Status, Channel, Data1, Data2, Deltaframes: integer);
var
  Event:PVstMidiEvent;
begin
  assert(SizeOf(Event) <= SizeOf(Dest));

  Event := PVstMidiEvent(Dest);

  Event^.vType := kVstMidiType;
  Event^.byteSize := sizeof(VstMidiEvent);
  Event^.flags := 0;
  Event^.noteLength := 0;
  Event^.noteOffset := 0;

  Event^.midiData[0] := Channel + Status;
  Event^.midiData[1] := Data1;
  Event^.midiData[2] := Data2;
  Event^.deltaFrames := Deltaframes;
end;

procedure ReadMidiEventFromVstEvent(const Source : PVstEvent; out Status, Channel, Data1, Data2, Deltaframes: integer);
begin
  assert( Source^.vType = kVstMidiType );
  assert( Source^.byteSize >= sizeof(VstMidiEvent) );

  Channel     := VstMidiEvent(Source^).midiData[0] and $0F;
  Status      := VstMidiEvent(Source^).midiData[0] and $F0;
  Data1       := VstMidiEvent(Source^).midiData[1] and $7F;
  Data2       := VstMidiEvent(Source^).midiData[2] and $7F;
  DeltaFrames := VstMidiEvent(Source^).deltaFrames;
end;


end.
