unit VamVst2.MidiEvent;

interface


type
  kMidiEventStatus = record
    const NoteOn  = $90;
    const NoteOff = $80;
    const ControlChange = $B0;
    const PitchBend = $E0;
  end;

type
  PMidiEvent = ^TMidiEvent;
  TMidiEvent = record
    Status, Channel, Data1, Data2, Deltaframes: integer;
  end;

  TArrayOfMidiEvent = array of TMidiEvent;


function IsMidiNoteOn(const ev : TMidiEvent):boolean;
function IsMidiNoteOff(const ev : TMidiEvent):boolean;

implementation

function IsMidiNoteOn(const ev : TMidiEvent):boolean;
begin
  result := ((ev.Status = kMidiEventStatus.NoteOn) and (ev.Data2 > 0));
end;

function IsMidiNoteOff(const ev : TMidiEvent):boolean;
begin
  result :=  ((ev.Status = kMidiEventStatus.NoteOn) and (ev.Data2 = 0)) or (ev.Status = kMidiEventStatus.NoteOff);
end;

end.
