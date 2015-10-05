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

implementation

end.
