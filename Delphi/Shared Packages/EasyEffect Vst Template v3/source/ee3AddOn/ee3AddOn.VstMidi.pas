unit ee3AddOn.VstMidi;

interface

uses
  DAEffect,
  DAEffectX,
  VamLib.Collections.Lists;

type
  TMidiStatus = record
  const
    NoteOn        = $90;
    NoteOff       = $80;
    ControlChange = $B0;
    PitchBend     = $E0;
    ProgramChange = $C0;
  end;

  TVstMidiList = class(TSimpleRecordList<VstMidiEvent>)
  end;


  // Extend VstMidiEvent with some helper functions.
  PVstMidiEventEx = ^TVstMidiEventEx;
  TVstMidiEventEx = record
  private
    {$Hints Off}
    //===== Field declarations must be identical to VstMidiEvent ======
    vType           : VstInt32;              // kVstMidiType
    byteSize        : VstInt32;              // sizeof (VstMidiEvent)
    deltaFrames     : VstInt32;              // sample frames related to the current block start sample position
    flags           : VstInt32;              // see VstMidiEventFlags
    noteLength      : VstInt32;              // (in sample frames) of entire note, if available, else 0
    noteOffset      : VstInt32;              // offset (in sample frames) into note from note start if available, else 0
    midiData        : array[0..3] of byte;   // 1 to 3 MIDI bytes; midiData[3] is reserved (zero)
    detune          : shortint;              // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
    noteOffVelocity : byte;                  // Note Off Velocity [0, 127]
    reserved1       : byte;                  // zero (Reserved for future use)
    reserved2       : byte;                  // zero (Reserved for future use)
    //==================================================================
    {$Hints On}
  public
    function Status:byte; inline;
    function Data1:byte; inline;
    function Data2:byte; inline;

    function IsNoteOn:boolean; inline;
    function IsNoteOff:boolean; inline;
    function IsControlChange:boolean; inline;
    function IsMidiPanic:boolean; inline;
    function IsPitchBend:boolean; inline;
    function IsModWheel:boolean; inline;
    function IsProgramChange:boolean; inline;
  end;

function VstMidiEventEx(ev:PVstMidiEvent):PVstMidiEventEx; inline;

implementation


function VstMidiEventEx(ev:PVstMidiEvent):PVstMidiEventEx;
begin
  result := PVstMidiEventEx(ev);
end;


{ TVstMidiEventEx }

function TVstMidiEventEx.Status: byte;
begin
  result := midiData[0] and $F0;
end;

function TVstMidiEventEx.Data1: byte;
begin
  result := midiData[1] and $7F;
end;

function TVstMidiEventEx.Data2: byte;
begin
  result := midiData[2] and $7F;
end;


function TVstMidiEventEx.IsControlChange: boolean;
begin
  if (Status = TMidiStatus.ControlChange) and (Data1 <> 120) and (Data1 <> 123)
    then result := true
    else result := false;
end;

function TVstMidiEventEx.IsMidiPanic: boolean;
begin
  // CC120 and CC123 are assigned to "All Sounds Off" and "All Notes Off" respectively,
  if (Status = TMidiStatus.ControlChange) and ((Data1 = 120) or (Data1 = 123))
    then result := true
    else result := false;
end;

function TVstMidiEventEx.IsModWheel: boolean;
begin
  if (Status = TMidiStatus.ControlChange) and (Data1 = 1)
    then result := true
    else result := false;
end;

function TVstMidiEventEx.IsNoteOff: boolean;
begin
  result := ((Status = TMidiStatus.NoteOn) and (Data2 = 0)) or (Status = TMidiStatus.NoteOff);
end;

function TVstMidiEventEx.IsNoteOn: boolean;
begin
  result := ((Status = TMidiStatus.NoteOn) and (Data2 > 0));
end;

function TVstMidiEventEx.IsPitchBend: boolean;
begin
  if (Status = TMidiStatus.PitchBend)
    then result := true
    else result := false;
end;

function TVstMidiEventEx.IsProgramChange: boolean;
begin
  if (Status = TMidiStatus.ProgramChange)
    then result := true
    else result := false;
end;

end.
