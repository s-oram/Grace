unit eeVstMidiTypes;

interface

{$INCLUDE DVstCommon.inc}

// NOTE: These type definitions have been copied from the DAEffectX.pas
// I've done this to break dependence on using the VST template units in many units.
// When sending events to vst plugins they can be cast to native VST types.
//
// I wonder if this is till the case? I wonder if there should be a better way?
// I wonder..........


//-------------------------------------------------------------------------------------------------------
// Integral Types
//-------------------------------------------------------------------------------------------------------
type
    VstInt16 = smallint;    // 16 bit integer type
    VstInt32 = longint;     // 32 bit integer type
    VstInt64 = int64;       // 64 bit integer type

    // pointers
    PVstInt16 = ^VstInt16;
    PVstInt32 = ^VstInt32;
    PVstInt64 = ^VstInt64;

//-------------------------------------------------------------------------------------------------------
// Generic Types
//-------------------------------------------------------------------------------------------------------
type
    // platform-dependent integer type, same size as pointer
    {$IFDEF VST_64BIT_PLATFORM}
    VstIntPtr = VstInt64;
    {$ELSE}
    VstIntPtr = VstInt32;
    {$ENDIF}

    // pointer
    PVstIntPtr = ^VStIntPtr;


//------------------------------------------------------------------------------
// VstEvent
// A generic timestamped event.
//------------------------------------------------------------------------------
type
    PVstEvent = ^VstEvent;
    VstEvent = packed record
      vType       : VstInt32;               // see VstEventTypes
      byteSize    : VstInt32;               // size of this event, excl. type and byteSize
      deltaFrames : VstInt32;               // sample frames related to the current block start sample position
      flags       : VstInt32;               // generic flags, none defined yet
      data        : array[0..15] of byte;   // data size may vary, depending on event type
    end;


//------------------------------------------------------------------------------
// VstEvent Types used by VstEvent.
//------------------------------------------------------------------------------
type
    VstEventTypes = VstInt32;

const
     kVstMidiType   = 1;    // MIDI event  @see VstMidiEvent
     kVstSysExType  = 6;    // MIDI system exclusive  @see VstMidiSysexEvent

     {$IFDEF VST_USE_DEPRECATED}
     kVstAudioType     = 2; // (deprecated) unused event type
     kVstVideoType     = 3; // (deprecated) unused event type
     kVstParameterType = 4; // (deprecated) unused event type
     kVstTriggerType   = 5; // (deprecated) unused event type
     {$ENDIF}


//------------------------------------------------------------------------------
// A block of events for the current processed audio block.
//------------------------------------------------------------------------------
type
    PVstEvents = ^VstEvents;
    VstEvents = packed record
      numEvents  : VstInt32;		              // number of Events in array
      reserved   : VstIntPtr;		              // zero (Reserved for future use)
      events     : array[0..1] of PVstEvent;  // event pointer array, variable size
      //events     : array of PVstEvent;  // event pointer array, variable size
    end;



//------------------------------------------------------------------------------
// MIDI Event (to be casted from VstEvent)
//------------------------------------------------------------------------------
type
    PVstMidiEvent = ^VstMidiEvent;
    VstMidiEvent = packed record
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
    end;


//------------------------------------------------------------------------------
// Flags used in #VstMidiEvent
//------------------------------------------------------------------------------
type
    VstMidiEventFlags = VstInt32;

const
     kVstMidiEventIsRealtime = 1 shl 0;   // means that this event is played life (not in playback from a sequencer track).\n This allows the Plug-In to handle these flagged events with higher priority, especially when the Plug-In has a big latency (AEffect::initialDelay)


//------------------------------------------------------------------------------
// MIDI Sysex Event (to be casted from #VstEvent)
//------------------------------------------------------------------------------
type
    PVstMidiSysExEvent = ^VstMidiSysExEvent;
    VstMidiSysExEvent = packed record
      vType       : VstInt32;   // #kVstSysexType
      byteSize    : VstInt32;   // sizeof(VstMidiSysexEvent)
      deltaFrames : VstInt32;   // sample frames related to the current block start sample position
      flags       : VstInt32;   // none defined yet (should be zero)
      dumpBytes   : VstInt32;   // byte size of sysexDump
      resvd1      : VstIntPtr;  // zero (Reserved for future use)
      sysexDump   : pbyte;      // sysex dump
      resvd2      : VstIntPtr;  // zero (Reserved for future use)
    end;



procedure SortVstEvents(Events:PVstEvents);

implementation

var
  SizeOfVstEvent:integer;


//TODO: SortMidiEvents could probably do with a faster algorithm
procedure SortVstEvents(Events:PVstEvents);
var
  c1:integer;
  pe1,pe2:PVstEvent;
  te:VstEvent;
begin
  c1 := 0;

  while c1 < Events^.numEvents-2 do
  begin
    pe1 := Events^.events[c1];
    pe2 := Events^.events[c1 + 1];

    if pe1^.deltaFrames > pe2^.deltaFrames then
    begin
      Move(pe2^, te, SizeOfVstEvent);
      Move(pe1^, pe2^, SizeOfVstEvent);
      Move(te, pe1^, SizeOfVstEvent);

      c1 := 0;
    end else
    begin
      inc(c1);
    end;          
  end;
  
end;

initialization
  SizeOfVstEvent := SizeOf(VstEvent);

end.
