unit uVstMidiInput;

interface

uses
  DAEffect, DAEffectX;

const
  // MIDI Status values...
  kNoteOn        = $90;
  kNoteOff       = $80;
  kControlChange = $B0;
  kPitchBend     = $E0;

type
  TVstEvents = record
    numEvents  : VstInt32;		              // number of Events in array
    reserved   : VstIntPtr;		              // zero (Reserved for future use)
    events     : array[0..1] of PVstEvent;  // event pointer array, variable size
  end;

  {
  TVstMidiInputBuffer has been designed to buffer Vst Midi Events. The 'buffered'
  vst midi events are sent as a single block of events to the vst plugin for
  processing.
  }

  TVstMidiInputBuffer = class
  private
    fMidiEventCount: integer;
    fMidiEventCapacity: integer;
    procedure SetMidiEventCapacity(const Value: integer);
  protected
    ReservedEvents : integer;
    fVstEventsPointer : PVstEvents;
    VstEventData   : PVstEvents;
    MidiEventData    : array of VstMidiEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddMidiEvent(const Channel, Status, Data1, Data2 : byte; const DeltaFrames : integer);
    procedure Clear;

    property MidiEventCount : integer read fMidiEventCount;
    property MidiEventCapacity : integer read fMidiEventCapacity write SetMidiEventCapacity;

    property VstEventsPointer : PVstEvents read fVstEventsPointer;
  end;

implementation


procedure GetMemForVstEvents(var ev : PVstEvents; const NumberOfEvents : integer);
var
  Size : integer;
begin
  Size := SizeOf(VstEvents) + SizeOf(VstMidiEvent) * NumberOfEvents;
  GetMem(ev, Size);
end;

procedure FreeMemForVstEvents(var ev : PVstEvents; const NumberOfEvents : integer);
var
  Size : integer;
begin
  Size := SizeOf(VstEvents) + SizeOf(VstMidiEvent) * NumberOfEvents;
  FreeMem(ev, Size);
  ev := nil;
end;

{ TVstMidiInputBuffer }

constructor TVstMidiInputBuffer.Create;
begin
  ReservedEvents := 0;

  MidiEventCapacity := 10;

  VstEventData^.numEvents := 0;
  fMidiEventCount := 0;
end;

destructor TVstMidiInputBuffer.Destroy;
begin
  MidiEventCapacity := 0;
  inherited;
end;

procedure TVstMidiInputBuffer.Clear;
begin
  fMidiEventCount := 0;
  VstEventData^.numEvents := 0;
end;

procedure TVstMidiInputBuffer.SetMidiEventCapacity(const Value: integer);
var
  c1: Integer;
begin
  fMidiEventCapacity := Value;
  SetLength(MidiEventData, Value);


  if ReservedEvents > 0 then
  begin
    FreeMemForVstEvents(VstEventData, ReservedEvents);
    ReservedEvents := 0;
  end;

  if Value > 0 then
  begin
    GetMemForVstEvents(VstEventData, Value);
    ReservedEvents := Value;
  end;





  for c1 := 0 to Value-1 do
  begin
    MidiEventData[c1].vType           := kVstMidiType;
    MidiEventData[c1].byteSize        := SizeOf(VstMidiEvent);
    MidiEventData[c1].flags           := 0;
    MidiEventData[c1].noteLength      := 0;
    MidiEventData[c1].noteOffset      := 0;
    MidiEventData[c1].detune          := 0;
    MidiEventData[c1].noteOffVelocity := 0;
    MidiEventData[c1].reserved1       := 0;
    MidiEventData[c1].reserved2       := 0;
    MidiEventData[c1].midiData[3]     := 0;

    VstEventData^.events[c1] := @MidiEventData[c1];
  end;

  fVstEventsPointer := VstEventData;
end;

procedure TVstMidiInputBuffer.AddMidiEvent(const Channel, Status, Data1, Data2: byte; const DeltaFrames : integer);
var
  Index : integer;
begin
  if MidiEventCount = MidiEventCapacity then MidiEventCapacity := MidiEventCapacity + 10;

  Index := MidiEventCount;

  MidiEventData[Index].deltaFrames := DeltaFrames;
  MidiEventData[Index].midiData[0] := Channel or Status;
  MidiEventData[Index].midiData[1] := Data1;
  MidiEventData[Index].midiData[2] := Data2;

  inc(fMidiEventCount);
  VstEventData^.numEvents := MidiEventCount;

end;



end.
