unit VamVst2.MidiEventOutputBuffer;

interface

uses
  VamVst2.DAEffectX,
  VamVst2.MidiEvent;


type
  ///  TMidiEventOutputBuffer has been designed for use as a Midi output
  ///  buffer. It allows events to be queued for output at anytime and in
  ///  any order. The flexibility does incure extra processing overhead
  ///  as the output events need to be selected and sorted according
  ///  to their deltaframe value.
  TMidiEventOutputBuffer = class
  private
    MidiEventCount : integer;
    MidiEventIndex : integer;
    MidiEvents : array of VstEvent;
    InUse : array of boolean;
    FVstEventsData : PVstEvents;
    FVstEventMemCount : integer;
    FCapacity: integer;
    FGlobalDelta: integer; // GlobalMidiEventDelta.
  protected
    procedure SetCapacity(const Value: integer);
    function FindFreeEventIndex:integer;
  public
    constructor Create(const aCapacity : integer);
    destructor Destroy; override;

    procedure Reset;

    procedure IncrementGlobalDelta(const SampleFrames : integer);
    procedure ResetGlobalDelta;

    // Add a collection of events.
    procedure AddEvents(const ev : PVstEvents);
    procedure AddMidiEvent(const Status, Channel, Data1, Data2: integer); overload;
    procedure AddMidiEvent(const Status, Channel, Data1, Data2, DeltaOffset: integer); overload;

    function PrepareOutputBuffer(const SampleFrames : integer):PVstEvents;
    procedure RemoveStaleEvents(const SampleFrames : integer);

    property Capacity : integer read FCapacity;
    property Count : integer read MidiEventCount;

    property GlobalDelta : integer read FGlobalDelta;
  end;

function VerifyEventSort(const ev : PVstEvents):boolean;

implementation

uses
  VamVst2.VstEvent;

// NOTE: Disable range checking for this unit.
// The VstEvents buffer will trigger range check exceptions otherwise.
{$R-}

function VerifyEventSort(const ev : PVstEvents):boolean;
var
  c1 : integer;
  evA, evB : PVstEvent;
begin
  for c1 := 0 to ev^.numEvents-2 do
  begin
    evA := ev^.events[c1];
    evB := ev^.events[c1+1];
    if evA^.deltaFrames > evB^.deltaFrames then exit(false);
  end;
  // There are no problems with the sort if we make it this far.
  result := true;
end;

procedure SortEventsByDeltaFrames(const ev : PVstEvents);
var
  c2 : integer;
  SortIndex : integer;
  evA, evB, evC : PVstEvent;
  IsSwapRequired : boolean;
  SwapIndex : integer;
begin
  SortIndex := 0;
  SwapIndex := -1;

  while SortIndex < ev^.numEvents-1 do
  begin
    IsSwapRequired := false;
    evA := ev^.events[SortIndex];
    evB := ev^.events[SortIndex];

    for c2 := SortIndex+1 to ev.numEvents-1 do
    begin
      evC := ev^.events[c2];
      if evC^.deltaFrames < evB^.deltaFrames then
      begin
        evB := evC;
        IsSwapRequired := true;
        SwapIndex := c2;
      end;
    end;

    if IsSwapRequired then
    begin
      ev^.events[SortIndex] := evB;
      ev^.events[SwapIndex] := evA;
    end;

    inc(SortIndex);
  end;

  assert(VerifyEventSort(ev), 'MIDI event sort error.');
end;





{ TVstEventBuffer }

constructor TMidiEventOutputBuffer.Create(const aCapacity : integer);
begin
  MidiEventCount := 0;
  MidiEventIndex := 0;
  FVstEventsData := nil;
  FVstEventMemCount := 0;
  SetCapacity(aCapacity);
  FGlobalDelta := 0;
end;

destructor TMidiEventOutputBuffer.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

procedure TMidiEventOutputBuffer.SetCapacity(const Value: integer);
begin
  FCapacity := Value;

  if (Value = 0) then
  begin
    if (FVstEventsData <> nil) then
    begin
      FreeMem(FVstEventsData, FVstEventMemCount);
      FVstEventsData := nil;
      FVstEventMemCount := 0;
    end;
    SetLength(MidiEvents, 0);
    SetLength(InUse, 0);
  end;

  if (Value > 0) then
  begin
    if (FVstEventsData <> nil) then
    begin
      FreeMem(FVstEventsData, FVstEventMemCount);
      FVstEventsData := nil;
      FVstEventMemCount := 0;
    end;

    FVstEventMemCount := SizeOf(VstEvents) + SizeOf(PVstEvent) * FCapacity;
    GetMem(FVstEventsData, FVstEventMemCount);

    SetLength(MidiEvents, FCapacity);
    SetLength(InUse, FCapacity);
  end;

  if MidiEventIndex >= Value then MidiEventIndex := 0;
end;

procedure TMidiEventOutputBuffer.Reset;
var
  c1: Integer;
begin
  FGlobalDelta := 0;
  MidiEventIndex := 0;

  for c1 := 0 to Capacity-1 do
  begin
    InUse[c1] := false;
    MidiEventCount := 0;
  end;
end;

function TMidiEventOutputBuffer.FindFreeEventIndex: integer;
begin
  while (InUse[MidiEventIndex] <> false) do
  begin
    if MidiEventIndex < Capacity-1
      then inc(MidiEventIndex)
      else MidiEventIndex := 0;
  end;

  result := MidiEventIndex;

  InUse[MidiEventIndex] := true;

  if MidiEventIndex < Capacity-1
      then inc(MidiEventIndex)
      else MidiEventIndex := 0;
end;

procedure TMidiEventOutputBuffer.IncrementGlobalDelta(const SampleFrames: integer);
begin
  inc(FGlobalDelta, SampleFrames);
end;

procedure TMidiEventOutputBuffer.AddMidiEvent(const Status, Channel, Data1, Data2: integer);
var
  WriteIndex : integer;
  DestEv : PVstEvent;
begin
  inc(MidiEventCount);
  if MidiEventCount >= Capacity then SetCapacity(Capacity + 16);

  WriteIndex := FindFreeEventIndex;
  DestEv := @MidiEvents[WriteIndex];

  WriteMidiEventToVstEvent(DestEv, Status, Channel, Data1, Data2, GlobalDelta);
end;

procedure TMidiEventOutputBuffer.AddMidiEvent(const Status, Channel, Data1, Data2, DeltaOffset: integer);
var
  WriteIndex : integer;
  DestEv : PVstEvent;
  x : integer;
begin
  inc(MidiEventCount);
  if MidiEventCount >= Capacity then SetCapacity(Capacity + 16);

  WriteIndex := FindFreeEventIndex;
  DestEv := @MidiEvents[WriteIndex];

  x := GlobalDelta + DeltaOffset;
  if x < 0 then x := 0;
  WriteMidiEventToVstEvent(DestEv, Status, Channel, Data1, Data2, x);
end;





procedure TMidiEventOutputBuffer.AddEvents(const ev: PVstEvents);
const
  kSizeOfVstEvent = SizeOf(VstEvent);
var
  c1: Integer;
  WriteIndex : integer;
  DestEv : PVstEvent;
  SourceEv : PVstEvent;
begin
  for c1 := 0 to ev^.numEvents-1 do
  begin
    if ev^.events[c1]^.vType = kVstMidiType then
    begin
      inc(MidiEventCount);
      if MidiEventCount >= Capacity then SetCapacity(Capacity + 16);

      WriteIndex := FindFreeEventIndex;
      SourceEv := ev^.events[c1];
      DestEv := @MidiEvents[WriteIndex];
      Move(SourceEv^, DestEv^, kSizeOfVstEvent);
    end;
  end;
end;

function TMidiEventOutputBuffer.PrepareOutputBuffer(const SampleFrames : integer): PVstEvents;
var
  c1: Integer;
  NumberOfEvents : integer;
begin
  NumberOfEvents := 0;
  for c1 := 0 to Capacity-1 do
  begin
    if (InUse[c1]) and (MidiEvents[c1].deltaFrames < SampleFrames) then
    begin
      FVstEventsData^.events[NumberOfEvents] := @MidiEvents[c1];
      inc(NumberOfEvents);
    end;
  end;
  FVstEventsData^.numEvents := NumberOfEvents;

  SortEventsByDeltaFrames(FVstEventsData);

  result := FVstEventsData;
end;

procedure TMidiEventOutputBuffer.RemoveStaleEvents(const SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := 0 to Capacity-1 do
  begin
    if (InUse[c1]) then
    begin
      if (MidiEvents[c1].deltaFrames < SampleFrames) then
      begin
        InUse[c1] := false;
        dec(MidiEventCount);
        assert(MidiEventCount >= 0);
      end else
      begin
        dec(MidiEvents[c1].deltaFrames, SampleFrames);
      end;
    end;
  end;
end;

procedure TMidiEventOutputBuffer.ResetGlobalDelta;
begin
  FGlobalDelta := 0;
end;

end.
