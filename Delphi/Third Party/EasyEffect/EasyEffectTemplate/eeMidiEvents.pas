{
  I think this MidiEventList class is too complicated to maintain because it uses
  a array of pointers and an array of midi events.

  One of the design dificulties is that the list must not create records/objects during normal
  operation, but only when the maximum size of the list changes. Any operations that require memory
  to be created/freed causes spikes in the VST processing time. (Which should be avoided whereever possible).     

  At the same time it needs to be possible to 'nil' a VST event to simulate deleting it. Maybe the best path
  forward would be to create a new VST class that contains the record and a pointer to it. The class can be created
  once and stored in ojbect list. To simulate deletion, the midi event pointer of the class would be set to nil
  and the container class object would be moved to the end of the object list.


  MIDI NOTE 36 = Cubase MIDI C1


}


unit eeMidiEvents;

interface

uses
  Contnrs;

const
  kNoteOn        = $90;
  kNoteOff       = $80;
  kControlChange = $B0;
  kPitchBend     = $E0;

  {
  MidiEvent Status values.
  - NoteOn:  (status = $90) and (data2 > 0)
  - NoteOff: (status = $90) and (data2 = 0)
  - NoteOff: (status = $80)
  - Control Change: (status = $B0)
  }

type
  PeeMidiEvent = ^TeeMidiEvent;
  TeeMidiEvent = class
  public
    Status:byte;
    Channel:byte;
    Data1:byte;
    Data2:byte;
    Deltaframes:longint;
    procedure AssignFrom(Source:TeeMidiEvent);
  end;

  TArrayOfeeMidiEvent = array of TeeMidiEvent;

  {
    TeeMidiEventBuffer is a buffer of midi events. It automatically creates and frees
    the TeeMidiEvent's as the buffer size is increased and decreased.

    This class exists because I wanted to have a buffer of midi events but didn't want to think
    about creating or freeing the events as the buffer size changed.
  }

  TeeMidiEventBuffer = class
  private
    fEvents: TArrayOfeeMidiEvent;
    fBufferSize: integer;
    procedure SetBufferSize(const NewBufferSize: integer);
  public
    constructor Create;
	  destructor Destroy; override;

    property BufferSize:integer read fBufferSize write SetBufferSize;
    property Events:TArrayOfeeMidiEvent read fEvents write fEvents;
  end;





  {
    TeeMidiEventList needs to should not need to request or free memory during normal usage.

    BufferSize, MaxBufferSize.
  }

  PeeMidiEventList = ^TeeMidiEventList;
  TeeMidiEventList = class
  private
    //List:TObjectList; //All references to 'List' should eventually be removed.
    fBufferSize:integer;
    fEventCount:integer;
    function GetEvent(Index: integer): TeeMidiEvent;
    procedure SetBufferSize(const NewBufferSize:integer);
    function GetEventAsPointer(Index: integer): PeeMidiEvent;
  protected
    EventBuffer:TeeMidiEventBuffer;       //Contains buffer of midi events.
    EventPointers:array of PeeMidiEvent;  //Can be nil, or point to one of the midi events.
    property BufferSize:integer read fBufferSize write SetBufferSize;
  public
    constructor Create;
	  destructor Destroy; override;

    function AddEvent(aEvent:TeeMidiEvent):integer; overload;
    function AddEventCopy(aEvent:TeeMidiEvent):integer;
    function AddEvent(Status, Channel, Data1, Data2, Deltaframes:longint):integer; overload;

    procedure ClearEvents;
    procedure DeleteEvent(Index:integer);
    procedure Pack;

    procedure SortEventsByDeltaFrames;

    procedure FilterEvents(SampleFrames:integer);

    property EventCount                    :integer      read fEventCount;
    property Events[Index:integer]         :TeeMidiEvent read GetEvent;  default;
    property EventAsPointer[Index:integer] :PeeMidiEvent read GetEventAsPointer;
  end;

function IsNoteOn(MidiEvent:TeeMidiEvent):boolean; inline;
function IsNoteOff(MidiEvent:TeeMidiEvent):boolean; inline;
function IsControlChange(MidiEvent:TeeMidiEvent):boolean; inline;
function IsMidiPanic(MidiEvent:TeeMidiEvent):boolean; inline;
function IsPitchBend(MidiEvent:TeeMidiEvent):boolean; inline;
function IsModWheel(MidiEvent:TeeMidiEvent):boolean; inline;

//GetPitchBendAmount() result range is -1..1
function GetPitchBendAmount(MidiEvent:TeeMidiEvent):single; inline;

implementation

uses
  Classes;

function IsNoteOn(MidiEvent:TeeMidiEvent):boolean; inline;
begin
  result := ((MidiEvent.Status = $90) and (MidiEvent.Data2 > 0));
end;

function IsNoteOff(MidiEvent:TeeMidiEvent):boolean; inline;
begin
  result := ((MidiEvent.Status = $90) and (MidiEvent.Data2 = 0)) or (MidiEvent.Status = $80);
end;

function IsControlChange(MidiEvent:TeeMidiEvent):boolean; inline;
begin
  // Checks for the Control change status, some CC messages are exclude as they have special purposes...
  // CC-120: All Sounds Off
  // CC-123: All Notes Off
  if (MidiEvent.Status = kControlChange) and (MidiEvent.Data1 <> 120) and (MidiEvent.Data1 <> 123)
    then result := true
    else result := false;
end;

function IsMidiPanic(MidiEvent:TeeMidiEvent):boolean; inline;
begin
  // CC120 and CC123 are assigned to "All Sounds Off" and "All Notes Off" respectively,
  if (MidiEvent.Status = kControlChange) and ((MidiEvent.Data1 = 120) or (MidiEvent.Data1 = 123))
    then result := true
    else result := false;
end;

function IsPitchBend(MidiEvent:TeeMidiEvent):boolean; inline;
begin
  if (MidiEvent.Status = kPitchBend)
    then result := true
    else result := false;
end;

function IsModWheel(MidiEvent:TeeMidiEvent):boolean; inline;
begin
  if (MidiEvent.Status = kControlChange) and (MidiEvent.Data1 = 1)
    then result := true
    else result := false;
end;

function GetPitchBendAmount(MidiEvent:TeeMidiEvent):single; inline;
var
  Bend:integer;
  BendFloat:single;
begin
  Bend      := (MidiEvent.Data1) + (MidiEvent.Data2 shl 7);
  BendFloat := Bend * (1/16383) * 2 - 1;
  result    := BendFloat;
  assert(result >= -1);
  assert(result <= 1);
end;


function SortFunctionDeltaFrames(Item1, Item2: Pointer): Integer;
var
  ev1,ev2:TeeMidiEvent;
  d1,d2:integer;
begin
  ev1 := PeeMidiEvent(Item1)^ as TeeMidiEvent;
  ev2 := PeeMidiEvent(Item2)^ as TeeMidiEvent;

  d1 := ev1.Deltaframes;
  d2 := ev2.Deltaframes;

  if d1 < d2 then result := 1
  else
  if d1 > d2 then result := -1
  else
    result := 0;


  {
  if PeeMidiEvent(Item1)^.Deltaframes < PeeMidiEvent(Item2)^.Deltaframes then result := -1
  else if PeeMidiEvent(Item1)^.Deltaframes < PeeMidiEvent(Item2)^.Deltaframes then result := 1
  else Result := 0;
  }
end;





{ TeeMidiEventBuffer }

constructor TeeMidiEventBuffer.Create;
begin
  fBufferSize := 0;
end;

destructor TeeMidiEventBuffer.Destroy;
begin
  BufferSize := 0;
  inherited;
end;


procedure TeeMidiEventBuffer.SetBufferSize(const NewBufferSize: integer);
var
  c1: Integer;
begin
  if NewBufferSize > fBufferSize then
  begin
    SetLength(fEvents, NewBufferSize);
    for c1 := fBufferSize to NewBufferSize - 1 do
    begin
      fEvents[c1] := TeeMidiEvent.Create;
    end;
  end;

  if NewBufferSize < fBufferSize then
  begin
    for c1 := fBufferSize-1 downto NewBufferSize do
    begin
      fEvents[c1].Free;
    end;
    SetLength(fEvents, NewBufferSize);
  end;

  fBufferSize := NewBufferSize;
end;




{ TeeMidiEventList }

constructor TeeMidiEventList.Create;
begin
  //List := TObjectList.Create;
  //List.OwnsObjects := true;

  EventBuffer := TeeMidiEventBuffer.Create;
  fBufferSize := 0;
  fEventCount := 0;
end;

destructor TeeMidiEventList.Destroy;
begin
  BufferSize := 0;
  //List.Free;
  EventBuffer.Free;
  inherited;
end;

function TeeMidiEventList.GetEvent(Index: integer): TeeMidiEvent;
begin
  //result := List[Index] as TeeMidiEvent;
  if Index >= fEventCount then raise EListError.Create('Index outside of list range.');
  result := EventPointers[Index]^;
end;

function TeeMidiEventList.GetEventAsPointer(Index: integer): PeeMidiEvent;
begin
  if Index >= fEventCount then raise EListError.Create('Index outside of list range.');
  result := EventPointers[Index];
end;

procedure TeeMidiEventList.Pack;
var
  c1:integer;
  Index1:integer;
begin
  // NOTE: WARNING: The pack method assumes all non-nil EventPointers point to
  // the midi event in the event buffer at the same index as it. This stops events
  // from being reordered by swapping pointers.
  //

  Index1 := 0;

  for c1 := 0 to EventCount - 1 do
  begin
    if (EventPointers[c1] <> nil) then
    begin
      if Index1 = c1 then
      begin
        inc(Index1)
      end else
      begin
        EventBuffer.Events[Index1] := EventBuffer.Events[c1];
        EventPointers[Index1] := @EventBuffer.Events[Index1];
        inc(index1);
      end;
    end;
  end;

  fEventCount := Index1;
end;

procedure TeeMidiEventList.SetBufferSize(const NewBufferSize: integer);
var
  c1: Integer;
begin
  fBufferSize := NewBufferSize;
  EventBuffer.BufferSize := NewBufferSize;
  SetLength(EventPointers, NewBufferSize);

  if NewBufferSize < fEventCount then fEventCount := NewBufferSize;

  // NOTE: Because we've changed the size of the buffer, we need to
  // update all events to point to the new location of existing events.
  for c1 := 0 to NewBufferSize - 1 do
  begin
    if EventPointers <> nil then EventPointers[c1] := @EventBuffer.Events[c1];
  end;
end;

procedure TeeMidiEventList.SortEventsByDeltaFrames;
//var
//  Index1,Index2:integer;
//  ev1,ev2:TeeMidiEvent;
begin
  //List.Sort(SortFunctionDeltaFrames);
  {
  if EventCount < 2 then exit;


  Index1 := 0;
  Index2 := 1;
  while Index2 < EventCount do
  begin
    ev1 := Events[Index1];
    ev2 := Events[Index2];

    if ev1.Deltaframes > ev2.Deltaframes then
    begin
      List.Exchange(Index1,Index2);
      Index1 := 0;
      Index2 := 1;
    end else
    begin
      inc(Index1);
      inc(Index2);
    end;

  end;
  }

end;

function TeeMidiEventList.AddEvent(aEvent: TeeMidiEvent): integer;
begin
  //NOTE: Events are 'owned' by the list. So they must not be free'ed elsewhere.
  //If wanting to add a copy of an event, and retain ownership (and thus the responsibility
  //to free the event) use the AddEventCopy method.
  //result := List.Add(aEvent);

  if EventCount = BufferSize then BufferSize := BufferSize + 10;

  EventBuffer.Events[EventCount].AssignFrom(aEvent);
  EventPointers[EventCount] := @EventBuffer.Events[EventCount];
  inc(fEventCount);

  // IMPORTANT
  // The caller epects the event to be free'ed, so lets do it.
  aEvent.Free;
  //-----------------------------------------------------------

  result := EventCount-1;
end;

function TeeMidiEventList.AddEventCopy(aEvent: TeeMidiEvent): integer;
//var
  //ev:TeeMidiEvent;
begin
  //ev := TeeMidiEvent.Create;
  //ev.AssignFrom(aEvent);
  //result := List.Add(ev);

  if EventCount = BufferSize then BufferSize := BufferSize + 10;

  EventBuffer.Events[EventCount].AssignFrom(aEvent);
  EventPointers[EventCount] := @EventBuffer.Events[EventCount];
  inc(fEventCount);

  result := EventCount-1;
end;



procedure TeeMidiEventList.DeleteEvent(Index: integer);
begin
  //List.Delete(Index);
  EventPointers[Index] := nil;
  Pack;

end;

function TeeMidiEventList.AddEvent(Status, Channel, Data1, Data2, Deltaframes: longint): integer;
//var
  //ev:TeeMidiEvent;
begin
  {
  ev := TeeMidiEvent.Create;
  ev.Status := Status;
  ev.Channel := Channel;
  ev.Data1 := Data1;
  ev.Data2 := Data2;
  ev.Deltaframes := DeltaFrames;

  result := List.Add(ev);
  }

  if EventCount = BufferSize then BufferSize := BufferSize + 10;

  EventBuffer.Events[EventCount].Status      := Status;
  EventBuffer.Events[EventCount].Channel     := Channel;
  EventBuffer.Events[EventCount].Data1       := Data1;
  EventBuffer.Events[EventCount].Data2       := Data2;
  EventBuffer.Events[EventCount].Deltaframes := DeltaFrames;

  EventPointers[EventCount] := @EventBuffer.Events[EventCount];

  inc(fEventCount);

  result := EventCount-1;
end;

procedure TeeMidiEventList.ClearEvents;
begin
  fEventCount := 0;
end;

procedure TeeMidiEventList.FilterEvents(SampleFrames: integer);
// Checks for out of order events, and events with invalid deltaFrame values.
// SampleFrames is equal to the audio buffer size of the current process block. 
var
  c1:integer;
  LastDelta:integer;
begin
  LastDelta := SampleFrames-1;
  for c1 := EventCount - 1 downto 0 do
  begin
    if Events[c1].Deltaframes < 0 then Events[c1].Deltaframes := 0;
    if Events[c1].Deltaframes > LastDelta then Events[c1].Deltaframes := LastDelta;
    LastDelta := Events[c1].Deltaframes;
  end;
end;






{ TeeMidiEvent }

procedure TeeMidiEvent.AssignFrom(Source: TeeMidiEvent);
begin
  Self.Status      := Source.Status;
  Self.Channel     := Source.Channel;
  Self.Data1       := Source.Data1;
  Self.Data2       := Source.Data2;
  Self.Deltaframes := Source.Deltaframes;
end;

end.
