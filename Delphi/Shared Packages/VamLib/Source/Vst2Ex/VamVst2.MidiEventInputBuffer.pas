unit VamVst2.MidiEventInputBuffer;

interface

uses
  VamVst2.DAEffectX,
  VamVst2.MidiEvent,
  VamVst2.VstEvent;

type
  ///  TMidiEventBuffer is a minimal event buffer class. It's been built
  ///  with the goal of using it as an input buffer. As such, it expects
  ///  the midi input to be well formed. No checks have been put in
  ///  place to ensure it is. Might do that in a future update.
  ///  There is no functionality to manipulate the buffer.

  TMidiEventInputBuffer = class
  private
    fVstEventArray : array of VstEvent;
    fBuffer : PVstEvents;
    fBufferSize : integer;
    fCapacity: integer;
    procedure SetCapacity(const Value: integer);
  public
    constructor Create(const aCapacity : integer);
    destructor Destroy; override;

    function ReadMidiEvent(const Index : integer):TMidiEvent;

    procedure AssignFrom(const ev : PVstEvents); overload;
    procedure AssignFrom(const ev : TArrayOfMidiEvent); overload;

    property Buffer : PVstEvents read fBuffer;
    property Capacity : integer read fCapacity write SetCapacity;
  end;

implementation

uses
  Test.VamVst2.MidiEventInputBuffer;

// NOTE: Disable range checking for this unit.
// The VstEvents buffer will trigger range check exceptions otherwise.
{$R-}


{ TVstEventBuffer }

constructor TMidiEventInputBuffer.Create(const aCapacity : integer);
begin
  fBuffer := nil;
  fBufferSize := 0;
  SetCapacity(aCapacity);
end;

destructor TMidiEventInputBuffer.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

procedure TMidiEventInputBuffer.AssignFrom(const ev: PVstEvents);
const
  kSizeOfVstEvent = SizeOf(VstEvent);
var
  c1: Integer;
  WriteIndex : integer;
  DestEv : PVstEvent;
  SourceEv : PVstEvent;
begin
  if ev^.numEvents > Capacity then Capacity := ev^.numEvents;

  WriteIndex := 0;
  for c1 := 0 to ev^.numEvents-1 do
  begin
    if ev^.events[c1]^.vType = kVstMidiType then
    begin
      DestEv := @fVstEventArray[c1];
      SourceEv := ev^.events[c1];
      Move(SourceEv^, DestEv^, kSizeOfVstEvent);
      inc(WriteIndex);
    end;
  end;

  fBuffer^.numEvents := WriteIndex;
end;

procedure TMidiEventInputBuffer.AssignFrom(const ev: TArrayOfMidiEvent);
var
  DestEv : PVstEvent;
  NumberOfEvents : integer;
  c1: Integer;
begin
  NumberOfEvents := Length(ev);
  if NumberOfEvents > Capacity then Capacity := NumberOfEvents;

  for c1 := 0 to NumberOfEvents-1 do
  begin
    DestEv := @fVstEventArray[c1];
    WriteMidiEventToVstEvent(DestEv, ev[c1].Status, ev[c1].Channel, ev[c1].Data1, ev[c1].Data2, ev[c1].Deltaframes);
  end;

  fBuffer.numEvents := NumberOfEvents;
end;

function TMidiEventInputBuffer.ReadMidiEvent(const Index: integer): TMidiEvent;
var
  ev : PVstMidiEvent;
begin
  assert(fBuffer <> nil);
  assert(Index < fBuffer.numEvents);

  ev := PVstMidiEvent(fBuffer^.events[Index]);
  assert(ev^.vType = kVstMidiType);

  result.Channel     := ev^.midiData[0] and $0F;
  result.Status      := ev^.midiData[0] and $F0;
  result.Data1       := ev^.midiData[1] and $7F;
  result.Data2       := ev^.midiData[2] and $7F;
  result.DeltaFrames := ev^.deltaFrames;
end;

procedure TMidiEventInputBuffer.SetCapacity(const Value: integer);
var
  c1: Integer;
begin
  fCapacity := Value;

  if (Value = 0) then
  begin
    if (fBuffer <> nil) then
    begin
      FreeMem(fBuffer, fBufferSize);
      fBuffer := nil;
      fBufferSize := 0;
    end;
    SetLength(fVstEventArray, 0);
  end;

  if (Value > 0) then
  begin
    if (fBuffer <> nil) then
    begin
      FreeMem(fBuffer, fBufferSize);
      fBuffer := nil;
      fBufferSize := 0;
    end;

    fBufferSize := SizeOf(VstEvents) + SizeOf(PVstEvent) * fCapacity;
    GetMem(fBuffer, fBufferSize);

    SetLength(fVstEventArray, fCapacity);
  end;

  // As the last step, point the VstEvents event pointers to the event array.
  if (fBuffer <> nil) then
  begin
    for c1 := 0 to fCapacity-1 do
    begin
      fBuffer^.events[c1] := @fVstEventArray[c1];
    end;
    fBuffer^.numEvents := 0;
  end;
end;

end.
