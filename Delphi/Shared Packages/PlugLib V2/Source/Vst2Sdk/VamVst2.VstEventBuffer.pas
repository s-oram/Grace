unit VamVst2.VstEventBuffer;

interface

uses
  VamVst2.DAEffectX;

type
  TVstEventBuffer = class
  private
    fVstEventArray : array of VstEvent;
    fBuffer : PVstEvents;
    fBufferSize : integer;
    fCapacity: integer;
    procedure SetCapacity(const Value: integer);
    function GetCount: integer;
    function GetEvent(Index: integer): PVstEvent;
  public
    constructor Create(const aCapacity : integer);
    destructor Destroy; override;

    procedure Clear;
    procedure AssignFrom(const ev : PVstEvents);

    procedure AddMidiEvent(const Status, Channel, Data1, Data2, Deltaframes: integer);
    procedure AddEvent(const ev : PVstEvent);

    property Buffer : PVstEvents read fBuffer;
    property Capacity : integer read fCapacity write SetCapacity;
    property Count : integer read GetCount;

    property Event [Index : integer] : PVstEvent read GetEvent; default;
  end;

implementation

uses
  VamVst2.VstEvent;

const
  kSizeOfVstEvent = SizeOf(VstEvent);

{ TVstEventBuffer }

constructor TVstEventBuffer.Create(const aCapacity : integer);
begin
  assert(aCapacity > 0);

  fBuffer := nil;
  fBufferSize := 0;
  SetCapacity(aCapacity);
end;

destructor TVstEventBuffer.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

function TVstEventBuffer.GetCount: integer;
begin
  assert(fBuffer <> nil);
  result := fBuffer^.numEvents;
end;

procedure TVstEventBuffer.Clear;
begin
  assert(fBuffer <> nil);
  fBuffer^.numEvents := 0;
end;

procedure TVstEventBuffer.AssignFrom(const ev: PVstEvents);
var
  c1: Integer;
begin
  // TODO:MED should the capacity automatically grow here?
  assert(fBuffer <> nil);
  assert(ev^.numEvents <= Capacity);

  fBuffer^.numEvents := ev^.numEvents;
  for c1 := 0 to ev^.numEvents do
  begin
    Move(ev^.events[c1]^, fBuffer^.events[c1]^, kSizeOfVstEvent);
  end;
end;

procedure TVstEventBuffer.AddEvent(const ev: PVstEvent);
begin
  // TODO:MED should the capacity automatically grow here?
  assert(Count < Capacity);

  Move(ev^, fBuffer^.events[Count]^, kSizeOfVstEvent);
  inc(fBuffer^.numEvents);
end;

procedure TVstEventBuffer.AddMidiEvent(const Status, Channel, Data1, Data2, Deltaframes: integer);
begin
  // TODO:MED should the capacity automatically grow here?
  assert(Count < Capacity);

  WriteMidiEventToVstEvent(fBuffer^.events[Count], Status, Channel, Data1, Data2, Deltaframes);
  inc(fBuffer^.numEvents);
end;

procedure TVstEventBuffer.SetCapacity(const Value: integer);
var
  c1: Integer;
begin
  if (fBuffer <> nil) then
  begin
    FreeMem(fBuffer, fBufferSize);
    fBuffer := nil;
    fBufferSize := 0;
    SetLength(fVstEventArray, 0);
  end;

  fCapacity := Value;

  if fCapacity > 0 then
  begin
    fBufferSize := SizeOf(VstEvents) + SizeOf(PVstEvent) * fCapacity;
    GetMem(fBuffer, fBufferSize);

    fBuffer^.numEvents := 0;
    fBuffer^.reserved  := 0;

    SetLength(fVstEventArray, fCapacity);

    for c1 := 0 to fCapacity-1 do
    begin
      fBuffer^.events[c1] := @fVstEventArray[c1];
    end;
  end;
end;

function TVstEventBuffer.GetEvent(Index: integer): PVstEvent;
begin
  assert(Index < Count);
  assert(fBuffer <> nil);
  result := fBuffer^.events[Index];
end;



end.
