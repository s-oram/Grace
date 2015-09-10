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
  public
    constructor Create(const aCapacity : integer);
    destructor Destroy; override;

    procedure Clear;
    procedure AssignFrom(ev : PVstEvents);

    property Buffer : PVstEvents read fBuffer;
    property Capacity : integer read fCapacity write SetCapacity;
  end;

implementation



{ TVstEventBuffer }

constructor TVstEventBuffer.Create(const aCapacity : integer);
begin
  fBuffer := nil;
  fBufferSize := 0;
  SetCapacity(aCapacity);
end;

destructor TVstEventBuffer.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

procedure TVstEventBuffer.Clear;
begin
  assert(fBuffer <> nil);
  fBuffer^.numEvents := 0;
end;

procedure TVstEventBuffer.AssignFrom(ev: PVstEvents);
const
  kSizeOfVstEvent = SizeOf(VstEvent);
var
  c1: Integer;
begin
  assert(fBuffer <> nil);
  assert(ev^.numEvents <= Capacity);

  fBuffer^.numEvents := ev^.numEvents;
  for c1 := 0 to ev^.numEvents do
  begin
    Move(ev^.events[c1], fBuffer^.events[c1], kSizeOfVstEvent);
  end;
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

    SetLength(fVstEventArray, fCapacity);

    for c1 := 0 to fCapacity-1 do
    begin
      fBuffer^.events[c1] := @fVstEventArray[c1];
    end;
  end;

end;

end.
