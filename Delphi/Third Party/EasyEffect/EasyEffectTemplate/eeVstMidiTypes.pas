unit eeVstMidiTypes;

interface

uses
  VamVst2.DAEffectX;

procedure SortVstEvents(Events:PVstEvents);

implementation

//TODO: SortMidiEvents could probably do with a faster algorithm
procedure SortVstEvents(Events:PVstEvents);
var
  c1:integer;
  pe1,pe2:PVstEvent;
  te:VstEvent;
  SizeOfVstEvent:integer;
begin
  SizeOfVstEvent := SizeOf(VstEvent);

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


end.
