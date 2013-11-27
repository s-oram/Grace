{
    A monophonic synth needs to keep track of which keys have been pressed, released
    and in which order. At any point in time the synth needs to know the
    last key that was pressed. TNoteStack is a class to do all of above.
  }

unit soNoteStack;

interface

type
  PNoteData = ^TNoteData;
  TNoteData = record
    IsValid  : boolean;
    Data1    : byte; //MIDI Note.
    Data2    : byte; //MIDI Velocity.
    procedure AssignFrom(const Source : TNoteData);
  end;

  TNoteStack = class
  private
  protected
    const
      kNoteStackSize = 32; //size is arbitary. Could be smaller or bigger.
    var
      NoteStack : array[0..kNoteStackSize-1] of TNoteData;
      StackCount : integer;
    procedure CondenseStack;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddNote(const Data1, Data2 : byte);
    function RemoveNote(Note : byte):boolean;

    function LastNote : PNoteData;

    property Count : integer read StackCount;
  end;

implementation



{ TNoteStack }

constructor TNoteStack.Create;
begin
  Clear;
end;

destructor TNoteStack.Destroy;
begin

  inherited;
end;

procedure TNoteStack.Clear;
var
  c1: Integer;
begin
  for c1 := 0 to kNoteStackSize-1 do
  begin
    NoteStack[c1].IsValid := false;
  end;

  StackCount := 0;
end;


procedure TNoteStack.AddNote(const Data1, Data2 : byte);
begin
  NoteStack[StackCount].IsValid  := true;
  NoteStack[StackCount].Data1    := Data1;
  NoteStack[StackCount].Data2    := Data2;

  inc(StackCount);

  if StackCount >= kNoteStackSize then CondenseStack;
end;


function TNoteStack.RemoveNote(Note: byte):boolean;
var
  c1: Integer;
  IsMostRecentNote : boolean;
begin
  if StackCount = 0 then
  begin
    result := false;
    exit; //===========>>
  end;

  // Check if the latest note has been released, if so, return true to
  // indicate that the active note has changed and the synth voice will
  // need to be retriggered / released accordingly.
  if (NoteStack[StackCount-1].Data1 = Note)
    then IsMostRecentNote := true
    else IsMostRecentNote := false;

  for c1 := 0 to StackCount-1 do
  begin
    if NoteStack[c1].Data1 = Note
      then NoteStack[c1].IsValid := false;
  end;

  while (StackCount > 0) and (NoteStack[StackCount-1].IsValid = false) do
  begin
    dec(StackCount);
  end;


  result := IsMostRecentNote;
end;


function TNoteStack.LastNote: PNoteData;
begin
  if StackCount > 0 then
  begin
    result := @NoteStack[StackCount-1]
  end else
  begin
    result := nil;
  end;
end;

procedure TNoteStack.CondenseStack;
var
  ReadIndex, WriteIndex : integer;
  c1: Integer;
begin
  // Null some old notes to ensure there is some empty room to condense
  // the stack. Don't null the very first note, NoteStack[0], as this
  // will be the oldest.
  NoteStack[1].IsValid := false; //second oldest.
  NoteStack[2].IsValid := false; //third oldest
  NoteStack[3].IsValid := false; //..and so on...
  NoteStack[4].IsValid := false;

  WriteIndex := 0;

  for ReadIndex := 0 to kNoteStackSize-1 do
  begin
    if (NoteStack[ReadIndex].IsValid) then
    begin
      NoteStack[WriteIndex].AssignFrom(NoteStack[ReadIndex]);
      inc(WriteIndex);
    end;
  end;

  StackCount := WriteIndex;
end;



{ TNoteData }

procedure TNoteData.AssignFrom(const Source: TNoteData);
begin
  Self.IsValid  := Source.IsValid;
  Self.Data1    := Source.Data1;
  Self.Data2    := Source.Data2;
end;

end.
