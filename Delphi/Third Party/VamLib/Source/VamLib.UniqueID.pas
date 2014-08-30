unit VamLib.UniqueID;

interface

type
  {
    TUniqueID
    I found myself wanting a type of unique identifier when
    writing the Animation, Throttler and Debouncer code. In each
    of these cases I've used one central object that processes
    multiple animation, throttling and debouncing requests.

    The advantage of using one central object is that only one timer
    object is used. Using one animation controller also makes it
    possible to easily control the global framerate and ensure
    multiple concurrent animations happen in the correct sequence.

    The disadvantage is the central controller needs some way
    to differentiate the request targets from each other. For example
    the animation controller needs to know if these two seperate
    animation requests are operating on the same target. If so,
    the first animation request should be stopped and replaced.

    My first idea was to use a constant ID value that would be
    set by the application developer. This will work if the
    application developer is disciplined in how they assign
    ID's to different sections of calling code.
  }


  TUniqueID = record
  private
  public
    Part1 : integer;
    Part2 : TDateTime;

    class operator Equal(a: TUniqueID; b : TUniqueID):boolean;
    class operator NotEqual(a: TUniqueID; b : TUniqueID):boolean;

    procedure Init;
    procedure Clear;

    function AsString : string;
  end;


implementation

uses
  SysUtils;

var
  GlobalCount : integer;

{$I InterlockedAPIs.inc}

{ TUniqueID }

procedure TUniqueID.Init;
begin
  Part1 := InterlockedIncrement(GlobalCount);
  Part2 := Now;
end;

function TUniqueID.AsString: string;
begin
  result :=  IntToStr(Part1) + '-' + FloatToStr(Part2);
end;

procedure TUniqueID.Clear;
begin
  self.Part1 := 0;
  self.Part2 := 0;
end;

class operator TUniqueID.Equal(a, b: TUniqueID): boolean;
begin
  // Check that the id's have been initialised.
  assert((a.Part1 <> 0) and (a.Part2 <> 0));
  assert((b.Part1 <> 0) and (b.Part2 <> 0));

  if (a.Part1 = b.Part1) and (a.Part2 = b.Part2)
    then result := true
    else result := false;
end;

class operator TUniqueID.NotEqual(a, b: TUniqueID): boolean;
begin
  // Check that the id's have been initialised.
  assert((a.Part1 <> 0) and (a.Part2 <> 0));
  assert((b.Part1 <> 0) and (b.Part2 <> 0));

  if (a.Part1 = b.Part1) and (a.Part2 = b.Part2)
    then result := false
    else result := true;
end;

initialization
  GlobalCount := 0;
finalization

end.
