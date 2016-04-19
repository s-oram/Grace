unit VamLib.UniqueID;

interface

type
  TUniqueID = record
  private
    Part1 : integer; //Part1 is a globally incremented counter value.
    Part2 : integer; //Part2 is a randomly generated number.
  public
    class operator Equal(a: TUniqueID; b : TUniqueID):boolean;
    class operator NotEqual(a: TUniqueID; b : TUniqueID):boolean;

    procedure Init;  // Init() generates a unique ID.
    procedure Clear; // Clear() resets the unique ID to zero.

    function IsSet : boolean; // returns true if the ID hasn't been zero'd.

    function AsString : string;

    procedure Assign(const Source : TUniqueID);
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
  Part2 := Random(High(Integer));
end;

function TUniqueID.IsSet: boolean;
begin
  if (self.Part1 = 0) and (self.Part2 = 0)
    then result := false
    else result := true;
end;

procedure TUniqueID.Assign(const Source: TUniqueID);
begin
  self.Part1 := Source.Part1;
  self.Part2 := Source.Part2;
end;

function TUniqueID.AsString: string;
begin
  result :=  IntToStr(Part1) + '-' + IntToStr(Part2);
end;

procedure TUniqueID.Clear;
begin
  self.Part1 := 0;
  self.Part2 := 0;
end;

class operator TUniqueID.Equal(a, b: TUniqueID): boolean;
begin
  if (a.Part1 = b.Part1) and (a.Part2 = b.Part2)
    then result := true
    else result := false;
end;

class operator TUniqueID.NotEqual(a, b: TUniqueID): boolean;
begin
  if (a.Part1 = b.Part1) and (a.Part2 = b.Part2)
    then result := false
    else result := true;
end;

initialization
  GlobalCount := 0;
finalization

end.
