unit VamLib.UniqueID;

interface

type
  TUniqueID = record
    Part1 : integer;
    Part2 : TDateTime;

    class operator Equal(a: TUniqueID; b : TUniqueID):boolean;
    class operator NotEqual(a: TUniqueID; b : TUniqueID):boolean;

    procedure Init;
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
