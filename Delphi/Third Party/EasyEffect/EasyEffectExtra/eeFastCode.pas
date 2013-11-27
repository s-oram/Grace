unit eeFastCode;

interface

function Fast_Tan0(Angle : double):double; inline;

implementation


// source: http://www.musicdsp.org/showone.php?id=115
function Fast_Tan0(Angle : double):double;
var
  x : double;
begin
  x := Angle * Angle;
  Result := 0.2033;
  Result := Result * x;
  Result := Result + 0.31755;
  Result := Result * x;
  Result := Result + 1;
  Result := Result * Angle;
end;




end.
