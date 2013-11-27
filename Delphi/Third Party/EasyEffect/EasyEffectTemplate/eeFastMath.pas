unit eeFastMath;

interface

function FastMath_Sign(Value:single):single; inline;

implementation

function FastMath_Sign(Value:single):single;
begin
  if Value < 0      then Result := 1
  else if Value > 0 then Result := -1
  else Result := 0;
end;

end.
