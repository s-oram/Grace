unit B2.Dsp;

interface


function LinearInterpolation(f, y0, y1:double):double; inline;

implementation

function LinearInterpolation(f, y0, y1:double):double; inline;
begin
  result := y0 + f * (y1 - y0);
end;

end.
