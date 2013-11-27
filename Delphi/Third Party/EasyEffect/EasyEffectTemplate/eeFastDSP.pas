{
  Fast DSP contains "fast" versions of regular math/DSP functions. They are fast
  because they have been optimised or approximate the results of the regular function.

  Some functions sacrifice numerical accuracy in exchange for execution speed.

  When using these functions in place of their normal counterparts, care will need to
  be taken that these functions fullfil the requirements of the calling code. (ie, that
  they are accurate enough, and will not introduce errors)
}


unit eeFastDSP;

interface





function Fast_Tanh(const x: double): double; inline;


implementation

// function Fast_Tanh()
//
// Notes :
// This is a rational function to approximate a tanh-like soft clipper. It is based
// on the pade-approximation of the tanh function with tweaked coefficients.
//
// The function is in the range x=-3..3 and outputs the range y=-1..1. Beyond this
// range the output must be clamped to -1..1.
//
// The first to derivatives of the function vanish at -3 and 3, so the transition
// to the hard clipped region is C2-continuous.
//
// When driven hard this funcion will create up to five harmonics. The fifth harmonic
// is very faint. Four times oversampling would probably be enough to effectively
// band-limit this function when used as a clipper. 
//
// Source:     http://www.musicdsp.org/showone.php?id=238
// Posted by:  mdsp.
function Fast_Tanh(const x: double): double; inline;
begin
  if x < -3 then
  begin
    result := -1
  end else
  if x > 3 then
  begin
    result := 1
  end else
  begin
    result :=  x * ( 27 + x * x ) / ( 27 + 9 * x * x );
  end;
end;




end.
