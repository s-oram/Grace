unit AudioIO_DSP;

interface

function Sinc(x:double):double; overload; inline;
function Sinc(const x, cutoff : double):double; overload; inline;

function BlackmanHarrisWindow(const x : double):double; inline;

implementation

// function Sinc(x:double):double;
// http://en.wikipedia.org/wiki/Sinc_function
// Docs: http://www.onesmallclue.com/wiki_private/index.php?title=Sinc&action=submit
function Sinc(x:double):double;
begin
  if x = 0
    then result := 1
    else result := sin(Pi * x) / (pi * x);
end;

function Sinc(const x, cutoff : double):double;
begin
  // Could this be called a Bandlimited sinc function?
  // Cutoff is normalised to 0..1.
  // NOTE: This function produces a kind of bandlimited sinc function.
  assert((Cutoff >= 0) and (Cutoff <= 1));

  if x <> 0
    then result := Sin(pi * x * Cutoff)/(pi * x)
    else result := Cutoff;
end;



function BlackmanHarrisWindow(const x : double):double;
// source: http://en.wikipedia.org/wiki/Window_function#Generalized_Hamming_windows
const
  a0 = 0.35875;
  a1 = 0.48829;
  a2 = 0.14128;
  a3 = 0.01168;
begin
  assert((x >= 0) and (x <= 1), 'x is out of range');
  result := a0 - a1 * cos(2 * pi * x) + a2 * cos(4 * pi * x) - a3 * cos(6 * pi * x);
end;



end.
