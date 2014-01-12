unit eeDspWindows;

interface

uses
  VamLib.MoreTypes;

type
  TWindowFunction = function(const x:double):double;


function Io(const x: Double): Double;

// NOTE: The window functions below are all normalised to use X in the range of
// 0 to 1. The window mid-point is at 0.5.
function BlackmanWindow(const x : double):double; inline;
function BlackmanHarrisWindow(const x : double):double; inline;
function BlackmanNuttallWindow(const x : double):double; inline;
function HammingWindow(const x : double):double; inline;


// NOTE: This is an older implementation of the KaiserBesselWindow() function.
// It needs to be re-implemented to use a 0..1 range for Offset (and rename to X)
function KaiserBesselWindow(const Offset:double; const HalfWindowWidth: integer; const Alpha: double):double; deprecated;


type
  TWindowType = (Blackman, BlackmanHarris, BlackmanNuttall);

procedure AppyWindowToBuffer(Buffer : PSingle; const SampleFrames : integer; const WindowType : TWindowType);

implementation

uses
  SysUtils,
  Math;



// function Io(const x: Double): Double;
//
// Io is a modified bessel function...
// function taken from DAV_DspWindowing.pas
//
// Docs: http://www.onesmallclue.com/wiki_private/index.php?title=Io
//
function Io(const x: Double): Double;
var
  y, de : Double;
  i     : Integer;
  sde   : Double;
const
  CEpsilon: Double = 1E-08;
begin
  y := 0.5 * x;
  de := 1.0;
  result := 1;
  for i := 1 to 25 do
  begin
    de := de * y / i;
    sde := sqr(de);
    result := result + sde;
    if (result * CEpsilon - sde) > 0  then break;
  end;
end;





// function KaiserBesselWindow(const Offset:single; const HalfWindowWidth: integer; const Alpha: Single):double;
//
// Function to generate a KaiserBesselWindow.
//
// The window peak is centered around 0.
//
// Offset is reference position used when calculating the (instantious) value of the window.
// HalfWindowWidth is the size of the window
// Alpha controls the shape of the window. range 1..12..
//
// Docs: http://www.onesmallclue.com/wiki_private/index.php?title=KaiserBesselWindow
//
function KaiserBesselWindow(const Offset:double; const HalfWindowWidth: integer; const Alpha: double):double;
var
  c1:double;
  k, p:double;
begin
  k := Offset + HalfWindowWidth-1;
  p := HalfWindowWidth-1; //Should p be an integer or float value???
  c1 := 1 - Power(((k-p) / p), 2);
  if c1 < 0
    then result := 0
    else result := Io(Alpha * sqrt(c1)) / Io(Alpha);

end;


function BlackmanWindow(const x : double):double; inline;
// source: http://en.wikipedia.org/wiki/Window_function#Generalized_Hamming_windows
const
  Alpha = 0.16;
  a0 = (1 - Alpha) * 0.5;
  a1 = 0.5;
  a2 = Alpha * 0.5;
begin
  assert((x >= 0) and (x <= 1), 'x is out of range');

  result := a0 - a1 * cos(2 * pi * x) + a2 * cos(4 * pi * x);
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

function BlackmanNuttallWindow(const x : double):double; inline;
// source: http://en.wikipedia.org/wiki/Window_function#Generalized_Hamming_windows
const
  a0 = 0.355768;
  a1 = 0.487396;
  a2 = 0.144232;
  a3 = 0.012604;
begin
  assert((x >= 0) and (x <= 1), 'x is out of range');
  result := a0 - a1 * cos(2 * pi * x) + a2 * cos(4 * pi * x) - a3 * cos(6 * pi * x);
end;


function HammingWindow(const x : double):double; inline;
begin
  result := 0.52 + 0.46 * cos(2 * pi * x);
end;



procedure AppyWindowToBuffer(Buffer : PSingle; const SampleFrames : integer; const WindowType : TWindowType);
var
  c1 : integer;
  sf : double;
begin
  case WindowType of
    Blackman:
    for c1 := 0 to SampleFrames-1 do
    begin
      sf := BlackmanWindow(c1 / (SampleFrames-1));
      Buffer^ := Buffer^ * sf;
      inc(Buffer);
    end;

    BlackmanHarris:
    for c1 := 0 to SampleFrames-1 do
    begin
      sf := BlackmanHarrisWindow(c1 / (SampleFrames-1));
      Buffer^ := Buffer^ * sf;
      inc(Buffer);
    end;

    BlackmanNuttall:
    for c1 := 0 to SampleFrames-1 do
    begin
      sf := BlackmanNuttallWindow(c1 / (SampleFrames-1));
      Buffer^ := Buffer^ * sf;
      inc(Buffer);
    end;

  else
    raise Exception.Create('Window type not handled.');
  end;
end;

end.
