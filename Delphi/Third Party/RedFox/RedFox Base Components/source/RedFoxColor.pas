unit RedFoxColor;

interface

uses
  Graphics, Agg2D, AggColor;

type
  PRedFoxColorString = ^TRedFoxColorString;
  TRedFoxColorString = string;

  PRedFoxColor = ^TRedFoxColor;
  TRedFoxColor = record
    class operator Implicit(a : TRedFoxColor):TColor;
    class operator Implicit(a : TRedFoxColorString):TRedFoxColor;
    class operator Implicit(a : TRedFoxColor):TRedFoxColorString;
    class operator Implicit(a: TRedFoxColor):TAggRgba8;
    class operator Implicit(a: TAggRgba8):TRedFoxColor;
    class operator Equal(a: TRedFoxColor; b: TRedFoxColor) : Boolean;
    class operator NotEqual(a: TRedFoxColor; b: TRedFoxColor): Boolean;



    procedure SetColor(A, R, G, B: Byte); overload;
    procedure SetColor(AggRgba8:TAggRgba8); overload;
    procedure SetColor(Hexdecimal:string); overload;
    function AsAggRgba8:TAggRgba8;
    function AsString:string;

    //=== Color Manipulation Functions ===
    // NOTE: I don't know much about graphics programming. As such
    // these functions will probably be modified in the future.
    procedure AdjustLightness(AdjustBy:double); //range -1..1.
    //====================================

    //======Chainable functions ===========
    class function RandomColor(AlphaValue : byte = 255):TRedFoxColor; static;
    function WithAlpha(AlphaValue : byte):TRedFoxColor;      //returns current color with new alpha value.
    function WithAlphaBlend(AlphaValue : byte):TRedFoxColor; //returns current color multiplied with applied alpha value.
    //====================================

    case integer of
      0: (ARGB: integer);
      1: (A, R, G, B: Byte);
      2: (Bytes: array [0..3] of Byte);
  end;



function GetTColor(Hexdecimal:string):TColor;

function GetAggColor(Color:TColor; Alpha:byte = 255):TAggColorRgba8; inline;
function GetRedFoxColor(Color:TColor; Alpha:byte = 255):TRedFoxColor; overload; inline;
function GetRedFoxColor(Hexdecimal:string):TRedFoxColor; overload; inline;
function GetRedFoxColor(Hexdecimal:string; Alpha : byte):TRedFoxColor; overload; inline;




// These color conversions functions were copied from
// http://www.programmersheaven.com/download/15373/Download.aspx
// Additionally, this was found in the file.
//     The following code is from Foley & Van Dam:  "Fundamentals of Interactive
//     Computer Graphics"  (found on the net, and converted to Pascal).   It
//     performs the conversion in both directions:
procedure RGBtoHSL(R,G,B: double; var H,S,L: double);
procedure HSLtoRGB(H,S,L: double; var R,G,B: double);
function HuetoRGB(m1,m2,h: double): double;
//==============================================================================

function ColorFade(const ColorA, ColorB: TRedFoxColor; const FadeAmt : byte):TRedFoxColor;
function ColorFadeF(const ColorA, ColorB: TRedFoxColor; const FadeAmt : single):TRedFoxColor;

function ByteCrossFade(const x1, x2, f : byte):byte;


implementation

uses
  SysUtils;

//linearly interpolates x1 to x2. f is the 'interpolation factor.
// f range is 0..255.
// f = 0   - no interpolation.
// f = 255 - full interpolation.
function ByteCrossFade(const x1, x2, f : byte):byte;
begin
  result := ((x1 * (256 - f)) + (x2 * f)) shr 8;
end;


function GetTColor(Hexdecimal:string):TColor;
var
  rfc : TRedFoxColor;
begin
  rfc := GetRedFoxColor(HexDecimal);
  result := (rfc.R) + (rfc.G shl 8) + (rfc.B shl 16)
end;


function GetAggColor(Color:TColor; Alpha:byte = 255):TAggColorRgba8;
var
  RealColor : integer;
begin
  // ColorToRGB() will automatically convert system theme colors (example clButtonFace)
  // to actual RGB values.
  RealColor := ColorToRGB(Color);

  result.R := RealColor          AND $000000FF;
  result.G := (RealColor shr 8)  AND $000000FF;
  result.B := (RealColor shr 16) AND $000000FF;
  result.A := Alpha;
end;

function GetRedFoxColor(Color:TColor; Alpha:byte = 255):TRedFoxColor;
var
  RealColor : integer;
begin
  // ColorToRGB() will automatically convert system theme colors (example clButtonFace)
  // to actual RGB values.
  RealColor := ColorToRGB(Color);

  result.R := RealColor          AND $000000FF;
  result.G := (RealColor shr 8)  AND $000000FF;
  result.B := (RealColor shr 16) AND $000000FF;
  result.A := Alpha;
end;

function GetRedFoxColor(Hexdecimal:string):TRedFoxColor;
begin
  result.SetColor(Hexdecimal);
end;

function GetRedFoxColor(Hexdecimal:string; Alpha : byte):TRedFoxColor;
begin
  result.SetColor(Hexdecimal);
  result.A := Alpha;
end;

// Four byte swap
// Taken from: https://forums.embarcadero.com/thread.jspa?threadID=55185
function Swap4 (i: Longint): Longint;  register; overload;
asm
  bswap eax;
end;

function Swap4 (i: cardinal): cardinal;  register; overload;
asm
  bswap eax;
end;

procedure RGBtoHSL(R,G,B: double; var H,S,L: double);
var
  cmax,cmin,delta : double;
begin
  cmax := B;
  cmin := B;
  if R > cmax then cmax := R;
  if G > cmax then cmax := G;
  if R < cmin then cmin := R;
  if G < cmin then cmin := G;
  L := (cmax + cmin)/2.0;
  if (cmax=cmin) then
  begin
     S := 0.0;
     H := 0.0;   {actually it's undefined}
  end else
  begin
    delta := cmax-cmin;
    if (L < 0.5) then S := delta/(cmax+cmin) else S := delta/(2.0-cmax-cmin);
    if (r=cmax) then
      H := (g-b)/delta
    else if (g=cmax) then
      H := 2.0 + (b-r)/delta
    else H := 4.0+(r-g)/delta;
    H := H/6.0;
    if (H < 0.0) then H := H+1;
  end;
end;

procedure HSLtoRGB(H,S,L: double; var R,G,B: double);
var
  m1,m2 : double;
begin
if (S = 0.0) then
begin
    R := L;
    G := L;
    B := L;
end else
begin
    if (L <= 0.5) then m2 := L*(1.0+S) else m2 := L+S-(L*S);
    m1 := 2.0 * L - m2;
    R := HuetoRGB(m1,m2,H+1.0/3.0);
    G := HuetoRGB(m1,m2,H);
    B := HuetoRGB(m1,m2,H-1.0/3.0);
end;
end;

function HuetoRGB(m1,m2,h: double): double;
begin
  if (h < 0) then h := h + 1.0;
  if (h > 1) then h := h - 1.0;

  if (6.0 * h < 1) then
  begin
      result := (m1+(m2-m1)*h*6.0)
  end else
  begin
      if (2.0 * h < 1)
        then result := m2
        else
      if (3.0*h < 2.0)
        then result := (m1+(m2-m1)*((2.0/3.0)-h)*6.0)
        else result := m1;
  end;
end;

{ TRedFoxColor }

function TRedFoxColor.AsAggRgba8: TAggRgba8;
begin
  result.A := Self.A;
  result.R := Self.R;
  result.G := Self.G;
  result.B := Self.B;
end;

procedure TRedFoxColor.SetColor(AggRgba8: TAggRgba8);
begin
  Self.A := AggRgba8.A;
  Self.R := AggRgba8.R;
  Self.G := AggRgba8.G;
  Self.B := AggRgba8.B;
end;

function TRedFoxColor.AsString: string;
begin
  result := '$' + IntToHex(Swap4(ARGB),8);
end;

class operator TRedFoxColor.Equal(a, b: TRedFoxColor): Boolean;
begin
  result := (a.ARGB = b.ARGB);
end;

class operator TRedFoxColor.Implicit(a: TRedFoxColorString): TRedFoxColor;
begin
  result.SetColor(a);
end;

class operator TRedFoxColor.Implicit(a: TRedFoxColor): TRedFoxColorString;
begin
  result := a.AsString;
end;

class operator TRedFoxColor.Implicit(a: TRedFoxColor): TAggRgba8;
begin
  result.Initialize(a.R, a.G, a.B, a.A);
end;

class operator TRedFoxColor.Implicit(a: TAggRgba8): TRedFoxColor;
begin
  result.SetColor(a.A, a.R, a.G, a.B);
end;

class operator TRedFoxColor.Implicit(a: TRedFoxColor): TColor;
begin
  result := a.R + (a.G shl 8) + (a.B shl 16);
end;

class operator TRedFoxColor.NotEqual(a, b: TRedFoxColor): Boolean;
begin
  result := (a.ARGB <> b.ARGB);
end;

class function TRedFoxColor.RandomColor(AlphaValue: byte): TRedFoxColor;
begin
  result.SetColor(AlphaValue, Random(255), Random(255), Random(255));
end;

function TRedFoxColor.WithAlpha(AlphaValue: byte): TRedFoxColor;
begin
  result := self;
  result.A := AlphaValue;
end;

function TRedFoxColor.WithAlphaBlend(AlphaValue: byte): TRedFoxColor;
begin
  result   := self;
  result.A := (self.A * AlphaValue) shr 8;
end;

procedure TRedFoxColor.SetColor(A, R, G, B: Byte);
begin
  self.R := R;
  self.G := G;
  self.B := B;
  self.A := A;
end;

procedure TRedFoxColor.SetColor(Hexdecimal: string);
begin
  self.ARGB := Swap4(StrToInt(Hexdecimal));
end;

procedure TRedFoxColor.AdjustLightness(AdjustBy: double);
// NOTE: I'm not sure if this function is working correctly.
var
  dRed, dGreen, dBlue : double;
  dHue, dSaturation, dLightness : double;
begin
  assert(AdjustBy >= -1);
  assert(AdjustBy <= 1);

  if AdjustBy = 0 then exit;

  dRed   := self.R / 255;
  dGreen := self.G / 255;
  dBlue  := self.B / 255;

  RgbToHsl(dRed, dGreen, dBlue, dHue, dSaturation, dLightness);

  dLightness := dLightness + AdjustBy;
  if dLightness > 1 then dLightness := 1;
  if dLightness < 0 then dLightness := 0;

  HslToRgb(dHue, dSaturation, dLightness, dRed, dGreen, dBlue);

  // NOTE: Rounding here isn't quite correct but it will do for now.
  // It should be floor(x * 256); if x = 256 then x := 255;
  self.R := round(dRed   * 255);
  self.G := round(dGreen * 255);
  self.B := round(dBlue  * 255);
end;


function ColorFade(const ColorA, ColorB: TRedFoxColor; const FadeAmt : byte):TRedFoxColor;
begin
  if FadeAmt = 0 then
  begin
    result := ColorA;
  end else
  if FadeAmt = 255 then
  begin
    result := ColorB;
  end else
  begin
    result.A := ByteCrossFade(ColorA.A, ColorB.A, FadeAmt);
    result.R := ByteCrossFade(ColorA.R, ColorB.R, FadeAmt);
    result.G := ByteCrossFade(ColorA.G, ColorB.G, FadeAmt);
    result.B := ByteCrossFade(ColorA.B, ColorB.B, FadeAmt);
  end;
end;

function ColorFadeF(const ColorA, ColorB: TRedFoxColor; const FadeAmt : single):TRedFoxColor;
var
  fx : byte;
begin
  assert(FadeAmt >= 0);
  assert(FadeAmt <= 1);

  if FadeAmt = 0 then
  begin
    result := ColorA;
  end else
  if FadeAmt = 1 then
  begin
    result := ColorB;
  end else
  begin
    fx := round(FadeAmt * 255);
    result.A := ByteCrossFade(ColorA.A, ColorB.A, fx);
    result.R := ByteCrossFade(ColorA.R, ColorB.R, fx);
    result.G := ByteCrossFade(ColorA.G, ColorB.G, fx);
    result.B := ByteCrossFade(ColorA.B, ColorB.B, fx);
  end;
end;


end.

